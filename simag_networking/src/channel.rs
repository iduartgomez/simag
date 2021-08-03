//! A stream and subscription handling behaviour/middleware to plug-in into libp2p swarm.
//!
//! This will open a socket with a peer accepting connections on-demand and expose it to
//! the library network handling layer to send/receive data packets. The connection will be
//! kept alive as long as necessary in order to transfer data.
//!
//! The protocol also has the following responsabilities:
//! - propagating changes in the Identify and Kademlia protocols to this stream
//!   (e.g. openning or closing new connections based on those protocols states).
//! - openning subscription, multi-producer/multi-consumer, channels on-demand and
//!   handling authorization and automatic subscription/unsubscription on changing internal state.

use crate::message::Message;
use handler::{ChannelHandler, ChannelHandlerEvent, ChannelHandlerInEvent};
use libp2p::{
    core::connection::ConnectionId,
    futures::{stream, AsyncRead, AsyncWrite, Sink},
    swarm::{
        NegotiatedSubstream, NetworkBehaviour, NetworkBehaviourAction, NotifyHandler,
        PollParameters, ProtocolsHandler,
    },
    Multiaddr, PeerId,
};
use protocol::SimagStream;
use smallvec::SmallVec;
use std::{
    collections::{HashMap, VecDeque},
    pin::Pin,
    task::{Context, Poll},
};

pub(crate) const CHANNEL_PROTOCOL: &str = "/simag/channel/0.1.0";

pub(crate) enum ChannelEvent {
    MessageReceived { peer: PeerId, msg: Message },
    ConnectionError { peer: PeerId, err: std::io::Error },
}

type NotifyKeepAlive = VecDeque<NetworkBehaviourAction<ChannelHandlerInEvent, ChannelEvent>>;
type NotifyShutdown = VecDeque<NetworkBehaviourAction<ChannelHandlerInEvent, ChannelEvent>>;

pub(crate) struct Channel {
    addresses: HashMap<PeerId, Vec<Multiaddr>>,
    /// open connections to a peer;
    /// never should have more than one inbound/outbound connection
    open_conn: HashMap<PeerId, SmallVec<[ChannelHandlerEvent; 3]>>,
    /// encoded messages pending to be sent to a given peer
    pending_messages: HashMap<PeerId, Vec<Message>>,
    // FIFO keep alive queue
    notify_keepalive: NotifyKeepAlive,
    // FIFO shutdown queue
    notify_shutdown: NotifyShutdown,
}

impl Channel {
    pub fn new() -> Channel {
        Channel {
            addresses: HashMap::new(),
            open_conn: HashMap::new(),
            pending_messages: HashMap::new(),
            notify_keepalive: VecDeque::new(),
            notify_shutdown: VecDeque::new(),
        }
    }

    pub fn send_message(&mut self, peer: &PeerId, msg: Message) {
        if let Some(queue) = self.pending_messages.get_mut(peer) {
            queue.push(msg);
        } else {
            let queue = vec![msg];
            self.pending_messages.insert(peer.clone(), queue);
        }
    }

    pub fn add_address(&mut self, peer: &PeerId, address: Multiaddr) {
        if let Some(queue) = self.addresses.get_mut(peer) {
            queue.push(address);
        } else {
            let addrs = vec![address];
            self.addresses.insert(peer.clone(), addrs);
        }
    }

    fn poll_send_msg(
        send_conn: &mut SimagStream<NegotiatedSubstream>,
        msg: Message,
        cx: &mut Context,
    ) -> Result<bool, std::io::Error> {
        match Sink::poll_ready(Pin::new(send_conn), cx) {
            Poll::Ready(Ok(())) => match Sink::start_send(Pin::new(send_conn), msg) {
                Ok(()) => match Sink::poll_flush(Pin::new(send_conn), cx) {
                    Poll::Pending => Ok(false),      /* not finished */
                    Poll::Ready(Ok(())) => Ok(true), /* finished */
                    Poll::Ready(Err(err)) => Err(err),
                },
                Err(err) => Err(err),
            },
            Poll::Pending => Ok(true), /* no message, so finished */
            Poll::Ready(Err(err)) => Err(err),
        }
    }

    fn poll_rcv_msg(
        rcv_conn: &mut SimagStream<NegotiatedSubstream>,
        cx: &mut Context,
    ) -> Result<(bool, Option<Message>), std::io::Error> {
        match stream::Stream::poll_next(Pin::new(rcv_conn), cx) {
            Poll::Ready(Some(Ok(msg))) => {
                // msg received
                Ok((false, Some(msg)))
            }
            Poll::Ready(Some(Err(err))) => Err(err),
            Poll::Ready(None) | Poll::Pending => Ok((false, None)),
            // Poll::Pending => Ok((true, None)),
        }
    }

    fn keep_alive(queue: &mut NotifyKeepAlive, peer: PeerId) {
        queue.push_back(NetworkBehaviourAction::NotifyHandler {
            peer_id: peer,
            event: ChannelHandlerInEvent::KeepAlive,
            handler: NotifyHandler::Any,
        });
    }

    fn shutdown(queue: &mut NotifyShutdown, peer: PeerId, refresh: bool) {
        queue.push_back(NetworkBehaviourAction::NotifyHandler {
            peer_id: peer,
            event: ChannelHandlerInEvent::Finished(refresh),
            handler: NotifyHandler::Any,
        });
    }
}

impl NetworkBehaviour for Channel {
    type ProtocolsHandler = ChannelHandler;
    type OutEvent = ChannelEvent;

    fn new_handler(&mut self) -> Self::ProtocolsHandler {
        ChannelHandler::new()
    }

    fn addresses_of_peer(&mut self, peer_id: &PeerId) -> Vec<Multiaddr> {
        self.addresses.get(peer_id).cloned().unwrap_or_default()
    }

    fn inject_connected(&mut self, peer_id: &PeerId) {
        log::debug!("Peer #{} connected", peer_id);
        self.open_conn
            .entry(peer_id.clone())
            .or_default()
            .push(ChannelHandlerEvent::Requested(true));
    }

    fn inject_disconnected(&mut self, peer_id: &PeerId) {
        log::debug!("Peer #{} disconnected", peer_id);
        self.addresses.remove(peer_id);
        self.open_conn.remove(peer_id);
    }

    fn inject_event(
        &mut self,
        peer_id: PeerId,
        _connection: ConnectionId,
        mut event: ChannelHandlerEvent,
    ) {
        let connections = self.open_conn.entry(peer_id).or_default();
        let mut replaced = false;
        for conn in connections.iter_mut() {
            let same_type = (conn.inbound_request() && event.inbound_request())
                || (conn.outbound_request() && event.outbound_request());
            if same_type {
                if let ChannelHandlerEvent::OutOpenChannel { flushing: true, .. } = conn {
                    let err_msg: &str =
                        "tried to replace an outbound channel while flushing apending message";
                    log::error!("{}", err_msg);
                    panic!("{}", err_msg);
                }
                // replace the existing connection for the new one
                event = std::mem::replace(conn, event);
                replaced = true;
                break;
            }
        }
        if !replaced {
            connections.push(event);
        }
    }

    fn poll(
        &mut self,
        cx: &mut Context,
        _params: &mut impl PollParameters,
    ) -> Poll<NetworkBehaviourAction<<ChannelHandler as ProtocolsHandler>::InEvent, ChannelEvent>>
    {
        if let Some(notification) = self.notify_keepalive.pop_front() {
            return Poll::Ready(notification);
        }

        for (peer, open_conn) in &mut self.open_conn {
            for conn in open_conn {
                match conn {
                    ChannelHandlerEvent::OutOpenChannel {
                        ref mut channel,
                        ref mut flushing,
                    } => {
                        if *flushing {
                            match Sink::poll_flush(Pin::new(channel), cx) {
                                Poll::Pending => {
                                    Self::keep_alive(&mut self.notify_keepalive, peer.clone());
                                }
                                Poll::Ready(Ok(_)) => {
                                    Self::shutdown(&mut self.notify_shutdown, peer.clone(), true);
                                    *flushing = false;
                                }
                                Poll::Ready(Err(err)) => {
                                    Self::shutdown(&mut self.notify_shutdown, peer.clone(), true);
                                    let ev = NetworkBehaviourAction::GenerateEvent(
                                        ChannelEvent::ConnectionError {
                                            peer: peer.clone(),
                                            err,
                                        },
                                    );
                                    return Poll::Ready(ev);
                                }
                            }
                        } else {
                            match self.pending_messages.get_mut(peer) {
                                Some(pending) if !pending.is_empty() => {
                                    while let Some(msg) = pending.pop() {
                                        match Channel::poll_send_msg(channel, msg, cx) {
                                            Err(err) => {
                                                Self::shutdown(
                                                    &mut self.notify_shutdown,
                                                    peer.clone(),
                                                    true,
                                                );
                                                let ev = NetworkBehaviourAction::GenerateEvent(
                                                    ChannelEvent::ConnectionError {
                                                        peer: peer.clone(),
                                                        err,
                                                    },
                                                );
                                                return Poll::Ready(ev);
                                            }
                                            Ok(done) => {
                                                if done {
                                                    Self::shutdown(
                                                        &mut self.notify_shutdown,
                                                        peer.clone(),
                                                        true,
                                                    );
                                                    *flushing = false
                                                } else {
                                                    Self::keep_alive(
                                                        &mut self.notify_keepalive,
                                                        peer.clone(),
                                                    );
                                                    *flushing = true;
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    ChannelHandlerEvent::InOpenChannel(ref mut rcv_conn) => {
                        match Channel::poll_rcv_msg(rcv_conn, cx) {
                            Ok((_pending, Some(msg))) => {
                                Self::shutdown(&mut self.notify_shutdown, peer.clone(), true);
                                return Poll::Ready(NetworkBehaviourAction::GenerateEvent(
                                    ChannelEvent::MessageReceived {
                                        peer: peer.clone(),
                                        msg,
                                    },
                                ));
                            }
                            Ok((pending, None)) => {
                                if pending {
                                    Self::keep_alive(&mut self.notify_keepalive, peer.clone());
                                } else {
                                    Self::shutdown(&mut self.notify_shutdown, peer.clone(), false);
                                }
                            }
                            Err(err) => {
                                let ev = NetworkBehaviourAction::GenerateEvent(
                                    ChannelEvent::ConnectionError {
                                        peer: peer.clone(),
                                        err,
                                    },
                                );
                                return Poll::Ready(ev);
                            }
                        }
                    }
                    ChannelHandlerEvent::Requested(requested) => {
                        if *requested {
                            *requested = false;
                            return Poll::Ready(NetworkBehaviourAction::NotifyHandler {
                                peer_id: peer.clone(),
                                event: ChannelHandlerInEvent::RequestChannel,
                                handler: NotifyHandler::Any,
                            });
                        }
                    }
                }
            }
        }

        if let Some(notification) = self.notify_shutdown.pop_front() {
            return Poll::Ready(notification);
        }

        Poll::Pending
    }
}

mod handler {
    use super::*;
    use crate::config::PEER_TIMEOUT_SECS;
    use libp2p::{swarm, InboundUpgrade, OutboundUpgrade};
    use protocol::ChannelProtocol;
    use std::time::{Duration, Instant};
    use swarm::KeepAlive;

    pub(crate) enum ChannelHandlerEvent {
        InOpenChannel(SimagStream<NegotiatedSubstream>),
        OutOpenChannel {
            channel: SimagStream<NegotiatedSubstream>,
            flushing: bool,
        },
        Requested(bool),
    }

    impl ChannelHandlerEvent {
        pub fn inbound_request(&self) -> bool {
            match self {
                Self::InOpenChannel(_) => true,
                _ => false,
            }
        }

        pub fn outbound_request(&self) -> bool {
            match self {
                Self::OutOpenChannel { .. } => true,
                _ => false,
            }
        }
    }

    #[derive(Clone)]
    pub enum ChannelHandlerInEvent {
        /// A dialing peer is requesting an open channel
        RequestChannel,
        Finished(bool),
        KeepAlive,
    }

    pub(super) enum SubstreamState {
        OutPendingOpen,
        Timeout(bool),
        KeepAlive,
    }

    pub(crate) struct ChannelHandler {
        keep_alive: swarm::KeepAlive,
        /// (already activated, pending dispatch)
        outbound_substream: (bool, Option<SimagStream<NegotiatedSubstream>>),
        /// pending dispatch
        inbound_substream: Option<SimagStream<NegotiatedSubstream>>,
        events: Vec<SubstreamState>,
        active: bool,
    }

    impl ChannelHandler {
        pub fn new() -> ChannelHandler {
            let keep_alive = Instant::now() + Duration::from_secs(PEER_TIMEOUT_SECS);
            ChannelHandler {
                keep_alive: swarm::KeepAlive::Until(keep_alive),
                outbound_substream: (false, None),
                inbound_substream: None,
                events: Vec::with_capacity(1),
                active: false,
            }
        }
    }

    impl ProtocolsHandler for ChannelHandler {
        type InEvent = ChannelHandlerInEvent;
        type OutEvent = ChannelHandlerEvent;
        type Error = std::io::Error;
        type InboundProtocol = ChannelProtocol;
        type OutboundProtocol = ChannelProtocol;
        type OutboundOpenInfo = ();
        type InboundOpenInfo = ();

        fn listen_protocol(
            &self,
        ) -> swarm::SubstreamProtocol<Self::InboundProtocol, Self::InboundOpenInfo> {
            swarm::SubstreamProtocol::new(ChannelProtocol, ())
        }

        fn inject_fully_negotiated_outbound(
            &mut self,
            substream: <Self::OutboundProtocol as OutboundUpgrade<NegotiatedSubstream>>::Output,
            _info: Self::OutboundOpenInfo,
        ) {
            if let (false, None) = self.outbound_substream {
                self.outbound_substream = (false, Some(substream));
            }
        }

        fn inject_fully_negotiated_inbound(
            &mut self,
            substream: <Self::InboundProtocol as InboundUpgrade<NegotiatedSubstream>>::Output,
            _info: Self::InboundOpenInfo,
        ) {
            self.inbound_substream = Some(substream);
        }

        fn inject_event(&mut self, event: Self::InEvent) {
            match event {
                ChannelHandlerInEvent::RequestChannel => {
                    self.events.push(SubstreamState::OutPendingOpen);
                }
                ChannelHandlerInEvent::Finished(refresh) => {
                    self.events.push(SubstreamState::Timeout(refresh))
                }
                ChannelHandlerInEvent::KeepAlive => self.events.push(SubstreamState::KeepAlive),
            }
        }

        fn inject_dial_upgrade_error(
            &mut self,
            _info: Self::OutboundOpenInfo,
            _error: swarm::ProtocolsHandlerUpgrErr<std::io::Error>,
        ) {
            self.keep_alive = swarm::KeepAlive::No;
        }

        fn connection_keep_alive(&self) -> swarm::KeepAlive {
            if self.active {
                KeepAlive::Yes
            } else {
                self.keep_alive
            }
        }

        fn poll(
            &mut self,
            _cx: &mut Context,
        ) -> Poll<
            swarm::ProtocolsHandlerEvent<
                Self::OutboundProtocol,
                Self::OutboundOpenInfo,
                Self::OutEvent,
                Self::Error,
            >,
        > {
            if let Some(event) = self.events.pop() {
                match event {
                    SubstreamState::OutPendingOpen => {
                        let ev = swarm::ProtocolsHandlerEvent::OutboundSubstreamRequest {
                            protocol: swarm::SubstreamProtocol::new(ChannelProtocol, ()),
                        };
                        return Poll::Ready(ev);
                    }
                    SubstreamState::KeepAlive => {
                        self.active = true;
                    }
                    SubstreamState::Timeout(refresh) => {
                        if refresh {
                            let keep_alive = KeepAlive::Until(
                                Instant::now() + Duration::from_secs(PEER_TIMEOUT_SECS),
                            );
                            self.keep_alive = keep_alive;
                        }
                        self.active = false;
                    }
                }
            }

            if self.inbound_substream.is_some() {
                if let Some(substream) = std::mem::replace(&mut self.inbound_substream, None) {
                    let ev = swarm::ProtocolsHandlerEvent::Custom(
                        ChannelHandlerEvent::InOpenChannel(substream),
                    );
                    return Poll::Ready(ev);
                }
            }

            if !self.outbound_substream.0 && self.outbound_substream.1.is_some() {
                if let (_, Some(substream)) =
                    std::mem::replace(&mut self.outbound_substream, (true, None))
                {
                    let ev =
                        swarm::ProtocolsHandlerEvent::Custom(ChannelHandlerEvent::OutOpenChannel {
                            channel: substream,
                            flushing: false,
                        });
                    return Poll::Ready(ev);
                }
            }

            Poll::Pending
        }
    }
}

mod protocol {
    use super::*;
    use crate::message::MessageCodec;
    use asynchronous_codec::Framed;
    use libp2p::{core::UpgradeInfo, futures::prelude::*, InboundUpgrade, OutboundUpgrade};
    use std::io;

    pub(super) type SimagStream<S> = Framed<S, MessageCodec>;

    pub(crate) struct ChannelProtocol;

    impl UpgradeInfo for ChannelProtocol {
        type Info = &'static [u8];
        type InfoIter = std::iter::Once<Self::Info>;

        fn protocol_info(&self) -> Self::InfoIter {
            std::iter::once(CHANNEL_PROTOCOL.as_bytes())
        }
    }

    impl<C> InboundUpgrade<C> for ChannelProtocol
    where
        C: AsyncRead + AsyncWrite + Unpin + Send + 'static,
    {
        type Output = SimagStream<C>;
        type Error = io::Error;
        type Future = future::Ready<Result<Self::Output, Self::Error>>;

        fn upgrade_inbound(self, incoming: C, _: Self::Info) -> Self::Future {
            future::ok(build_conn_stream(incoming))
        }
    }

    impl<C> OutboundUpgrade<C> for ChannelProtocol
    where
        C: AsyncRead + AsyncWrite + Unpin + Send + 'static,
    {
        type Output = SimagStream<C>;
        type Error = io::Error;
        type Future = future::Ready<Result<Self::Output, Self::Error>>;

        fn upgrade_outbound(self, incoming: C, _: Self::Info) -> Self::Future {
            future::ok(build_conn_stream(incoming))
        }
    }

    fn build_conn_stream<C: AsyncRead + AsyncWrite + Unpin>(incoming: C) -> SimagStream<C> {
        Framed::new(incoming, MessageCodec::new(true))
    }

    #[derive(serde::Serialize, serde::Deserialize)]
    pub(super) enum ChannelRcp {
        /// Open an individual channel with the remote
        OpenChannel,
        /// Close the individual channel with the remote
        CloseChannel,
    }
}
