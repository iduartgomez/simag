//! A stream and subscription handling behaviour/middleware to plug-in into libp2p swarm.
//!
//! This will open a socket with a peer accepting connections on-demand and expose it to
//! the library network handling layer to send/receive data packets. The connection will be
//! kept alive as long as necessary in order to transfer data.
//!
//! `libp2p` then handles encryption and connection handling, while by using this protocol
//! we relegate control of direct communication to the top network abstraction.
//!
//! The protocol also has the following responsabilities:
//! - propagating changes in the Identify and Kademlia protocols to this stream
//!   (e.g. openning or closing new connections based on those protocols states).
//! - openning subscription, multi-producer/multi-consumer, channels on-demand and
//!   handling authorization and automatic subscription/unsubscription on changing internal state.

use crate::message::Message;
use handler::{StreamHandler, StreamHandlerEvent, StreamHandlerInEvent};
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
    collections::HashMap,
    pin::Pin,
    task::{Context, Poll},
};

pub(crate) const STREAM_PROTOCOL: &str = "/simag/stream/0.1.0";

pub(crate) enum StreamEvent {
    MessageReceived { peer: PeerId, msg: Vec<u8> },
    ConnectionError { peer: PeerId, err: std::io::Error },
}

pub(crate) struct Stream {
    addresses: HashMap<PeerId, Vec<Multiaddr>>,
    /// open connections to a peer;
    /// never should have more than one inbound/outbound connection
    open_conn: HashMap<PeerId, SmallVec<[StreamHandlerEvent; 3]>>,
    /// encoded messages pending to be sent to a given peer
    pending_messages: HashMap<PeerId, Vec<Message>>,
}

impl Stream {
    pub fn new() -> Stream {
        Stream {
            addresses: HashMap::new(),
            open_conn: HashMap::new(),
            pending_messages: HashMap::new(),
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

    #[inline]
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

    #[inline]
    fn poll_rcv_msg(
        rcv_conn: &mut SimagStream<NegotiatedSubstream>,
        cx: &mut Context,
    ) -> Result<Option<Vec<u8>>, std::io::Error> {
        match stream::Stream::poll_next(Pin::new(rcv_conn), cx) {
            Poll::Ready(Some(Ok(msg))) => {
                // msg received
                Ok(Some(msg.data))
            }
            Poll::Ready(Some(Err(err))) => Err(err),
            Poll::Ready(None) | Poll::Pending => Ok(None),
        }
    }
}

impl NetworkBehaviour for Stream {
    type ProtocolsHandler = StreamHandler;
    type OutEvent = StreamEvent;

    fn new_handler(&mut self) -> Self::ProtocolsHandler {
        StreamHandler::new()
    }

    fn addresses_of_peer(&mut self, peer_id: &PeerId) -> Vec<Multiaddr> {
        self.addresses.get(peer_id).cloned().unwrap_or_default()
    }

    fn inject_connected(&mut self, peer_id: &PeerId) {
        log::debug!("Peer #{} connected", peer_id);
        self.open_conn
            .entry(peer_id.clone())
            .or_default()
            .push(StreamHandlerEvent::Requested(true));
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
        mut event: StreamHandlerEvent,
    ) {
        let connections = self.open_conn.entry(peer_id).or_default();
        let mut replaced = false;
        for conn in connections.iter_mut() {
            let same_type = (conn.inbound_request() && event.inbound_request())
                || (conn.outbound_request() && event.outbound_request());
            if same_type {
                if let StreamHandlerEvent::OutOpenChannel { flushing: true, .. } = conn {
                    unreachable!(
                        "tried to replace an outbound channel while flushing apending message"
                    );
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
    ) -> Poll<NetworkBehaviourAction<<StreamHandler as ProtocolsHandler>::InEvent, StreamEvent>>
    {
        for (peer, open_conn) in &mut self.open_conn {
            for conn in open_conn {
                match conn {
                    StreamHandlerEvent::OutOpenChannel {
                        ref mut channel,
                        ref mut flushing,
                    } => {
                        if *flushing {
                            match Sink::poll_flush(Pin::new(channel), cx) {
                                Poll::Pending => {}
                                Poll::Ready(Ok(_)) => *flushing = false,
                                Poll::Ready(Err(err)) => {
                                    let ev = NetworkBehaviourAction::GenerateEvent(
                                        StreamEvent::ConnectionError {
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
                                        match Stream::poll_send_msg(channel, msg, cx) {
                                            Err(err) => {
                                                let ev = NetworkBehaviourAction::GenerateEvent(
                                                    StreamEvent::ConnectionError {
                                                        peer: peer.clone(),
                                                        err,
                                                    },
                                                );
                                                return Poll::Ready(ev);
                                            }
                                            Ok(true) => *flushing = false,
                                            Ok(false) => *flushing = true,
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    StreamHandlerEvent::InOpenChannel(ref mut rcv_conn) => {
                        match Stream::poll_rcv_msg(rcv_conn, cx) {
                            Ok(Some(msg)) => {
                                return Poll::Ready(NetworkBehaviourAction::GenerateEvent(
                                    StreamEvent::MessageReceived {
                                        peer: peer.clone(),
                                        msg,
                                    },
                                ));
                            }
                            Ok(None) => {}
                            Err(err) => {
                                let ev = NetworkBehaviourAction::GenerateEvent(
                                    StreamEvent::ConnectionError {
                                        peer: peer.clone(),
                                        err,
                                    },
                                );
                                return Poll::Ready(ev);
                            }
                        }
                    }
                    StreamHandlerEvent::Requested(requested) => {
                        if *requested {
                            *requested = false;
                            return Poll::Ready(NetworkBehaviourAction::NotifyHandler {
                                peer_id: peer.clone(),
                                event: StreamHandlerInEvent::RequestChannel,
                                handler: NotifyHandler::Any,
                            });
                        }
                    }
                }
            }
        }

        Poll::Pending
    }
}

mod handler {
    use super::*;
    use libp2p::{swarm, InboundUpgrade, OutboundUpgrade};
    use protocol::StreamProtocol;

    pub(crate) enum StreamHandlerEvent {
        InOpenChannel(SimagStream<NegotiatedSubstream>),
        OutOpenChannel {
            channel: SimagStream<NegotiatedSubstream>,
            flushing: bool,
        },
        Requested(bool),
    }

    impl StreamHandlerEvent {
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
    pub enum StreamHandlerInEvent {
        /// A dialing peer is requesting an open channel
        RequestChannel,
    }

    pub(super) enum SubstreamState {
        OutPendingOpen,
    }

    pub(crate) struct StreamHandler {
        keep_alive: swarm::KeepAlive,
        /// (already activated, pending dispatch)
        outbound_substream: (bool, Option<SimagStream<NegotiatedSubstream>>),
        /// pending dispatch
        inbound_substream: Option<SimagStream<NegotiatedSubstream>>,
        events: Vec<SubstreamState>,
    }

    impl StreamHandler {
        pub fn new() -> StreamHandler {
            StreamHandler {
                keep_alive: swarm::KeepAlive::Yes,
                outbound_substream: (false, None),
                inbound_substream: None,
                events: Vec::with_capacity(1),
            }
        }
    }

    impl ProtocolsHandler for StreamHandler {
        type InEvent = StreamHandlerInEvent;
        type OutEvent = StreamHandlerEvent;
        type Error = std::io::Error;
        type InboundProtocol = StreamProtocol;
        type OutboundProtocol = StreamProtocol;
        type OutboundOpenInfo = ();

        fn listen_protocol(&self) -> swarm::SubstreamProtocol<Self::InboundProtocol> {
            swarm::SubstreamProtocol::new(StreamProtocol)
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
        ) {
            self.inbound_substream = Some(substream);
        }

        fn inject_event(&mut self, event: Self::InEvent) {
            match event {
                StreamHandlerInEvent::RequestChannel => {
                    self.events.push(SubstreamState::OutPendingOpen);
                }
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
            self.keep_alive
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
                            protocol: swarm::SubstreamProtocol::new(StreamProtocol),
                            info: (),
                        };
                        return Poll::Ready(ev);
                    }
                }
            }

            if self.inbound_substream.is_some() {
                if let Some(substream) = std::mem::replace(&mut self.inbound_substream, None) {
                    let ev = swarm::ProtocolsHandlerEvent::Custom(
                        StreamHandlerEvent::InOpenChannel(substream),
                    );
                    return Poll::Ready(ev);
                }
            }

            if !self.outbound_substream.0 && self.outbound_substream.1.is_some() {
                if let (_, Some(substream)) =
                    std::mem::replace(&mut self.outbound_substream, (true, None))
                {
                    let ev =
                        swarm::ProtocolsHandlerEvent::Custom(StreamHandlerEvent::OutOpenChannel {
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
    use futures_codec::Framed;
    use libp2p::{core::UpgradeInfo, futures::prelude::*, InboundUpgrade, OutboundUpgrade};
    use std::io;

    type Stream<S> = Framed<S, MessageCodec>;
    pub(super) type SimagStream<S> = Stream<S>;

    pub(crate) struct StreamProtocol;

    impl UpgradeInfo for StreamProtocol {
        type Info = &'static [u8];
        type InfoIter = std::iter::Once<Self::Info>;

        fn protocol_info(&self) -> Self::InfoIter {
            std::iter::once(STREAM_PROTOCOL.as_bytes())
        }
    }

    impl<C> InboundUpgrade<C> for StreamProtocol
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

    impl<C> OutboundUpgrade<C> for StreamProtocol
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
    pub(crate) enum StreamRcp {
        /// Open an individual channel with the remote
        OpenChannel,
        /// Close the individual channel with the remote
        CloseChannel,
    }
}
