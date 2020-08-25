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
const MAX_MSG_SIZE: usize = 4096;

pub(crate) enum StreamEvent {
    MessageReceived { peer: PeerId, msg: Vec<u8> },
    ConnectionError { peer: PeerId, err: std::io::Error },
}

pub(crate) struct Stream {
    addresses: HashMap<PeerId, Vec<Multiaddr>>,
    /// open connections to a peer;
    /// never should have more than one inbound/outbound connection really
    open_conn: HashMap<PeerId, SmallVec<[StreamHandlerEvent; 2]>>,
    /// encoded messages pending to be sent to a given peer
    pending_messages: HashMap<PeerId, Vec<Vec<u8>>>,
    /// pending messages to be sent to peer connection handlers
    pending_handler_msg: Vec<(PeerId, StreamHandlerInEvent)>,
}

impl Stream {
    pub fn new() -> Stream {
        Stream {
            addresses: HashMap::new(),
            open_conn: HashMap::new(),
            pending_messages: HashMap::new(),
            pending_handler_msg: Vec::new(),
        }
    }

    pub fn send_message(&mut self, peer: &PeerId, msg: Vec<u8>) {
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
        msg: Vec<u8>,
        cx: &mut Context,
    ) -> Result<(), std::io::Error> {
        match Sink::poll_ready(Pin::new(send_conn), cx) {
            Poll::Ready(Ok(())) => match Sink::start_send(Pin::new(send_conn), msg) {
                Ok(()) => loop {
                    match Sink::poll_flush(Pin::new(send_conn), cx) {
                        Poll::Pending => {}
                        Poll::Ready(Ok(())) => {
                            break;
                        }
                        Poll::Ready(err) => return err,
                    }
                },
                Err(err) => return Err(err),
            },
            Poll::Pending => {}
            Poll::Ready(err) => return err,
        }
        Ok(())
    }

    #[inline]
    fn poll_rcv_msg(
        rcv_conn: &mut SimagStream<NegotiatedSubstream>,
        cx: &mut Context,
    ) -> Result<Option<Vec<u8>>, std::io::Error> {
        match stream::Stream::poll_next(Pin::new(rcv_conn), cx) {
            Poll::Ready(Some(Ok(msg))) => {
                // msg received
                Ok(Some(msg.into_iter().collect()))
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
        eprintln!("Peer #{} connected", peer_id);
        self.open_conn.entry(peer_id.clone()).or_default();
    }

    fn inject_disconnected(&mut self, peer_id: &PeerId) {
        eprintln!("Peer #{} disconnected", peer_id);
        self.addresses.remove(peer_id);
        self.open_conn.remove(peer_id);
    }

    fn inject_event(
        &mut self,
        peer_id: PeerId,
        _connection: ConnectionId,
        event: StreamHandlerEvent,
    ) {
        self.open_conn.entry(peer_id).or_default().push(event);
    }

    fn poll(
        &mut self,
        cx: &mut Context,
        _params: &mut impl PollParameters,
    ) -> Poll<NetworkBehaviourAction<<StreamHandler as ProtocolsHandler>::InEvent, StreamEvent>>
    {
        if let Some((peer_id, event)) = self.pending_handler_msg.pop() {
            return Poll::Ready(NetworkBehaviourAction::NotifyHandler {
                peer_id,
                event,
                handler: NotifyHandler::All,
            });
        }

        for (peer, open_conn) in &mut self.open_conn {
            if open_conn.is_empty() {
                return Poll::Ready(NetworkBehaviourAction::NotifyHandler {
                    peer_id: peer.clone(),
                    event: StreamHandlerInEvent::RequestChannel,
                    handler: NotifyHandler::Any,
                });
            }

            for conn in open_conn {
                match conn {
                    StreamHandlerEvent::OutOpenChannel(ref mut send_conn) => {
                        match self.pending_messages.get_mut(peer) {
                            Some(pending) if !pending.is_empty() => {
                                while let Some(msg) = pending.pop() {
                                    if let Err(err) = Stream::poll_send_msg(send_conn, msg, cx) {
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
                            _ => {}
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
                }
            }
        }

        Poll::Pending
    }
}

pub(super) mod handler {
    use super::*;
    use libp2p::{swarm, InboundUpgrade, OutboundUpgrade};
    use protocol::StreamProtocol;

    pub(crate) enum StreamHandlerEvent {
        InOpenChannel(SimagStream<NegotiatedSubstream>),
        OutOpenChannel(SimagStream<NegotiatedSubstream>),
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
        /// (already activated, pending dispatch)
        inbound_substream: (bool, Option<SimagStream<NegotiatedSubstream>>),
        events: Vec<SubstreamState>,
    }

    impl StreamHandler {
        pub fn new() -> StreamHandler {
            StreamHandler {
                keep_alive: swarm::KeepAlive::Yes,
                outbound_substream: (false, None),
                inbound_substream: (false, None),
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
            if let (false, None) = self.inbound_substream {
                self.inbound_substream = (false, Some(substream));
            }
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

            if !self.inbound_substream.0 && self.inbound_substream.1.is_some() {
                if let (_, Some(substream)) =
                    std::mem::replace(&mut self.inbound_substream, (true, None))
                {
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
                    let ev = swarm::ProtocolsHandlerEvent::Custom(
                        StreamHandlerEvent::OutOpenChannel(substream),
                    );
                    return Poll::Ready(ev);
                }
            }

            Poll::Pending
        }
    }
}

pub(super) mod protocol {
    use super::*;
    use futures_codec::Framed;
    use libp2p::{core::UpgradeInfo, futures::prelude::*, InboundUpgrade, OutboundUpgrade};
    use std::io;
    use unsigned_varint::codec::UviBytes;

    type Stream<S> = sink::With<
        Framed<S, UviBytes<io::Cursor<Vec<u8>>>>,
        io::Cursor<Vec<u8>>,
        Vec<u8>,
        future::Ready<Result<io::Cursor<Vec<u8>>, io::Error>>,
        fn(Vec<u8>) -> future::Ready<Result<io::Cursor<Vec<u8>>, io::Error>>,
    >;

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
        C: AsyncRead + AsyncWrite + Unpin,
    {
        type Output = SimagStream<C>;
        type Error = std::io::Error;
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
        type Error = std::io::Error;
        type Future = future::Ready<Result<Self::Output, Self::Error>>;

        fn upgrade_outbound(self, incoming: C, _: Self::Info) -> Self::Future {
            future::ok(build_conn_stream(incoming))
        }
    }

    fn build_conn_stream<C: AsyncRead + AsyncWrite + Unpin>(incoming: C) -> SimagStream<C> {
        let mut codec = UviBytes::default();
        codec.set_max_len(MAX_MSG_SIZE);
        Framed::new(incoming, codec)
            .with::<_, _, fn(_) -> _, _>(|req| future::ok(io::Cursor::new(req)))
    }
}
