//! A stream and subscription handling behaviour/middleware to plug-in into libp2p swarm.
//!
//! This will open a socket with a peer accepting connections on-demand and expose it to
//! the library network handling layer to send/receive data packets. The connection will be
//! kept alive as long as necessary in order to transfer data.
//!
//! `libp2p` then handles encryption and connection handling, while by using this protocol
//! we relegate control of direct communication to the layer.
//!
//! The protocol also has the following responsabilities:
//! - propagating changes in the Identify and Kademlia protocols to this stream
//!   (e.g. openning or closing new connections based on those protocols states).
//! - openning subscription, multi-producer/multi-consumer, channels on-demand and
//!   handling authorization and automatic subscription/unsubscription on changing internal state.

use futures_codec::BytesMut;
use handler::{StreamHandler, StreamHandlerEvent};
use libp2p::{
    core::connection::ConnectionId,
    futures::{stream, AsyncRead, AsyncWrite, Sink},
    swarm::{
        NegotiatedSubstream, NetworkBehaviour, NetworkBehaviourAction, PollParameters,
        ProtocolsHandler,
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
pub(crate) type ConnId = usize;

pub(crate) enum StreamEvent {
    MessageReceived(Vec<u8>),
}

pub(crate) struct Stream {
    addresses: HashMap<PeerId, Vec<Multiaddr>>,
    open_conn: HashMap<PeerId, SmallVec<[StreamHandlerEvent; 2]>>,
    /// encoded messages pending to be sent to a given peer
    pending_messages: HashMap<PeerId, Vec<Vec<u8>>>,
}

impl Stream {
    pub fn new() -> Stream {
        Stream {
            addresses: HashMap::new(),
            open_conn: HashMap::new(),
            pending_messages: HashMap::new(),
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

    fn poll_send_msg(
        send_conn: &mut SimagStream<NegotiatedSubstream>,
        msg: Vec<u8>,
        cx: &mut Context,
    ) {
        match Sink::poll_ready(Pin::new(send_conn), cx) {
            Poll::Ready(Ok(())) => match Sink::start_send(Pin::new(send_conn), msg) {
                Ok(()) => loop {
                    match Sink::poll_flush(Pin::new(send_conn), cx) {
                        Poll::Pending => {}
                        Poll::Ready(Ok(())) => {
                            break;
                        }
                        Poll::Ready(Err(_err)) => panic!(),
                    }
                },
                Err(_) => panic!(),
            },
            Poll::Pending => {}
            Poll::Ready(Err(_err)) => panic!(),
        }
    }

    fn poll_rcv_msg(
        rcv_conn: &mut SimagStream<NegotiatedSubstream>,
        cx: &mut Context,
    ) -> Result<Option<Vec<u8>>, ()> {
        loop {
            match stream::Stream::poll_next(Pin::new(rcv_conn), cx) {
                Poll::Ready(Some(Ok(msg))) => break Ok(Some(msg.into_iter().collect())),
                Poll::Ready(Some(_err)) => break Err(()),
                Poll::Ready(None) => break Ok(None),
                Poll::Pending => {}
            }
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
        self.addresses.entry(peer_id.clone()).or_default();
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
        for (peer, open_conn) in &mut self.open_conn {
            for conn in open_conn {
                match conn {
                    StreamHandlerEvent::OutOpenChannel(ref mut send_conn) => {
                        match self.pending_messages.get_mut(peer) {
                            Some(pending) if !pending.is_empty() => {
                                while let Some(msg) = pending.pop() {
                                    eprintln!("Output stream w/ peer: {}", peer);
                                    Stream::poll_send_msg(send_conn, msg, cx);
                                    eprintln!("Sent stream msg!");
                                }
                            }
                            _ => {}
                        }
                    }
                    StreamHandlerEvent::InOpenChannel(ref mut rcv_conn) => {
                        if let Some(msg) = Stream::poll_rcv_msg(rcv_conn, cx).unwrap() {
                            return Poll::Ready(NetworkBehaviourAction::GenerateEvent(
                                StreamEvent::MessageReceived(msg),
                            ));
                        }
                    }
                }
            }
        }

        Poll::Pending
    }
}

pub(super) mod protocol {
    use super::*;
    use futures_codec::Framed;
    use libp2p::{core::UpgradeInfo, futures::prelude::*, InboundUpgrade, OutboundUpgrade};
    use std::io;
    use unsigned_varint::codec::UviBytes;

    /// Composed stream with req and resp types.
    type Stream<S, A, B> = stream::AndThen<
        sink::With<
            stream::ErrInto<Framed<S, UviBytes<io::Cursor<Vec<u8>>>>, io::Error>,
            io::Cursor<Vec<u8>>,
            A,
            future::Ready<Result<io::Cursor<Vec<u8>>, io::Error>>,
            fn(A) -> future::Ready<Result<io::Cursor<Vec<u8>>, io::Error>>,
        >,
        future::Ready<Result<B, io::Error>>,
        fn(BytesMut) -> future::Ready<Result<B, io::Error>>,
    >;

    pub(super) type SimagStream<S> = Stream<S, Vec<u8>, BytesMut>;

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
        codec.set_max_len(4096);

        Framed::new(incoming, codec)
            .err_into()
            .with::<_, _, fn(_) -> _, _>(|req| future::ok(io::Cursor::new(req)))
            .and_then::<_, fn(_) -> _>(future::ok)
    }
}

pub(super) mod handler {
    use super::*;
    use libp2p::{swarm, InboundUpgrade, OutboundUpgrade};
    use protocol::StreamProtocol;
    use smallvec::SmallVec;
    use std::time::{Duration, Instant};

    pub(crate) enum StreamHandlerEvent {
        InOpenChannel(SimagStream<NegotiatedSubstream>),
        OutOpenChannel(SimagStream<NegotiatedSubstream>),
    }

    #[derive(Clone)]
    pub enum StreamHandlerInEvent {
        /// A dialing peer is requesting an open channel
        RequestChannel,
    }

    enum SubstreamState {
        OutPendingOpen,
        OutOpenChannel(SimagStream<NegotiatedSubstream>),
        InOpenChannel(SimagStream<NegotiatedSubstream>, ConnId),
    }

    pub(crate) struct StreamHandler {
        keep_alive: swarm::KeepAlive,
        events: SmallVec<[SubstreamState; 2]>,
        next_connec_unique_id: usize,
    }

    impl StreamHandler {
        const KEEP_ALIVE: Duration = Duration::from_secs(10);

        pub fn new() -> StreamHandler {
            let _keep_alive = swarm::KeepAlive::Until(Instant::now() + Self::KEEP_ALIVE);
            let keep_alive = swarm::KeepAlive::Yes;
            StreamHandler {
                keep_alive,
                events: SmallVec::new(),
                next_connec_unique_id: 0,
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
            protocol: <Self::OutboundProtocol as OutboundUpgrade<NegotiatedSubstream>>::Output,
            _info: Self::OutboundOpenInfo,
        ) {
            self.events.push(SubstreamState::OutOpenChannel(protocol));
        }

        fn inject_fully_negotiated_inbound(
            &mut self,
            protocol: <Self::InboundProtocol as InboundUpgrade<NegotiatedSubstream>>::Output,
        ) {
            let next_conn_id = self.next_connec_unique_id;
            self.next_connec_unique_id += 1;
            self.events
                .push(SubstreamState::InOpenChannel(protocol, next_conn_id));
        }

        fn inject_event(&mut self, event: Self::InEvent) {
            // TODO: receive and process requests to join certain peers here
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
            if self.events.is_empty() {
                return Poll::Pending;
            }

            if let Some(event) = self.events.pop() {
                match event {
                    SubstreamState::OutPendingOpen => {
                        let ev = swarm::ProtocolsHandlerEvent::OutboundSubstreamRequest {
                            protocol: swarm::SubstreamProtocol::new(StreamProtocol),
                            info: (),
                        };
                        return Poll::Ready(ev);
                    }
                    SubstreamState::InOpenChannel(substream, conn_id) => {
                        let ev = swarm::ProtocolsHandlerEvent::Custom(
                            StreamHandlerEvent::InOpenChannel(substream),
                        );
                        return Poll::Ready(ev);
                    }
                    SubstreamState::OutOpenChannel(substream) => {
                        let ev = swarm::ProtocolsHandlerEvent::Custom(
                            StreamHandlerEvent::OutOpenChannel(substream),
                        );
                        return Poll::Ready(ev);
                    }
                }
            }

            Poll::Pending
        }
    }
}
