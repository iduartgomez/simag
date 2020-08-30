use crate::network::{GlobalExecutor, KadKey, KadValue};
use libp2p::{identity, PeerId};
use serde::{de::DeserializeOwned, Serialize};
use std::{borrow::Borrow, collections::HashMap, fmt::Debug, fs::File, hash::Hash, io::Write};
use tokio::sync::mpsc::{
    error::{TryRecvError, TrySendError},
    Receiver, Sender,
};

type MsgId = usize;

/// A handle to a running network connection.
pub struct NetworkHandle<K, V, M>
where
    K: KadKey,
    V: KadValue,
    M: Serialize + DeserializeOwned,
{
    pub stats: NetworkStats<K>,
    sender: Sender<NetHandleCmd<K, V>>,
    rcv: Receiver<NetHandleAnsw<K, M>>,
    local_key: identity::ed25519::Keypair,
    pending: HashMap<MsgId, PendingCmd<K, M>>,
    next_msg_id: MsgId,
    is_dead: bool,
    rcv_msg: HashMap<PeerId, Vec<M>>,
}

impl<K, V, M> NetworkHandle<K, V, M>
where
    K: KadKey,
    V: KadValue,
    M: Serialize + DeserializeOwned + Debug + Send + 'static,
{
    pub(crate) fn new(
        sender: Sender<NetHandleCmd<K, V>>,
        rcv: Receiver<NetHandleAnsw<K, M>>,
        local_key: identity::ed25519::Keypair,
    ) -> Self {
        NetworkHandle {
            sender,
            rcv,
            local_key,
            pending: HashMap::new(),
            next_msg_id: 0,
            is_dead: false,
            stats: NetworkStats::<K>::default(),
            rcv_msg: HashMap::new(),
        }
    }

    /// Set this peer as a provider for a certain key.
    pub fn put(&mut self, key: K, value: V) {
        let id = self.next_id();
        self.pending.insert(
            id,
            PendingCmd {
                cmd: SentCmd::AddedKey(key.borrow().to_owned()),
                answ: None,
            },
        );
        let msg = NetHandleCmd::ProvideResource { id, key, value };
        GlobalExecutor::block_on(self.sender.send(msg)).expect("failed sending message");
    }

    pub fn send_message(&mut self, value: M, peer: PeerId) {
        let id = self.next_id();
        let mut sender = self.sender.clone();
        GlobalExecutor::spawn(async move {
            let value = bincode::serialize(&value).map_err(|_| ())?;
            let msg = NetHandleCmd::<K, V>::SendMessage { id, value, peer };
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        self.pending.insert(
            id,
            PendingCmd {
                cmd: SentCmd::SentMsg,
                answ: None,
            },
        );
    }

    pub fn get(&mut self, key: K) {
        let id = self.next_id();
        self.pending.insert(
            id,
            PendingCmd {
                cmd: SentCmd::PullContent(key.borrow().to_owned()),
                answ: None,
            },
        );
        let msg = NetHandleCmd::PullResource { id, key };
        GlobalExecutor::block_on(self.sender.send(msg)).expect("failed sending message");
    }

    /// Get this peer id encoded as a Base58 string.
    pub fn get_peer_id(&self) -> String {
        peer_id_from_ed25519(self.local_key.public()).to_base58()
    }

    /// Saves this peer secret key to a file in bytes. This file should be kept in a secure location.
    pub fn save_secret_key<T: AsRef<std::path::Path>>(&self, path: T) -> std::io::Result<()> {
        let enconded_key = self.local_key.encode().to_vec();
        let mut file = File::create(path.as_ref())?;
        file.write_all(enconded_key.as_slice())
    }

    /// Returns Ok while the connection to the network still is running or Err if it has shutdown.
    pub fn is_running(&mut self) -> bool {
        if self.is_dead {
            return false;
        }
        self.process_answer_buffer();

        let msg_id = self.next_id();
        let msg = NetHandleCmd::IsRunning(msg_id);
        self.sender
            .try_send(msg)
            .map(|_| true)
            .unwrap_or_else(|err| match err {
                TrySendError::Full(_) => true,
                TrySendError::Closed(_) => {
                    self.is_dead = true;
                    false
                }
            })
    }

    /// Commands the network to shutdown asynchrnously, returns inmediately if the network has shutdown or error
    /// if it already had disconnected for any reason.
    ///
    /// The network may not shutdown inmediately if still is listening and waiting for any events.
    pub fn shutdown(&mut self) -> Result<bool, ()> {
        if self.is_dead {
            return Err(());
        }
        self.process_answer_buffer();
        if let Ok(answ) = self.try_getting_shutdown_answ() {
            return Ok(answ);
        }

        let msg_id = self.next_id();
        let msg = NetHandleCmd::Shutdown(msg_id);
        match self.sender.try_send(msg) {
            Ok(()) => {}
            Err(TrySendError::Full(_msg)) => {
                self.pending.insert(
                    msg_id,
                    PendingCmd {
                        cmd: SentCmd::ShutDown,
                        answ: None,
                    },
                );
            }
            Err(TrySendError::Closed(_)) => {
                self.is_dead = true;
                return Err(());
            }
        }

        self.process_answer_buffer();
        Ok(self.try_getting_shutdown_answ().unwrap_or(false))
    }

    fn try_getting_shutdown_answ(&mut self) -> Result<bool, ()> {
        match self.pending.values().find(|p| p.shutdown()) {
            Some(PendingCmd {
                cmd: SentCmd::ShutDown,
                answ: Some(NetHandleAnsw::HasShutdown { id, answ }),
            }) => {
                let answ = *answ;
                let id = *id;
                self.pending.remove(&id.clone());
                Ok(answ)
            }
            Some(_) => Ok(false),
            _ => Err(()),
        }
    }

    fn process_answer_buffer(&mut self) {
        loop {
            match self.rcv.try_recv() {
                Ok(NetHandleAnsw::HasShutdown { id, answ }) => {
                    if let Some(q) = self.pending.get_mut(&id) {
                        q.answ = Some(NetHandleAnsw::HasShutdown { id, answ })
                    }
                }
                Ok(NetHandleAnsw::KeyAdded { id }) => {
                    if let Some(PendingCmd {
                        cmd: SentCmd::AddedKey(key),
                        ..
                    }) = self.pending.remove(&id)
                    {
                        log::debug!("Added key: {:?}", K::try_from(&key).unwrap());
                    } else {
                        unreachable!()
                    }
                }
                Ok(NetHandleAnsw::GotRecord { key, .. }) => {
                    self.stats.key_stats.entry(key).or_default().times_received += 1;
                }
                Ok(NetHandleAnsw::RcvMsg { peer, msg }) => {
                    log::debug!("Received streaming msg from {}: {:?}", peer, msg);
                    if let Some(stats) = self.stats.rcv_msgs.iter_mut().find(|p| p.0 == peer) {
                        stats.1 += 1;
                    } else {
                        self.stats.rcv_msgs.push((peer.clone(), 1));
                    }
                    self.rcv_msg.entry(peer).or_default().push(msg);
                }
                Err(TryRecvError::Closed) => {
                    self.is_dead = true;
                    break;
                }
                Err(TryRecvError::Empty) => break,
            }
        }
    }

    fn next_id(&mut self) -> usize {
        let msg_id = self.next_msg_id;
        self.next_msg_id += 1;
        msg_id
    }
}

pub struct NetworkStats<K: Hash + Eq> {
    key_stats: HashMap<K, KeyStats>,
    rcv_msgs: Vec<(PeerId, usize)>,
}

impl<K: Eq + Hash> Default for NetworkStats<K> {
    fn default() -> Self {
        NetworkStats {
            key_stats: HashMap::new(),
            rcv_msgs: Vec::new(),
        }
    }
}

impl<K: Eq + Hash> NetworkStats<K> {
    pub fn for_key(&self, key: &K) -> Option<&KeyStats> {
        self.key_stats.get(key)
    }

    pub fn received_messages(&self) -> &[(PeerId, usize)] {
        &self.rcv_msgs
    }
}

#[derive(Default)]
pub struct KeyStats {
    pub times_served: usize,
    pub times_received: usize,
}

struct PendingCmd<K, M>
where
    K: Borrow<[u8]>,
    M: DeserializeOwned,
{
    cmd: SentCmd,
    answ: Option<NetHandleAnsw<K, M>>,
}

impl<K: Borrow<[u8]>, M: DeserializeOwned> PendingCmd<K, M> {
    fn shutdown(&self) -> bool {
        match self.cmd {
            SentCmd::ShutDown => true,
            _ => false,
        }
    }
}

enum SentCmd {
    AddedKey(Vec<u8>),
    PullContent(Vec<u8>),
    ShutDown,
    SentMsg,
}

#[derive(Debug, Clone)]
pub(crate) enum NetHandleCmd<K, V>
where
    K: Borrow<[u8]> + Debug,
    V: Serialize + Debug,
{
    /// query the network about current status
    IsRunning(usize),
    /// put a resource in this node
    ProvideResource { id: usize, key: K, value: V },
    /// pull a resource in this node
    PullResource { id: usize, key: K },
    /// issue a shutdown command
    Shutdown(usize),
    /// send a serialized message
    SendMessage {
        id: usize,
        value: Vec<u8>,
        peer: PeerId,
    },
}

#[derive(Clone)]
pub(crate) enum NetHandleAnsw<K, M>
where
    K: Borrow<[u8]>,
    M: DeserializeOwned,
{
    KeyAdded { id: usize },
    GotRecord { id: usize, key: K },
    HasShutdown { id: usize, answ: bool },
    RcvMsg { peer: PeerId, msg: M },
}

fn peer_id_from_ed25519(key: identity::ed25519::PublicKey) -> PeerId {
    PeerId::from_public_key(identity::PublicKey::Ed25519(key))
}
