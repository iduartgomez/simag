// use crossbeam::{Receiver, Sender};
use libp2p::{identity, PeerId};
use std::{borrow::Borrow, collections::HashMap, fs::File, hash::Hash, io::Write};
use tokio::sync::mpsc::{
    error::{TryRecvError, TrySendError},
    Receiver, Sender,
};

/// A handle to a running network connection.
pub struct NetworkHandle<K>
where
    K: Borrow<[u8]> + Eq + Hash + Clone + Copy + std::fmt::Display,
{
    sender: Sender<NetHandleCmd<K>>,
    rcv: Receiver<NetHandleAnsw<K>>,
    local_key: identity::ed25519::Keypair,
    pending: HashMap<usize, (NetHandleCmd<K>, Option<NetHandleAnsw<K>>)>,
    next_msg_id: usize,
    is_dead: bool,
    pub stats: HashMap<K, KeyStats>,
}

impl<K> NetworkHandle<K>
where
    K: Borrow<[u8]> + Eq + Hash + Clone + Copy + std::fmt::Display,
{
    pub(crate) fn new(
        sender: Sender<NetHandleCmd<K>>,
        rcv: Receiver<NetHandleAnsw<K>>,
        local_key: identity::ed25519::Keypair,
    ) -> Self {
        NetworkHandle {
            sender,
            rcv,
            local_key,
            pending: HashMap::new(),
            next_msg_id: 0,
            is_dead: false,
            stats: HashMap::new(),
        }
    }

    /// Set this peer as a provider for a certain key.
    pub fn put(&mut self, key: K) {
        let id = self.next_id();
        let msg = NetHandleCmd::ProvideResource { id, key };
        self.pending.insert(id, (msg, None));
        if smol::run(self.sender.send(msg)).is_err() {
            panic!()
        }
    }

    pub fn get(&mut self, key: K) {
        let id = self.next_id();
        let msg = NetHandleCmd::PullResource { id, key };
        self.pending.insert(id, (msg, None));
        if smol::run(self.sender.send(msg)).is_err() {
            panic!()
        }
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
        self.clean_answer_buffer();

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
        self.clean_answer_buffer();
        if let Ok(answ) = self.try_getting_shutdown_answ() {
            return Ok(answ);
        }

        let msg_id = self.next_id();
        let msg = NetHandleCmd::Shutdown(msg_id);
        match self.sender.try_send(msg) {
            Ok(()) => {}
            Err(TrySendError::Full(msg)) => {
                self.pending.insert(msg_id, (msg, None));
            }
            Err(TrySendError::Closed(_)) => {
                self.is_dead = true;
                return Err(());
            }
        }

        self.clean_answer_buffer();
        Ok(self.try_getting_shutdown_answ().unwrap_or(false))
    }

    fn try_getting_shutdown_answ(&mut self) -> Result<bool, ()> {
        match self.pending.values().find(|msg| {
            if let (NetHandleCmd::Shutdown(_), _) = msg {
                true
            } else {
                false
            }
        }) {
            Some((NetHandleCmd::Shutdown(id), Some(NetHandleAnsw::HasShutdown { answ, .. }))) => {
                let answ = *answ;
                let id = *id;
                self.pending.remove(&id.clone());
                Ok(answ)
            }
            Some((_, None)) => Ok(false),
            _ => Err(()),
        }
    }

    fn clean_answer_buffer(&mut self) {
        loop {
            match self.rcv.try_recv() {
                Ok(NetHandleAnsw::HasShutdown { id, answ }) => {
                    if let Some(q) = self.pending.get_mut(&id) {
                        q.1 = Some(NetHandleAnsw::HasShutdown { id, answ })
                    }
                }
                Ok(NetHandleAnsw::KeyAdded { id }) => {
                    if let Some((NetHandleCmd::ProvideResource { key, .. }, _)) =
                        self.pending.remove(&id)
                    {
                        eprintln!("Added key #{}", key);
                    } else {
                        unreachable!()
                    }
                }
                Ok(NetHandleAnsw::GotRecord { key, .. }) => {
                    self.stats.entry(key).or_default().times_received += 1;
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

#[derive(Default)]
pub struct KeyStats {
    pub times_served: usize,
    pub times_received: usize,
}

#[derive(Clone, Copy)]
pub(crate) enum NetHandleCmd<K>
where
    K: Borrow<[u8]> + Clone + Copy,
{
    /// query the network about current status
    IsRunning(usize),
    /// put a resource in this node
    ProvideResource { id: usize, key: K },
    /// pull a resource in this node
    PullResource { id: usize, key: K },
    /// issue a shutdown command
    Shutdown(usize),
}

#[derive(Clone, Copy)]
pub(crate) enum NetHandleAnsw<K>
where
    K: Borrow<[u8]> + Clone + Copy,
{
    KeyAdded { id: usize },
    GotRecord { id: usize, key: K },
    HasShutdown { id: usize, answ: bool },
}

fn peer_id_from_ed25519(key: identity::ed25519::PublicKey) -> PeerId {
    PeerId::from_public_key(identity::PublicKey::Ed25519(key))
}
