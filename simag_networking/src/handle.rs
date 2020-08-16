use crossbeam::{Receiver, Sender};
use libp2p::{identity, PeerId};
use std::{
    collections::HashMap,
    io::Write,
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
    time::Duration,
};

/// A handle to a running network connection.
pub struct NetworkHandle {
    sender: Sender<NetHandleCmd>,
    rcv: Receiver<NetHandleAnsw>,
    local_key: identity::ed25519::Keypair,
    pending: HashMap<usize, (NetHandleCmd, Option<NetHandleAnsw>)>,
    current_message_id: AtomicUsize,
    is_dead: AtomicBool,
}

impl NetworkHandle {
    pub(crate) fn new(
        sender: Sender<NetHandleCmd>,
        rcv: Receiver<NetHandleAnsw>,
        local_key: identity::ed25519::Keypair,
    ) -> Self {
        NetworkHandle {
            sender,
            rcv,
            local_key,
            pending: HashMap::new(),
            current_message_id: AtomicUsize::default(),
            is_dead: AtomicBool::new(false),
        }
    }

    /// Get this peer id encoded as a Base58 string.
    pub fn get_peer_id(&self) -> String {
        peer_id_from_ed25519(self.local_key.public()).to_base58()
    }

    /// Saves this peer secret key to a file in bytes. This file should be kept in a secure location.
    pub fn save_secret_key<T: AsRef<std::path::Path>>(&self, path: T) -> std::io::Result<()> {
        let enconded_key = self.local_key.encode().to_vec();
        use std::fs::File;
        let mut file = File::create(path.as_ref())?;
        file.write_all(enconded_key.as_slice())
    }

    /// Returns Ok while the connection to the network still is running or Err if it has shutdown.
    pub fn is_running(&mut self) -> bool {
        if self.is_dead.load(Ordering::SeqCst) {
            return false;
        }
        self.clean_answer_buffer();

        let msg_id = self.current_message_id.fetch_add(1, Ordering::SeqCst);
        let msg = NetHandleCmd::IsRunning(msg_id);
        self.sender
            .send_timeout(msg, Duration::from_secs(1))
            .map(|_| true)
            .unwrap_or_else(|err| match err {
                crossbeam::SendTimeoutError::Timeout(_) => true,
                crossbeam::SendTimeoutError::Disconnected(_) => {
                    self.is_dead.store(true, Ordering::SeqCst);
                    false
                }
            })
    }

    /// Commands the network to shutdown asynchrnously, returns inmediately if the network has shutdown or error
    /// if it already had disconnected for any reason.
    ///
    /// The network may not shutdown inmediately if still is listening and waiting for any events.
    pub fn shutdown(&mut self) -> Result<bool, ()> {
        if self.is_dead.load(Ordering::SeqCst) {
            return Err(());
        }
        self.clean_answer_buffer();
        if let Ok(answ) = self.try_getting_shutdown_answ() {
            return Ok(answ);
        }

        let msg_id = self.current_message_id.fetch_add(1, Ordering::SeqCst);
        let msg = NetHandleCmd::Shutdown(msg_id);
        match self.sender.send_timeout(msg, Duration::from_secs(1)) {
            Ok(()) => {}
            Err(crossbeam::SendTimeoutError::Timeout(_)) => {
                self.pending.insert(msg_id, (msg, None));
            }
            Err(crossbeam::SendTimeoutError::Disconnected(_)) => {
                self.is_dead.store(true, Ordering::SeqCst);
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
                Err(crossbeam::TryRecvError::Disconnected) => {
                    self.is_dead.store(true, Ordering::SeqCst);
                    break;
                }
                Err(crossbeam::TryRecvError::Empty) => break,
            }
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum NetHandleCmd {
    /// query the network about current status
    IsRunning(usize),
    /// issue a shutdown command
    Shutdown(usize),
}

pub(crate) enum NetHandleAnsw {
    HasShutdown { id: usize, answ: bool },
}

fn peer_id_from_ed25519(key: identity::ed25519::PublicKey) -> PeerId {
    PeerId::from_public_key(identity::PublicKey::Ed25519(key))
}
