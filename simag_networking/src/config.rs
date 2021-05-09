use libp2p::{identity, PeerId};
use once_cell::sync::Lazy;
use std::{
    convert::TryFrom,
    fs::File,
    future::Future,
    io::Read,
    net::{IpAddr, Ipv4Addr},
    path::PathBuf,
    pin::Pin,
    str::FromStr,
};
use tokio::runtime::Runtime;

const DEFAULT_BOOTSTRAP_PORT: u16 = 7800;
pub(crate) static CONF: Lazy<Config> =
    Lazy::new(|| Config::load_conf().expect("Failed to load configuration"));
pub(crate) const PEER_TIMEOUT_SECS: u64 = 10;

static ASYNC_RT: Lazy<Option<Runtime>> = Lazy::new(GlobalExecutor::build_async_executor);

pub(crate) struct Config {
    pub bootstrap_ip: IpAddr,
    pub bootstrap_port: u16,
    pub bootstrap_id: Option<PeerId>,
    pub local_peer_keypair: Option<identity::ed25519::Keypair>,
    pub log_level: log::LevelFilter,
}

impl Config {
    pub fn load_conf() -> Result<Config, ()> {
        #[cfg(any(debug_assertions, test))]
        {
            self::tracing::Logger::get_logger();
        }

        let mut settings = config::Config::new();
        settings
            .merge(config::Environment::with_prefix("SIMAG"))
            .unwrap();

        let local_peer_keypair =
            if let Ok(path_to_key) = settings.get_str("local_peer_key_file").map(PathBuf::from) {
                let mut key_file = File::open(&path_to_key).unwrap_or_else(|_| {
                    panic!(
                        "Failed to open key file: {}",
                        &path_to_key.to_str().unwrap()
                    )
                });
                let mut buf = Vec::new();
                key_file.read_to_end(&mut buf).unwrap();
                Some(identity::ed25519::Keypair::decode(&mut buf).map_err(|_| ())?)
            } else {
                None
            };

        let (bootstrap_ip, bootstrap_port, bootstrap_id) = Config::get_bootstrap_host(&settings)?;
        Ok(Config {
            bootstrap_ip,
            bootstrap_port,
            bootstrap_id,
            local_peer_keypair,
            log_level: log::LevelFilter::Off,
        })
    }

    fn get_bootstrap_host(settings: &config::Config) -> Result<(IpAddr, u16, Option<PeerId>), ()> {
        let bootstrap_ip = IpAddr::from_str(
            &settings
                .get_str("bootstrap_host")
                .unwrap_or_else(|_| format!("{}", Ipv4Addr::LOCALHOST)),
        )
        .map_err(|_err| ())?;

        let bootstrap_port = settings
            .get_int("bootstrap_port")
            .map(u16::try_from)
            .unwrap_or(Ok(DEFAULT_BOOTSTRAP_PORT))
            .map_err(|_err| ())?;

        let id_str = settings
            .get_str("bootstrap_id")
            .ok()
            .map(|id| id.parse().map_err(|_err| ()).ok())
            .flatten();

        Ok((bootstrap_ip, bootstrap_port, id_str))
    }
}

pub(crate) struct GlobalExecutor;

impl GlobalExecutor {
    pub fn new() -> Self {
        GlobalExecutor
    }

    fn build_async_executor() -> Option<Runtime> {
        if tokio::runtime::Handle::try_current().is_ok() {
            None
        } else {
            Some(
                tokio::runtime::Builder::new_multi_thread()
                    .enable_all()
                    .thread_name("simag-nw-exec")
                    .build()
                    .expect("failed to build tokio runtime"),
            )
        }
    }

    pub fn spawn<R: Send + 'static>(
        f: impl Future<Output = R> + Send + 'static,
    ) -> tokio::task::JoinHandle<R> {
        if let Ok(handle) = tokio::runtime::Handle::try_current() {
            handle.spawn(f)
        } else if let Some(rt) = &*ASYNC_RT {
            rt.spawn(f)
        } else {
            unreachable!("the executor must have been initialized")
        }
    }
    pub fn spawn_blocking<F, R>(f: F) -> tokio::task::JoinHandle<R>
    where
        F: FnOnce() -> R + Send + 'static,
        R: Send + 'static,
    {
        if let Ok(handle) = tokio::runtime::Handle::try_current() {
            handle.spawn_blocking(f)
        } else if let Some(rt) = &*ASYNC_RT {
            rt.spawn_blocking(f)
        } else {
            unreachable!("the executor must have been initialized")
        }
    }
}

impl libp2p::core::Executor for GlobalExecutor {
    fn exec(&self, future: Pin<Box<dyn Future<Output = ()> + 'static + Send>>) {
        GlobalExecutor::spawn(future);
    }
}

#[cfg(any(debug_assertions, test))]
pub(super) mod tracing {
    use super::*;

    #[derive(Clone, Copy)]
    pub struct Logger;

    impl Logger {
        /// Get or initialize a logger
        pub fn get_logger() -> &'static Logger {
            Lazy::force(&LOGGER)
        }
    }

    #[allow(unused_must_use)]
    static LOGGER: Lazy<Logger> = Lazy::new(|| {
        env_logger::builder()
            .format_module_path(true)
            .format_timestamp_nanos()
            .target(env_logger::Target::Stderr)
            .filter(None, log::LevelFilter::Debug)
            .try_init();

        Logger
    });
}
