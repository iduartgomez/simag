#![allow(dead_code)]

use log::LevelFilter;
use once_cell::sync::Lazy;
use std::str::FromStr;

static CONF: Lazy<ConfigVars> = Lazy::new(|| {
    let log_level = std::env::var("SIMAG_LOG_LEVEL")
        .or_else::<std::env::VarError, _>(|_| Ok("info".to_owned()))
        .ok()
        .map(|l| LevelFilter::from_str(&l).unwrap_or_else(|_| LevelFilter::Debug))
        .unwrap_or_else(|| LevelFilter::Debug);

    ConfigVars { log_level }
});

pub(super) struct ConfigVars {
    pub log_level: log::LevelFilter,
}

#[cfg(debug_assertions)]
pub(super) mod tracing {
    use super::*;

    #[derive(Clone, Copy)]
    pub struct Logger;

    impl Logger {
        pub fn get_logger() -> &'static Logger {
            Lazy::force(&LOGGER)
        }
    }

    static LOGGER: Lazy<Logger> = Lazy::new(|| {
        env_logger::builder()
            .format_module_path(true)
            .format_timestamp_nanos()
            .target(env_logger::Target::Stderr)
            .filter(None, CONF.log_level)
            .init();

        Logger
    });

    pub fn tracing_info<T: std::fmt::Display, I: std::fmt::Display>(
        item_to_log: T,
        log_level: log::Level,
        extra_info: Option<I>,
    ) {
        use log::{log, log_enabled};

        if log_enabled!(log_level) {
            if let Some(info) = extra_info {
                log!(log_level, "{}: {}", info, item_to_log);
            } else {
                log!(log_level, "{}", item_to_log);
            }
        }
    }
}
