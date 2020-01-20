//! Main knowledge-base logic module, in this module reside the different
//! types that transform and store the data for the individual agents and
//! serve as representations of the different objects and the relationships
//! between them.
pub(in crate::agent) mod bms;
pub(self) mod class;
mod inference;
pub mod repl;
pub(super) mod repr;

#[cfg(test)]
mod test;

use super::lang::{GroundedFunc, GroundedMemb};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone)]
pub(in crate::agent) struct VarAssignment<'rep> {
    pub name: &'rep str,
    classes: HashMap<&'rep str, Arc<GroundedMemb>>,
    funcs: HashMap<&'rep str, Vec<Arc<GroundedFunc>>>,
}

impl<'rep> VarAssignment<'rep> {
    #[inline]
    pub fn get_class(&self, name: &str) -> Option<&Arc<GroundedMemb>> {
        self.classes.get(name)
    }

    #[inline]
    pub fn get_relationship(&self, func: &GroundedFunc) -> Option<&Arc<GroundedFunc>> {
        if let Some(funcs) = self.funcs.get(func.get_name()) {
            for owned_f in funcs {
                if owned_f.comparable(func) {
                    return Some(owned_f);
                }
            }
        }
        None
    }
}

impl<'rep> std::cmp::PartialEq for VarAssignment<'rep> {
    fn eq(&self, other: &VarAssignment) -> bool {
        *self.name == *other.name
    }
}

impl<'rep> std::cmp::Eq for VarAssignment<'rep> {}

impl<'rep> std::hash::Hash for VarAssignment<'rep> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (*self.name).hash(state);
    }
}

impl<'rep> std::fmt::Display for VarAssignment<'rep> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'rep> std::fmt::Debug for VarAssignment<'rep> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[cfg(feature = "tracing")]
pub(self) mod tracing {
    #[derive(Clone, Copy)]
    pub struct Logger;

    impl Logger {
        pub fn get_logger() -> &'static Logger {
            Lazy::force(&LOGGER)
        }
    }

    use once_cell::sync::Lazy;

    static LOGGER: Lazy<Logger> = Lazy::new(|| {
        use log::LevelFilter;

        env_logger::builder()
            .format_module_path(true)
            .format_timestamp_nanos()
            .target(env_logger::Target::Stderr)
            .filter(None, LevelFilter::Trace)
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
