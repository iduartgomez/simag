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
pub(in crate::agent) struct VarAssignment<'a> {
    pub name: &'a str,
    classes: HashMap<&'a str, Arc<GroundedMemb>>,
    funcs: HashMap<&'a str, Vec<Arc<GroundedFunc>>>,
}

impl<'a> VarAssignment<'a> {
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

impl<'a> std::cmp::PartialEq for VarAssignment<'a> {
    fn eq(&self, other: &VarAssignment) -> bool {
        *self.name == *other.name
    }
}

impl<'a> std::cmp::Eq for VarAssignment<'a> {}

impl<'a> std::hash::Hash for VarAssignment<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (*self.name).hash(state);
    }
}

impl<'a> std::fmt::Display for VarAssignment<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a> std::fmt::Debug for VarAssignment<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[cfg(feature = "tracing")]
#[derive(Clone, Copy)]
struct Logger;

#[cfg(feature = "tracing")]
impl Logger {
    fn get_logger() -> &'static Logger {
        Lazy::force(&LOGGER)
    }
}

#[cfg(feature = "tracing")]
use once_cell::sync::Lazy;

#[cfg(feature = "tracing")]
static LOGGER: Lazy<Logger> = Lazy::new(|| {
    use log::LevelFilter;

    env_logger::builder()
        .default_format_module_path(true)
        .default_format_timestamp_nanos(true)
        .target(env_logger::Target::Stderr)
        .filter(None, LevelFilter::Trace)
        .init();

    Logger
});

#[cfg(feature = "tracing")]
fn tracing_info<T: std::fmt::Display, I: std::fmt::Display>(
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
