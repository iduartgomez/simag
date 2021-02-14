//! Persistence layer and facilities to allow efficient storage of representation data structures.
use std::sync::Arc;

use parking_lot::RwLock;
use serde::{Deserialize, Deserializer};

use super::kb::bms::{BmsKind, BmsWrapper};

pub fn ser_locked<S, T>(val: &RwLock<T>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: serde::Serialize,
{
    let val = &*val.read();
    val.serialize(serializer)
}

pub fn deser_locked<'de, D, T>(deserializer: D) -> Result<RwLock<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    let val: T = Deserialize::deserialize(deserializer)?;
    Ok(RwLock::new(val))
}

pub(in crate::agent) fn ser_optional_bms<S, T>(
    val: &Option<Arc<BmsWrapper<T>>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: BmsKind,
{
    match val.as_ref().map(|v| &*v) {
        None => serializer.serialize_none(),
        Some(wrapper) => serializer.serialize_some(&**wrapper),
    }
}

pub(in crate::agent) fn deser_optional_bms<'de, D, T>(
    deserializer: D,
) -> Result<Option<Arc<BmsWrapper<T>>>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: BmsKind,
{
    let val: Option<BmsWrapper<T>> = Deserialize::deserialize(deserializer)?;
    Ok(val.map(|v| Arc::new(v)))
}
