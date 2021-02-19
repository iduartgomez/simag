//! Persistence layer and facilities to allow efficient storage of representation data structures.
use std::{ops::Deref, pin::Pin};

use parking_lot::RwLock;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

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

pub(in crate::agent) fn ser_pinned<S, T>(
    val: &Pin<Box<T>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    T: Serialize,
{
    let obj = &**val;
    Ok(obj.serialize(serializer)?)
}

pub(in crate::agent) fn deser_pinned<'de, D, T>(deserializer: D) -> Result<Pin<Box<T>>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    let val: T = Deserialize::deserialize(deserializer)?;
    Ok(Box::pin(val))
}

pub(in crate::agent) fn ser_optional_bms<S, T>(
    val: &Option<Pin<Box<BmsWrapper<T>>>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    T: BmsKind,
{
    match val.as_ref().map(|v| &*v) {
        None => serializer.serialize_none(),
        Some(wrapper) => serializer.serialize_some(&**wrapper),
    }
}

pub(in crate::agent) fn deser_optional_bms<'de, D, T>(
    deserializer: D,
) -> Result<Option<Pin<Box<BmsWrapper<T>>>>, D::Error>
where
    D: Deserializer<'de>,
    T: BmsKind,
{
    let val: Option<BmsWrapper<T>> = Deserialize::deserialize(deserializer)?;
    Ok(val.map(|v| Box::pin(v)))
}
