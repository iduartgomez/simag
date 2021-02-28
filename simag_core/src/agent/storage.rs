//! Persistence layer and facilities to allow efficient storage of representation data structures.
use std::pin::Pin;

use serde::{Deserialize, Deserializer, Serializer};

use super::kb::bms::{BmsKind, BmsWrapper};

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
