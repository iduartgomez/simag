#![allow(dead_code)]
use dashmap::DashMap;
#[cfg(feature = "persistence")]
use serde::{Deserialize, Serialize};
use std::{
    collections::hash_map::RandomState,
    hash::{BuildHasher, Hash, Hasher},
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
pub(crate) struct IdToken {
    key: u64,
    map_id: usize,
}

impl IdToken {
    pub fn generate_uid(&self) -> Vec<u8> {
        let mut uid = self
            .map_id
            .to_le_bytes()
            .iter()
            .copied()
            .collect::<Vec<_>>();
        uid.extend(self.key.to_le_bytes().iter());
        uid
    }
}

impl Hash for IdToken {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // safely ignore map_id
        state.write_u64(self.key);
    }
}

#[derive(Clone, Copy, Default)]
struct NoOpHasher {
    key: Option<u64>,
}

impl Hasher for NoOpHasher {
    fn finish(&self) -> u64 {
        self.key.unwrap()
    }

    fn write_u64(&mut self, i: u64) {
        self.key = Some(i);
    }

    fn write(&mut self, _bytes: &[u8]) {
        unimplemented!()
    }
}

impl BuildHasher for NoOpHasher {
    type Hasher = NoOpHasher;

    fn build_hasher(&self) -> Self::Hasher {
        NoOpHasher { key: None }
    }
}

/// Data structure which holds the inner data of the variable map.
pub(crate) struct TableData<T>(DashMap<IdToken, Box<T>, NoOpHasher>)
where
    T: 'static + ?Sized;

// pub(crate) struct TableData<T: 'static + ?Sized>(&'static InnerTableData<T>);

impl<T> TableData<T>
where
    T: 'static + ?Sized,
{
    pub fn init_static() -> TableData<T> {
        TableData(DashMap::with_hasher(NoOpHasher::default()))
    }
}

static NEXT_MAP_ID: AtomicUsize = AtomicUsize::new(0);

/// A static map of variables (identified by a token) and their assigned static values.
///
/// This map only allows for values which are heap allocated, with a stable memory addresses
/// for the duration of the program. It's an append-only map and values cannot be modified after
/// insertion.
///
/// The references returned are to the underlying values data and with a static lifetime.
///
/// # Warnings
/// Don't store anything that requires destructors run, as the behaviour is platform-specific
/// on exit.
#[derive(Clone)]
pub(crate) struct VariableMap<T: 'static + ?Sized> {
    items: &'static TableData<T>,
    /// While it would be nice to use a faster hash there is an open attack vector since keys
    /// ultimately may come from hostile actors from the network, so don't be tempted to change
    /// for a non-resistant hasher.
    ///
    /// Here we use the default hasher in the std lib which is guaranteed to be secure.
    hash_builder: RandomState,
    /// Unique identifier of this map
    map_id: usize,
}

impl<T> VariableMap<T>
where
    T: Hash + ?Sized,
{
    pub fn new(inner_table: &'static TableData<T>) -> Self {
        VariableMap {
            items: inner_table,
            hash_builder: RandomState::new(),
            map_id: NEXT_MAP_ID.fetch_add(1, Ordering::SeqCst),
        }
    }

    pub fn get(&self, index: &IdToken) -> Option<&'static T> {
        if index.map_id != self.map_id {
            return None;
        }
        let r = self.items.0.get(index);
        // Safety: everything here is static, so getting a ptr to the heap allocated item value
        // will be fine and the item address will remain stable for the duration of the program
        // since is the pointer to the inner boxed data.
        r.map(|r| unsafe { &*(r.as_ref() as *const _) as &'static _ })
    }

    /// Adds a new item to the table if it didn't exists previously, otherwise is a no-op.
    pub fn push<Boxable: Into<Box<T>>>(&self, item: Boxable) -> IdToken {
        let boxed = item.into();
        let mut hasher = self.hash_builder.build_hasher();
        boxed.as_ref().hash(&mut hasher);
        let key = hasher.finish();
        self.items
            .0
            .entry(IdToken {
                key,
                map_id: self.map_id,
            })
            .or_insert(boxed);
        IdToken {
            key,
            map_id: self.map_id,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use once_cell::sync::Lazy;

    #[test]
    fn create_var_table() {
        static TEST_VAR_TABLE: Lazy<TableData<str>> = Lazy::new(TableData::init_static);
        let map = VariableMap::new(&*TEST_VAR_TABLE);

        let key0 = map.push("key0".to_owned().into_boxed_str());
        let key1 = map.push("key1".to_owned().into_boxed_str());
        assert_eq!(map.get(&key0).unwrap(), "key0");
        assert_eq!(map.get(&key1).unwrap(), "key1");
    }

    #[test]
    fn return_existing_value() {
        static TEST_VAR_TABLE: Lazy<TableData<str>> = Lazy::new(TableData::init_static);
        let map = VariableMap::new(&*TEST_VAR_TABLE);

        let key0_loc0 = "key0".to_owned().into_boxed_str();
        let key0_loc1 = "key0".to_owned().into_boxed_str();
        assert_ne!(&*key0_loc0 as *const _, &*key0_loc1 as *const _);

        let key0_id0 = map.push(key0_loc0);
        let key0_id1 = map.push(key0_loc1);
        assert_eq!(key0_id0, key0_id1);

        let key0_val0 = map.get(&key0_id0).unwrap();
        let key0_val1 = map.get(&key0_id1).unwrap();
        let val0_ptr = key0_val0 as *const str;
        let val1_ptr = key0_val1 as *const str;
        assert_eq!(val0_ptr, val1_ptr);
        assert_eq!(key0_val0, key0_val1);
    }
}
