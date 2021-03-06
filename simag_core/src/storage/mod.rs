use std::{
    fs::{File, OpenOptions},
    hash::Hash,
    io,
    marker::PhantomData,
    ops::Deref,
    path::Path,
    pin::Pin,
    result::Result as StdResult,
};

#[cfg(test)]
use arbitrary::Arbitrary;
use parking_lot::RwLock;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

mod index;
mod manager;

pub(crate) type Result<T> = std::result::Result<T, StorageError>;

pub(crate) use manager::{
    Loadable, Metadata, MetadataKind, MetadataOwner, MetadataOwnerKind, StorageManager,
};

/// In-memory address for a record.
#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash, Clone, Copy, Debug)]
#[cfg_attr(test, derive(Arbitrary))]
pub(super) struct MemAddr<T: MemAddrMapp>(u64, PhantomData<T>);

pub(super) trait MemAddrMapp: Ord + PartialOrd + Eq + PartialEq + Clone + Hash {}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
pub(super) struct NonMapped;
impl MemAddrMapp for NonMapped {}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
pub(super) struct Mapped;
impl MemAddrMapp for Mapped {}

impl From<u64> for MemAddr<NonMapped> {
    fn from(addr: u64) -> Self {
        MemAddr(addr, PhantomData)
    }
}

impl From<u64> for MemAddr<Mapped> {
    fn from(addr: u64) -> Self {
        MemAddr(addr, PhantomData)
    }
}

impl<T: MemAddrMapp> Deref for MemAddr<T> {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// File addresss from which to fetch data from (based on the length of the record).
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
#[cfg_attr(test, derive(Arbitrary))]
struct DiscAddr(u64);

impl From<u64> for DiscAddr {
    fn from(addr: u64) -> Self {
        DiscAddr(addr)
    }
}

impl Deref for DiscAddr {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Type of the record being stored, used to deserialize the data on a later time.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[cfg_attr(test, derive(Arbitrary))]
pub(super) enum BinType {
    LogSent = 0,
    GrMemb = 1,
    GrFunc = 2,
    Movement = 3,
}

impl From<u8> for BinType {
    fn from(byte: u8) -> BinType {
        match byte {
            0 => BinType::LogSent,
            1 => BinType::GrMemb,
            2 => BinType::GrFunc,
            3 => BinType::Movement,
            byte => panic!("cannot cast {} to BinType", byte),
        }
    }
}

pub(crate) trait ToBinaryObject {
    fn get_type() -> BinType;
}

#[cfg_attr(test, derive(Clone, Arbitrary))]
pub(super) struct BinaryObj {
    /// the current physical address, used to preserve relationships
    pub address: MemAddr<NonMapped>,
    /// the actual binary serialized content of the record:
    // [key_size: u64 as [u8], key as [u8], data as [u8]]
    pub record: Vec<u8>,
    pub ty: BinType,
}

impl BinaryObj {
    pub fn build<T>(address: MemAddr<NonMapped>, key: &str, data: &T) -> bincode::Result<Self>
    where
        T: Serialize + ToBinaryObject,
    {
        // TODO: optimize this if possible to avoid copying unnecesarilly
        let bin_key = bincode::serialize(key)?;
        let mut record: Vec<u8> = (bin_key.len() as u64).to_le_bytes().to_vec();
        record.extend(bin_key);
        let record_data = &mut bincode::serialize::<T>(&data)?;
        record.append(record_data);
        Ok(BinaryObj {
            address,
            record,
            ty: <T as ToBinaryObject>::get_type(),
        })
    }
}

const U64_SIZE: usize = std::mem::size_of::<u64>();

/// Deserializes a chunk of bytes into a key-value pair of an instance of the expected type
/// built from bytes and the assigned key to the object.
pub fn get_from_metadata_storage<D>(data: Vec<u8>) -> Result<(String, D)>
where
    D: for<'a> Deserialize<'a>,
{
    let mut key_len = [0; U64_SIZE];
    key_len.copy_from_slice(&data[0..U64_SIZE]);
    let key_len = u64::from_le_bytes(key_len);
    let data_start = U64_SIZE + key_len as usize;
    let key: String = bincode::deserialize(&data[U64_SIZE..data_start])?;
    let data: D = bincode::deserialize(&data[data_start..])?;
    Ok((key, data))
}

pub fn ser_locked<S, T>(val: &RwLock<T>, serializer: S) -> StdResult<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: serde::Serialize,
{
    let val = &*val.read();
    val.serialize(serializer)
}

pub fn deser_locked<'de, D, T>(deserializer: D) -> StdResult<RwLock<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    let val: T = Deserialize::deserialize(deserializer)?;
    Ok(RwLock::new(val))
}

pub(crate) fn ser_pinned<S, T>(val: &Pin<Box<T>>, serializer: S) -> StdResult<S::Ok, S::Error>
where
    S: Serializer,
    T: Serialize,
{
    let obj = &**val;
    Ok(obj.serialize(serializer)?)
}

pub(crate) fn deser_pinned<'de, D, T>(deserializer: D) -> StdResult<Pin<Box<T>>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    let val: T = Deserialize::deserialize(deserializer)?;
    Ok(Box::pin(val))
}

fn open_dat_file(path: &Path) -> io::Result<File> {
    OpenOptions::new()
        .read(true)
        .write(true)
        .truncate(false)
        .create(true)
        .open(path)
}

#[derive(Debug, thiserror::Error)]
pub enum StorageError {
    #[error("manager not found in metadata")]
    ManagerNotFound,
    #[error("serialization error")]
    Serialization(#[from] bincode::Error),
    #[error("failed to write storage data to disc")]
    FailedToWrite,
    #[error("i/o error")]
    Io(#[from] io::Error),
}

#[cfg(test)]
pub(crate) mod test {
    use super::*;

    #[macro_export]
    macro_rules! assert_or_err {
        (io: $assertion:expr) => {{
            assert_or_err!(io: $assertion; io::Error::from(io::ErrorKind::InvalidInput))
        }};
        (io: $assertion:expr; $err:expr) => {{
            if !$assertion {
                return Err($err.into());
            }
        }};
    }

    pub fn tear_down<F>(test_fn: F, path: &Path) -> Result<()>
    where
        F: FnOnce() -> Result<()>,
    {
        let res = test_fn();
        std::fs::remove_dir_all(path)?;
        res
    }

    impl ToBinaryObject for [i32; 5] {
        fn get_type() -> BinType {
            // just for testing purpouses
            BinType::GrFunc
        }
    }

    #[test]
    fn deser_to_bin_obj() -> Result<()> {
        let data = [1i32; 5];
        let BinaryObj { record, .. } = BinaryObj::build(MemAddr(0, PhantomData), "test", &data)?;
        let (k, v) = get_from_metadata_storage::<[i32; 5]>(record)?;
        assert_eq!(k, "test");
        assert_eq!(v, [1i32; 5]);
        Ok(())
    }
}
