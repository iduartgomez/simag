use std::{
    collections::{BTreeMap, HashMap},
    convert::TryFrom,
    fs::{File, OpenOptions},
    io,
    path::Path,
};

#[cfg(unix)]
use std::os::unix::prelude::FileExt;

use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::agent::Representation;

type MemoryAddress = usize;
/// The position from which to fetch data from.
type CursorPosition = u64;
/// The amount of data to be fetched from a given position.
type DataLength = u64;

#[repr(u8)]
#[derive(Clone, Copy)]
pub(super) enum TypeId {
    LogSent = 0,
    GrMemb = 1,
    GrFunc = 2,
}

struct Metadata(TypeId, CursorPosition, DataLength);

impl Into<[u8; 17]> for Metadata {
    fn into(self) -> [u8; 17] {
        let mut bytes = [0u8; 17];
        bytes[0] = self.0 as u8;
        let (_, pos_part) = bytes.split_at_mut(1);
        pos_part.copy_from_slice(&self.1.to_le_bytes());
        let (_, len_part) = bytes.split_at_mut(9);
        len_part.copy_from_slice(&self.2.to_le_bytes());
        bytes
    }
}

/// The name of the parent obj (class, func or entity)
pub type BinParentName = Vec<u8>;

pub(super) trait ToBinaryObject {
    fn destruct(self) -> (MemoryAddress, Vec<u8>);
    fn get_type_id() -> TypeId;
}

#[derive(Serialize, Deserialize)]
pub(super) struct BinGrFuncRecord {
    /// the current physical address, used to preserve relationships
    pub address: usize,
    /// the actual binary serialized content of the func
    pub func: Vec<u8>,
}

#[derive(Serialize, Deserialize)]
pub(super) struct BinGrMembRecord {
    /// the current physical address, used to preserve relationships
    pub address: usize,
    /// the actual binary serialized content of the grmemb rec
    pub memb: Vec<u8>,
}

impl ToBinaryObject for BinGrMembRecord {
    fn destruct(self) -> (MemoryAddress, Vec<u8>) {
        (self.address, self.memb)
    }

    fn get_type_id() -> TypeId {
        TypeId::GrMemb
    }
}

#[derive(Serialize, Deserialize)]
pub(super) struct BinLogSentRecord {
    /// the current physical address, used to preserve relationships
    pub address: usize,
    /// the actual binary serialized content of the sentence
    pub sent: Vec<u8>,
}

/// A handle in memory of the on-disk storage for a given Representation.
/// Allows for rebuilding of an agent from a binary blob as well as keeping the copy of data
/// in the storage layer up to date. Each representation has it's own file for storing their
/// own objects.
///
/// Accurately rebuilds any shared objects, maintaining the underlying memory relationship
/// and shared memory space.
pub(super) struct ReprStorage {
    file: File,
    id: Uuid,
    /// C0 in-memory tree, buffer for data waiting to be flushed to disk.
    /// This is only for record data, meaning records which hold a stable address in memory
    /// which is used as a key.
    records_buffer: BTreeMap<MemoryAddress, Vec<u8>>,
    buffer_size: u64,
    file_size: u64,
}

impl ReprStorage {
    /// Create a new ReprStorage for the given representation, will be stored at the given directory
    /// if provided or in a temporary directory otherwise.
    pub fn new(id: Uuid, path: Option<&Path>) -> Result<Self, io::Error> {
        let location = if let Some(path) = path {
            let md = path.metadata()?;
            if !md.is_dir() {
                return Err(io::ErrorKind::InvalidInput.into());
            }
            if md.permissions().readonly() {
                return Err(io::ErrorKind::PermissionDenied.into());
            }
            path.into()
        } else {
            std::env::temp_dir().join(format!("simag-{}", id))
        };

        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .truncate(false)
            .create(true)
            .open(&location)?;

        Ok(ReprStorage {
            file,
            id,
            records_buffer: BTreeMap::new(),
            buffer_size: 0,
            file_size: 0,
        })
    }

    /// Inserts a new record, if it exists the new record will be updated.
    ///
    /// Internally this does not flush to disk, but only stores the data in the record buffer.
    pub fn insert_rec<O>(&mut self, rec: O)
    where
        O: ToBinaryObject,
    {
        let (mem_addr, data) = rec.destruct();
        if self.buffer_size + data.len() as u64 > Table::PAGE_SIZE {
            // should flush first
            self.flush::<O>();
            self.buffer_size = 0;
        }
        self.buffer_size += data.len() as u64;
        self.records_buffer.insert(mem_addr, data);
    }

    pub fn flush<O: ToBinaryObject>(&mut self) {
        let mut to_flush = BTreeMap::new();
        let type_id = O::get_type_id() as u8;
        std::mem::swap(&mut to_flush, &mut self.records_buffer);
        let mut buffer = Vec::with_capacity(Table::PAGE_SIZE as usize);
        let mut table_data = HashMap::new();
        let mut position = 0u64;
        for (mem_addr, rec) in to_flush.into_iter() {
            let size = rec.len() as u64;
            // add [type_id: u8, mem_addr: u64 as [u8], size: u64 as [u8]], total 17 bytes header
            buffer.push(type_id);
            buffer.extend(&mem_addr.to_be_bytes());
            buffer.extend(&size.to_be_bytes());
            // add the value itself
            buffer.extend(rec);
            table_data.insert(mem_addr, Metadata(O::get_type_id(), position, size));
            position += size;
        }

        #[cfg(unix)]
        {
            self.file.write_at(&buffer, self.file_size).unwrap();
        }
        self.file_size = buffer.len() as u64;
    }
}

impl TryFrom<&Path> for ReprStorage {
    type Error = bincode::Error;

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        let file = File::open(path)?;

        todo!()
    }
}

struct Table {
    addresses: BTreeMap<MemoryAddress, (CursorPosition, DataLength)>,
}

impl Table {
    /// Modern disks sectors are usually 4096 bytes wide, and are optimized for such size writes/reads.
    const PAGE_SIZE: u64 = 4096;
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn flush_data() -> Result<(), std::io::Error> {
        let mut rep = ReprStorage::new(Uuid::nil(), None)?;
        let memb = String::from("test_data").into_bytes();
        let rec = BinGrMembRecord {
            address: 0x1234usize,
            memb,
        };

        rep.insert_rec(rec);
        rep.flush::<BinGrMembRecord>();

        Ok(())
    }
}
