use std::{
    collections::{BTreeMap, HashMap},
    convert::TryFrom,
    fs::File,
    io,
    iter::FromIterator,
    ops::Deref,
    path::{Path, PathBuf},
    sync::atomic::{AtomicUsize, Ordering},
};

#[cfg(unix)]
use std::os::unix::prelude::FileExt;

use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// In-memory address for a record.
#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash)]
pub(super) struct MemAddr(u64);

impl From<u64> for MemAddr {
    fn from(addr: u64) -> Self {
        MemAddr(addr)
    }
}

impl Deref for MemAddr {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// File addresss from which to fetch data from (based on the length of the record).
struct DiskAddr(u64);

impl From<u64> for DiskAddr {
    fn from(addr: u64) -> Self {
        DiskAddr(addr)
    }
}

impl Deref for DiskAddr {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// The amount of data to be fetched from a given position.
type RecordLength = u64;

/// TypeId of the record being stored, used to deserialize the data later.
#[repr(u8)]
#[derive(Clone, Copy)]
pub(super) enum TypeId {
    LogSent = 0,
    GrMemb = 1,
    GrFunc = 2,
}

pub(super) trait ToBinaryObject
where
    Self: Sized,
{
    fn destruct(self) -> (MemAddr, Vec<u8>);
    fn get_type_id() -> TypeId;
    fn build<T>(address: MemAddr, key: &str, data: &T) -> bincode::Result<Self>
    where
        T: Serialize;
}

macro_rules! binary_storage {
    ( struct $bin:ident -> $type:expr) => {
        pub(super) struct $bin {
            /// the current physical address, used to preserve relationships
            address: MemAddr,
            /// the actual binary serialized content of the record:
            // [key_size: u64 as [u8], key as [u8], data as [u8]]
            record: Vec<u8>,
        }

        impl ToBinaryObject for $bin {
            fn build<T: Serialize>(address: MemAddr, key: &str, data: &T) -> bincode::Result<Self> {
                // TODO: optimize this if possible to avoid copying
                let bin_key = bincode::serialize(key)?;
                let mut record: Vec<u8> = (bin_key.len() as u64).to_le_bytes().to_vec();
                record.extend(bin_key);
                let record_data = &mut bincode::serialize::<T>(&data)?;
                record.append(record_data);
                Ok($bin { address, record })
            }

            fn destruct(self) -> (MemAddr, Vec<u8>) {
                (self.address, self.record)
            }

            fn get_type_id() -> TypeId {
                $type
            }
        }
    };
}

binary_storage!(struct BinGrFuncRecord -> TypeId::GrFunc);
binary_storage!(struct BinGrMembRecord -> TypeId::GrMemb);
binary_storage!(struct BinLogSentRecord -> TypeId::LogSent);

/// A handle in memory of the on-disk storage for a given Representation.
/// Allows for rebuilding of an agent from a binary blob as well as keeping the copy of data
/// in the storage layer up to date. Each representation has it's own file for storing their
/// own objects.
///
/// Accurately rebuilds any shared objects, maintaining the underlying memory relationship
/// and shared memory space.
pub(super) struct ReprStorageManager {
    /// C0 in-memory tree, buffer for data waiting to be flushed to disk.
    /// This is only for record data, meaning records which hold a stable address in memory
    /// which is used as a key.
    level0_map: BTreeMap<MemAddr, Vec<u8>>,
    buffer_size: u64,
    file_size: u64,
    page_manager: PageManager,
}

impl ReprStorageManager {
    /// Create a new ReprStorage for the given representation, will be stored at the given directory
    /// if provided or in a temporary directory otherwise.
    pub fn new(id: Uuid, path: Option<&Path>) -> Result<Self, io::Error> {
        let file_dir = if let Some(path) = path {
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

        // let file = OpenOptions::new()
        //     .read(true)
        //     .write(true)
        //     .truncate(false)
        //     .create(true)
        //     .open(&location)?;

        Ok(ReprStorageManager {
            level0_map: BTreeMap::new(),
            buffer_size: 0,
            file_size: 0,
            page_manager: PageManager::new(file_dir),
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
        if self.buffer_size + data.len() as u64 > PageManager::PAGE_SIZE {
            // should flush first
            self.flush::<O>();
            self.buffer_size = 0;
        }
        self.buffer_size += data.len() as u64;
        self.level0_map.insert(mem_addr, data);
    }

    pub fn flush<O: ToBinaryObject>(&mut self) {
        let mut to_flush = BTreeMap::new();
        let type_id = O::get_type_id() as u8;
        std::mem::swap(&mut to_flush, &mut self.level0_map);
        let mut buffer = Vec::with_capacity(PageManager::PAGE_SIZE as usize);
        //key_addr: BTreeMap<MemAddr, (DiskAddr, RecordLength)>
        let mut table_data = HashMap::new();
        let mut addr = 0u64;
        for (mem_addr, rec) in to_flush.into_iter() {
            let len = rec.len() as u64;
            // add [type_id: u8, mem_addr: u64 as [u8], size: u64 as [u8]], total 17 bytes header
            buffer.push(type_id);
            buffer.extend((&*mem_addr).to_le_bytes().as_ref());
            buffer.extend(&len.to_le_bytes());
            // add the value itself
            buffer.extend(rec);
            table_data.insert(
                mem_addr,
                DiskRecord {
                    type_id: O::get_type_id(),
                    addr: DiskAddr::from(addr),
                    len,
                },
            );
            addr += len;
        }

        if (buffer.len() as u64) < PageManager::PAGE_SIZE {
            // Fill with zeros the remaining until the page is full.
            let remainder = PageManager::PAGE_SIZE - (buffer.len() as u64);
            let remaining = vec![0; remainder as usize];
            buffer.extend(remaining);
        }
        debug_assert_eq!(buffer.len() as u64, PageManager::PAGE_SIZE);

        // self.page_manager;
        // #[cfg(unix)]
        // {
        //     self.file.write_at(&buffer, self.file_size).unwrap();
        // }
        self.file_size = buffer.len() as u64;
    }
}

impl TryFrom<&Path> for ReprStorageManager {
    type Error = bincode::Error;

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        let file = File::open(path)?;

        todo!()
    }
}

struct DiskRecord {
    type_id: TypeId,
    addr: DiskAddr,
    len: RecordLength,
}

impl Into<[u8; 17]> for DiskRecord {
    fn into(self) -> [u8; 17] {
        let mut bytes = [0u8; 17];
        bytes[0] = self.type_id as u8;
        let (_, pos_part) = bytes.split_at_mut(1);
        pos_part.copy_from_slice((&*self.addr).to_le_bytes().as_ref());
        let (_, len_part) = bytes.split_at_mut(9);
        len_part.copy_from_slice(self.len.to_le_bytes().as_ref());
        bytes
    }
}

static NEXT_PAGE_ID: AtomicUsize = AtomicUsize::new(0);
type PageId = usize;

struct PageMeta {
    /// Current size of page.
    size: u64,
    /// In representations a lot of records map well to key-values tuples, where the key is
    /// the shared memory address pointer and the value the underlying data.
    /// On pages we append the record to the log, and later on we can merge the data
    /// using the address as a key, fetching from the registered file disk address.
    key_addr: BTreeMap<MemAddr, (DiskAddr, RecordLength)>,
}

enum MapLevel {
    Level0,
    Level1,
    Level2,
    Level3,
}

struct PageManager {
    file_dir: PathBuf,
    levels: [BTreeMap<PageId, PageMeta>; 3],
}

impl PageManager {
    /// Modern disks sectors are usually 4KB bytes wide, and are optimized for such size writes/reads.
    /// All the pages must be then a multiple of this so they are written to block effitiently.
    const PAGE_SIZE: u64 = 1024 * 4;

    /// The LEVEL0 tree is maintained in-memory.
    /// Max size for all the pages
    const LEVEL1_SIZE: u64 = 1024 * 1024 * 10; // 10MB
    const LEVEL1_NUM_PAGES: u64 = Self::LEVEL1_SIZE / Self::PAGE_SIZE;
    const LEVEL2_SIZE: u64 = Self::LEVEL1_SIZE ^ 10; // 100 MB
    const LEVEL2_NUM_PAGES: u64 = Self::LEVEL2_SIZE / Self::PAGE_SIZE;
    // LEVEL3 size is unbounded
    const LEVEL3_MAX_FILE_SIZE: u64 = Self::LEVEL2_SIZE;

    fn new(file_dir: PathBuf) -> PageManager {
        let level1_map = BTreeMap::from_iter((0..Self::LEVEL1_NUM_PAGES).map(|_| {
            (
                NEXT_PAGE_ID.load(Ordering::SeqCst),
                PageMeta {
                    size: 0,
                    key_addr: BTreeMap::new(),
                },
            )
        }));
        debug_assert_eq!(level1_map.len() as u64, Self::LEVEL1_NUM_PAGES);

        let level2_map = BTreeMap::from_iter((0..Self::LEVEL2_NUM_PAGES).map(|_| {
            (
                NEXT_PAGE_ID.load(Ordering::SeqCst),
                PageMeta {
                    size: 0,
                    key_addr: BTreeMap::new(),
                },
            )
        }));
        debug_assert_eq!(level2_map.len() as u64, Self::LEVEL2_NUM_PAGES);

        PageManager {
            file_dir,
            levels: [level1_map, level2_map, BTreeMap::new()],
        }
    }

    #[inline(always)]
    fn spill_to_disk(&mut self, data: Vec<u8>) {
        let level1_map = &mut self.levels[0];
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn flush_data() -> Result<(), std::io::Error> {
        let mut rep = ReprStorageManager::new(Uuid::nil(), None)?;
        let record = String::from("test_data").into_bytes();
        let rec = BinGrMembRecord {
            address: 0x1234u64.into(),
            record,
        };

        rep.insert_rec(rec);
        rep.flush::<BinGrMembRecord>();

        Ok(())
    }
}
