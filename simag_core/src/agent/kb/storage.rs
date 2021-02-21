use std::{
    collections::BTreeMap,
    convert::TryFrom,
    fs::{File, OpenOptions},
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
#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash, Clone, Copy)]
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
#[derive(Clone, Copy)]
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
        #[derive(Clone)]
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
    level0_map: BTreeMap<MemAddr, Record>,
    buffer_size: u64,
    page_manager: PageManager,
}

impl ReprStorageManager {
    /// Create a new ReprStorage for the given representation, will be stored at the given directory
    /// if provided or in a temporary directory otherwise.
    pub fn new(id: Uuid, path: Option<&Path>) -> io::Result<Self> {
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
            let p = std::env::temp_dir().join(format!("simag-{}", id));
            if !p.exists() {
                std::fs::create_dir(&p)?;
            }
            p
        };

        Ok(ReprStorageManager {
            level0_map: BTreeMap::new(),
            buffer_size: 0,
            page_manager: PageManager::new(file_dir, id),
        })
    }

    /// Inserts a new record, if it exists the new record will be updated.
    ///
    /// Internally this does not flush to disk, but only stores the data in the record buffer.
    pub fn insert_rec<O>(&mut self, rec: O) -> io::Result<()>
    where
        O: ToBinaryObject,
    {
        let (mem_addr, data) = rec.destruct();
        let rec = Record::new(O::get_type_id(), mem_addr, data);
        if self.buffer_size + rec.len() as u64 > PageManager::PAGE_SIZE {
            // should flush first
            self.flush()?;
            self.buffer_size = 0;
        }
        self.buffer_size += rec.len() as u64;
        self.level0_map.insert(mem_addr, rec);
        Ok(())
    }

    pub fn flush(&mut self) -> io::Result<()> {
        let mut to_flush = BTreeMap::new();
        std::mem::swap(&mut to_flush, &mut self.level0_map);
        let mut buffer = Vec::with_capacity(PageManager::PAGE_SIZE as usize);
        let mut table_data = BTreeMap::new();
        let mut addr = 0u64;
        for (mem_addr, rec) in to_flush.into_iter() {
            let len = rec.len() as u64;
            let type_id = rec.type_id();
            rec.append_to_buf(&mut buffer);
            table_data.insert(
                mem_addr,
                DiscRecordRef {
                    type_id,
                    addr: DiscAddr::from(addr),
                    len,
                },
            );
            addr += len;
        }
        self.page_manager.spill_to_disk(buffer, table_data)
    }
}

impl TryFrom<&Path> for ReprStorageManager {
    type Error = bincode::Error;

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        let _file = File::open(path)?;
        todo!()
    }
}

/// A record prepared to be stored on disk.
struct Record {
    type_id: TypeId,
    mem_addr: MemAddr,
    data: Vec<u8>,
    /// computed lenth after encoding to bytes
    length: u64,
}

impl Record {
    fn new(type_id: TypeId, mem_addr: MemAddr, data: Vec<u8>) -> Self {
        let length = data.len() as u64 + 17;
        Record {
            type_id,
            mem_addr,
            data,
            length,
        }
    }

    #[inline]
    fn len(&self) -> u64 {
        self.length
    }

    fn append_to_buf(self, buffer: &mut Vec<u8>) {
        let Record {
            type_id,
            mem_addr,
            data,
            ..
        } = self;
        let header = Self::make_header(type_id, mem_addr, data.len() as u64);
        buffer.extend(&header);
        buffer.extend(data);
    }

    fn type_id(&self) -> TypeId {
        self.type_id
    }

    #[inline(always)]
    fn make_header(type_id: TypeId, mem_addr: MemAddr, len: u64) -> [u8; 17] {
        let mut header = [0u8; 17];
        let addr_part = &mut header[0..8];
        addr_part.copy_from_slice((&*mem_addr).to_le_bytes().as_ref());
        header[8] = type_id as u8;
        let len_part = &mut header[9..];
        len_part.copy_from_slice(len.to_le_bytes().as_ref());
        header
    }
}

struct DiscRecordRef {
    type_id: TypeId,
    addr: DiscAddr,
    len: RecordLength,
}

static NEXT_PAGE_ID: AtomicUsize = AtomicUsize::new(0);
type PageId = usize;

struct PageMeta {
    id: PageId,
    offset: u64,
    /// Current free space of page.
    free_space: u64,
    /// In representations a lot of records map well to key-values tuples, where the key is
    /// the shared memory address pointer and the value the underlying data.
    /// On pages we append the record to the log, and later on we can merge the data
    /// using the address as a key, fetching from the registered file disk address.
    key_addr: BTreeMap<MemAddr, DiscRecordRef>,
}

/// Manages pages and file synchronization with memory data for log record-like data.
struct PageManager {
    id: Uuid,
    file_dir: PathBuf,
    levels: [Vec<PageMeta>; 3],
}

impl PageManager {
    /// Modern disks blocks are usually 4KB bytes wide, and are optimized for such size writes/reads.
    /// All the pages must be then a multiple of this so they are written to block effitiently.
    const PAGE_SIZE: u64 = 1024 * 4;

    /// The LEVEL0 tree is maintained in-memory.
    /// Max size for all the pages
    const LEVEL1_SIZE: u64 = 1024 * 1024 * 10; // 10MB
    const LEVEL1_NUM_PAGES: u64 = Self::LEVEL1_SIZE / Self::PAGE_SIZE;
    const LEVEL2_SIZE: u64 = Self::LEVEL1_SIZE * 10; // 100 MB
    const LEVEL2_NUM_PAGES: u64 = Self::LEVEL2_SIZE / Self::PAGE_SIZE;
    // LEVEL3 size is unbounded
    const LEVEL3_MAX_FILE_SIZE: u64 = Self::LEVEL2_SIZE;

    fn new(file_dir: PathBuf, id: Uuid) -> PageManager {
        let mut lvl1_offset = 0;
        let lvl1_pages = Vec::from_iter((0..Self::LEVEL1_NUM_PAGES).map(|_| {
            let p = PageMeta {
                id: NEXT_PAGE_ID.load(Ordering::SeqCst),
                offset: lvl1_offset,
                free_space: Self::PAGE_SIZE,
                key_addr: BTreeMap::new(),
            };
            lvl1_offset += Self::PAGE_SIZE;
            p
        }));
        debug_assert_eq!(lvl1_pages.len() as u64, Self::LEVEL1_NUM_PAGES);

        let mut lvl2_offset = 0;
        let lvl2_pages = Vec::from_iter((0..Self::LEVEL2_NUM_PAGES).map(|_| {
            let p = PageMeta {
                id: NEXT_PAGE_ID.load(Ordering::SeqCst),
                offset: lvl2_offset,
                free_space: Self::PAGE_SIZE * 10,
                key_addr: BTreeMap::new(),
            };
            lvl2_offset += Self::PAGE_SIZE;
            p
        }));
        debug_assert_eq!(lvl2_pages.len() as u64, Self::LEVEL2_NUM_PAGES);

        PageManager {
            id,
            file_dir,
            levels: [lvl1_pages, lvl2_pages, Vec::new()],
        }
    }

    #[inline(always)]
    fn spill_to_disk(
        &mut self,
        mut buffer: Vec<u8>,
        table_map: BTreeMap<MemAddr, DiscRecordRef>,
    ) -> io::Result<()> {
        let p = self.file_dir.join("data0.dat");

        let lvl1_pages = &mut self.levels[0];
        lvl1_pages.sort_unstable_by_key(|e| e.free_space);
        if lvl1_pages[0].free_space >= buffer.len() as u64 {
            let page = &mut lvl1_pages[0];
            let buf_len = buffer.len() as u64;
            if buf_len < page.free_space {
                // Fill with zeros the remaining until the page is full.
                let remainder = page.free_space - buf_len;
                let remaining = vec![0; remainder as usize];
                buffer.extend(remaining);
            }

            let file = OpenOptions::new()
                .write(true)
                .truncate(false)
                .create(true)
                .open(&p)?;

            let offset = page.offset + (Self::PAGE_SIZE - page.free_space);
            #[cfg(unix)]
            {
                file.write_at(&buffer, offset)?;
            }

            for (addr, rec) in table_map {
                page.key_addr.insert(addr, rec);
            }
            page.free_space -= buf_len;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn flush_data() -> io::Result<()> {
        let mut rep = ReprStorageManager::new(Uuid::nil(), None)?;
        let record = String::from("test_data").into_bytes();
        let rec = BinGrMembRecord {
            address: 0x1234u64.into(),
            record,
        };
        for _ in 0..3 {
            rep.insert_rec(rec.clone())?;
            rep.flush()?;
        }
        rep.insert_rec(rec.clone())?;
        rep.insert_rec(rec)?;
        rep.flush()?;
        Ok(())
    }
}
