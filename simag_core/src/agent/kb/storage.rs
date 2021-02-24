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

#[cfg(test)]
use arbitrary::Arbitrary;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// In-memory address for a record.
#[derive(PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash, Clone, Copy)]
#[cfg_attr(test, derive(Arbitrary))]
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

/// Type of the record being stored, used to deserialize the data later.
#[repr(u8)]
#[derive(Clone, Copy)]
pub(super) enum BinType {
    LogSent = 0,
    GrMemb = 1,
    GrFunc = 2,
}

pub(super) trait ToBinaryObject
where
    Self: Sized,
{
    fn destruct(self) -> (MemAddr, Vec<u8>);
    fn get_type() -> BinType;
    fn build<T>(address: MemAddr, key: &str, data: &T) -> bincode::Result<Self>
    where
        T: Serialize;
}

macro_rules! binary_storage {
    ( struct $bin:ident -> $type:expr) => {
        #[derive(Clone)]
        // #[cfg_attr(test, derive(Arbitrary))]
        pub(super) struct $bin {
            /// the current physical address, used to preserve relationships
            address: MemAddr,
            /// the actual binary serialized content of the record:
            // [key_size: u64 as [u8], key as [u8], data as [u8]]
            record: Vec<u8>,
        }

        impl ToBinaryObject for $bin {
            fn build<T: Serialize>(address: MemAddr, key: &str, data: &T) -> bincode::Result<Self> {
                // TODO: optimize this if possible to avoid copying unnecesarilly
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

            fn get_type() -> BinType {
                $type
            }
        }

        #[cfg(test)]
        impl Arbitrary for $bin {
            fn arbitrary(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
                let iter = u.arbitrary_iter::<u8>()?;
                let record: Result<Vec<_>, _> = iter.collect();
                Ok($bin {
                    address: u.arbitrary()?,
                    record: record?,
                })
            }
        }
    };
}

binary_storage!(struct BinGrFuncRecord -> BinType::GrFunc);
binary_storage!(struct BinGrMembRecord -> BinType::GrMemb);
binary_storage!(struct BinLogSentRecord -> BinType::LogSent);

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
            page_manager: PageManager::new(file_dir, id)?,
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
        let rec = Record::new(O::get_type(), mem_addr, data);
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
                    length: len,
                },
            );
            addr += len;
        }
        self.page_manager.spill_to_disc(buffer, table_data)
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
    type_id: BinType,
    mem_addr: MemAddr,
    data: Vec<u8>,
    /// computed lenth after encoding to bytes
    length: u64,
}

impl Record {
    const HEADER_SIZE: usize = 17;

    fn new(type_id: BinType, mem_addr: MemAddr, data: Vec<u8>) -> Self {
        let length = data.len() as u64 + Self::HEADER_SIZE as u64;
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

    fn type_id(&self) -> BinType {
        self.type_id
    }

    #[inline(always)]
    fn make_header(type_id: BinType, mem_addr: MemAddr, len: u64) -> [u8; Self::HEADER_SIZE] {
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
    type_id: BinType,
    addr: DiscAddr,
    length: RecordLength,
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
    lvl1_data: File,
    lvl2_data: File,
    lvl3_data: Vec<File>,
    levels_pages: [Vec<PageMeta>; 3],
    current_page: [usize; 3],
}

enum Table {
    Level0,
    Level1,
    Level2,
    Level3,
}

impl PageManager {
    /// Modern disks blocks are usually 4KB bytes wide, and are optimized for such size writes/reads.
    /// All the pages must be then a multiple of this so they are written to block effitiently.
    const PAGE_SIZE: u64 = 1024 * 4;

    /// The LEVEL0 tree is maintained in-memory.
    /// Max size for all the pages
    const LEVEL1_SIZE: u64 = 1024 * 1024 * 8; // 8MB
    const LEVEL1_NUM_PAGES: u64 = Self::LEVEL1_SIZE / Self::PAGE_SIZE;
    const LEVEL2_SIZE: u64 = Self::LEVEL1_SIZE * 8; // 64 MB
    const LEVEL2_NUM_PAGES: u64 = Self::LEVEL2_SIZE / Self::PAGE_SIZE;
    // LEVEL3 size is unbounded
    const LEVEL3_MAX_FILE_SIZE: u64 = Self::LEVEL2_SIZE * 8;

    fn new(file_dir: PathBuf, id: Uuid) -> io::Result<PageManager> {
        let mut lvl1_offset = 0;
        let lvl1_pages = Vec::from_iter((0..Self::LEVEL1_NUM_PAGES).map(|_| {
            let p = PageMeta {
                id: NEXT_PAGE_ID.fetch_add(1, Ordering::SeqCst),
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
                id: NEXT_PAGE_ID.fetch_add(1, Ordering::SeqCst),
                offset: lvl2_offset,
                free_space: Self::PAGE_SIZE * 10,
                key_addr: BTreeMap::new(),
            };
            lvl2_offset += Self::PAGE_SIZE;
            p
        }));
        debug_assert_eq!(lvl2_pages.len() as u64, Self::LEVEL2_NUM_PAGES);

        let lvl1_path = file_dir.join("simag.1.dat");
        let lvl1_data = Self::open_dat_file(&lvl1_path)?;

        let lvl2_path = file_dir.join("simag.2.dat");
        let lvl2_data = Self::open_dat_file(&lvl2_path)?;

        Ok(PageManager {
            id,
            file_dir,
            lvl1_data,
            lvl2_data,
            lvl3_data: Vec::new(),
            levels_pages: [lvl1_pages, lvl2_pages, Vec::new()],
            current_page: [0; 3],
        })
    }

    fn spill_to_disc(
        &mut self,
        mut buffer: Vec<u8>,
        table_map: BTreeMap<MemAddr, DiscRecordRef>,
    ) -> io::Result<()> {
        let lvl1_pages = &mut self.levels_pages[0];
        let mut current_lvl1_page = self.current_page[0];
        let mut fits_in_lvl1 = true;
        while lvl1_pages[current_lvl1_page].free_space < buffer.len() as u64 {
            if current_lvl1_page + 1 < lvl1_pages.len() {
                current_lvl1_page += 1;
            } else {
                fits_in_lvl1 = false;
                break;
            }
        }

        if fits_in_lvl1 {
            let active_lvl1_page = &mut self.levels_pages[0][current_lvl1_page];
            let buf_len = buffer.len() as u64;
            if buf_len < active_lvl1_page.free_space {
                // Fill with zeros the remaining until the page is full.
                let remainder = active_lvl1_page.free_space - buf_len;
                let remaining = vec![0; remainder as usize];
                buffer.extend(remaining);
            }

            let offset = active_lvl1_page.offset + (Self::PAGE_SIZE - active_lvl1_page.free_space);
            #[cfg(unix)]
            {
                self.lvl1_data.write_at(&buffer, offset)?;
            }

            for (addr, rec) in table_map {
                active_lvl1_page.key_addr.insert(addr, rec);
            }
            active_lvl1_page.free_space -= buf_len;

            return Ok(());
        }

        // lvl1 pages are full, merge to lvl 2
        self.compact(Table::Level1)?;

        Ok(())
    }

    fn compact(&mut self, lvl: Table) -> io::Result<()> {
        let (merge_src_pages, merge_src_data, merge_target, target_size) = match lvl {
            Table::Level1 => (
                &mut self.levels_pages[0],
                &mut self.lvl1_data,
                Table::Level2,
                Self::LEVEL1_SIZE / 4,
            ),
            Table::Level2 => (
                &mut self.levels_pages[1],
                &mut self.lvl2_data,
                Table::Level3,
                Self::LEVEL2_SIZE / 4,
            ),
            _ => unreachable!(),
        };

        // pages are sorted by insertion order, so don't need to do anything
        // just keep the last pointer per key
        let mut merged_data = BTreeMap::new();
        let mut size = 0u64;
        for page in merge_src_pages {
            if page.free_space < (target_size - size) {
                // empty all addresses
                let mut page_addresses = BTreeMap::new();
                std::mem::swap(&mut page_addresses, &mut page.key_addr);
                for (addr, rec) in page_addresses {
                    size += rec.length;
                    merged_data.insert(addr, rec);
                }
                page.free_space = Self::PAGE_SIZE;
            } else {
                let mut keys_to_rm = vec![];
                for (k, v) in page.key_addr.iter() {
                    let new_len = size + v.length;
                    if new_len > target_size {
                        break;
                    }
                    keys_to_rm.push(*k);
                    size = new_len;
                }
                // TODO: need to consolidate the lingering empty space in this page
                for key in keys_to_rm {
                    if let Some(page) = page.key_addr.remove(&key) {
                        merged_data.insert(key, page);
                    }
                }
                break;
            }
        }

        // read the data from disc to memory
        struct LoadedRec {
            type_id: BinType,
            length: RecordLength,
            page_num: usize,
        }

        let mut pages_buf =
            vec![vec![0u8; Self::PAGE_SIZE as usize]; (target_size / Self::PAGE_SIZE) as usize];
        let mut readed = vec![];
        for (page_num, buf) in pages_buf.iter_mut().enumerate() {
            let mut r_cursor = 0usize;
            let mut copied = vec![];
            for (addr, rec) in merged_data.iter() {
                if (r_cursor + rec.length as usize) <= Self::PAGE_SIZE as usize {
                    #[cfg(unix)]
                    {
                        merge_src_data.read_exact_at(
                            &mut buf[r_cursor..(r_cursor + rec.length as usize)],
                            *rec.addr,
                        )?;
                    }
                    r_cursor += rec.length as usize;
                    copied.push(*addr);
                } else {
                    break;
                }
            }
            // remove entries already copied to buffer so they are not reprocessed
            for addr in copied {
                if let Some(rec) = merged_data.remove(&addr) {
                    readed.push((
                        addr,
                        LoadedRec {
                            type_id: rec.type_id,
                            length: rec.length,
                            page_num,
                        },
                    ));
                }
            }
        }
        assert!(merged_data.is_empty());

        match merge_target {
            Table::Level2 => {
                let target_pages = &mut self.levels_pages[1];
                let target_file = &mut self.lvl2_data;
                let mut curr_page_buf = 0;
                for page in target_pages.iter_mut() {
                    if page.free_space > 0 && curr_page_buf + 1 < pages_buf.len() {
                        let page_buf = &pages_buf[curr_page_buf];
                        #[cfg(unix)]
                        {
                            target_file.write_at(page_buf, page.offset)?;
                        }
                        curr_page_buf += 1;
                        page.free_space = 0;
                    } else {
                        break;
                    }
                    let mut w_cursor = page.offset;
                    for (addr, rec) in readed.iter() {
                        if rec.page_num == curr_page_buf {
                            let rec = DiscRecordRef {
                                type_id: rec.type_id,
                                addr: DiscAddr(w_cursor),
                                length: rec.length,
                            };
                            w_cursor += rec.length;
                            page.key_addr.insert(*addr, rec);
                        }
                    }
                }
                if curr_page_buf + 1 < pages_buf.len() {
                    // no space left at lvl2, trigger a compaction from lvl2 to lvl3
                    todo!()
                }
            }
            Table::Level3 => {}
            _ => unreachable!(),
        }

        Ok(())
    }

    fn open_dat_file(path: &Path) -> io::Result<File> {
        OpenOptions::new()
            .read(true)
            .write(true)
            .truncate(false)
            .create(true)
            .open(path)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use arbitrary::Unstructured;
    use rand::Rng;

    fn raw_sample(iters: usize) -> Vec<u8> {
        let mut sample = Vec::new();
        for _ in 0..iters {
            let k: u64 = rand::random();
            sample.extend(&k.to_be_bytes());
            sample.extend((0..128).map(|_| rand::random::<u8>()));
        }
        sample
    }

    #[test]
    fn flush_data() -> io::Result<()> {
        const NUM_REC: usize = 500_000;
        let sample = raw_sample(NUM_REC);
        let mut u = Unstructured::new(&sample);
        let mut rep = ReprStorageManager::new(Uuid::nil(), None)?;

        let mut rng = rand::thread_rng();
        for _ in 0..NUM_REC {
            match rng.gen_range(0..3) {
                0 => {
                    let rec: BinGrFuncRecord = u.arbitrary().unwrap();
                    rep.insert_rec(rec)?;
                }
                1 => {
                    let rec: BinGrMembRecord = u.arbitrary().unwrap();
                    rep.insert_rec(rec)?;
                }
                _ => {
                    let rec: BinLogSentRecord = u.arbitrary().unwrap();
                    rep.insert_rec(rec)?;
                }
            }
        }
        rep.flush()?;
        Ok(())
    }
}
