#[cfg(unix)]
use std::os::unix::fs::FileExt;
use std::{
    collections::{BTreeMap, HashMap},
    convert::TryFrom,
    fs::File,
    io,
    iter::FromIterator,
    marker::PhantomData,
    path::{Path, PathBuf},
    result::Result as StdResult,
    sync::atomic::{self, AtomicU64},
};

#[cfg(test)]
use arbitrary::Arbitrary;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::{
    index::Index, open_dat_file, BinType, BinaryObj, DiscAddr, Mapped, MemAddr, NonMapped, Result,
    StorageError,
};

pub(super) const DISC_REC_REF_SIZE: usize = std::mem::size_of::<DiscRecordRef>();
static NEXT_MAPPED_MEM_ADDR: AtomicU64 = AtomicU64::new(0);

/// The amount of data to be fetched from a given position.
type RecordLength = u64;
/// A handle in memory of the on-disk storage for a given Representation.
/// Allows for rebuilding of an agent from a binary blob as well as keeping the copy of data
/// in the storage layer up to date. Each representation has it's own file for storing their
/// own objects.
///
/// Accurately rebuilds any shared objects, maintaining the underlying memory relationship
/// and shared memory space. The data inserted must be "addressable" on-memory and maintain
/// an static address for the duration of the program.
pub(crate) struct StorageManager {
    /// C0 in-memory tree, buffer for data waiting to be flushed to disk.
    /// This is only for record data, meaning records which hold a stable address in memory
    /// which is used as a key.
    c0: BTreeMap<MemAddr<Mapped>, Record>,
    buffer_size: u64,
    page_manager: PageManager,
    /// Memory addresses are not stable over process executions, the reference stored on disc
    /// is a map to the address on memory. This way references can be reconstructed over different
    /// processes.
    mem_addr_map: HashMap<MemAddr<NonMapped>, MemAddr<Mapped>>,
    idx: Index,
}

impl StorageManager {
    /// Create a new StorageManager for the given representation, will be stored at the given directory
    /// if provided or in a temporary directory otherwise. If there is already any loaded data present
    /// in the file system it will be loaded instead.
    pub fn new(path: Option<&Path>, lvl1_size: Option<u64>) -> io::Result<Self> {
        let (file_dir, idx) = if let Some(path) = path {
            if path.exists() {
                let md = path.metadata()?;
                if !md.is_dir() {
                    return Err(io::ErrorKind::InvalidInput.into());
                }
                if md.permissions().readonly() {
                    return Err(io::ErrorKind::PermissionDenied.into());
                }
                let idx = Index::try_from(path)?;
                (path.into(), idx)
            } else {
                std::fs::create_dir(path)?;
                let idx = Index::new(&path)?;
                (path.into(), idx)
            }
        } else {
            let p = std::env::temp_dir().join(format!("simag-{}", Uuid::new_v4()));
            if !p.exists() {
                std::fs::create_dir(&p)?;
            }
            let idx = Index::new(&p)?;
            (p, idx)
        };

        Ok(StorageManager {
            c0: BTreeMap::new(),
            buffer_size: 0,
            idx,
            page_manager: PageManager::new(file_dir, lvl1_size)?,
            mem_addr_map: HashMap::new(),
        })
    }

    /// Inserts a new record, if it exists the new record will be updated.
    ///
    /// Internally this does not flush to disk, but only stores the data in the record buffer.
    pub fn insert_rec(&mut self, rec: BinaryObj) -> io::Result<MemAddr<Mapped>> {
        let BinaryObj {
            address: mem_addr,
            record: data,
            ty,
        } = rec;
        let mapped_addr = *self.mem_addr_map.entry(mem_addr).or_insert_with(|| {
            MemAddr(
                NEXT_MAPPED_MEM_ADDR.fetch_add(1, atomic::Ordering::SeqCst),
                PhantomData,
            )
        });
        let rec = Record::new(ty, mapped_addr, data);
        if self.buffer_size + rec.len() as u64 > PageManager::PAGE_SIZE {
            // should flush first
            self.flush()?;
            self.buffer_size = 0;
        }
        self.buffer_size += rec.len() as u64;
        self.c0.insert(mapped_addr, rec);
        Ok(mapped_addr)
    }

    pub fn flush(&mut self) -> io::Result<()> {
        let mut to_flush = BTreeMap::new();
        std::mem::swap(&mut to_flush, &mut self.c0);
        let mut buffer = Vec::with_capacity(PageManager::PAGE_SIZE as usize);
        let mut table_data = BTreeMap::new();
        let mut addr = 0u64;
        let mut batch_insert = Vec::with_capacity(to_flush.len());
        for (mem_addr, rec) in to_flush {
            let len = rec.len() as u64;
            let type_id = rec.type_id();
            rec.append_to_buf(&mut buffer);
            let dref = DiscRecordRef {
                table: Table::C1,
                type_id,
                addr: DiscAddr::from(addr),
                length: len,
            };
            batch_insert.push((mem_addr, dref.clone()));
            table_data.insert(mem_addr, dref);
            addr += len;
        }
        self.idx.insert_batch(batch_insert.into_iter())?;
        self.page_manager
            .spill_to_disc(&mut self.idx, buffer, table_data)
    }

    pub fn load_from_disc<L>(&mut self, loadable: &mut L) -> Result<()>
    where
        L: Loadable,
    {
        use super::index::IdxRecord;
        let records = self.idx.fetch_disc_refs()?;
        let metadata_objs = self.idx.fetch_metadata()?;

        for (i, rec) in records.into_iter().enumerate() {
            let IdxRecord { key, value: dref } = rec;
            let file = match dref.table {
                Table::C1 => &self.page_manager.c1_data,
                Table::C2 => &self.page_manager.c2_data,
                Table::C3 => todo!(),
            };

            let mut data = vec![0; dref.length as usize];
            #[cfg(unix)]
            file.read_exact_at(&mut data, *dref.addr)?;
            let Record {
                type_id,
                data,
                mem_addr,
                ..
            } = Record::deserialize(data, i);
            if type_id != dref.type_id {
                let err: io::Error = io::ErrorKind::InvalidData.into();
                return Err(err.into());
            }

            for md in &metadata_objs {
                if let Some(owner) = md.stored_data.get(&mem_addr) {
                    let addr = loadable.load(owner, mem_addr, dref.type_id, data).unwrap();
                    self.mem_addr_map.insert(addr, key);
                    break;
                }
            }
        }

        Ok(())
    }
}

impl TryFrom<&Path> for StorageManager {
    type Error = bincode::Error;

    fn try_from(path: &Path) -> StdResult<Self, Self::Error> {
        let _file = File::open(path)?;
        todo!()
    }
}

pub(crate) trait Loadable {
    /// Loads to the implementing object the new in-memory instance from a previously mapped address.
    fn load(
        &mut self,
        owner_addr: &MemAddr<Mapped>,
        owned_addr: MemAddr<Mapped>,
        dtype: BinType,
        data: Vec<u8>,
    ) -> Result<MemAddr<NonMapped>>;
}

#[derive(Serialize, Deserialize)]
pub(crate) struct Metadata<'mg> {
    owners: HashMap<MetadataOwner, MemAddr<Mapped>>,
    /// A map of ptr from owned to owners
    stored_data: HashMap<MemAddr<Mapped>, MemAddr<Mapped>>,
    #[serde(skip_serializing, skip_deserializing)]
    manager: Option<&'mg mut StorageManager>,
}

impl<'mg> Metadata<'mg> {
    pub fn new(manager: &'mg mut StorageManager) -> Self {
        Metadata {
            owners: HashMap::new(),
            stored_data: HashMap::new(),
            manager: Some(manager),
        }
    }

    pub fn register_owner(&mut self, owner: &impl MetadataOwnerKind) -> MetadataOwner {
        let addr = owner.get_addr();
        let kind = owner.get_kind();
        MetadataOwner(addr, kind)
    }

    pub fn insert_metadata(self) -> Result<()> {
        let Metadata {
            owners,
            stored_data,
            manager,
        } = self;

        let manager = manager.ok_or_else(|| StorageError::ManagerNotFound)?;
        let serialized = {
            let md = Metadata {
                owners,
                stored_data,
                manager: None,
            };
            bincode::serialize(&md)?
        };
        // force a flush of any non-persisted data
        manager.flush()?;
        manager.idx.insert_metadata(serialized)?;
        Ok(())
    }

    pub fn insert_rec(&mut self, bin: BinaryObj, owner: &MetadataOwner) -> io::Result<()> {
        if let Some(ref mut md) = self.manager {
            let owner_addr = *self.owners.entry(owner.clone()).or_insert_with(|| {
                let mapped_owner_addr = NEXT_MAPPED_MEM_ADDR.fetch_add(1, atomic::Ordering::SeqCst);
                MemAddr(mapped_owner_addr, PhantomData)
            });
            let owned_obj_addr = md.insert_rec(bin)?;
            self.stored_data.insert(owned_obj_addr, owner_addr);
        }
        Ok(())
    }
}

#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize, Clone)]
pub(crate) struct MetadataOwner(MemAddr<NonMapped>, MetadataKind);

#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize, Clone)]
pub(crate) enum MetadataKind {
    Entity,
}

pub(crate) trait MetadataOwnerKind {
    fn get_kind(&self) -> MetadataKind;
    fn get_addr(&self) -> MemAddr<NonMapped>;
}

/// A record prepared to be stored on disk.
struct Record {
    type_id: BinType,
    mem_addr: MemAddr<Mapped>,
    data: Vec<u8>,
    /// computed lenth after encoding to bytes
    length: u64,
}

impl Record {
    const HEADER_SIZE: usize = 17;

    fn new(type_id: BinType, mem_addr: MemAddr<Mapped>, data: Vec<u8>) -> Self {
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
        buffer.extend(std::array::IntoIter::new(header));
        buffer.extend(data);
    }

    fn type_id(&self) -> BinType {
        self.type_id
    }

    #[inline(always)]
    fn make_header(
        type_id: BinType,
        mem_addr: MemAddr<Mapped>,
        len: u64,
    ) -> [u8; Self::HEADER_SIZE] {
        let mut header = [0u8; 17];
        let addr_part = &mut header[0..8];
        addr_part.copy_from_slice((&*mem_addr).to_le_bytes().as_ref());
        header[8] = type_id as u8;
        let len_part = &mut header[9..];
        len_part.copy_from_slice(len.to_le_bytes().as_ref());
        header
    }

    #[inline(always)]
    fn load_header(data: &mut Vec<u8>) -> (Vec<u8>, MemAddr<Mapped>, BinType) {
        let header = data.drain(0..Self::HEADER_SIZE).collect::<Vec<_>>();
        let mut mem_addr = [0u8; std::mem::size_of::<u64>()];
        mem_addr.copy_from_slice(&header[0..8]);
        let mem_addr: MemAddr<Mapped> = u64::from_le_bytes(mem_addr).into();
        let type_id = BinType::from(header[8]);
        (header, mem_addr, type_id)
    }

    fn deserialize(mut data: Vec<u8>, i: usize) -> Self {
        let (header, mem_addr, type_id) = Self::load_header(&mut data);
        if cfg!(debug_assertions) {
            let mut len = [0u8; std::mem::size_of::<u64>()];
            len.copy_from_slice(&header[9..]);
            let len = u64::from_le_bytes(len);
            // assert_eq!(data.len() as u64, len);
            if data.len() != len as usize {
                eprintln!(
                    "iter {}, unexpected bytes array length: {} != {}; data: {:?}",
                    i,
                    data.len(),
                    len,
                    data
                );
            }
        }

        Record {
            length: data.len() as u64 + Self::HEADER_SIZE as u64,
            data,
            mem_addr,
            type_id,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
#[cfg_attr(test, derive(Arbitrary))]
pub(super) struct DiscRecordRef {
    pub(crate) table: Table,
    pub type_id: BinType,
    pub addr: DiscAddr,
    pub length: RecordLength,
}

impl DiscRecordRef {
    pub fn serialize(self) -> [u8; std::mem::size_of::<Self>()] {
        let mut serialized = [0u8; std::mem::size_of::<Self>()];

        let addr_part = &mut serialized[0..8];
        addr_part.copy_from_slice((self.addr).to_le_bytes().as_ref());

        let len_part = &mut serialized[9..17];
        len_part.copy_from_slice(self.length.to_le_bytes().as_ref());

        serialized[8] = self.type_id as u8;
        serialized[18] = self.table as u8;

        serialized
    }
}

impl From<&'_ [u8; std::mem::size_of::<DiscRecordRef>()]> for DiscRecordRef {
    fn from(buf: &'_ [u8; DISC_REC_REF_SIZE]) -> Self {
        let mut addr: [u8; std::mem::size_of::<u64>()] = [0u8; 8];
        addr.copy_from_slice(&buf[0..8]);
        let addr = u64::from_le_bytes(addr).into();

        let mut length: [u8; std::mem::size_of::<u64>()] = [0u8; 8];
        length.copy_from_slice(&buf[9..17]);
        let length = u64::from_le_bytes(length);

        DiscRecordRef {
            table: Table::from(buf[18]),
            type_id: BinType::from(buf[8]),
            addr,
            length,
        }
    }
}

struct Page {
    offset: u64,
    /// Current free space of page.
    free_space: u64,
    /// In representations a lot of records map well to key-values tuples, where the key is
    /// the shared memory address pointer and the value the underlying data.
    /// On pages we append the record to the log, and later on we can merge the data
    /// using the address as a key, fetching from the registered file disk address.
    key_addr: BTreeMap<MemAddr<Mapped>, DiscRecordRef>,
}

/// Manages pages and file synchronization with memory data for log record-like data.
struct PageManager {
    c1_data: File,
    c2_data: File,
    c3_data: Vec<File>,
    levels_pages: [Vec<Page>; 3],
    current_page: [usize; 3],
    config: PageManagerConfig,
}

struct PageManagerConfig {
    file_dir: PathBuf,
    c1_size: u64,
    c2_size: u64,
}

#[repr(u8)]
#[derive(Clone, PartialEq, Eq, Debug)]
#[cfg_attr(test, derive(Arbitrary))]
pub(super) enum Table {
    C1 = 1,
    C2 = 2,
    C3 = 3,
}

impl From<u8> for Table {
    fn from(val: u8) -> Self {
        match val {
            1 => Table::C1,
            2 => Table::C2,
            3 => Table::C3,
            _ => unreachable!(),
        }
    }
}

struct CachedRec {
    type_id: BinType,
    length: RecordLength,
    page_num: usize,
}

impl PageManager {
    /// Modern disks blocks are usually 4KB bytes wide, and are optimized for such size writes/reads.
    /// All the pages must be then a multiple of this so they are written to block effitiently.
    const PAGE_SIZE: u64 = 1024 * 4;

    /// The C0 tree is maintained in-memory.
    /// Max size for all the pages
    const C1_SIZE: u64 = 1024 * 1024 * 8; // 8MB
    const C2_SIZE: u64 = Self::C1_SIZE * 8; // 64 MB
    const C3_MAX_FILE_SIZE: u64 = Self::C2_SIZE * 8;

    fn new(file_dir: PathBuf, c1_size: Option<u64>) -> io::Result<PageManager> {
        let config = {
            let (c1_size, c2_size) = if let Some(c1_size) = c1_size {
                // should be a multiple of page size and > 0
                assert!(c1_size > 0);
                assert_eq!(c1_size % Self::PAGE_SIZE, 0);
                (c1_size, c1_size * 8)
            } else {
                (Self::C1_SIZE, Self::C2_SIZE)
            };

            PageManagerConfig {
                file_dir,
                c1_size,
                c2_size,
            }
        };

        let c1_num_pages = config.c1_size / Self::PAGE_SIZE;
        let c2_num_pages = config.c2_size / Self::PAGE_SIZE;

        let mut c1_offset = 0;
        let c1_pages = Vec::from_iter((0..c1_num_pages).map(|_| {
            let p = Page {
                offset: c1_offset,
                free_space: Self::PAGE_SIZE,
                key_addr: BTreeMap::new(),
            };
            c1_offset += Self::PAGE_SIZE;
            p
        }));
        debug_assert_eq!(c1_pages.len() as u64, c1_num_pages);

        let mut c2_offset = 0;
        let c2_pages = Vec::from_iter((0..c2_num_pages).map(|_| {
            let p = Page {
                offset: c2_offset,
                free_space: Self::PAGE_SIZE * 10,
                key_addr: BTreeMap::new(),
            };
            c2_offset += Self::PAGE_SIZE;
            p
        }));
        debug_assert_eq!(c2_pages.len() as u64, c2_num_pages);

        let c1_path = config.file_dir.join("simag.1.dat");
        let c1_data = open_dat_file(&c1_path)?;

        let c2_path = config.file_dir.join("simag.2.dat");
        let c2_data = open_dat_file(&c2_path)?;

        Ok(PageManager {
            c1_data,
            c2_data,
            c3_data: Vec::new(),
            levels_pages: [c1_pages, c2_pages, Vec::new()],
            current_page: [0; 3],
            config,
        })
    }

    fn spill_to_disc(
        &mut self,
        idx: &mut Index,
        mut buffer: Vec<u8>,
        table_map: BTreeMap<MemAddr<Mapped>, DiscRecordRef>,
    ) -> io::Result<()> {
        let c1_size = self.levels_pages[0].len();
        // let lvl1_pages = &mut self.levels_pages[0];
        let mut current_c1_page = self.current_page[0];
        let mut loop_guard = 0;
        while self.levels_pages[0][current_c1_page].free_space < buffer.len() as u64 {
            if loop_guard > (c1_size + (c1_size / 2)) {
                unreachable!("something went wrong and pages are not being compacted properly");
            }
            loop_guard += 1;
            if current_c1_page + 1 < c1_size {
                current_c1_page += 1;
            } else {
                // lvl1 pages are full, merge to lvl 2
                self.compact(idx, Table::C1)?;
                current_c1_page = 0;
            }
        }
        self.current_page[0] = current_c1_page;

        let active_lvl1_page = &mut self.levels_pages[0][current_c1_page];
        let buf_len = buffer.len() as u64;
        if buf_len < active_lvl1_page.free_space {
            // padd with zeros the the buffer to fully fill the page
            let remainder = active_lvl1_page.free_space - buf_len;
            let remaining = vec![0; remainder as usize];
            buffer.extend(remaining);
        }

        let offset = active_lvl1_page.offset + (Self::PAGE_SIZE - active_lvl1_page.free_space);
        #[cfg(unix)]
        {
            self.c1_data.write_all_at(&buffer, offset)?;
        }

        for (addr, rec) in table_map {
            active_lvl1_page.key_addr.insert(addr, rec);
        }
        active_lvl1_page.free_space = 0;
        Ok(())
    }

    fn compact(&mut self, idx: &mut Index, lvl: Table) -> io::Result<()> {
        let (merge_src_pages, merge_src_data, merge_target, target_size) = match lvl {
            Table::C1 => (
                &mut self.levels_pages[0],
                &mut self.c1_data,
                Table::C2,
                self.config.c1_size / 4,
            ),
            Table::C2 => (
                &mut self.levels_pages[1],
                &mut self.c2_data,
                Table::C3,
                self.config.c2_size / 4,
            ),
            _ => unreachable!(),
        };

        // merge the pages
        let (merged_data, size) = Self::merge_pages(merge_src_pages, target_size);
        // read the data from disc to memory
        let mut pages_buf = vec![vec![0u8; Self::PAGE_SIZE as usize]; Self::required_pages(size)];
        debug_assert!(pages_buf.len() * pages_buf[0].len() == target_size as usize);
        let cached = Self::fetch_data(&mut pages_buf, merged_data, merge_src_data)?;
        self.write_merged_data(idx, merge_target, pages_buf, cached)
    }

    /// Merge a serie of pages targetting a max size for the merge.
    /// Returns a map of the memory addresses of the objects being merged and their location on disc.
    fn merge_pages(
        src_pages: &mut Vec<Page>,
        target_size: u64,
    ) -> (BTreeMap<MemAddr<Mapped>, DiscRecordRef>, u64) {
        // pages are sorted by insertion order, so don't need to do anything
        // just keep the last pointer per key
        let mut merged_data = BTreeMap::new();
        let mut size = 0u64;
        for page in src_pages {
            let expected_max_size = size + Self::PAGE_SIZE;
            if expected_max_size <= target_size && page.free_space < (target_size - size) {
                // the whole page can be swapped, empty all addresses
                let mut page_addresses = BTreeMap::new();
                // FIXME: this could potentially be a data loss since we haven't yet written anything to disc
                // but are removing the info from disc locations, making them irrecoverable
                std::mem::swap(&mut page_addresses, &mut page.key_addr);
                for (addr, rec) in page_addresses {
                    size += rec.length;
                    if let Some(prev_inserted) = merged_data.insert(addr, rec) {
                        size -= prev_inserted.length;
                    }
                }
                page.free_space = Self::PAGE_SIZE;
            } else {
                // only need to swap out partially some of the memory
                let mut keys_to_rm = vec![];
                for (k, v) in page.key_addr.iter() {
                    if merged_data.contains_key(k) {
                        // ignore keys that were previously merged to simplify;
                        // leaving the most recent version in the previous cache lvl
                        continue;
                    }
                    let new_len = size + v.length;
                    if new_len > target_size {
                        break;
                    }
                    keys_to_rm.push(*k);
                    size = new_len;
                }
                // TODO: need to consolidate the lingering now writable space in this page
                for key in keys_to_rm {
                    if let Some(page) = page.key_addr.remove(&key) {
                        if let Some(prev_inserted) = merged_data.insert(key, page) {
                            size -= prev_inserted.length;
                        }
                    }
                }
                if !(size < target_size) {
                    break;
                }
            }
        }
        debug_assert!(size <= target_size);
        debug_assert!(merged_data.values().fold(0u64, |acc, v| { acc + v.length }) <= target_size);
        (merged_data, size)
    }

    /// Read bytes from disk into the pages buffer from a given map of disc references and a src file.
    fn fetch_data(
        pages_buf: &mut Vec<Vec<u8>>,
        src_refs: BTreeMap<MemAddr<Mapped>, DiscRecordRef>,
        src: &mut File,
    ) -> io::Result<Vec<(MemAddr<Mapped>, CachedRec)>> {
        let mut cached = Vec::with_capacity(src_refs.len());
        let mut page_num = 0;
        let mut r_cursor = 0usize;
        for (addr, rec) in src_refs.into_iter() {
            let page_buf = &mut pages_buf[page_num];
            if (r_cursor + rec.length as usize) <= Self::PAGE_SIZE as usize {
                // fill this page buffer
                #[cfg(unix)]
                {
                    src.read_exact_at(
                        &mut page_buf[r_cursor..(r_cursor + rec.length as usize)],
                        *rec.addr,
                    )?;
                }
                r_cursor += rec.length as usize;
            } else {
                page_num += 1;
                r_cursor = 0;
                let page_buf = &mut pages_buf[page_num];
                #[cfg(unix)]
                {
                    src.read_exact_at(
                        &mut page_buf[r_cursor..(r_cursor + rec.length as usize)],
                        *rec.addr,
                    )?;
                }
            }
            cached.push((
                addr,
                CachedRec {
                    type_id: rec.type_id,
                    length: rec.length,
                    page_num,
                },
            ));
        }
        Ok(cached)
    }

    /// Writes merged data to the target layer.
    fn write_merged_data(
        &mut self,
        idx: &mut Index,
        merge_target: Table,
        pages_buf: Vec<Vec<u8>>,
        cached: Vec<(MemAddr<Mapped>, CachedRec)>,
    ) -> io::Result<()> {
        match merge_target {
            Table::C2 => {
                let target_pages = &mut self.levels_pages[1];
                let target_file = &mut self.c2_data;
                let mut curr_page_buf = 0;
                for page in target_pages.iter_mut() {
                    if page.free_space > 0 && curr_page_buf + 1 <= pages_buf.len() {
                        let page_buf = &pages_buf[curr_page_buf];
                        // FIXME: if there is a failure here it leaves this page
                        // in a potentially corrupted state; the correct operation should:
                        // - append to an op log, cache in-memory the current content
                        // - apply changes
                        // - mark as succeded the on-disc op or rollback
                        // - write to the idx the new address if everything went right
                        // - mark as succeded the indexing op
                        // - clean up the op log
                        #[cfg(unix)]
                        {
                            target_file.write_all_at(page_buf, page.offset)?;
                        }
                        page.free_space = 0;
                        let mut w_cursor = page.offset;
                        let mut new_idx = Vec::with_capacity(page.key_addr.len());
                        for (addr, rec) in
                            cached.iter().filter(|(_, r)| r.page_num == curr_page_buf)
                        {
                            let rec = DiscRecordRef {
                                table: Table::C2,
                                type_id: rec.type_id,
                                addr: DiscAddr(w_cursor),
                                length: rec.length,
                            };
                            w_cursor += rec.length;
                            page.key_addr.insert(*addr, rec.clone());
                            new_idx.push((*addr, rec));
                        }
                        idx.insert_batch(new_idx.into_iter())?;
                        curr_page_buf += 1;
                    } else {
                        break;
                    }
                }
                if curr_page_buf + 1 < pages_buf.len() {
                    // no space left at lvl2, trigger a compaction from lvl2 to lvl3
                    todo!()
                }
            }
            Table::C3 => {}
            _ => unreachable!(),
        }
        Ok(())
    }

    /// Return the amount of pages needed to fit the given amount of bytes.
    fn required_pages(target: u64) -> usize {
        if Self::PAGE_SIZE > target {
            return 1;
        }
        let z = Self::PAGE_SIZE / 2;
        let n = target + z;
        let pages_amt = (n - (n % Self::PAGE_SIZE)) / Self::PAGE_SIZE;
        if Self::PAGE_SIZE * pages_amt >= target {
            pages_amt as usize
        } else {
            (pages_amt + 1) as usize
        }
    }
}

#[cfg(test)]
mod test {
    use arbitrary::Unstructured;
    use rand::Rng;

    use super::*;
    use crate::assert_or_err;
    #[macro_use]
    use crate::storage::test::*;

    const LVL1_TEST_SIZE: u64 = 4096 * 4;

    fn raw_sample(iters: usize) -> Vec<u8> {
        let mut sample = Vec::new();
        let mut rng = rand::thread_rng();
        for _ in 0..iters {
            let k: u64 = rand::random();
            sample.extend(std::array::IntoIter::new(k.to_le_bytes()));
            sample.extend((0..16).map(|_| rng.gen::<u8>()));
        }
        sample
    }

    struct TestMdOwnwer;

    impl MetadataOwnerKind for TestMdOwnwer {
        fn get_kind(&self) -> MetadataKind {
            MetadataKind::Entity
        }

        fn get_addr(&self) -> MemAddr<NonMapped> {
            MemAddr::from(0)
        }
    }

    fn insert_rnd_data_in_storage(
        num_recs: usize,
        storage: &mut StorageManager,
    ) -> io::Result<Metadata> {
        let sample = raw_sample(num_recs);
        let mut unstr = Unstructured::new(&sample);
        let mut md = Metadata::new(storage);
        let owner = TestMdOwnwer;
        let owner_token = md.register_owner(&owner);
        for _ in 0..num_recs {
            let rec: BinaryObj = unstr.arbitrary().unwrap();
            md.insert_rec(rec, &owner_token)?;
        }
        Ok(md)
    }

    struct TestBinLoader {
        obj: Option<Vec<u8>>,
    }

    impl Loadable for TestBinLoader {
        fn load(
            &mut self,
            _owner_addr: &MemAddr<Mapped>,
            _owned_addr: MemAddr<Mapped>,
            _dtype: BinType,
            data: Vec<u8>,
        ) -> Result<MemAddr<NonMapped>> {
            self.obj = Some(data);
            Ok(MemAddr::from(0))
        }
    }

    #[test]
    fn serialize_disc_rec_ref() {
        let dref = DiscRecordRef {
            table: Table::C1,
            type_id: BinType::GrFunc,
            addr: 0.into(),
            length: 10,
        };

        let serialized = dref.serialize();
        let deser = DiscRecordRef::from(&serialized);
        assert_eq!(deser.table, Table::C1);
        assert_eq!(deser.length, 10);
        assert_eq!(deser.addr, 0.into());
        assert_eq!(deser.type_id, BinType::GrFunc);
    }

    #[test]
    fn expected_buf_size() {
        assert_eq!(PageManager::required_pages(PageManager::PAGE_SIZE / 2), 1);
        assert_eq!(PageManager::required_pages(PageManager::PAGE_SIZE), 1);
        assert_eq!(
            PageManager::required_pages((PageManager::PAGE_SIZE as f64 * 1.5).round() as u64),
            2
        );
    }

    #[test]
    fn merge_pages() {
        let key_addr = BTreeMap::from_iter(vec![(
            MemAddr(0, PhantomData),
            DiscRecordRef {
                table: Table::C1,
                type_id: BinType::GrMemb,
                addr: DiscAddr(0),
                length: 2048,
            },
        )]);
        let mut key_addr2 = key_addr.clone();
        key_addr2.insert(
            MemAddr(4096, PhantomData),
            DiscRecordRef {
                table: Table::C1,
                type_id: BinType::GrMemb,
                addr: DiscAddr(4096),
                length: 2048,
            },
        );
        let pages = &mut vec![
            Page {
                offset: 0,
                free_space: 0,
                key_addr,
            },
            Page {
                offset: 4096,
                free_space: 0,
                key_addr: key_addr2,
            },
        ];

        // should remove two different records, of 2048 size each
        let (res, size) = PageManager::merge_pages(pages, 4096);
        assert_eq!(res.len(), 2);
        assert_eq!(
            res.keys().collect::<Vec<_>>(),
            vec![&MemAddr(0, PhantomData), &MemAddr(4096, PhantomData)]
        );
        assert_eq!(size, 4096);
    }

    #[test]
    fn insert_and_flush_recs() -> Result<()> {
        const NUM_ITERS: usize = 3;
        const REC_SIZE: usize = Record::HEADER_SIZE + 3;
        let path = std::env::temp_dir().join(format!("simag-{}", Uuid::new_v4()));
        let test = || -> Result<()> {
            let mut storage = StorageManager::new(Some(&path), Some(LVL1_TEST_SIZE))?;
            for i in 0..NUM_ITERS {
                let mut rec = BinaryObj {
                    address: MemAddr::from(i as u64),
                    record: vec![0u8, 1, 2],
                    ty: BinType::GrFunc,
                };
                storage.insert_rec(rec.clone())?;
                rec.address = MemAddr::from(i as u64 + 1);
                storage.insert_rec(rec)?;
                storage.flush()?;
            }

            use io::Read;
            let mut data = Vec::new();
            let mut f = File::open(path.join("simag.1.dat"))?;
            f.read_to_end(&mut data)?;

            assert_or_err!(io: data.len() == 4096 * NUM_ITERS; io::Error::from(io::ErrorKind::InvalidData));
            for page in data.chunks(4096) {
                let rec = page.iter().take(REC_SIZE).copied().collect();
                let rec = Record::deserialize(rec, 0);
                assert_or_err!(io: rec.length == REC_SIZE as u64);
                assert_or_err!(io: rec.data == vec![0, 1, 2]);

                let rec = page.iter().skip(REC_SIZE).take(REC_SIZE).copied().collect();
                let rec = Record::deserialize(rec, 0);
                assert_or_err!(io: rec.length == REC_SIZE as u64);
                assert_or_err!(io: rec.data == vec![0, 1, 2]);
            }

            Ok(())
        };
        tear_down(test, &path)
    }

    #[test]
    fn flush_data() -> Result<()> {
        let path = std::env::temp_dir().join(format!("simag-{}", Uuid::new_v4()));
        let test = || -> Result<()> {
            const NUM_RECS: usize = 5_000;
            let mut storage = StorageManager::new(Some(&path), Some(LVL1_TEST_SIZE))?;
            let md = insert_rnd_data_in_storage(NUM_RECS, &mut storage)?;
            // write to disc
            md.insert_metadata()?;

            // check there was written data actually
            let mut storage = StorageManager::new(Some(&path), Some(LVL1_TEST_SIZE))?;
            let mut loader = TestBinLoader { obj: None };
            storage.load_from_disc(&mut loader)?;
            assert_or_err!(io: loader.obj.is_some(); io::Error::from(io::ErrorKind::InvalidInput));
            Ok(())
        };
        tear_down(test, &path)
    }
}
