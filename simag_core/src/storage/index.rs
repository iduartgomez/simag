#[cfg(unix)]
use std::os::unix::fs::FileExt;
use std::{
    collections::{hash_map::Entry, HashMap},
    convert::TryFrom,
    fs::File,
    io,
    marker::PhantomData,
    path::Path,
    result::Result as StdResult,
};

use super::{
    manager::{DiscRecordRef, DISC_REC_REF_SIZE},
    open_dat_file, DiscAddr, Mapped, MemAddr, Metadata, Result,
};

const RECORD_SIZE: usize = std::mem::size_of::<Record>();
const U64_SIZE: usize = std::mem::size_of::<u64>();

/// The index is composed by two different sections on disc:
/// - The first sector contains metadata, that can be inserted and updated at any time.
///   This data is inserted and updated on bulk, since it should be small enough to not
///   necessitate any kind of fine-grained optimizations.
/// - A second sector contains all indexing information, synchronized with the memory
///   representation. In-memory we keep a map of the memory addresses the pointer to the disc
///   indexing information. Then this disc index can be used to quickly insert, update new entries
///   or used to recreate the new data.
pub(super) struct Index {
    file: File,
    metadata_sector: Sector<Vec<u8>>,
    idx_sector: Sector<DiscRecordRef>,
    idx: HashMap<MemAddr<Mapped>, DiscAddr>,
}

impl Index {
    const METADATA_DEFAULT_SIZE: u64 = 1024 * 1024 * 10;
    const IDX_DEFAULT_SIZE: u64 = 200_000 * DISC_REC_REF_SIZE as u64;

    pub fn new(path: &Path) -> io::Result<Self> {
        #[allow(unused_mut)]
        let mut file = open_dat_file(&path.join("simag.idx"))?;

        // write the default capacity for the metadata sector
        #[cfg(unix)]
        file.write_all_at(&Self::METADATA_DEFAULT_SIZE.to_le_bytes(), 0)?;

        Ok(Index {
            file,
            metadata_sector: Sector {
                ptr: 0.into(),
                /// data starts after storing the length of the current capacity sector in bytes
                offset: U64_SIZE.to_le_bytes().len() as u64,
                capacity: Self::METADATA_DEFAULT_SIZE,
                _data_type: PhantomData,
            },
            idx_sector: Sector {
                // starts at the end of the metadata sector
                ptr: Self::METADATA_DEFAULT_SIZE.into(),
                /// data starts after storing the length of the current capacity sector in bytes
                offset: U64_SIZE.to_le_bytes().len() as u64,
                capacity: Self::IDX_DEFAULT_SIZE,
                _data_type: PhantomData,
            },
            idx: HashMap::new(),
        })
    }

    pub fn insert_metadata(&mut self, serialized: Vec<u8>) -> io::Result<()> {
        let mut header = [0; U64_SIZE];
        header.copy_from_slice(&(serialized.len() as u64).to_le_bytes());

        let offset = *self.metadata_sector.ptr + self.metadata_sector.offset;
        let new_offset = serialized.len() as u64 + header.len() as u64;
        if (new_offset - *self.metadata_sector.ptr) >= self.metadata_sector.capacity {
            self.increase_metadata_capacity()?;
        }

        #[cfg(unix)]
        {
            self.file.write_all_at(&header, offset)?;
            self.file
                .write_all_at(&serialized, offset + header.len() as u64)?;
        }
        self.metadata_sector.offset += new_offset;

        Ok(())
    }

    /// Insert or update an entry in the index.
    fn insert(&mut self, key: MemAddr<Mapped>, value: DiscRecordRef) -> io::Result<()> {
        if self.idx_sector.capacity <= (self.idx_sector.offset + DISC_REC_REF_SIZE as u64) {
            // need to increase capacity as we would run out of space for the index
            self.increase_idx_capacity();
        }

        let rec = (Record { key, value }).serialize();
        match self.idx.entry(key) {
            Entry::Occupied(rec_addr) => {
                let addr = rec_addr.get();
                // replace the bytes at this location with those of the new record
                #[cfg(unix)]
                {
                    self.file.write_all_at(&rec, **addr)?;
                }
            }
            Entry::Vacant(vac) => {
                let next_addr = *self.idx_sector.ptr + self.idx_sector.offset;
                let next_offset = self.idx_sector.offset + RECORD_SIZE as u64;
                #[cfg(unix)]
                {
                    self.file.write_all_at(&rec, next_addr)?;
                    self.file
                        .write_all_at(&next_offset.to_le_bytes(), *self.idx_sector.ptr)?;
                }
                vac.insert(DiscAddr(next_addr));
                self.idx_sector.offset = next_offset;
            }
        }
        Ok(())
    }

    pub fn insert_batch(
        &mut self,
        values: impl Iterator<Item = (MemAddr<Mapped>, DiscRecordRef)>,
    ) -> io::Result<()> {
        let mut new_values: Vec<u8> = Vec::with_capacity(values.size_hint().0 * RECORD_SIZE);
        let mut next_offset = self.idx_sector.offset;
        for (key, value) in values {
            if self.idx.contains_key(&key) {
                self.insert(key, value)?;
            } else {
                // FIXME: if there is a failure while writing to disc this could end up being
                // in a inconsistent state
                self.idx
                    .insert(key, DiscAddr((*self.idx_sector.ptr) + next_offset));
                next_offset += RECORD_SIZE as u64;
                new_values.extend(std::array::IntoIter::new(
                    (Record { key, value }).serialize(),
                ));
            }
        }
        let next_addr = *self.idx_sector.ptr + self.idx_sector.offset;
        #[cfg(unix)]
        {
            self.file.write_all_at(&new_values, next_addr)?;
            self.file
                .write_all_at(&next_offset.to_le_bytes(), *self.idx_sector.ptr)?;
        }
        Ok(())
    }

    /// Doubles the capacity of the index and metadata sectors to fit more data.
    fn increase_metadata_capacity(&mut self) -> io::Result<()> {
        // read indexes data from disc into buffer, metadata sector comes first so it can
        // be safely ignored, leaving garbage on disc to be overwritten at a later time
        let idx_cap = self.idx_sector.capacity as usize;
        let mut indexes = Vec::with_capacity(idx_cap);
        #[cfg(unix)]
        self.file
            .read_exact_at(&mut indexes[..], *self.idx_sector.ptr)?;

        let new_metadata_capacity = self.metadata_sector.capacity * 2;
        // rewrite the idx data starting from double the current offset, reserving the rest
        // of the space for new metadata
        #[cfg(unix)]
        self.file
            .write_all_at(&indexes, *self.metadata_sector.ptr + new_metadata_capacity)?;

        // write the new capacity at the start of the metadata sector
        #[cfg(unix)]
        self.file.write_all_at(
            &new_metadata_capacity.to_le_bytes(),
            *self.metadata_sector.ptr,
        )?;

        self.metadata_sector.capacity = new_metadata_capacity;
        self.idx_sector.ptr = (*self.metadata_sector.ptr + self.metadata_sector.capacity).into();

        Ok(())
    }

    fn increase_idx_capacity(&mut self) {
        self.idx_sector.capacity *= 2;
    }

    pub(super) fn load_idx(&mut self) -> io::Result<()> {
        let mut md_capacity = [0u8; U64_SIZE];
        #[cfg(unix)]
        self.file
            .read_exact_at(&mut md_capacity, *self.metadata_sector.ptr)?;
        self.metadata_sector.capacity = u64::from_le_bytes(md_capacity);

        self.idx_sector.ptr = self.metadata_sector.capacity.into();
        let mut idx_offset = [0u8; U64_SIZE];
        #[cfg(unix)]
        self.file
            .read_exact_at(&mut idx_offset, *self.idx_sector.ptr)?;
        self.idx_sector.offset = u64::from_le_bytes(idx_offset);

        let idx_data_offset = *self.idx_sector.ptr + U64_SIZE as u64;
        let idx_size = (self.file.metadata()?.len() - idx_data_offset) as usize;
        let mut indexes = vec![0u8; idx_size];
        #[cfg(unix)]
        self.file.read_exact_at(&mut indexes, idx_data_offset)?;

        // TODO: pending on stabilization of `array_chunks` this could be done more efficiently
        for (idx, f) in indexes.chunks(RECORD_SIZE).enumerate() {
            let Record { key, .. } = Record::try_from(f)?;
            let disc_addr = idx_data_offset + (idx * RECORD_SIZE) as u64;
            self.idx.insert(key, disc_addr.into());
        }
        Ok(())
    }

    /// Get all the recorded data references.
    pub(super) fn fetch_disc_refs(&mut self) -> Result<Vec<Record>> {
        let idx_data_offset = *self.idx_sector.ptr + U64_SIZE as u64;
        let mut indexes = vec![0u8; self.idx.len() * RECORD_SIZE];
        #[cfg(unix)]
        self.file.read_exact_at(&mut indexes, idx_data_offset)?;

        let mut records = Vec::with_capacity(indexes.len() / RECORD_SIZE);
        for f in indexes.chunks(RECORD_SIZE) {
            let r = Record::try_from(f)?;
            records.push(r);
        }
        Ok(records)
    }

    pub(super) fn fetch_metadata(&self) -> Result<Vec<Metadata>> {
        let mut metadata = Vec::with_capacity(self.metadata_sector.capacity as usize - U64_SIZE);
        #[cfg(unix)]
        self.file
            .read_exact_at(&mut metadata, *self.metadata_sector.ptr + U64_SIZE as u64)?;

        let mut md_objs = Vec::new();
        let mut offset = 0;
        while offset < metadata.len() {
            let mut size = [0; U64_SIZE];
            size.copy_from_slice(&metadata[offset..U64_SIZE]);
            let size = u64::from_le_bytes(size);

            let data_pos = U64_SIZE + offset;
            let metadata: Metadata =
                bincode::deserialize(&metadata[data_pos..data_pos + size as usize])?;
            md_objs.push(metadata);
            offset = data_pos + size as usize;
        }

        Ok(md_objs)
    }
}

impl TryFrom<&Path> for Index {
    type Error = std::io::Error;

    fn try_from(path: &Path) -> StdResult<Self, Self::Error> {
        if path.join("simag.idx").exists() {
            let mut idx = Index::new(path)?;
            {
                // read the current metadata capacity and update the idx sector
                // offset to match the capacity.
                let mut metadata_cap = [0; U64_SIZE];
                #[cfg(unix)]
                idx.file
                    .read_exact_at(&mut metadata_cap, *idx.metadata_sector.ptr)?;
                idx.metadata_sector.capacity = u64::from_le_bytes(metadata_cap);
                idx.idx_sector.ptr =
                    (*idx.metadata_sector.ptr + idx.metadata_sector.capacity).into();
            }
            idx.load_idx()?;
            Ok(idx)
        } else {
            Err(io::Error::from(io::ErrorKind::NotFound))
        }
    }
}

pub(super) struct Record {
    pub key: MemAddr<Mapped>,
    pub value: DiscRecordRef,
}

impl Record {
    fn serialize(self) -> [u8; RECORD_SIZE] {
        let mut serialized = [0; RECORD_SIZE];

        let key = (*self.key).to_le_bytes();
        let key_part = &mut serialized[0..8];
        key_part.copy_from_slice(&key);

        let value = self.value.serialize();
        let value_part = &mut serialized[8..];
        value_part.copy_from_slice(&value);

        serialized
    }
}

impl TryFrom<&'_ [u8]> for Record {
    type Error = io::Error;

    fn try_from(buf: &'_ [u8]) -> io::Result<Self> {
        if buf.len() != RECORD_SIZE {
            return Err(io::ErrorKind::InvalidInput.into());
        }

        let mut fixed = [0; RECORD_SIZE];
        fixed.copy_from_slice(buf);
        Ok(Record::from(&fixed))
    }
}

impl From<&'_ [u8; RECORD_SIZE]> for Record {
    fn from(buf: &'_ [u8; RECORD_SIZE]) -> Self {
        let mut key: [u8; U64_SIZE] = [0u8; 8];
        key.copy_from_slice(&buf[0..8]);
        let key = MemAddr(u64::from_le_bytes(key).into(), PhantomData);

        let mut value = [0u8; DISC_REC_REF_SIZE];
        value.copy_from_slice(&buf[8..]);
        let value = DiscRecordRef::from(&value);

        Record { key, value }
    }
}

struct Sector<T>
where
    T: Sized,
{
    /// starting offset for the sector in the file
    ptr: DiscAddr,
    /// the current offset from ptr to write a new stream of bytes
    offset: u64,
    /// the amount of bytes that can be written in the current allocated space
    capacity: u64,
    /// the type being written in this sector
    _data_type: PhantomData<T>,
}

#[cfg(test)]
mod test {
    use arbitrary::Unstructured;
    use rand::Rng;

    use super::*;
    use crate::storage::{
        manager::{DiscRecordRef, Table},
        BinType,
    };

    fn raw_sample(iters: usize) -> Vec<u8> {
        let mut rng = rand::thread_rng();
        (0..iters).map(|_| rng.gen()).collect()
    }

    #[test]
    fn serialize_rec() {
        let value = DiscRecordRef {
            table: Table::C1,
            type_id: BinType::GrFunc,
            addr: 1.into(),
            length: 10,
        };
        let rec = Record {
            key: MemAddr(0, PhantomData),
            value: value.clone(),
        };

        let serialized = rec.serialize();
        let deser = Record::from(&serialized);
        assert_eq!(deser.key, MemAddr(0, PhantomData));
        assert_eq!(deser.value, value);
    }

    #[test]
    fn write_and_load_index() -> io::Result<()> {
        // (num_keys, raw byte sample)
        // this would trigger a resize:
        // const TEST_SIZE: (usize, usize) = (300_000, Index::IDX_DEFAULT_SIZE as usize * 2usize);
        const TEST_SIZE: (usize, usize) = (10, 1000);

        let mut idx = Index::new(&std::env::temp_dir())?;
        let bytes = raw_sample(TEST_SIZE.1);
        let mut unstr = Unstructured::new(&bytes);
        // this insert sequence will trigger a resize
        for _ in 0..TEST_SIZE.0 {
            let key: MemAddr<Mapped> = unstr.arbitrary().unwrap();
            let dref: DiscRecordRef = unstr.arbitrary().unwrap();
            idx.insert(key, dref)?;
        }

        let new_idx = Index::try_from(&*std::env::temp_dir())?;
        assert_eq!(
            new_idx.metadata_sector.capacity,
            Index::METADATA_DEFAULT_SIZE
        );
        assert_eq!(new_idx.idx_sector.ptr, idx.idx_sector.ptr);
        assert_eq!(new_idx.idx_sector.offset, idx.idx_sector.offset);
        assert_eq!(new_idx.idx.len(), idx.idx.len());

        let idx_rand_kv: Vec<_> = idx.idx.iter().take(10).collect();
        let mut new_disc_ref: Vec<_> = idx_rand_kv.iter().map(|a| new_idx.idx[&a.0]).collect();
        new_disc_ref.sort();
        let mut old_disc_ref = idx_rand_kv.into_iter().map(|a| *a.1).collect::<Vec<_>>();
        old_disc_ref.sort();
        assert_eq!(old_disc_ref, new_disc_ref);

        Ok(())
    }
}
