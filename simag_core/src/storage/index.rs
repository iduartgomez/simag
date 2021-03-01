#[cfg(unix)]
use std::os::unix::fs::FileExt;
use std::{
    collections::{hash_map::Entry, HashMap},
    fs::File,
    io,
    marker::PhantomData,
    path::Path,
};

use super::{manager::DiscRecordRef, open_dat_file, DiscAddr, Mapped, MemAddr};

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
    const IDX_DEFAULT_SIZE: u64 = 200_000 * std::mem::size_of::<DiscRecordRef>() as u64;

    pub fn new(path: &Path) -> io::Result<Self> {
        let file = open_dat_file(&path.join("simag.idx"))?;
        Ok(Index {
            file,
            metadata_sector: Sector {
                ptr: 0.into(),
                offset: 0,
                capacity: Self::METADATA_DEFAULT_SIZE,
                _data_type: PhantomData,
            },
            idx_sector: Sector {
                // starts at the end of the metadata sector
                ptr: Self::METADATA_DEFAULT_SIZE.into(),
                offset: 0,
                capacity: Self::IDX_DEFAULT_SIZE,
                _data_type: PhantomData,
            },
            idx: HashMap::new(),
        })
    }

    /// Insert or update an entry in the index.
    // TODO: this could be a batched operation and write more than 1 rec at a time,
    // since is only called when a whole page is flushed to disc.
    pub fn insert(&mut self, key: MemAddr<Mapped>, value: DiscRecordRef) -> io::Result<()> {
        if self.idx_sector.capacity
            <= (self.idx_sector.offset + std::mem::size_of::<DiscRecordRef>() as u64)
        {
            // need to increase capacity as we would run out of space for the index
            self.increase_capacity()?;
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
                #[cfg(unix)]
                {
                    self.file.write_all_at(&rec, next_addr)?;
                }
                vac.insert(DiscAddr(next_addr));
                self.idx_sector.offset += std::mem::size_of::<Record>() as u64;
            }
        }
        Ok(())
    }

    /// Doubles the capacity of the index and metadata sectors to fit more data.
    fn increase_capacity(&mut self) -> io::Result<()> {
        // read indexes data from disc into buffer, metadata sector comes first so it can
        // be safely ignored, leaving garbage on disc to be overwritten at a later time
        let idx_cap = self.idx_sector.capacity as usize;
        let mut indexes = Vec::with_capacity(idx_cap);
        #[cfg(unix)]
        self.file
            .read_exact_at(&mut indexes[..], *self.idx_sector.ptr)?;

        // rewrite the data starting from double the current offset, reserving the rest
        // of the space for new metadata
        let new_metadata_capacity = self.metadata_sector.capacity * 2;
        #[cfg(unix)]
        self.file
            .write_all_at(&indexes, *self.metadata_sector.ptr + new_metadata_capacity)?;

        self.metadata_sector.capacity = new_metadata_capacity;
        self.idx_sector.ptr = (*self.metadata_sector.ptr + self.metadata_sector.capacity).into();
        self.idx_sector.capacity *= 2;

        Ok(())
    }
}

struct Record {
    key: MemAddr<Mapped>,
    value: DiscRecordRef,
}

impl Record {
    fn serialize(self) -> [u8; std::mem::size_of::<Self>()] {
        let mut serialized = [0; std::mem::size_of::<Self>()];

        let key = (*self.key).to_be_bytes();
        let key_part = &mut serialized[0..8];
        key_part.copy_from_slice(&key);

        let value = self.value.serialize();
        let value_part = &mut serialized[8..];
        value_part.copy_from_slice(&value);

        serialized
    }
}

impl From<&'_ [u8; std::mem::size_of::<Record>()]> for Record {
    fn from(buf: &'_ [u8; std::mem::size_of::<Record>()]) -> Self {
        let mut key: [u8; std::mem::size_of::<u64>()] = [0u8; 8];
        key.copy_from_slice(&buf[0..8]);
        let key = MemAddr(u64::from_le_bytes(key).into(), PhantomData);

        let mut value = [0u8; std::mem::size_of::<DiscRecordRef>()];
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
    use super::*;
    use crate::storage::{manager::DiscRecordRef, BinType};

    #[test]
    fn serialize_rec() {
        let value = DiscRecordRef {
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
}
