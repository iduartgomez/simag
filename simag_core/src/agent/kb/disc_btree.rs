#[cfg(unix)]
use std::os::unix::prelude::FileExt;
use std::{fs::File, io};

use super::{DiscAddr, DiscRecordRef, Mapped, MemAddr};

/// The index is composed by two different sections on disc:
/// - The first sector contains metadata, that can be inserted and updated at any time.
///   This data is inserted and updated on bulk, since it should be small enough to not
///   necessitate any kind of fine-grained optimizations.
/// - A second sector contains all indexing information, synchronized with the memory
///   representation. The data type is a representation of the on-disc BTree.
pub(super) struct Index {
    file: File,
    metadata_sector: DiscAddr,
    idx_root: DiscAddr,
    idx_capacity: usize,
}

impl Index {
    pub fn new() -> Self {
        todo!()
    }

    pub fn insert(&mut self, addr: MemAddr<Mapped>, rec: DiscRecordRef) {
        todo!()
    }

    /// Doubles the capacity of the index and metadata sectors to fit more data.
    fn increase_capacity(&self) -> io::Result<()> {
        // read indexes data from disc into buffer, metadata sector comes first so it can
        // be safely ignored, leaving garbage on disc to be overwritten at a later time
        let idx_capacity = std::mem::size_of::<DiscRecordRef>() * self.idx_capacity;
        let mut indexes = Vec::with_capacity(idx_capacity);
        #[cfg(unix)]
        self.file.read_exact_at(&mut indexes[..], *self.idx_root)?;

        // rewrite the data starting from double the current offset, reserving the rest
        // of the space for new metadata
        let new_metadata_capacity = (*self.idx_root - *self.metadata_sector) * 2;
        #[cfg(unix)]
        self.file
            .write_at(&indexes, *self.metadata_sector + new_metadata_capacity)?;

        Ok(())
    }
}
