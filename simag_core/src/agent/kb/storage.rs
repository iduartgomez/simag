use std::path::Path;
/// Builder of an agent from a binary blob.
///
/// Accurately rebuilds any shared objects, maintaining the underlying memory relationship
/// and shared memory space.
struct ReprBuilder;

impl ReprBuilder {
    pub fn new(path: &Path) -> Self {
        todo!()
    }
}
