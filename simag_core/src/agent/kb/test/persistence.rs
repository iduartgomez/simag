use std::{
    fs::read_dir,
    io,
    panic::{catch_unwind, UnwindSafe},
    path::{Path, PathBuf},
};

use io::Read;

use crate::{
    agent::{kb::repr::Representation, lang::GroundedMemb},
    storage::{self, *},
};

struct TestBinLoader {
    obj: Option<GroundedMemb>,
}

impl Loadable for TestBinLoader {
    fn load(
        &mut self,
        owner_addr: &MemAddr<Mapped>,
        owned_addr: MemAddr<Mapped>,
        dtype: BinType,
        data: Vec<u8>,
    ) -> Result<MemAddr<NonMapped>> {
        match dtype {
            BinType::GrMemb => {
                let (key, obj): (_, GroundedMemb) = storage::get_from_metadata_storage(data)?;
                self.obj = Some(obj);
            }
            _ => return Err(StorageError::ManagerNotFound),
        }
        Ok(MemAddr::from(0))
    }
}

fn tear_down<F>(test_fn: F) -> Result<()>
where
    F: FnOnce() -> Result<()> + UnwindSafe,
{
    let res = catch_unwind(test_fn).unwrap_or_else(|_| Err(StorageError::FailedToWrite));
    for e in read_dir(std::env::temp_dir())
        .unwrap()
        .filter_map(|e| e.ok())
    {
        if e.file_name().to_str().unwrap().starts_with("simag") {
            std::fs::remove_dir_all(e.path()).unwrap();
        }
    }
    res
}

#[test]
fn create_from_disc() -> Result<()> {
    fn test() -> Result<()> {
        // write the repr to disc
        let mut repr = create_test_rep();
        let path = flush_repr(&mut repr)?;

        // try reading from persisted dir
        let repr = Representation::load_from_disc(None, &path)?;
        assert!(!repr.classes.is_empty());
        Ok(())
    }
    tear_down(test)
}

#[test]
fn write_to_disc() -> Result<()> {
    fn test() -> Result<()> {
        let mut repr = create_test_rep();
        let path = flush_repr(&mut repr)?;
        let (data, idx) = get_written_data(&path)?;
        assert!(!data.is_empty());
        assert!(data.iter().filter(|b| *b != &0u8).next().is_some());
        assert!(!idx.is_empty());
        assert!(idx.iter().filter(|b| *b != &0u8).next().is_some());

        let mut storage = StorageManager::new(repr.config.id, Some(&path), None)?;
        let mut loader = TestBinLoader { obj: None };
        storage.load_from_disc(&mut loader)?;

        Ok(())
    }
    tear_down(test)
}

fn flush_repr(repr: &mut Representation) -> Result<PathBuf> {
    repr.enable_persistence();
    repr.write_to_disc()?;
    let path = std::env::temp_dir().join(format!("simag-{}", repr.config.id));
    Ok(path)
}

fn create_test_rep() -> Representation {
    let repr = Representation::default();
    repr.tell("(professor[$Lucy,u=1])").unwrap();
    assert!(!repr.classes.is_empty());
    repr
}

fn get_written_data(path: &Path) -> io::Result<(Vec<u8>, Vec<u8>)> {
    assert!(path.exists());
    let idx_file = path.join("simag.idx");
    let dat_file = path.join("simag.1.dat");
    assert!(idx_file.exists());
    assert!(dat_file.exists());

    let dat_content = std::fs::File::open(dat_file)?
        .bytes()
        .filter_map(|f| f.ok())
        .collect::<Vec<_>>();

    let idx_content = std::fs::File::open(idx_file)?
        .bytes()
        .filter_map(|f| f.ok())
        .collect::<Vec<_>>();

    Ok((dat_content, idx_content))
}
