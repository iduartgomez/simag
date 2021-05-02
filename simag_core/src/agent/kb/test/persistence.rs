use std::{
    fs::read_dir,
    io::{self, Read},
    path::{Path, PathBuf},
};

use crate::{
    agent::{kb::repr::Representation, lang::GroundedMemb},
    storage::{self, *},
};

macro_rules! assert_or_err {
    (io: $assertion:expr; $err:expr) => {{
        if !$assertion {
            return Err($err);
        }
    }};
}

struct TestBinLoader {
    obj: Option<GroundedMemb>,
}

impl Loadable for TestBinLoader {
    fn load(
        &mut self,
        _owner_addr: &MemAddr<Mapped>,
        _owned_addr: MemAddr<Mapped>,
        dtype: BinType,
        data: Vec<u8>,
    ) -> Result<MemAddr<NonMapped>> {
        match dtype {
            BinType::GrMemb => {
                let (_key, obj): (_, GroundedMemb) = storage::get_from_metadata_storage(data)?;
                self.obj = Some(obj);
            }
            _ => return Err(StorageError::ManagerNotFound),
        }
        Ok(MemAddr::from(0))
    }
}

fn tear_down<F>(test_fn: F) -> Result<()>
where
    F: FnOnce() -> Result<()>,
{
    let res = test_fn();
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

fn create_test_rep() -> Result<Representation> {
    let no_data_err: StorageError = io::Error::from(io::ErrorKind::InvalidData).into();

    let repr = Representation::default();
    repr.tell("(professor[$Lucy,u=1])").unwrap();
    assert_or_err!(io: !repr.classes.is_empty(); no_data_err);
    Ok(repr)
}

fn flush_repr(repr: &mut Representation) -> Result<PathBuf> {
    repr.enable_persistence();
    repr.write_to_disc()?;
    let path = std::env::temp_dir().join(format!("simag-{}", repr.config.id));
    Ok(path)
}

fn get_written_data(path: &Path) -> io::Result<(Vec<u8>, Vec<u8>)> {
    assert_or_err!(io: path.exists(); io::ErrorKind::NotFound.into());
    let idx_file = path.join("simag.idx");
    let dat_file = path.join("simag.1.dat");
    assert_or_err!(io: idx_file.exists(); io::ErrorKind::NotFound.into());
    assert_or_err!(io: dat_file.exists(); io::ErrorKind::NotFound.into());

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

#[test]
fn create_from_disc() -> Result<()> {
    fn test() -> Result<()> {
        // write the repr to disc
        let mut repr = create_test_rep()?;
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
        let no_data_err: StorageError = io::Error::from(io::ErrorKind::InvalidData).into();
        let inv_data_err: StorageError = io::Error::from(io::ErrorKind::InvalidInput).into();

        let mut repr = create_test_rep()?;
        let path = flush_repr(&mut repr)?;
        let (data, idx) = get_written_data(&path)?;
        assert_or_err!(io: !data.is_empty(); no_data_err);
        assert_or_err!(io: data.iter().filter(|b| *b != &0u8).next().is_some(); inv_data_err);
        assert_or_err!(io: !idx.is_empty(); no_data_err);
        assert_or_err!(io: idx.iter().filter(|b| *b != &0u8).next().is_some(); inv_data_err);

        let mut storage = StorageManager::new(Some(&path), None)?;
        let mut loader = TestBinLoader { obj: None };
        storage.load_from_disc(&mut loader)?;
        assert_or_err!(io: loader.obj.is_some(); inv_data_err);

        Ok(())
    }
    tear_down(test)
}
