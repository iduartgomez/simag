use std::io;

use crate::{
    agent::{kb::repr::Representation, lang::GroundedMemb},
    assert_or_err,
    storage::{self, test::tear_down, *},
};

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

fn create_test_rep() -> Result<Representation> {
    let no_data_err: StorageError = io::Error::from(io::ErrorKind::InvalidData).into();

    let repr = Representation::default();
    repr.tell("(professor[$Lucy,u=1])").unwrap();
    assert_or_err!(io: !repr.classes.is_empty(); no_data_err);
    Ok(repr)
}

fn flush_repr(repr: &mut Representation) -> Result<()> {
    repr.enable_persistence();
    repr.write_to_disc()
}

#[test]
fn create_from_disc() -> Result<()> {
    let mut repr = create_test_rep()?;
    let path = std::env::temp_dir().join(format!("simag-{}", repr.config.id));
    let test = || -> Result<()> {
        // write the repr to disc
        flush_repr(&mut repr)?;

        // try reading from persisted dir
        let repr = Representation::load_from_disc(None, &path)?;
        assert!(!repr.classes.is_empty());
        Ok(())
    };
    tear_down(test, &path)
}
