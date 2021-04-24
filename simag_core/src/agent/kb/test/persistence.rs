use std::{
    io,
    path::{Path, PathBuf},
};

use crate::{agent::kb::repr::Representation, storage::Result};

#[test]
fn create_from_disc() -> Result<()> {
    let mut repr = create_test_rep();
    let path = flush_repr(&mut repr)?;

    // try reading from persisted dir
    let repr = Representation::load_from_disc(None, &path)?;
    assert!(!repr.classes.is_empty());

    Ok(())
}

#[test]
fn write_to_disc() -> Result<()> {
    let mut repr = create_test_rep();
    let path = flush_repr(&mut repr)?;
    check_if_written(&path)?;
    Ok(())
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

fn check_if_written(path: &Path) -> io::Result<()> {
    assert!(path.exists());
    let idx_file = path.join("simag.idx");
    let dat_file = path.join("simag.1.dat");
    assert!(idx_file.exists());
    assert!(dat_file.exists());

    let dat_size = dat_file.metadata()?.len();
    let idx_size = idx_file.metadata()?.len();
    assert!(dat_size > 0);
    assert!(idx_size > 0);

    Ok(())
}
