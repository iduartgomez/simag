//! Support mathematical methods library for the `simAG` framework

extern crate float_cmp;
//extern crate ndarray;
extern crate itertools;
extern crate rand;
extern crate uuid;
extern crate rgsl;

pub mod model;
pub mod sampling;
pub mod dists;

//const FLOAT_EQ_ULPS: i64 = 2;

/// Probability type
pub type P = f64;

pub use model::DiscreteModel;

use rgsl::types::rng::Rng as GSLRng;

pub struct RGSLRng {
    inner: GSLRng,
    cnt: usize,
}

impl RGSLRng {
    pub fn new() -> RGSLRng {
        use rgsl::types::rng;
        let rng_type = rng::algorithms::taus2();
        if let Some(generator) = rng::Rng::new(&rng_type) {
            generator.set(0);
            RGSLRng {
                inner: generator,
                cnt: 0,
            }
        } else {
            panic!("failed to instantiate the RNG from GSL")
        }
    }

    fn reseed(&mut self) {
        use rand::{OsRng, Rng};
        let seed = OsRng::new().unwrap().next_u32() as usize;
        self.inner.set(seed);
    }

    fn rng(&mut self) -> &GSLRng {
        self.cnt += 1;
        if self.cnt == 32 * 1024 {
            self.reseed();
        }
        &self.inner
    }
}

impl Clone for RGSLRng {
    fn clone(&self) -> RGSLRng {
        let mut rng = RGSLRng {
            inner: self.inner.clone(),
            cnt: 0,
        };
        rng.reseed();
        rng
    }
}

use std::fmt;

impl fmt::Debug for RGSLRng {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RGSLRng")
    }
}
