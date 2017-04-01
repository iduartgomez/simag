//! Support mathematical methods library for the `simAG` framework

extern crate float_cmp;
extern crate itertools;
extern crate rand;
extern crate uuid;
extern crate rgsl;
//extern crate ndarray;

pub mod model;
pub mod sampling;
pub mod dists;

//const FLOAT_EQ_ULPS: i64 = 2;

pub use model::DiscreteModel;

use rgsl::types::rng::Rng as GSLRng;
use rand::{OsRng, Rng};

pub struct RGSLRng {
    inner: GSLRng,
    thread_rng: OsRng,
    cnt: usize,
}

const ENTROPY: usize = 32 * 1024;

impl RGSLRng {
    pub fn new() -> RGSLRng {
        use rgsl::types::rng;
        let rng_type = rng::algorithms::taus2();
        if let Some(mut generator) = rng::Rng::new(&rng_type) {
            let mut thread_rng = OsRng::new().unwrap();
            let seed = thread_rng.next_u32() as usize;
            generator.set(seed);
            RGSLRng {
                inner: generator,
                thread_rng: thread_rng,
                cnt: 0,
            }
        } else {
            panic!("failed to instantiate the RNG from GSL")
        }
    }

    #[inline]
    pub fn uniform_pos(&mut self) -> f64 {
        self.cnt += 1;
        if self.cnt == ENTROPY {
            self.reseed();
        }
        self.inner.uniform_pos()
    }

    #[inline]
    pub fn uniform_int(&mut self, n: usize) -> usize {
        self.cnt += 1;
        if self.cnt == ENTROPY {
            self.reseed();
        }
        self.inner.uniform_int(n)
    }

    fn reseed(&mut self) {
        let seed = self.thread_rng.next_u32() as usize;
        self.inner.set(seed);
    }

    fn rng(&mut self) -> &GSLRng {
        self.cnt += 1;
        if self.cnt == ENTROPY {
            self.reseed();
        }
        &self.inner
    }
}

impl Clone for RGSLRng {
    fn clone(&self) -> RGSLRng {
        let thread_rng = OsRng::new().unwrap();
        let mut rng = RGSLRng {
            inner: self.inner.clone(),
            thread_rng: thread_rng,
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
