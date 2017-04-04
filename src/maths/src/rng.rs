use rgsl::types::rng::Rng as GSLRng;
use rand::{OsRng, Rng};

use err::ErrMsg;

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
            panic!(ErrMsg::RGSLRngInstance.panic_msg())
        }
    }

    #[inline]
    pub fn uniform(&mut self) -> f64 {
        self.cnt += 1;
        if self.cnt == ENTROPY {
            self.reseed();
        }
        self.inner.uniform()
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

    pub fn get_gen(&mut self) -> &GSLRng {
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
