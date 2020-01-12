use err::ErrMsg;
use RGSLRng;

#[derive(Debug, Clone)]
pub struct ChiSquared {
    k: u32,
}

impl ChiSquared {
    pub fn new(k: u32) -> Result<ChiSquared, ()> {
        if k == 0 {
            return Err(());
        }

        Ok(ChiSquared { k })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::chi_squared::chisq_pdf;

        chisq_pdf(x, f64::from(self.k))
    }
}

use super::{Sample, CDF};

impl Sample for ChiSquared {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::chi_squared::chisq;

        chisq(rng.get_gen(), f64::from(self.k))
    }
}

impl CDF for ChiSquared {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::chi_squared::chisq_P;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        chisq_P(x, f64::from(self.k))
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::chi_squared::chisq_Pinv;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        chisq_Pinv(x, f64::from(self.k))
    }
}
