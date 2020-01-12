use err::ErrMsg;
use RGSLRng;

#[derive(Debug, Clone)]
pub struct FDist {
    d1: f64,
    d2: f64,
}

impl FDist {
    pub fn new(d1: f64, d2: f64) -> Result<FDist, ()> {
        if d1 <= 0.0 || d2 <= 0.0 {
            return Err(());
        }

        Ok(FDist { d1, d2 })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::f_distribution::fdist_pdf;

        fdist_pdf(x, self.d1, self.d2)
    }
}

use super::{Sample, CDF};

impl Sample for FDist {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::f_distribution::fdist;

        fdist(rng.get_gen(), self.d1, self.d2)
    }
}

impl CDF for FDist {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::f_distribution::fdist_P;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        fdist_P(x, self.d1, self.d2)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::f_distribution::fdist_Pinv;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        fdist_Pinv(x, self.d1, self.d2)
    }
}
