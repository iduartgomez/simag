use err::ErrMsg;
use RGSLRng;

#[derive(Debug, Clone)]
pub struct TDist {
    nu: f64,
}

impl TDist {
    pub fn new(nu: f64) -> Result<TDist, ()> {
        if nu <= 0.0 {
            return Err(());
        }

        Ok(TDist { nu })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::t_distribution::tdist_pdf;

        tdist_pdf(x, self.nu)
    }
}

use super::{Sample, CDF};

impl Sample for TDist {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::t_distribution::tdist;

        tdist(rng.get_gen(), self.nu)
    }
}

impl CDF for TDist {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::t_distribution::tdist_P;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        tdist_P(x, self.nu)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::t_distribution::tdist_Pinv;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        tdist_Pinv(x, self.nu)
    }
}
