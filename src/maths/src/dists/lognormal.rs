use RGSLRng;
use err::ErrMsg;

#[derive(Debug, Clone)]
pub struct LogNormal {
    zeta: f64,
    sigma: f64,
}

impl LogNormal {
    pub fn new(zeta: f64, sigma: f64) -> Result<LogNormal, ()> {
        if sigma <= 0.0 {
            return Err(());
        }

        Ok(LogNormal {
            zeta: zeta,
            sigma: sigma,
        })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::lognormal::lognormal_pdf;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        lognormal_pdf(x, self.zeta, self.sigma)
    }
}

use super::{Sample, CDF};

impl Sample for LogNormal {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::lognormal::lognormal;

        lognormal(rng.get_gen(), self.zeta, self.sigma)
    }
}

impl CDF for LogNormal {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::lognormal::lognormal_P;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        lognormal_P(x, self.zeta, self.sigma)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::lognormal::lognormal_Pinv;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        lognormal_Pinv(x, self.zeta, self.sigma)
    }
}
