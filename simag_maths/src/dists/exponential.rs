use RGSLRng;
use err::ErrMsg;

#[derive(Debug, Clone)]
pub struct Exponential {
    rate: f64,
    mean: f64,
}

impl Exponential {
    pub fn new(lambda: f64) -> Result<Exponential, ()> {
        if lambda <= 0.0 {
            return Err(());
        }

        Ok(Exponential {
            rate: lambda,
            mean: 1. / lambda,
        })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::exponential::exponential_pdf;

        exponential_pdf(x, self.mean)
    }
}

use super::{Sample, CDF};

impl Sample for Exponential {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::exponential::exponential;

        exponential(rng.get_gen(), self.mean)
    }
}

impl CDF for Exponential {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::exponential::exponential_P;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        exponential_P(x, self.mean)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::exponential::exponential_Pinv;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        exponential_Pinv(x, self.mean)
    }
}
