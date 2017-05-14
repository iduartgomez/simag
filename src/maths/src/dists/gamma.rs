use RGSLRng;
use err::ErrMsg;

#[derive(Debug, Clone)]
pub struct Gamma {
    a: f64,
    b: f64,
}

impl Gamma {
    pub fn new(a: f64, b: f64) -> Result<Gamma, ()> {
        if a <= 0.0 || b <= 0.0 {
            return Err(());
        }

        Ok(Gamma { a: a, b: b })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::gamma::gamma_pdf;

        gamma_pdf(x, self.a, self.b)
    }
}

use super::{Sample, CDF};

impl Sample for Gamma {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::gamma::gamma;

        gamma(rng.get_gen(), self.a, self.b)
    }
}

impl CDF for Gamma {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::gamma::gamma_P;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        gamma_P(x, self.a, self.b)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::gamma::gamma_Pinv;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        gamma_Pinv(x, self.a, self.b)
    }
}
