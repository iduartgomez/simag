use err::ErrMsg;
use RGSLRng;

#[derive(Debug, Clone)]
pub struct Pareto {
    alpha: f64,
    scale: f64,
}

impl Pareto {
    pub fn new(alpha: f64, scale: f64) -> Result<Pareto, ()> {
        if scale <= 0.0 {
            return Err(());
        }

        Ok(Pareto { alpha, scale })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::pareto::pareto_pdf;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        pareto_pdf(x, self.alpha, self.scale)
    }
}

use super::{Sample, CDF};

impl Sample for Pareto {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::pareto::pareto;

        pareto(rng.get_gen(), self.alpha, self.scale)
    }
}

impl CDF for Pareto {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::pareto::pareto_P;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        pareto_P(x, self.alpha, self.scale)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::pareto::pareto_Pinv;

        if x.is_sign_negative() {
            panic!(ErrMsg::PositiveReal.panic_msg_with_arg(&self));
        }
        pareto_Pinv(x, self.alpha, self.scale)
    }
}
