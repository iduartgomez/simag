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

        Ok(Pareto {
            alpha: alpha,
            scale: scale,
        })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::pareto::pareto_pdf;

        assert!(x.is_sign_positive());
        pareto_pdf(x, self.alpha, self.scale)
    }
}

use super::{Sample, CDF};

impl Sample for Pareto {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::pareto::pareto;

        pareto(rng.rng(), self.alpha, self.scale)
    }
}

impl CDF for Pareto {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::pareto::pareto_P;

        assert!(x.is_sign_positive());
        pareto_P(x, self.alpha, self.scale)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::pareto::pareto_Pinv;

        assert!(x.is_sign_positive());
        pareto_Pinv(x, self.alpha, self.scale)
    }
}
