use RGSLRng;

#[derive(Debug, Clone)]
pub struct Logistic;

impl Logistic {
    pub fn std() -> Logistic {
        Logistic
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::logistic::logistic_pdf;

        logistic_pdf(x, 1.0)
    }
}

use super::{Sample, CDF};

impl Sample for Logistic {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::logistic::logistic;

        logistic(rng.get_gen(), 1.0)
    }
}

impl CDF for Logistic {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::logistic::logistic_P;

        logistic_P(x, 1.0)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::logistic::logistic_Pinv;

        logistic_Pinv(x, 1.0)
    }
}
