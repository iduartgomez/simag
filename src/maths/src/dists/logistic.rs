use RGSLRng;

#[derive(Debug, Clone)]
pub struct Logistic {
    x: f64,
    k: f64,
}

impl Logistic {
    pub fn new(x: f64, k: f64) -> Result<Logistic, ()> {
        if k < 0.0 {
            return Err(());
        }

        Ok(Logistic {
            x: x,
            k: k,
        })
    }

    pub fn std() -> Logistic {
        Logistic {
            x: 0.0,
            k: 1.0,
        }
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::logistic::logistic_pdf;
        
        logistic_pdf(x - self.x, self.k)
    }
}

use super::{Sample, CDF,};

impl Sample for Logistic {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::logistic::logistic;

        logistic(rng.rng(), self.k) + self.x
    }
}

impl CDF for Logistic {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::logistic::logistic_P;
        let std = x - self.x;

        logistic_P(std, self.k)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::logistic::logistic_Pinv;

        logistic_Pinv(x - self.x, self.k)
    }
}
