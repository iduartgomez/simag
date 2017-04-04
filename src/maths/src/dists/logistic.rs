use RGSLRng;

#[derive(Debug, Clone)]
pub struct Logistic {
    mu: f64,
    sigma: f64,
}

impl Logistic {
    pub fn new(mu: f64, sigma: f64) -> Result<Logistic, ()> {
        if sigma < 0.0 {
            return Err(());
        }

        Ok(Logistic {
            mu: mu,
            sigma: sigma,
        })
    }

    pub fn std() -> Logistic {
        Logistic {
            mu: 0.0,
            sigma: 1.0,
        }
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::logistic::logistic_pdf;

        logistic_pdf(x - self.mu, self.sigma)
    }
}

use super::{Sample, CDF};

impl Sample for Logistic {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::logistic::logistic;

        logistic(rng.get_gen(), self.sigma) + self.mu
    }
}

impl CDF for Logistic {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::logistic::logistic_P;
        let std = x - self.mu;

        logistic_P(std, self.sigma)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::logistic::logistic_Pinv;

        logistic_Pinv(x - self.mu, self.sigma)
    }
}
