use RGSLRng;

#[derive(Debug, Clone)]
pub struct Cauchy {
    mu: f64,
    gamma: f64,
}

impl Cauchy {
    pub fn new(mu: f64, gamma: f64) -> Result<Cauchy, ()> {
        if gamma <= 0.0 {
            return Err(());
        }

        Ok(Cauchy { mu: mu, gamma })
    }

    pub fn std(gamma: f64) -> Result<Cauchy, ()> {
        if gamma <= 0.0 {
            return Err(());
        }

        Ok(Cauchy { mu: 0.0, gamma })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::cauchy::cauchy_pdf;

        cauchy_pdf(x - self.mu, self.gamma)
    }
}

use super::{Sample, CDF};

impl Sample for Cauchy {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::cauchy::cauchy;

        cauchy(rng.get_gen(), self.gamma) + self.mu
    }
}

impl CDF for Cauchy {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::cauchy::cauchy_P;
        let std = x - self.mu;

        cauchy_P(std, self.gamma)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::cauchy::cauchy_Pinv;

        cauchy_Pinv(x - self.mu, self.gamma)
    }
}
