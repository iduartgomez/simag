use RGSLRng;

#[derive(Debug, Clone)]
pub struct Normal {
    pub mu: f64,
    pub sigma: f64,
    std: bool,
}

impl Normal {
    pub fn std() -> Normal {
        Normal {
            mu: 0.,
            sigma: 1.,
            std: true,
        }
    }

    pub fn new(mu: f64, sigma: f64) -> Result<Normal, ()> {
        if sigma < 0. {
            return Err(());
        }

        if mu == 0. && sigma == 1. {
            Ok(Normal::std())
        } else {
            Ok(Normal {
                mu: mu,
                sigma: sigma.abs(),
                std: false,
            })
        }
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::gaussian::gaussian_pdf;
        use rgsl::randist::gaussian::ugaussian_pdf;
        
        if self.std {
            ugaussian_pdf(x)
        } else {
            gaussian_pdf(x - self.mu, self.sigma)
        }
    }
}

use super::{Sample, CDF};

impl Sample for Normal {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::gaussian::gaussian_ziggurat;
        use rgsl::randist::gaussian::ugaussian;

        if self.std {
            ugaussian(rng.rng())
        } else {
            let s = gaussian_ziggurat(rng.rng(), self.sigma);
            s + self.mu
        }
    }
}

impl CDF for Normal {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::gaussian::gaussian_P;
        use rgsl::randist::gaussian::ugaussian_P;

        if self.std {
            ugaussian_P(x)
        } else {
            let x = x - self.mu;
            gaussian_P(x, self.sigma)
        }
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::gaussian::gaussian_Pinv;
        use rgsl::randist::gaussian::ugaussian_Pinv;

        if self.std {
            ugaussian_Pinv(x)
        } else {
            gaussian_Pinv(x - self.mu, self.sigma)
        }
    }
}
