use RGSLRng;

#[derive(Debug, Clone)]
pub struct Cauchy {
    x: f64,
    gamma: f64,
}

impl Cauchy {
    pub fn new(x: f64, gamma: f64) -> Result<Cauchy, ()> {
        if gamma <= 0.0 {
            return Err(());
        }

        Ok(Cauchy {
            x: x,
            gamma: gamma,
        })
    }

    pub fn std(gamma: f64) -> Result<Cauchy, ()> {
        if gamma <= 0.0 {
            return Err(());
        }

        Ok(Cauchy {
            x: 0.0,
            gamma: gamma,
        })
    }

    #[inline]
    pub fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::cauchy::cauchy_P;
        let std = x - self.x;

        cauchy_P(std, self.gamma)
    }
}

use super::{Sample, InverseCDF, InverseDensity};

impl Sample for Cauchy {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::cauchy::cauchy;

        cauchy(rng.rng(), self.gamma) + self.x
    }
}

impl InverseCDF for Cauchy {
    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::cauchy::cauchy_Pinv;

        cauchy_Pinv(x - self.x, self.gamma)
    }
}

impl InverseDensity for Cauchy {
    fn inverse_density(&self, y: f64) -> f64 {
        use rgsl::randist::cauchy::cauchy_pdf;

        let c = self.gamma * self.gamma;
        let y = y - self.x;
        cauchy_pdf(y, self.gamma / c) + self.x
    }
}
