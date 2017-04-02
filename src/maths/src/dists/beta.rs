use RGSLRng;

#[derive(Debug, Clone)]
pub struct Beta {
    a: f64,
    b: f64,
}

impl Beta {
    pub fn new(a: f64, b: f64) -> Result<Beta, ()> {
        if a <= 0.0 || b <= 0.0 {
            return Err(());
        }

        Ok(Beta { a: a, b: b })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::beta::beta_pdf;
        
        beta_pdf(x, self.a, self.b)
    }
}

use super::{Sample, CDF};

impl Sample for Beta {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::beta::beta;

        beta(rng.rng(), self.a, self.b)
    }
}

impl CDF for Beta {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::beta::beta_P;

        if x < 0.0 || x > 1.0 {
            panic!("simag: expected a real number in the interval [0,1] when computing CDF of \
                    beta dist")
        }
        beta_P(x, self.a, self.b)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::beta::beta_Pinv;

        if x < 0.0 || x > 1.0 {
            panic!("simag: expected a real number in the interval [0,1] when computing inverse \
                    CDF of beta dist")
        }
        beta_Pinv(x, self.a, self.b)
    }
}
