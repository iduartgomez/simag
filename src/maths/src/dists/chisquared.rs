use RGSLRng;

#[derive(Debug, Clone)]
pub struct ChiSquared {
    k: u32,
}

impl ChiSquared {
    pub fn new(k: u32) -> Result<ChiSquared, ()> {
        if k == 0 {
            return Err(());
        }

        Ok(ChiSquared { k: k })
    }

    #[inline]
    pub fn pdf(&self, x: f64) -> f64 {
        use rgsl::randist::chi_squared::chisq_pdf;

        chisq_pdf(x, self.k as f64)
    }
}

use super::{Sample, CDF};

impl Sample for ChiSquared {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::chi_squared::chisq;

        chisq(rng.rng(), self.k as f64)
    }
}

impl CDF for ChiSquared {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::chi_squared::chisq_P;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing CDF of chi-squared")
        }
        chisq_P(x, self.k as f64)
    }

    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::chi_squared::chisq_Pinv;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing inverse CDF of \
                    chi-squared")
        }
        chisq_Pinv(x, self.k as f64)
    }
}
