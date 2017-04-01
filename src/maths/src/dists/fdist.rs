use RGSLRng;

#[derive(Debug, Clone)]
pub struct FDist {
    d1: f64,
    d2: f64,
}

impl FDist {
    pub fn new(d1: f64, d2: f64) -> Result<FDist, ()> {
        if d1 <= 0.0 || d2 <= 0.0 {
            return Err(());
        }

        Ok(FDist { d1: d1, d2: d2 })
    }

    #[inline]
    pub fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::f_distribution::fdist_P;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing CDF of F-distribution")
        }
        fdist_P(x, self.d1, self.d2)
    }
}

use super::{Sample, InverseCDF, InverseDensity};

impl Sample for FDist {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::f_distribution::fdist;

        fdist(rng.rng(), self.d1, self.d2)
    }
}

impl InverseCDF for FDist {
    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::f_distribution::fdist_Pinv;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing inverse CDF of \
                    F-distribution")
        }
        fdist_Pinv(x, self.d1, self.d2)
    }
}

impl InverseDensity for FDist {
    fn inverse_density(&self, y: f64) -> f64 {
        use rgsl::randist::f_distribution::fdist_pdf;
        // inverse of F(x; d1, d2) is simply F(x; d2, d1)
        fdist_pdf(y, self.d2, self.d1)
    }
}
