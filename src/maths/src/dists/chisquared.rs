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
    pub fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::chi_squared::chisq_P;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing CDF of chi-squared")
        }
        chisq_P(x, self.k as f64)
    }
}

use super::{Sample, InverseCDF, InverseDensity};

impl Sample for ChiSquared {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::chi_squared::chisq;

        chisq(rng.rng(), self.k as f64)
    }
}

impl InverseCDF for ChiSquared {
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

impl InverseDensity for ChiSquared {
    fn inverse_density(&self, y: f64) -> f64 {
        use rgsl::gamma_beta::gamma::lngamma;

        let d = self.k as f64 / 2.0;
        if y <= 0.0 {
            panic!("simag: expected y > 0 real number when computing the density of \
                    the inverse chi-squared distribution")
        } else if d > 171.0 {
            let of = d - 171.0;
            panic!("simag: overflow by {of}\nmax value of the operation (k + 1 / 2) when \
                    computing the density of the inverse chi-squared T dist is 171.0,\n the \
                    parameter k of the distribution was in this instance: {nu}",
                   of = of,
                   nu = self.k)
        }

        let x = 2.0_f64;
        x.powf(-d) / lngamma(d) * y.powf(-d - 1.0) * (-1.0 / 2.0 * y).exp()
    }
}
