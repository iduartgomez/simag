use RGSLRng;

#[derive(Debug, Clone)]
pub struct Exponential {
    rate: f64,
    mean: f64,
}

impl Exponential {
    pub fn new(lambda: f64) -> Result<Exponential, ()> {
        if lambda.is_sign_negative() {
            return Err(());
        }

        Ok(Exponential {
            rate: lambda,
            mean: 1. / lambda,
        })
    }

    #[inline]
    pub fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::exponential::exponential_P;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing CDF of exponential \
                    distribution")
        }
        exponential_P(x, self.mean)
    }
}

use super::{Sample, InverseCDF};

impl Sample for Exponential {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::exponential::exponential;

        exponential(rng.rng(), self.mean)
    }
}

impl InverseCDF for Exponential {
    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::exponential::exponential_Pinv;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing inverse CDF of \
                    exponential distribution")
        }
        exponential_Pinv(x, self.mean)
    }
}
