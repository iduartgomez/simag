use RGSLRng;

#[derive(Debug, Clone)]
pub struct Gamma {
    a: f64,
    b: f64,
}

impl Gamma {
    pub fn new(a: f64, b: f64) -> Result<Gamma, ()> {
        if a <= 0.0 || b <= 0.0 {
            return Err(());
        }

        Ok(Gamma { a: a, b: b })
    }

    #[inline]
    pub fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::gamma::gamma_P;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing CDF of gamma dist")
        }
        gamma_P(x, self.a, self.b)
    }
}

use super::{Sample, InverseCDF, InverseDensity};

impl Sample for Gamma {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::gamma::gamma;

        gamma(rng.rng(), self.a, self.b)
    }
}

impl InverseCDF for Gamma {
    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::gamma::gamma_Pinv;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing inverse CDF of \
                    gamma dist")
        }
        gamma_Pinv(x, self.a, self.b)
    }
}

impl InverseDensity for Gamma {
    fn inverse_density(&self, y: f64) -> f64 {
        use rgsl::randist::gamma::gamma_pdf;

        if y <= 0.0 {
            panic!("simag: expected y > 0 real number when computing the density of \
                    the inverse gamma distribution")
        } else if self.a > 171.0 {
            let of = self.a - 171.0;
            panic!("simag: overflow by {of}\nmax value of the parameter alpha when \
                    computing the density of the inverse gamma T dist is 171.0,\n the \
                    parameter k of the distribution was in this instance: {nu}",
                   of = of,
                   nu = self.a)
        }

        let y = y / self.b;
        gamma_pdf(y, self.a, 1.0) / self.b
    }
}
