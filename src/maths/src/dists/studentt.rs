use RGSLRng;

#[derive(Debug, Clone)]
pub struct StudentT {
    nu: f64,
}

impl StudentT {
    pub fn new(nu: f64) -> Result<StudentT, ()> {
        if nu <= 0.0 {
            return Err(());
        }

        Ok(StudentT { nu: nu })
    }

    #[inline]
    pub fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::t_distribution::tdist_P;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing CDF of Student's T \
                    distribution")
        }
        tdist_P(x, self.nu)
    }
}

use super::{Sample, InverseCDF, InverseDensity};

impl Sample for StudentT {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::t_distribution::tdist;

        tdist(rng.rng(), self.nu)
    }
}

impl InverseCDF for StudentT {
    #[inline]
    fn inverse_cdf(&self, x: f64) -> f64 {
        use rgsl::randist::t_distribution::tdist_Pinv;

        if x.is_sign_negative() {
            panic!("simag: expected positive real number when computing inverse CDF of \
                    Student's T distribution")
        }
        tdist_Pinv(x, self.nu)
    }
}

impl InverseDensity for StudentT {
    fn inverse_density(&self, y: f64) -> f64 {
        use rgsl::gamma_beta::gamma::lngamma;
        use std::f64::consts::PI;

        let p = (self.nu + 1.0) / 2.0;
        if y <= 0.0 {
            panic!("simag: expected y > 0 real number when computing the density of \
                    the inverse Student's T distribution")
        } else if p > 171.0 {
            let of = p - 171.0;
            panic!("simag: overflow by {of}\nmax value of the operation (nu + 1 / 2) when \
                    computing the density of the inverse Student's T dist is 171.0,\n the parameter \
                    nu of the distribution was in this instance: {nu}",
                   of = of,
                   nu = self.nu)
        }

        let lg1 = lngamma(self.nu / 2.0);
        let lg2 = lngamma(p);
        let d = (y * y * (1.0 + (y * y * self.nu).powi(-1))).powf(-p);
        ((lg2 - lg1).exp() / (PI * self.nu).sqrt()) * d
    }
}
