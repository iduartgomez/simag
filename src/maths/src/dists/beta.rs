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
    pub fn cdf(&self, x: f64) -> f64 {
        use rgsl::randist::beta::beta_P;

        if x < 0.0 || x > 1.0 {
            panic!("simag: expected a real number in the interval [0,1] when computing CDF of \
                    beta dist")
        }
        beta_P(x, self.a, self.b)
    }
}

use super::{Sample, InverseCDF, InverseDensity};

impl Sample for Beta {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        use rgsl::randist::beta::beta;

        beta(rng.rng(), self.a, self.b)
    }
}

impl InverseCDF for Beta {
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

impl InverseDensity for Beta {
    fn inverse_density(&self, y: f64) -> f64 {
        unimplemented!()
        /*
        double gsl_ran_beta_pdf (const double x, const double a, const double b)
        {
            if (x < 0 || x > 1) {
                return 0 ;
            } else {
                double p;

                double gab = gsl_sf_lngamma (a + b);
                double ga = gsl_sf_lngamma (a);
                double gb = gsl_sf_lngamma (b);
                
                if (x == 0.0 || x == 1.0)  {
                    if (a > 1.0 && b > 1.0) {
                        p = 0.0;
                    } else {
                        p = exp (gab - ga - gb) * pow (x, a - 1) * pow (1 - x, b - 1);
                    }
                } else {
                    p = exp (gab - ga - gb + log(x) * (a - 1)  + log1p(-x) * (b - 1));
                }
            return p;
            }
        }
        */
    }
}
