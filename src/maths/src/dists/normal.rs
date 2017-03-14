use float_cmp::ApproxEqUlps;
use FLOAT_EQ_ULPS;

#[derive(Debug, Clone)]
pub struct Normal {
    mu: f64,
    sigma: f64,
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

        Ok(Normal {
            mu: mu,
            sigma: sigma.abs(),
            std: false,
        })
    }

    pub fn phi(&self, x: f64) -> f64 {
        // transform x to z
        let x = {
            if self.std ||
               (self.mu.approx_eq_ulps(&0_f64, FLOAT_EQ_ULPS) &&
                self.sigma.approx_eq_ulps(&1_f64, FLOAT_EQ_ULPS)) {
                x
            } else {
                (x - self.mu) / self.sigma
            }
        };

        
        // constants
        let a1: f64 = 0.254829592;
        let a2: f64 = -0.284496736;
        let a3: f64 = 1.421413741;
        let a4: f64 = -1.453152027;
        let a5: f64 = 1.061405429;
        let p: f64 = 0.3275911;

        let sign: f64 = {
            if x < 0_f64 { -1. } else { 1. }
        };
        let x = x.abs() / 2_f64.sqrt();
        
        let t: f64 = 1. / (1. + p * x);
        let y: f64 = 1. - ((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t * (-x * x).exp();
        0.5 * (1. + sign * y)
    }

    pub fn sample() {}
}

#[cfg(test)]
mod test {
    use super::*;
    use test::Bencher;

    #[bench]
    fn compute_phi(b: &mut test::Bencher) {
        fn b_phi() {
            let d = Normal::new(2., 2.).unwrap();
            let p = d.phi(3.);
        }
        b.iter(|| b_phi());

        let d = Normal::std();
        let p = d.phi(0.);
        assert!(p < 0.5001 && p > 0.4999);

        let d = Normal::new(2., 2.).unwrap();
        let p = d.phi(3.);
        assert!(p < 0.6916 && p > 0.6914);
    }
}
