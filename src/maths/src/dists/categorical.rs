use std::u8;

use super::{GSRelaxation, GumbelSoftmax, Sample, CDF};
use RGSLRng;

use itertools;

/// A discrete categorical distribution where k < 256 and sum(f64(k)) == 1.
///
/// The distribution is assumed to be unlabelled and ordinal with sequence
/// [0, k) (zero-indexed distribution).
#[derive(Debug, Clone)]
pub struct Categorical {
    k: u8,
    event_prob: Vec<f64>,
    cdf: Vec<f64>,
}

impl Categorical {
    /// Takes a vector of length K > 2 representing the probability of each category
    /// for the random variable and returns the categorical distribution.
    ///
    /// For binary variables use the Bernoulli distribution.
    pub fn new(categories: Vec<f64>) -> Result<Categorical, String> {
        if categories.len() > u8::MAX as usize {
            let overflow = categories.len() - (u8::MAX as usize);
            return Err(format!("overflow by: {} categories", overflow));
        } else if categories.len() < 3 {
            return Err(
                "requires at least three categories, use Bernoulli distribution for \
                 distributions with 2 categories"
                    .to_string(),
            );
        }

        let mut intervals = vec![0_f64; categories.len()];
        let mut i1 = 0_f64;
        let sum = itertools::fold(categories.iter().enumerate(), 0_f64, |a, (i, &b)| {
            i1 += b;
            if i == categories.len() - 1 {
                intervals[i] = 1.;
            } else {
                intervals[i] = i1;
            }
            a + b
        });

        if sum > 1. {
            return Err(format!("sum(p) == {} > 1.0", sum));
        }

        Ok(Categorical {
            k: categories.len() as u8,
            event_prob: categories,
            cdf: intervals,
        })
    }

    /// Samples from a univariate categorical distribution and returns
    /// a category choice (zero-based indexed).
    #[inline]
    pub fn sample(&self, rng: &mut RGSLRng) -> u8 {
        use std::cmp::Ordering;

        let val = rng.uniform_pos();
        let res = self.cdf.binary_search_by(|x| {
            if x < &val {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });
        res.unwrap_err() as u8
    }

    #[inline]
    pub fn pmf(&self, x: u8) -> f64 {
        let x = x as usize;
        if x > self.event_prob.len() - 1 {
            panic!(
                "simag: index out of bounds; the number of categories for this random variable \
                 are `{}`, idx provided was `{}`",
                self.event_prob.len(),
                x
            )
        }
        self.event_prob[x]
    }

    #[inline]
    pub fn cmf(&self, x: u8) -> f64 {
        let x = x as usize;
        if x > self.cdf.len() - 1 {
            panic!(
                "simag: index out of bounds; the number of categories for this random variable \
                 are `{}`, idx provided was `{}`",
                self.cdf.len(),
                x
            )
        }
        self.cdf[x]
    }

    #[inline]
    pub fn k_num(&self) -> u8 {
        self.event_prob.len() as u8
    }
}

#[derive(Debug, Clone)]
pub struct Bernoulli {
    event_prob: f64,
}

impl Bernoulli {
    /// Takes the probability of success as an argument.
    pub fn new(prob: f64) -> Result<Bernoulli, String> {
        if prob >= 1. {
            return Err(format!("sum(p) == {} > 1.0", prob));
        }

        Ok(Bernoulli { event_prob: prob })
    }

    /// Samples from a univariate Bernoulli distribution and returns zero if
    /// the trial was a failure or one if it was a success.
    #[inline]
    pub fn sample(&self, rng: &mut RGSLRng) -> u8 {
        let val = rng.uniform_pos();
        if val <= self.event_prob {
            1
        } else {
            0
        }
    }

    #[inline]
    pub fn inverse_cdf(&self, p: f64) -> u8 {
        if p <= self.event_prob {
            1
        } else {
            0
        }
    }

    #[inline]
    pub fn success(&self) -> f64 {
        self.event_prob
    }

    #[inline]
    pub fn failure(&self) -> f64 {
        1. - self.event_prob
    }
}

impl GSRelaxation<RelaxedBernoulli> for Bernoulli {
    fn relaxed(&self, temperature: Option<f64>) -> RelaxedBernoulli {
        let t = if let Some(t) = temperature { t } else { 0.5 };
        RelaxedBernoulli {
            l: self.event_prob / 1. - self.event_prob,
            t,
            s: self.event_prob,
        }
    }
}

#[derive(Clone, Debug)]
pub struct RelaxedBernoulli {
    l: f64,
    t: f64,
    s: f64,
}

impl RelaxedBernoulli {
    pub fn pdf(&self, x: f64) -> f64 {
        let a = self.l * x;
        let b = 1.0 - x;
        let e = self.t - 1.0;
        (self.t * a).powf(-e) * b.powf(-e) / (a.powf(-self.t) + b.powf(-self.t)).powi(2)
    }

    /// Given a probability `p` return if the event was a success or not.
    #[inline]
    pub fn discretized(&self, p: f64) -> bool {
        p >= self.s
    }
}

impl GumbelSoftmax for RelaxedBernoulli {}

impl Sample for RelaxedBernoulli {
    #[inline]
    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        let log = -((1.0 / rng.uniform_pos()) - 1.0).ln();
        1.0 / (1.0 + (-(log + self.l.ln()) / self.t).exp())
    }
}

impl CDF for RelaxedBernoulli {
    #[inline]
    fn cdf(&self, x: f64) -> f64 {
        let log = -((1.0 / x) - 1.0).ln();
        1.0 / (1.0 + (-(log + self.l.ln()) / self.t).exp())
    }

    #[inline]
    fn inverse_cdf(&self, p: f64) -> f64 {
        let f = -(1.0 / p - 1.0).ln();
        (1.0 / (1.0 + (-self.t * f - self.l.ln()).exp()))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[ignore]
    #[test]
    fn relaxed_bernoulli() {
        let trials = 10000;
        let mut rng = RGSLRng::new();
        let dist = RelaxedBernoulli {
            l: 0.7 / 0.3,
            t: 0.1,
            s: 0.7,
        };
        let mut err = 0.01;
        let mut success: f64 = 0.;
        for _ in 0..trials {
            let sample = dist.sample(&mut rng);
            let cdf = dist.cdf(sample);
            let inv = dist.inverse_cdf(cdf);
            err = (err + (sample - inv).abs()) / 2.0;
            if dist.discretized(inv) {
                success += 1.;
            }
        }
        let mean = success / f64::from(trials);
        println!("mean: {}, err: {}", mean, err);
        assert!(err < 0.05);
        assert!((mean > 0.65) && (mean < 0.75));
    }
}
