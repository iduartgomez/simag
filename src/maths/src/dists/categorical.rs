use std::u8;

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
    /// Takes a vector representing the probability of each category
    /// for the random variable.
    pub fn new(categories: Vec<f64>) -> Result<Categorical, String> {
        if categories.len() > u8::MAX as usize {
            let overflow = categories.len() - (u8::MAX as usize);
            return Err(format!("overflow by: {}", overflow));
        } else if categories.len() < 2 {
            return Err("requires at least two categories".to_string())
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
        let val = rng.uniform_pos();
        self.cdf.iter().position(|&p| p > val).unwrap() as u8
    }

    #[inline]
    pub fn pmf(&self, x: u8) -> f64 {
        let x = x as usize;
        if x > self.event_prob.len() - 1 {
            panic!("simag: index out of bounds; the number of categories for this random variable \
                    are `{}`, idx provided was `{}`",
                   self.event_prob.len(),
                   x)
        }
        self.event_prob[x]
    }

    #[inline]
    pub fn cmf(&self, x: u8) -> f64 {
        let x = x as usize;
        if x > self.event_prob.len() - 1 {
            panic!("simag: index out of bounds; the number of categories for this random variable \
                    are `{}`, idx provided was `{}`",
                   self.event_prob.len(),
                   x)
        }
        let mut acc = 0.0_f64;
        for i in 0..(x + 1) {
            acc += self.event_prob[i];
        }
        acc
    }

    #[inline]
    pub fn k_num(&self) -> u8 {
        self.event_prob.len() as u8
    }
}

#[derive(Debug, Clone)]
pub struct Binomial {
    event_prob: f64,
}

impl Binomial {
    /// Takes the probability of success as an argument.
    pub fn new(prob: f64) -> Result<Binomial, String> {
        if prob >= 1. {
            return Err(format!("sum(p) == {} > 1.0", prob));
        }

        Ok(Binomial { event_prob: prob })
    }

    /// Samples from a univariate binomial distribution and returns a category choice
    /// (zero-based indexed).
    #[inline]
    pub fn sample(&self, rng: &mut RGSLRng) -> u8 {
        let val = rng.uniform_pos();
        if val < (1. - self.event_prob) { 0 } else { 1 }
    }

    pub fn success(&self) -> f64 {
        self.event_prob
    }

    pub fn failure(&self) -> f64 {
        1. - self.event_prob
    }
}
