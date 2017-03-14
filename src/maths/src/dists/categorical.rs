use std::u8;

use P;

use itertools;
use rand::{random, Open01};

/// A discrete categorical distribution where k < 256 and sum(P(k)) == 1.
#[derive(Debug, Clone)]
pub struct Categorical {
    k: u8,
    event_prob: Vec<P>,
    sample_intervals: Vec<f32>,
}

impl Categorical {
    /// Takes a vector representing the probability of each category 
    /// for the random variable.
    pub fn new(categories: Vec<P>) -> Result<Categorical, String> {
        if categories.len() > u8::MAX as usize {
            let overflow = categories.len() - (u8::MAX as usize);
            return Err(format!("overflow by: {}", overflow));
        }

        let mut intervals = vec![0_f32; categories.len()];
        let mut i1 = 0_f32;
        let sum = itertools::fold(categories.iter().enumerate(), 0_f64, |a, (i, &b)| {
            i1 += b as f32;
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
            sample_intervals: intervals,
        })
    }

    /// Samples from a univariate categorical distribution and returns
    /// a category choice (zero-based indexed).
    pub fn sample(&self) -> u8 {
        let Open01(val) = random::<Open01<f32>>();
        self.sample_intervals.iter().position(|&p| p > val).unwrap() as u8
    }

    pub fn probabilities(&self) -> &[P] {
        &self.event_prob
    }
}

#[derive(Debug, Clone)]
pub struct Binomial {
    event_prob: P,
}

impl Binomial {
    /// Takes the probability of success as an argument.
    pub fn new(prob: P) -> Result<Binomial, String> {
        if prob >= 1. {
            return Err(format!("sum(p) == {} > 1.0", prob));
        } 

        Ok(Binomial {
            event_prob: prob,
        })
    }

    /// Samples from a univariate binomial distribution and returns a category choice 
    /// (zero-based indexed).
    pub fn sample(&self) -> u8 {
        let Open01(val) = random::<Open01<P>>();
        if val < (1. - self.event_prob) {
            0
        } else {
            1
        }
    }

    pub fn success_probability(&self) -> P {
        self.event_prob
    }

    pub fn probabilities(&self) -> [P; 2] {
        [1. - self.event_prob, self.event_prob]
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test::Bencher;

    #[bench]
    fn sample_categ(b: &mut test::Bencher) {
        let d = Categorical::new(vec![0.00392155_f64; 255]).unwrap();
        b.iter(|| d.sample());
    }

    #[bench]
    fn sample_binomial(b: &mut test::Bencher) { 
        let d = Binomial::new(0.5).unwrap();
        b.iter(|| d.sample());
    }
}
