//! Markov chain Monte Carlo sampling methods

use super::*;
use super::model::Distribution;


pub type DefaultSampler = Gibbs;

trait MCMCSampler: Sampler {}

pub struct Gibbs {}

impl Gibbs {
    pub fn new() -> Gibbs {
        Gibbs {}
    }
}

impl Sampler for Gibbs {
    type O = EventObs;

    fn get_sample(input: &Distribution<Self::O>) -> Self::O {
        unimplemented!()
    }
}

impl MCMCSampler for Gibbs {}
