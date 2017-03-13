//! Sampling for pure discrete models.

use std::marker::PhantomData;
use std::hash::Hash;

use super::*;
use model::{DiscreteModel, DiscreteDist, Observation, Node};
use model::{Discrete, Boolean, DType};

const ITER_TIMES: usize = 1000;
const BURN_IN: usize = 200;

#[derive(Debug)]
pub struct Gibbs<D, O> {
    _obtype: PhantomData<O>,
    _disttype: PhantomData<D>,
    times: usize,
    burnin: usize,
    samples: Vec<Vec<u8>>,
}

impl<D, O> Gibbs<D, O>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation
{
    pub fn new() -> Gibbs<D, O> {
        Gibbs {
            _obtype: PhantomData,
            _disttype: PhantomData,
            times: ITER_TIMES,
            burnin: BURN_IN,
            samples: Vec::with_capacity(ITER_TIMES),
        }
    }

    fn iter_var(&self, t: usize, var: &Node<D, O>) -> u8 {
        let mut mb_values = Vec::with_capacity(var.parents_len());
        // P(var|mb(var))
        for i in var.get_parents_positions() {
            // P(x) at t where x = parent has been calculated before the P(var|mb(var))
            // therefore take the the probabilities sampled from the posterior
            let val_at_t = self.samples[t][i];
            mb_values.push(val_at_t);
        }
        var.get_categ(&mb_values)
    }

    fn initialize(&mut self, net: &DiscreteModel<D, O, Gibbs<D, O>>) {
        // draw prior values from the distribution of each value
        let mut priors = Vec::with_capacity(net.var_numb());
        for distribution in net.iter_vars() {
            priors.push(self.initialization(distribution));
        }
        self.samples.push(priors);
    }

    fn initialization(&self, var: &Node<D, O>) -> u8 {
        // pick a random value of a prior dist for dist D with proper hyperparameter
        unimplemented!()
    }
}

impl<D, O> DiscreteSampler<D, O> for Gibbs<D, O>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation
{
    fn get_samples(mut self, net: &DiscreteModel<D, O, Gibbs<D, O>>) -> Vec<Vec<u8>> {
        let k = net.var_numb();
        self.samples = vec![vec![0_u8; k]; self.times];
        for t in 0..self.times {
            for (i, var_dist) in net.iter_vars().enumerate() {
                let transition_p = self.iter_var(t, var_dist);
                self.samples[t][i] = transition_p;
            }
        }
        self.samples
    }
}

impl<D, O> ::std::clone::Clone for Gibbs<D, O>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation
{
    fn clone(&self) -> Gibbs<D, O> {
        Gibbs::new()
    }
}
