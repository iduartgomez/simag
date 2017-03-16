//! Sampling for pure discrete models.

use std::marker::PhantomData;

use super::*;
use model::{DiscreteModel, DiscreteVar, Observation, DiscreteNode};

const ITER_TIMES: usize = 1000;
const BURN_IN: usize = 0;

#[derive(Debug)]
pub struct Gibbs<D, O> {
    _obtype: PhantomData<O>,
    _disttype: PhantomData<D>,
    steeps: usize,
    burnin: usize,
    samples: Vec<Vec<u8>>,
}

impl<D, O> Gibbs<D, O>
    where D: DiscreteVar<O>,
          O: Observation
{
    pub fn new(steeps: Option<usize>, burnin: Option<usize>) -> Gibbs<D, O> {
        let steeps = match steeps {
            Some(val) => val,
            None => ITER_TIMES,
        };

        let burnin = match burnin {
            Some(val) => val,
            None => BURN_IN,
        };

        Gibbs {
            _obtype: PhantomData,
            _disttype: PhantomData,
            steeps: steeps,
            burnin: burnin,
            samples: Vec::with_capacity(ITER_TIMES),
        }
    }

    fn var_val(&self, t: usize, var: &DiscreteNode<D, O>) -> u8 {
        let mut mb_values = Vec::new();
        // P(var|mb(var))
        let parents = var.get_parents_positions();
        if !parents.is_empty() {
            for i in parents {
                // P(x) at t where x = parent has been calculated before the P(var|mb(var))
                // therefore take the the probabilities sampled from the posterior
                let val_at_t = self.samples[t][i];
                mb_values.push(val_at_t);
            }
            var.draw_sample(&mb_values)
        } else {
            var.dist.sample()
        }
    }

    fn initialize<'a: 'b, 'b>(&'b mut self, net: &'b DiscreteModel<'a, D, O, Gibbs<D, O>>) {
        use model::Node;

        // draw prior values from the distribution of each value
        let mut priors = Vec::with_capacity(net.var_num());
        for node in net.iter_vars().filter(|x| x.is_root()) {
            priors.push(node.dist.sample());
        }
        self.samples.push(priors);
    }
}

impl<D, O> DiscreteSampler<D, O> for Gibbs<D, O>
    where D: DiscreteVar<O>,
          O: Observation
{
    fn get_samples(mut self, net: &DiscreteModel<D, O, Gibbs<D, O>>) -> Vec<Vec<u8>> {
        let k = net.var_num();
        self.samples = vec![vec![0_u8; k]; self.steeps];
        self.initialize(net);
        for t in 0..self.steeps {
            for (i, var_dist) in net.iter_vars().enumerate() {
                let choice = self.var_val(t, var_dist);
                self.samples[t][i] = choice;
            }
        }
        self.samples
    }
}

impl<D, O> ::std::clone::Clone for Gibbs<D, O>
    where D: DiscreteVar<O>,
          O: Observation
{
    fn clone(&self) -> Gibbs<D, O> {
        let steeps = Some(self.steeps);
        let burnin = Some(self.burnin);
        Gibbs::new(steeps, burnin)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {

    }
}
