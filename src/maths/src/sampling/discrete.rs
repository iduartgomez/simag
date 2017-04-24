//! Sampling for pure discrete models.

use RGSLRng;
use super::{MarginalSampler, DiscMargSampler};
use model::{DiscreteModel, DiscreteNode};

const ITER_TIMES: usize = 2000;
const BURN_IN: usize = 500;

/// A Gibbs sampler inteded for Bayesian nets (encoded causality through directed
/// acyclic graphs) formed by discrete variables.
#[derive(Debug)]
pub struct Gibbs {
    steeps: usize,
    burnin: usize,
    samples: Vec<Vec<u8>>,
    parents: Vec<Vec<usize>>,
    rng: RGSLRng,
}

impl MarginalSampler for Gibbs {
    fn new(steeps: Option<usize>, burnin: Option<usize>) -> Gibbs {
        let steeps = match steeps {
            Some(val) => val,
            None => ITER_TIMES,
        };

        let burnin = match burnin {
            Some(val) => val,
            None => BURN_IN,
        };

        Gibbs {
            steeps: steeps,
            burnin: burnin,
            samples: Vec::with_capacity(steeps),
            parents: vec![],
            rng: RGSLRng::new(),
        }
    }
}

impl Gibbs {
    fn var_val<'a, N>(&mut self, t: usize, var: &N, var_pos: usize) -> u8
        where N: DiscreteNode<'a>
    {
        let parents = &self.parents[var_pos];
        let mut mb_values = Vec::with_capacity(parents.len());
        // P(var|mb(var))
        if !parents.is_empty() {
            for i in parents {
                // P(x) at t where x = parent has been calculated before the P(var|mb(var))
                // therefore take the the probabilities sampled from the posterior
                let val_at_t = self.samples[t][*i];
                mb_values.push(val_at_t);
            }
            var.draw_sample(&mut self.rng, &mb_values)
        } else {
            var.init_sample(&mut self.rng)
        }
    }

    fn initialize<'a, N>(&mut self, net: &DiscreteModel<'a, N>)
        where N: DiscreteNode<'a>
    {
        // draw prior values from the distribution of each value
        let mut priors = Vec::with_capacity(net.var_num());
        for node in net.iter_vars().filter(|x| x.is_root()) {
            let parents = node.get_parents_positions();
            self.parents.push(parents);
            priors.push(node.init_sample(&mut self.rng));
        }
        self.samples.push(priors);
    }
}

impl DiscMargSampler for Gibbs {
    fn get_samples<'a, N>(mut self, net: &DiscreteModel<'a, N>) -> Vec<Vec<u8>>
        where N: DiscreteNode<'a>
    {
        let k = net.var_num();
        self.initialize(net);
        for t in 0..self.steeps + self.burnin {
            let mut steep = Vec::with_capacity(k);
            for (i, var_dist) in net.iter_vars().enumerate() {
                let choice = self.var_val(t, &*var_dist, i);
                steep.push(choice);
            }
            if t >= self.burnin {
                self.samples.push(steep)
            }
        }
        self.samples
    }
}

impl ::std::clone::Clone for Gibbs {
    fn clone(&self) -> Gibbs {
        let steeps = Some(self.steeps);
        let burnin = Some(self.burnin);
        Gibbs::new(steeps, burnin)
    }
}
