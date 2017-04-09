//! Sampling for pure discrete models.

use RGSLRng;
use super::{Sampler, DiscreteSampler};
use model::{DiscreteModel, DiscreteNode};

const ITER_TIMES: usize = 1000;
const BURN_IN: usize = 0;

#[derive(Debug)]
pub struct Gibbs {
    steeps: usize,
    burnin: usize,
    samples: Vec<Vec<u8>>,
    rng: RGSLRng,
}

impl Sampler for Gibbs {
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
            samples: Vec::with_capacity(ITER_TIMES),
            rng: RGSLRng::new(),
        }
    }
}

impl Gibbs {
    fn var_val<'a, N: 'a>(&mut self, t: usize, var: &N) -> u8
        where N: DiscreteNode<'a>
    {
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
            var.draw_sample(&mut self.rng, &mb_values)
        } else {
            var.init_sample(&mut self.rng)
        }
    }

    fn initialize<'a, N: 'a>(&mut self, net: &DiscreteModel<'a, N, Gibbs>)
        where N: DiscreteNode<'a>
    {
        // draw prior values from the distribution of each value
        let mut priors = Vec::with_capacity(net.var_num());
        for node in net.iter_vars().filter(|x| x.is_root()) {
            priors.push(node.init_sample(&mut self.rng));
        }
        self.samples.push(priors);
    }
}

impl DiscreteSampler for Gibbs {
    fn get_samples<'a, N>(mut self, net: &DiscreteModel<'a, N, Gibbs>) -> Vec<Vec<u8>>
        where N: DiscreteNode<'a>
    {
        let k = net.var_num();
        self.samples = Vec::with_capacity(self.steeps);
        self.initialize(net);
        for t in 0..self.steeps {
            let mut steep = Vec::with_capacity(k);
            for var_dist in net.iter_vars() {
                let choice = self.var_val(t, &*var_dist);
                steep.push(choice);
            }
            self.samples.push(steep)
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
