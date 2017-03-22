use super::{Sampler, ContinuousSampler};
use model::{ContModel, ContNode, DefContVar};
use dists::Gaussianization;
use RGSLRng;

const ITER_TIMES: usize = 1000;
const BURN_IN: usize = 0;

#[derive(Debug)]
pub struct Gibbs {
    steeps: usize,
    burnin: usize,
    normalized: Vec<Normalized>,
    samples: Vec<Vec<f64>>,
    rng: RGSLRng,
}

#[derive(Debug)]
struct Normalized {
    var: DefContVar,
    pt_cr: Vec<f64>,
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
            normalized: vec![],
            samples: Vec::with_capacity(ITER_TIMES),
            rng: RGSLRng::new(),
        }
    }
}

impl Gibbs {
    fn var_val<'a, N: 'a>(&mut self, t: usize, var: &N) -> f64
        where N: ContNode<'a>
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

    /// In a pure continuous random variable Bayesian net, the following algorithm is used:
    ///
    /// 1)  Every variable is transformed to the standard unit normal distribution.
    /// 2)  Construct the vine for the standard normal variables.
    /// 3)
    fn initialize<'a, N: 'a>(&mut self, net: &ContModel<'a, N, Gibbs>)
        where N: ContNode<'a>
    {
        // construct a transformed BBN
        for node in net.iter_vars() {
            let d = node.get_dist().as_normal().into_default();
            let n = Normalized {
                pt_cr: Vec::with_capacity(node.childs_num()),
                var: d,
            };
            self.normalized.push(n);
        }

        /*
        let mut priors = Vec::with_capacity(net.var_num());
        for node in net.iter_vars() {
            priors.push(node.init_sample(&mut self.rng));
        }
        self.samples.push(priors);
        */
    }
}

impl ContinuousSampler for Gibbs {
    fn get_samples<'a, N: 'a>(mut self, net: &ContModel<'a, N, Gibbs>) -> Vec<Vec<f64>>
        where N: ContNode<'a>
    {
        let k = net.var_num();
        self.samples = vec![vec![0.0_f64; k]; self.steeps];
        self.initialize(net);
        for t in 0..self.steeps {
            for (i, var_dist) in net.iter_vars().enumerate() {
                let choice = self.var_val(t, &*var_dist);
                self.samples[t][i] = choice;
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

#[cfg(test)]
mod test {
    //use super::*;

    #[test]
    fn sample() {}
}
