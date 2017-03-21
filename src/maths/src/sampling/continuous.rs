use super::{Sampler, ContinuousSampler};
use model::{ContModel, ContNode, ContDAG};

const ITER_TIMES: usize = 1000;
const BURN_IN: usize = 0;

#[derive(Debug)]
pub struct Gibbs {
    steeps: usize,
    burnin: usize,
    normalized: usize, //ContModel<ContDAG>
    samples: Vec<Vec<f64>>,
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
            normalized: 0,
            samples: Vec::with_capacity(ITER_TIMES),
        }
    }
}

impl Gibbs {
    fn var_val<'a, N: 'a>(&self, t: usize, var: &N) -> f64
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
            var.draw_sample(&mb_values)
        } else {
            var.init_sample()
        }
    }

    fn initialize<'a, N: 'a>(&mut self, net: &ContModel<'a, N, Gibbs>)
        where N: ContNode<'a>
    {
        // draw prior values from the distribution of each value
        let mut priors = Vec::with_capacity(net.var_num());
        for node in net.iter_vars().filter(|x| x.is_root()) {
            priors.push(node.init_sample());
        }
        self.samples.push(priors);
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
