use std::f64::consts::PI;
use std::ops::Deref;
//use std::collections::{HashMap};

use rgsl::{MatrixF64, VectorF64};
use rgsl;

use super::{MarginalSampler, HybridMargSampler, HybridRes};
use model::{Variable, HybridNode, ContVar, DefContVar, DType, IterModel};
use dists::{Normal, Normalization, CDF};
use err::ErrMsg;
use RGSLRng;

const ITER_TIMES: usize = 2000;
const BURN_IN: usize = 500;

/// An exact sampler intended for Bayesian nets (encoded causality through directed
/// acyclic graphs) formed by discreted and/or continuous variables.
///
/// In a pure continuous Bayesian net, the following algorithm is used:
///
/// 1.  Every distribution is transformed to the standard unit normal distribution
///     and previously sampled.
/// 2.  Construct the vine for the standard normal variables.
/// 3.  Compute the correlation matrix for the full joint distribution (correlation 0
///     entails independence)
/// 4.  Using a matrix obtained through factorization of the correlation matrix,
///     compute the sample for each distribution and transform back to the original,
///     distribution using the inverse cumulative distribution function.
/// 5.  Repeat 4 as many times as specified by the `steeps` parameter of the sampler.
///
/// In addition to those steeps, for an hybrid network, the discrete variables are
/// transformed to continuous variables through the available method implemented for
/// the model's variable type.
#[derive(Debug)]
pub struct ExactNormalized<'a> {
    steeps: usize,
    burnin: usize,
    normalized: Vec<Normalized<'a>>,
    samples: Vec<Vec<HybridRes>>,
    rng: RGSLRng,
    cr_matrix: MatrixF64,
}

unsafe impl<'a> Sync for ExactNormalized<'a> {}
unsafe impl<'a> Send for ExactNormalized<'a> {}

#[derive(Debug)]
struct Normalized<'a> {
    var: DefContVar,
    original: &'a DType,
}

impl<'a> MarginalSampler for ExactNormalized<'a> {
    fn new(steeps: Option<usize>, burnin: Option<usize>) -> ExactNormalized<'a> {
        let steeps = match steeps {
            Some(val) => val,
            None => ITER_TIMES,
        };

        let burnin = match burnin {
            Some(val) => val,
            None => BURN_IN,
        };

        ExactNormalized {
            steeps: steeps,
            burnin: burnin,
            normalized: vec![],
            samples: Vec::with_capacity(ITER_TIMES),
            rng: RGSLRng::new(),
            cr_matrix: MatrixF64::new(1, 1).unwrap(),
        }
    }
}

impl<'a> ExactNormalized<'a> {
    fn initialize<M>(&mut self, net: &M)
        where <<<M as IterModel>::Iter as Iterator>::Item as Deref>::Target: HybridNode<'a>,
              <<M as IterModel>::Iter as Iterator>::Item: Deref,
              M: IterModel
    {
        use model::ContNode;
        //use super::partial_correlation;

        const PI_DIV_SIX: f64 = PI / 6.0;
        // construct a std normal variables net and the joint partial correlation matrix
        // we do this by:
        // 1) sampling from each random variable in the network (where for
        // variable X and prob func F(x) it has an invertible F^-1(x) function, otherwise panic)
        // and normalizing the sample using the inverse CDF of std normal
        // 2) compute the partial correlation of each edge conditioned on other edges which are
        // conditioning the child, the order depends on the topological sort algorithm of the DAG
        // 3) repeating steep 2 for the whole vine will specify the correlation matrix for the full
        // correlation matrix for the joint mulvariate std normal distribution
        let d = net.var_num();
        self.cr_matrix = MatrixF64::new(d, d).unwrap();
        self.cr_matrix.set_identity();
        //let mut cached: HashMap<(usize, usize), f64> = HashMap::new();
        for (i, node) in net.iter_vars().enumerate() {
            let dist = node.get_dist().as_normal(self.steeps).into_default();
            for (pt_cr, j) in node.get_edges() {
                // ρ{i,j}|D = 2 * sin( π/6 * r{i,j}|D )
                let rho_xy = 2.0 * (PI_DIV_SIX * pt_cr).sin();
                //cached.insert((i, j), rho_xy);
                self.cr_matrix.set(i, j, rho_xy);
            }
            //let anc = node.get_all_ancestors();
            //partial_correlation(i, &anc, &mut cached, &mut self.cr_matrix);
            let d = unsafe {
                &*(node.get_dist() as *const _) 
                as &'a <<<<M as IterModel>::Iter as Iterator>::Item as Deref>::Target 
                             as ContNode>::Var 
            };
            let n = Normalized {
                var: dist,
                original: d.dist_type(),
            };
            self.normalized.push(n);
        }
    }
}

impl<'a> HybridMargSampler<'a> for ExactNormalized<'a> {
    fn get_samples<M>(mut self, net: &M) -> Vec<Vec<HybridRes>>
        where <<<M as IterModel>::Iter as Iterator>::Item as Deref>::Target: HybridNode<'a>,
              <<M as IterModel>::Iter as Iterator>::Item: Deref,
              M: IterModel
    {
        use rgsl::blas::level2::dtrmv;

        let std = Normal::std();
        self.initialize(net);
        let d = self.normalized.len();
        for t in 0..self.steeps {
            let mut steep_sample = VectorF64::new(d).unwrap();
            for (i, normal) in self.normalized.iter().enumerate() {
                // get each normalized sample obtained during initialization
                // for var i at steep t, then transform back to the original distribution
                // using the inverse dist function F^-1
                let sample = normal.var.get_obs_unchecked(t);
                steep_sample.set(i, sample);
            }
            dtrmv(rgsl::cblas::Uplo::Lower,
                  rgsl::cblas::Transpose::NoTrans,
                  rgsl::cblas::Diag::NonUnit,
                  &self.cr_matrix,
                  &mut steep_sample);
            let mut f = Vec::with_capacity(d);
            for i in 0..d {
                let sample = std.cdf(steep_sample.get(i));
                let sample = match *self.normalized[i].original {
                    DType::Normal(ref dist) => HybridRes::Continuous(sample + dist.mu),
                    DType::Exponential(ref dist) => {
                        HybridRes::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::Beta(ref dist) => {
                        HybridRes::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::Gamma(ref dist) => {
                        HybridRes::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::ChiSquared(ref dist) => {
                        HybridRes::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::TDist(ref dist) => {
                        HybridRes::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::FDist(ref dist) => {
                        HybridRes::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::Cauchy(ref dist) => {
                        HybridRes::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::LogNormal(ref dist) => {
                        HybridRes::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::Logistic(ref dist) => {
                        HybridRes::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::Pareto(ref dist) => {
                        HybridRes::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::RelaxedBernoulli(ref dist) => {
                        let success = if dist.discretized(dist.inverse_cdf(sample)) {
                            1
                        } else {
                            0
                        };
                        HybridRes::Discrete(success)
                    }
                    ref d => panic!(ErrMsg::DiscDistContNode.panic_msg_with_arg(d)),
                };
                f.push(sample)
            }
            self.samples.push(f);
        }
        self.samples
    }
}

impl<'a> ::std::clone::Clone for ExactNormalized<'a> {
    fn clone(&self) -> ExactNormalized<'a> {
        let steeps = Some(self.steeps);
        let burnin = Some(self.burnin);
        ExactNormalized::new(steeps, burnin)
    }
}


/// Gibbs sampler for Markov Random Fields.
///
/// This algorithm draws a sample for each variable in the model for *t* steeps
/// and converges to the stationary distribution.
///
/// The initialization ans sampling sorting at each steep is done following this procedure:
/// 1 -
/// 
/// Drawing from each full contional univariate distribution is done using the information
/// from the built Markov chain and the graph model. In each steep the univariates are 
/// conditioned on the values of the variables in Markov blanket of variable *j* at steep *t* 
/// (for variable *0...j-1*) and *t-1* (for variable *j+1...k).

pub struct Gibbs {
    steeps: usize,
    burnin: usize,
    samples: Vec<Vec<HybridRes>>,
    vars: Vec<VarData>,
    rng: RGSLRng,
}

struct VarData {
    pos: usize,
    blanket: Vec<usize>,
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
            steeps,
            burnin,
            samples: Vec::with_capacity(steeps),
            vars: vec![],
            rng: RGSLRng::new(),
        }
    }
}

impl<'a> Gibbs {
    /// Choose an initial state and a support set for each variable and construct the target 
    /// proposal for each variable in the process.
    fn initialize<M>(&mut self, net: &M)
        where <<<M as IterModel>::Iter as Iterator>::Item as Deref>::Target: HybridNode<'a>,
              <<M as IterModel>::Iter as Iterator>::Item: Deref,
              M: IterModel
    {
        use model::ContNode;
        use model::Node;

        let mut priors = Vec::with_capacity(net.var_num());
        self.vars = Vec::with_capacity(net.var_num());
        for node in net.iter_vars() {
            let var = VarData {
                pos: node.position(),
                blanket: net.var_neighbours(node.position()),
            };
            self.vars.push(var);
            let sample = if !node.was_discrete() {
                HybridRes::Continuous(node.init_sample(&mut self.rng))
            } else {
                HybridRes::Discrete(node.inverse_cdf(node.init_sample(&mut self.rng)))
            };
            priors.push(sample);
        }
        self.samples.push(priors);
    }

    fn var_val<N: HybridNode<'a>>(&mut self, t: usize, var: &N, pos: usize) -> HybridRes
    {
        let var_data = &self.vars[pos];
        let mut mb_values = Vec::with_capacity(var_data.blanket.len());
        if !var_data.blanket.is_empty() {
            for i in &var_data.blanket {
                if i < &pos {
                    mb_values.push(self.samples[t][*i]);
                } else {
                    mb_values.push(self.samples[t - 1][*i]);
                }
            }
            //var.draw_sample(&mut self.rng, &mb_values)
            unimplemented!()
        } else {
            let sample = var.init_sample(&mut self.rng);
            if !var.was_discrete() {
                HybridRes::Continuous(sample)
            } else {
                HybridRes::Discrete(var.inverse_cdf(sample))
            }
        }
    }
}

impl<'a> HybridMargSampler<'a> for Gibbs {
    fn get_samples<M>(mut self, net: &M) -> Vec<Vec<HybridRes>>
        where <<<M as IterModel>::Iter as Iterator>::Item as Deref>::Target: HybridNode<'a>,
              <<M as IterModel>::Iter as Iterator>::Item: Deref,
              M: IterModel
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
