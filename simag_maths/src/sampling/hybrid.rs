use std::f64::consts::PI;

use rgsl;
use rgsl::{MatrixF64, VectorF64};

use super::HybridRes;
use dists::{Normal, Normalization, CDF};
use err::ErrMsg;
use model::{ContNode, ContVar, DType, DefContVar, HybridModel, HybridNode, IterModel, Variable};
use RGSLRng;

const ITER_TIMES: usize = 2000;
const _BURN_IN: usize = 500;

/// An efficient exact sampler intended for Bayesian nets (encoded causality through
/// direct acyclic graphs) formed by discreted and/or continuous variables. Requires
/// a rank correlation specification for each arc in the model.
///
/// Returns a matrix of *t* x *k* dimensions of samples (*t* = steeps; *k* = number of variables),
/// samples each marginal distribution each steep.
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
/// the discrete variable node. In the case of the provided rank correlations
/// (not partial correlations!), is assumed that the discrete variables have a monotonic
/// relationship with their parents/childs based on the order of their categories.
#[derive(Debug)]
pub struct ExactNormalized<'a> {
    steeps: usize,
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

impl<'a> ExactNormalized<'a> {
    pub fn new(steeps: Option<usize>) -> ExactNormalized<'a> {
        let steeps = match steeps {
            Some(val) => val,
            None => ITER_TIMES,
        };

        ExactNormalized {
            steeps,
            normalized: vec![],
            samples: Vec::with_capacity(ITER_TIMES),
            rng: RGSLRng::new(),
            cr_matrix: MatrixF64::new(1, 1).unwrap(),
        }
    }

    fn initialize<N>(&mut self, net: &HybridModel<'a, N>) -> Result<(), ()>
    where
        N: HybridNode<'a>,
    {
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
                let rho_xy = 2.0 * (PI_DIV_SIX * pt_cr.ok_or(())?).sin();
                //cached.insert((i, j), rho_xy);
                self.cr_matrix.set(i, j, rho_xy);
            }
            //let anc = node.get_all_ancestors();
            //partial_correlation(i, &anc, &mut cached, &mut self.cr_matrix);
            let d = unsafe { &*(node.get_dist() as *const _) as &'a <N as ContNode>::Var };
            let n = Normalized {
                var: dist,
                original: d.dist_type(),
            };
            self.normalized.push(n);
        }
        Ok(())
    }

    pub fn get_samples<N>(mut self, net: &HybridModel<'a, N>) -> Result<Vec<Vec<HybridRes>>, ()>
    where
        N: HybridNode<'a>,
    {
        use rgsl::blas::level2::dtrmv;

        let std = Normal::std();
        self.initialize(net)?;
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
            dtrmv(
                rgsl::cblas::Uplo::Lower,
                rgsl::cblas::Transpose::NoTrans,
                rgsl::cblas::Diag::NonUnit,
                &self.cr_matrix,
                &mut steep_sample,
            );
            let mut f = Vec::with_capacity(d);
            for i in 0..d {
                let sample = std.cdf(steep_sample.get(i));
                let sample = match *self.normalized[i].original {
                    DType::Normal(ref dist) => HybridRes::Continuous(sample + dist.mu),
                    DType::Exponential(ref dist) => HybridRes::Continuous(dist.inverse_cdf(sample)),
                    DType::Beta(ref dist) => HybridRes::Continuous(dist.inverse_cdf(sample)),
                    DType::Gamma(ref dist) => HybridRes::Continuous(dist.inverse_cdf(sample)),
                    DType::ChiSquared(ref dist) => HybridRes::Continuous(dist.inverse_cdf(sample)),
                    DType::TDist(ref dist) => HybridRes::Continuous(dist.inverse_cdf(sample)),
                    DType::FDist(ref dist) => HybridRes::Continuous(dist.inverse_cdf(sample)),
                    DType::Cauchy(ref dist) => HybridRes::Continuous(dist.inverse_cdf(sample)),
                    DType::LogNormal(ref dist) => HybridRes::Continuous(dist.inverse_cdf(sample)),
                    DType::Logistic(ref dist) => HybridRes::Continuous(dist.inverse_cdf(sample)),
                    DType::Pareto(ref dist) => HybridRes::Continuous(dist.inverse_cdf(sample)),
                    DType::RelaxedBernoulli(ref dist) => {
                        if dist.discretized(dist.inverse_cdf(sample)) {
                            HybridRes::Discrete(1)
                        } else {
                            HybridRes::Discrete(0)
                        }
                    }
                    ref d => panic!(ErrMsg::DiscDistContNode.panic_msg_with_arg(d)),
                };
                f.push(sample)
            }
            self.samples.push(f);
        }
        Ok(self.samples)
    }
}

impl<'a> Default for ExactNormalized<'a> {
    fn default() -> ExactNormalized<'a> {
        Self::new(None)
    }
}

impl<'a> Clone for ExactNormalized<'a> {
    fn clone(&self) -> ExactNormalized<'a> {
        let steeps = Some(self.steeps);
        ExactNormalized::new(steeps)
    }
}

/// Marginal sampler for Constrained Continuous Markov Random Field.
///
/// This algorithm draws a sample from a proposal marginal distribution of a random variable
/// in the model for *t* steeps and converges to the stationary distribution for that variable.
pub struct CCMRFMarginal {
    steeps: usize,
    samples: Vec<HybridRes>,
    rng: RGSLRng,
}

struct VarInfo<'a, T: ContNode<'a>> {
    blanket: Vec<<T as ContNode<'a>>::Var>,
    // aprox: DType,
}

impl<'a> CCMRFMarginal {
    pub fn new(steeps: Option<usize>) -> CCMRFMarginal {
        let steeps = match steeps {
            Some(val) => val,
            None => ITER_TIMES,
        };
        CCMRFMarginal {
            steeps,
            samples: Vec::with_capacity(steeps),
            rng: RGSLRng::new(),
        }
    }

    /// Choose an initial state and a support set for each variable and construct the target
    /// proposal for each variable in the process.
    fn initialize<N>(&mut self, var: &N) -> VarInfo<'a, N>
    where
        N: HybridNode<'a>,
    {
        /*
        let blanket = var.get_markov_blanket();
        // find an aproximate distribution
        //let aprox = ...;
        */
        let info = VarInfo { blanket: vec![] };
        let prior = if !var.was_discrete() {
            HybridRes::Continuous(var.init_sample(&mut self.rng))
        } else {
            HybridRes::Discrete(var.inverse_cdf(var.init_sample(&mut self.rng)))
        };
        self.samples.push(prior);
        info
    }

    fn var_val<N>(&mut self, var: &N, info: &VarInfo<'a, N>) -> HybridRes
    where
        N: HybridNode<'a>,
    {
        if !info.blanket.is_empty() {
            /*
            let sample = info.aprox.draw_sample();
            if !var.was_discrete() {
                HybridRes::Continuous(sample)
            } else {
                HybridRes::Discrete(var.inverse_cdf(sample))
            }
            */
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

    pub fn get_samples<N>(mut self, var: &N) -> Result<Vec<HybridRes>, ()>
    where
        N: HybridNode<'a>,
    {
        let var_info = self.initialize(var);
        for _ in 0..self.steeps {
            let sample = self.var_val(var, &var_info);
            self.samples.push(sample);
        }
        Ok(self.samples)
    }
}

impl Clone for CCMRFMarginal {
    fn clone(&self) -> CCMRFMarginal {
        let steeps = Some(self.steeps);
        CCMRFMarginal::new(steeps)
    }
}

impl Default for CCMRFMarginal {
    fn default() -> CCMRFMarginal {
        Self::new(None)
    }
}
