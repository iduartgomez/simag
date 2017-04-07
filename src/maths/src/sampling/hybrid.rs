use std::f64::consts::PI;
use std::collections::{HashMap, VecDeque};

use rgsl::{MatrixF64, VectorF64};
use rgsl;

use super::{Sampler, HybridSampler, HybridSamplerResult};
use model::{Variable, HybridModel, HybridNode, ContVar, DefContVar, DType};
use dists::{Normal, Gaussianization, CDF};
use err::ErrMsg;
use RGSLRng;

const ITER_TIMES: usize = 1000;
const BURN_IN: usize = 0;

#[derive(Debug)]
pub struct ExactNormalized<'a> {
    steeps: usize,
    burnin: usize,
    normalized: Vec<Normalized<'a>>,
    samples: Vec<Vec<HybridSamplerResult>>,
    rng: RGSLRng,
    cr_matrix: MatrixF64,
}

#[derive(Debug)]
struct Normalized<'a> {
    var: DefContVar,
    original: &'a DType,
}

impl<'a> Sampler for ExactNormalized<'a> {
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
    fn initialize<N>(&mut self, net: &HybridModel<'a, N, ExactNormalized<'a>>)
        where N: HybridNode<'a>
    {
        use rgsl::linear_algebra::symmtd_decomp;
        use model::ContNode;

        const PI_DIV_SIX: f64 = PI / 6.0;
        // construct a std normal variables net and the joint partial correlation matrix
        // we do this by:
        // 1) sampling from each random variable in the network (where for
        // variable X and prob func F(x) it has an invertible F^-1(x) function, otherwise panic)
        // and normalizing the sample using the inverse CDF of std normal
        // 2) calculating the partial correlation of each edge conditioned on other edges which
        // are conditioning the child, the order depends on the topological sort algorithm of the DAG
        // 3) construct the correlation matrix with the values obtained, specifying the full
        // correlation matrix for the joint mulvariate std normal distribution
        let d = net.var_num();
        let mut cr_matrix = MatrixF64::new(d, d).unwrap();
        cr_matrix.set_identity();
        let mut cached: HashMap<(usize, usize), f64> = HashMap::new();
        for (x, node) in net.iter_vars().enumerate() {
            let dist = node.get_dist().as_normal(self.steeps).into_default();
            let mut cond = VecDeque::new();
            for (pt_cr, y) in node.get_edges() {
                cond.push_front(y);
                // ρ{i,j}|D = 2 * sin( π/6 * r{i,j}|D )
                let rho_xy = 2.0 * (PI_DIV_SIX * pt_cr).sin();
                let p_cr = partial_correlation((x, y), rho_xy, cond.as_slices().0, &mut cached);
                cr_matrix.set(x, y, p_cr);
                cr_matrix.set(y, x, p_cr);
            }
            let d = unsafe { &*(node.get_dist() as *const _) as &'a <N as ContNode>::Var };
            let n = Normalized {
                var: dist,
                original: d.dist_type(),
            };
            self.normalized.push(n);
        }
        // find matrix A such as { A x A(transposed) = Σ } where { Σ = correlation matrix }
        // we do this using symmetric tridiagonal decomposition to avoid any problem
        // of A not being positive-definite instead of using Cholesky decomposition
        let decomp_vec = VectorF64::new(d - 1).unwrap();
        match symmtd_decomp(&cr_matrix, &decomp_vec) { 
            rgsl::Value::Success => {}
            _ => panic!(ErrMsg::CorrMtxInstance.panic_msg()),
        }
        // BLAS ops only accept float matrixes, so we have to convert from f64 to f32
        //self.cr_matrix = matrixf64_to_matrixf32(cr_matrix);
        self.cr_matrix = cr_matrix;
    }
}

/// Returns rho{x,y|z...n}
fn partial_correlation((x, y): (usize, usize),
                       rho_xy: f64,
                       cond: &[usize],
                       cached: &mut HashMap<(usize, usize), f64>)
                       -> f64 {
    if let Some(rho_xy) = cached.get(&(x, y)) {
        return *rho_xy;
    }
    if cond.len() == 1 {
        cached.insert((x, y), rho_xy);
        return rho_xy;
    }

    let rho_xz = *cached.get(&(x, cond[1])).unwrap();
    let rho_zy = if let Some(rho_zy) = cached.get(&(cond[0], cond[1])) {
        *rho_zy
    } else {
        0.0_f64
    };
    let rho_xyz = (rho_xy - rho_xz * rho_zy) /
                  (((1.0 - rho_xz.powi(2)) * (1.0 - rho_zy.powi(2))).sqrt());
    cached.insert((x, cond[0]), rho_xyz);
    rho_xyz
}

impl<'a> HybridSampler<'a> for ExactNormalized<'a> {
    /// In a pure continuous Bayesian net, the following algorithm is used:
    ///
    /// 1)  Every distribution is transformed to the standard unit normal distribution
    ///     and previously sampled.
    /// 2)  Construct the vine for the standard normal variables.
    /// 3)  Compute the correlation matrix for the full joint distribution (correlation 0
    ///     entails independence)
    /// 4)  Using a matrix obtained through factorization of the correlation matrix,
    ///     compute the sample for each distribution and transform back to the original,
    ///     distribution using the inverse cumulative distribution function.
    /// 5)  Repeat 4 as many times as specified by the `steeps` parameter of the sampler.
    fn get_samples<N>(mut self,
                      net: &HybridModel<'a, N, ExactNormalized<'a>>)
                      -> Vec<Vec<HybridSamplerResult>>
        where N: HybridNode<'a>
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
                    DType::Normal(ref dist) => HybridSamplerResult::Continuous(sample + dist.mu),
                    DType::Exponential(ref dist) => {
                        HybridSamplerResult::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::Beta(ref dist) => {
                        HybridSamplerResult::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::Gamma(ref dist) => {
                        HybridSamplerResult::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::ChiSquared(ref dist) => {
                        HybridSamplerResult::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::TDist(ref dist) => {
                        HybridSamplerResult::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::FDist(ref dist) => {
                        HybridSamplerResult::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::Cauchy(ref dist) => {
                        HybridSamplerResult::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::LogNormal(ref dist) => {
                        HybridSamplerResult::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::Logistic(ref dist) => {
                        HybridSamplerResult::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::Pareto(ref dist) => {
                        HybridSamplerResult::Continuous(dist.inverse_cdf(sample))
                    }
                    DType::RelaxedBernoulli(ref dist) => {
                        let success = if dist.discretized(dist.inverse_cdf(sample)) {
                            1
                        } else {
                            0
                        };
                        HybridSamplerResult::Discrete(success)
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
