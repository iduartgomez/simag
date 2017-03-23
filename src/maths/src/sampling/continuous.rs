use std::f64::consts::PI;
use std::collections::{HashMap, VecDeque};

use ndarray as arr;
use rgsl::{MatrixF64, VectorF64};
use rgsl;

use super::{Sampler, ContinuousSampler};
use model::{Variable, ContModel, ContNode, ContVar, DefContVar, DType};
use dists::{Normal, Inverse, Gaussianization};
use RGSLRng;

const ITER_TIMES: usize = 1000;
const BURN_IN: usize = 0;

#[derive(Debug)]
pub struct AnalyticNormal<'a> {
    steeps: usize,
    burnin: usize,
    normalized: Vec<Normalized<'a>>,
    samples: Vec<Vec<f64>>,
    rng: RGSLRng,
    cr_matrix: MatrixF64,
}

#[derive(Debug)]
struct Normalized<'a> {
    var: DefContVar,
    original: &'a DType,
}

impl<'a> Sampler for AnalyticNormal<'a> {
    fn new(steeps: Option<usize>, burnin: Option<usize>) -> AnalyticNormal<'a> {
        let steeps = match steeps {
            Some(val) => val,
            None => ITER_TIMES,
        };

        let burnin = match burnin {
            Some(val) => val,
            None => BURN_IN,
        };
        AnalyticNormal {
            steeps: steeps,
            burnin: burnin,
            normalized: vec![],
            samples: Vec::with_capacity(ITER_TIMES),
            rng: RGSLRng::new(),
            cr_matrix: MatrixF64::new(1, 1).unwrap(),
        }
    }
}

impl<'a> AnalyticNormal<'a> {
    /// In a pure continuous random variable Bayesian net, the following algorithm is used:
    ///
    /// 1)  Every variable is transformed to the standard unit normal distribution.
    /// 2)  Construct the vine for the standard normal variables.
    /// 3)
    fn initialize<N>(&mut self, net: &ContModel<'a, N, AnalyticNormal<'a>>)
        where N: ContNode<'a>
    {
        use rgsl::linear_algebra::symmtd_decomp;

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
        self.cr_matrix = MatrixF64::new(d, d).unwrap();
        self.cr_matrix.set_identity();
        let mut cached: HashMap<(usize, usize), f64> = HashMap::new();
        for (x, node) in net.iter_vars().enumerate() {
            let dist = node.get_dist().as_normal(self.steeps).into_default();
            let mut cond = VecDeque::new();
            for (pt_cr, y) in node.get_edges() {
                cond.push_front(y);
                // ρ{i,j}|D = 2 * sin( π/6 * r{i,j}|D )
                let rho_xy = 2.0 * (PI_DIV_SIX * pt_cr).sin();
                let p_cr = partial_correlation((x, y), rho_xy, cond.as_slices().0, &mut cached);
                self.cr_matrix.set(x, y, p_cr);
                self.cr_matrix.set(y, x, p_cr);
            }
            let n = Normalized {
                var: dist,
                original: node.get_dist().dist_type(),
            };
            self.normalized.push(n);
        }
        // find matrix A such as { A x A(transposed) = Σ } where { Σ = correlation matrix }
        // we do this using symmetric tridiagonal decomposition to avoid any problem
        // of A not being positive-definite instead of using Cholesky decomposition
        let decomp_vec = VectorF64::new(d).unwrap();
        match symmtd_decomp(&self.cr_matrix, &decomp_vec) { 
            rgsl::Value::Success => {}
            _ => panic!("simag: failed to decompose correlation matrix while initialising sampler"), 
        }
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

impl<'a> ContinuousSampler<'a> for AnalyticNormal<'a> {
    fn get_samples<N>(mut self, net: &ContModel<'a, N, AnalyticNormal<'a>>) -> Vec<Vec<f64>>
        where N: ContNode<'a>
    {
        let std = Normal::std();
        let k = net.var_num();
        self.initialize(net);
        let d = self.normalized.len();
        for t in 0..self.steeps {
            let mut steep_sample = Vec::with_capacity(d);
            for normal in &self.normalized {
                // get each normalized sample obtained during initialization
                // for var i at steep t, then transform back to the original distribution
                // using the inverse dist function F^-1
                let sample = std.cdf(normal.var.get_obs_unchecked(t));
                let sample = match *normal.original {
                    DType::Exponential(ref d) => d.inverse(sample),
                    _ => panic!()
                };
                steep_sample.push(sample);
            }
            self.samples.push(steep_sample);
        }
        self.samples
    }
}

impl<'a> ::std::clone::Clone for AnalyticNormal<'a> {
    fn clone(&self) -> AnalyticNormal<'a> {
        let steeps = Some(self.steeps);
        let burnin = Some(self.burnin);
        AnalyticNormal::new(steeps, burnin)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn pt_cr() {
        /*
        let pt_cr_matrix: arr::Array<f64, arr::Dim<[usize; 2]>> =
            arr::ArrayBase::from_shape_fn((d, d), |dims: (usize, usize)| if &dims.0 == &dims.1 {
                1.0
            } else {
                0.0
            });
        */


    }
}
