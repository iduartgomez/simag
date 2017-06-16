use std::f64::consts::PI;
//use std::collections::HashMap;

use rgsl::{MatrixF64, VectorF64};
use rgsl;

use model::{Variable, ContModel, ContNode, ContVar, DefContVar, DType};
use dists::{Normal, Normalization, CDF};
use err::ErrMsg;
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
#[derive(Debug)]
pub struct ExactNormalized<'a> {
    steeps: usize,
    normalized: Vec<Normalized<'a>>,
    samples: Vec<Vec<f64>>,
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
            steeps: steeps,
            normalized: vec![],
            samples: Vec::with_capacity(ITER_TIMES),
            rng: RGSLRng::new(),
            cr_matrix: MatrixF64::new(1, 1).unwrap(),
        }
    }

    fn initialize<N>(&mut self, net: &ContModel<'a, N>) -> Result<(), ()>
        where N: ContNode<'a>
    {
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
                let rho_xy = 2.0 * (PI_DIV_SIX * pt_cr.ok_or(())?).sin();
                //cached.insert((i, j), rho_xy);
                self.cr_matrix.set(i, j, rho_xy);
            }
            //let anc = node.get_parents_positions();
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

    pub fn get_samples<N>(mut self, net: &ContModel<'a, N>) -> Result<Vec<Vec<f64>>, ()>
        where N: ContNode<'a>
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
            dtrmv(rgsl::cblas::Uplo::Lower,
                  rgsl::cblas::Transpose::NoTrans,
                  rgsl::cblas::Diag::NonUnit,
                  &self.cr_matrix,
                  &mut steep_sample);
            let mut f = Vec::with_capacity(d);
            for i in 0..d {
                let sample = std.cdf(steep_sample.get(i));
                let sample = match *self.normalized[i].original {
                    DType::Normal(ref dist) => sample + dist.mu,
                    DType::Exponential(ref dist) => dist.inverse_cdf(sample),
                    DType::Beta(ref dist) => dist.inverse_cdf(sample),
                    DType::Gamma(ref dist) => dist.inverse_cdf(sample),
                    DType::ChiSquared(ref dist) => dist.inverse_cdf(sample),
                    DType::TDist(ref dist) => dist.inverse_cdf(sample),
                    DType::FDist(ref dist) => dist.inverse_cdf(sample),
                    DType::Cauchy(ref dist) => dist.inverse_cdf(sample),
                    DType::LogNormal(ref dist) => dist.inverse_cdf(sample),
                    DType::Logistic(ref dist) => dist.inverse_cdf(sample),
                    DType::Pareto(ref dist) => dist.inverse_cdf(sample),
                    ref d => panic!(ErrMsg::DiscDistContNode.panic_msg_with_arg(d)),
                };
                f.push(sample)
            }
            self.samples.push(f);
        }
        Ok(self.samples)
    }
}

impl<'a> Clone for ExactNormalized<'a> {
    fn clone(&self) -> ExactNormalized<'a> {
        let steeps = Some(self.steeps);
        ExactNormalized::new(steeps)
    }
}

impl<'a> Default for ExactNormalized<'a> {
    fn default() -> ExactNormalized<'a> {
        Self::new(None)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use RGSLRng;
    use rgsl;

    use std::collections::HashMap;

    fn _pt_cr() {
        use rgsl::linear_algebra::symmtd_decomp;
        use rgsl::blas::level2::dtrmv;
        use rgsl::randist::gaussian::gaussian;

        let mut rng = RGSLRng::new();
        let mut mtx = MatrixF64::new(6, 6).unwrap();
        let mut map = HashMap::new();
        for i in 0..6 {
            for j in 0..6 {
                let sign: f64 = if rng.uniform_pos() < 0.5 { -1.0 } else { 1.0 };
                if i == j {
                    mtx.set(i, j, 1.0);
                } else if map.get(&(j, i)).is_some() {
                    let val = map.get(&(j, i)).unwrap();
                    mtx.set(i, j, *val);
                } else {
                    let x = rng.uniform_pos() * sign;
                    map.insert((i, j), x);
                    mtx.set(i, j, x);
                }
            }
        }
        println!("\nSYNTHETIC CORR MTX:\n{:?}\n", mtx);
        let mut v = VectorF64::new(5).unwrap();
        symmtd_decomp(&mut mtx, &mut v);
        println!("TAU:\n{:?}", v);
        println!("A:\n{:?}\n", mtx);

        let mut samples = VectorF64::new(6).unwrap();
        for i in 0..6 {
            let s = gaussian(rng.get_gen(), 1.0);
            samples.set(i, s);
        }
        println!("SYNTHETIC SAMPLES: {:?}", samples);
        dtrmv(rgsl::cblas::Uplo::Lower,
              rgsl::cblas::Transpose::NoTrans,
              rgsl::cblas::Diag::NonUnit,
              &mtx,
              &mut samples);
        println!("s x A = {:?}\n", samples);
    }
}

