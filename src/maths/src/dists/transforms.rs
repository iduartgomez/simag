/// Methods for transforming between distribution kinds.

use model::{ContVar, DiscreteVar, Variable};
use model::{DType, DefContVar};
use dists::*;

use RGSLRng;
use err::ErrMsg;

/// Whenever a distribution implements this trait it can be relaxed as a Gumbel-Softmax
/// distribution. This trick is useful for treating discrete distributions as a discrete
/// with an underlying continuous distribution.
pub trait GSRelaxation<T>
    where T: GumbelSoftmax
{
    fn relaxed(&self, temperature: Option<f64>) -> T;
}

/// This distribution is a relaxation of a discrete variable throught the Gumbel-Softmax trick.
///
/// A distribution of this kind it's instantiated upon calling the `relaxed` method
/// for a distribution which implements the `GSRelaxation` trait.
pub trait GumbelSoftmax: Clone + ::std::fmt::Debug + Sized {}

pub trait AsContinuous
    where Self: DiscreteVar + Sized
{
    fn as_continuous<C: ContVar>(&self) -> Result<C, ()> {
        let dist = match *self.dist_type() {
            DType::Bernoulli(ref dist) => DType::RelaxedBernoulli(dist.relaxed(None)), 
            _ => return Err(()),
        };
        let mut var = <C as Variable>::new();
        var.set_dist(dist)?;
        Ok(var)
    }
}

/// This trait is not used at the moment.
pub trait AsDiscrete
    where Self: ContVar + Sized
{
    fn as_discrete<C: DiscreteVar>(&self) -> Result<C, ()>;
}

/// Takes a random variable with an invertible distribution function F and returns
/// a standard normal distribution.
pub trait Normalization
    where Self: ContVar + Sized
{
    fn as_normal(&self, samples: usize) -> Self {
        let mut normal = Self::new();
        match *self.dist_type() {
            DType::Normal(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::Exponential(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::Beta(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::Gamma(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::ChiSquared(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::TDist(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::FDist(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::Cauchy(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::LogNormal(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::Logistic(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::Pareto(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            DType::RelaxedBernoulli(ref dist) => {
                let (sampler, cdf) = (dist as &Sample, dist as &CDF);
                sample_cont::<Self>(sampler, cdf, samples, &mut normal);
                normal
            }
            ref d => panic!(ErrMsg::DistNotInvertible.panic_msg_with_arg(d)),
        }
    }

    fn into_default(self) -> DefContVar;
}

#[inline]
fn sample_cont<D: ContVar>(sampler: &Sample, cdf: &CDF, steeps: usize, normal: &mut D) {
    let mut rng = RGSLRng::new();
    let std = Normal::std();
    for _ in 0..steeps {
        let s = cdf.cdf(sampler.sample(&mut rng));
        let y = D::float_into_event(std.inverse_cdf(s));
        normal.push_observation(y);
    }
    normal.set_dist(DType::Normal(std)).unwrap();
}
