/// Methods for transforming between distribution kinds.

use model::{ContVar, DiscreteVar};
use model::{DType, DefContVar};
use dists::*;

use RGSLRng;
use err::ErrMsg;

pub trait IntoContinuous
    where Self: DiscreteVar + Sized
{
    type Output: ContVar;
    fn into_continuous(self) -> Self::Output;
}

pub trait IntoDiscrete
    where Self: ContVar + Sized
{
    type Output: DiscreteVar;
    fn into_discrete(self) -> Self::Output;
}

/// Takes a random variable with an invertible distribution function F and returns
/// a standard normal distribution.
pub trait Gaussianization
    where Self: ContVar + Sized
{
    fn as_normal(&self, samples: usize) -> Self {
        let (sampler, cdf) = match *self.dist_type() {
            DType::Normal(ref dist) => (dist as &Sample, dist as &CDF),
            DType::Exponential(ref dist) => (dist as &Sample, dist as &CDF),
            DType::Beta(ref dist) => (dist as &Sample, dist as &CDF),
            DType::Gamma(ref dist) => (dist as &Sample, dist as &CDF),
            DType::ChiSquared(ref dist) => (dist as &Sample, dist as &CDF),
            DType::TDist(ref dist) => (dist as &Sample, dist as &CDF),
            DType::FDist(ref dist) => (dist as &Sample, dist as &CDF),
            DType::Cauchy(ref dist) => (dist as &Sample, dist as &CDF),
            DType::LogNormal(ref dist) => (dist as &Sample, dist as &CDF),
            DType::Logistic(ref dist) => (dist as &Sample, dist as &CDF),
            DType::Pareto(ref dist) => (dist as &Sample, dist as &CDF),
            ref d => panic!(ErrMsg::DistNotInvertible.panic_msg_with_arg(d)),
        };

        let mut rng = RGSLRng::new();
        let std = Normal::std();
        let mut normal = Self::new();
        for _ in 0..samples {
            let s = cdf.cdf(sampler.sample(&mut rng));
            let y = Self::float_into_event(std.inverse_cdf(s));
            normal.push_observation(y);
        }
        normal.set_dist(DType::Normal(std)).unwrap();
        normal
    }

    fn into_default(self) -> DefContVar;
}
