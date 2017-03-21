/// Methods for transforming between distribution kinds.

use model::{ContVar, DiscreteVar, Variable};
use model::DType;
use dists::*;
use RGSLRng;

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
    type Output: ContVar;
    fn into_normal(self) -> Self::Output {
        let dist = match *self.dist_type() {
            DType::Normal(ref dist) => dist as &Sample,
            DType::Exponential(ref dist) => dist as &Sample,
            _ => panic!("simag: the distribution does not actually have an invertible function"),
        };

        let mut rng = RGSLRng::new();
        let std = Normal::std();
        let mut normal = Self::Output::new();
        for _ in 0..1000 {
            let s = dist.sample(&mut rng);
            let y = Self::Output::float_into_event(std.inverse_cdf(s));
            normal.push_observation(y);
        }
        normal.set_dist(DType::Normal(std));
        normal
    }
}
