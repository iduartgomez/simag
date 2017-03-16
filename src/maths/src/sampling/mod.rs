mod discrete;

pub use self::discrete::Gibbs;

use model::{DiscreteModel, DiscreteVar, Observation};

pub type DefaultSampler<D, O> = Gibbs<D, O>;

pub trait DiscreteSampler<D, O>
    where D: DiscreteVar<O>,
          O: Observation
{
    /// Return a matrix of t x k dimension samples (t = steeps; k = number of vars).
    fn get_samples(self, state_space: &DiscreteModel<D, O, Self>) -> Vec<Vec<u8>>
        where Self: Sized + Clone;
}

pub trait ContinuousSampler<D, O>
    where D: DiscreteVar<O>,
          O: Observation
{
}

trait MixedSampler<D, O>
    where D: DiscreteVar<O>,
          O: Observation
{
}
