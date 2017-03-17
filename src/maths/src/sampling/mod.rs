mod discrete;

pub use self::discrete::Gibbs;

use model::{DiscreteModel, DiscreteNode};

pub type DefSampler = Gibbs;

pub trait DiscreteSampler {
    /// Return a matrix of t x k dimension samples (t = steeps; k = number of vars).
    fn get_samples<'a, N: 'a>(self, state_space: &DiscreteModel<'a, N, Self>) -> Vec<Vec<u8>>
        where Self: Sized + Clone,
              N: DiscreteNode<'a>;
}

pub trait ContinuousSampler {}

trait MixedSampler {}
