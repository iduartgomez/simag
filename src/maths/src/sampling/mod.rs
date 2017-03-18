mod discrete;

pub use self::discrete::Gibbs;

use model::{DiscreteModel, DiscreteNode};

pub type DefSampler = Gibbs;

pub trait Sampler
    where Self: Sized + Clone
{
    fn new(steeps: Option<usize>, burnin: Option<usize>) -> Self;
}

pub trait DiscreteSampler: Sampler {
    /// Return a matrix of t x k dimension samples (t = steeps; k = number of vars).
    fn get_samples<'a, N: 'a>(self, state_space: &DiscreteModel<'a, N, Self>) -> Vec<Vec<u8>>
        where N: DiscreteNode<'a>;
}

//trait ContinuousSampler {}

//trait MixedSampler {}
