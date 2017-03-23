//! Models can be of three classed:
//!
//! -   Bayesian Belief Networks with encoded causality.
//! -   Full contiditional networks with non-encoded causality (locality
//!   is assumed and sampling from the Markob blanket).
//! -   A sample space with random varibles and unknown non-encoded
//!   causality (with possibley additional latent variables), non-locality
//!   assumed.
//!
//! At the same time they come with three variants:
//!
//! -   Pure discrete random variables models.
//! -   Pure continuous random variables models.
//! -   Hybrid discrete and continuous random variables models.
//!
//! Each of the above follows a distinc strategy for sampling and testing.

mod discrete;
mod continuous;

pub use self::discrete::Gibbs as DiscreteGibbs;
pub use self::continuous::AnalyticNormal;

use model::{DiscreteModel, DiscreteNode};
use model::{ContModel, ContNode};

pub type DefDiscreteSampler = DiscreteGibbs;
pub type DefContSampler<'a> = AnalyticNormal<'a>;

pub trait Sampler
    where Self: Sized + Clone
{
    fn new(steeps: Option<usize>, burnin: Option<usize>) -> Self;
}

pub trait DiscreteSampler: Sampler {
    /// Return a matrix of t x k dimension samples (t = steeps; k = number of vars).
    fn get_samples<'a, N>(self, state_space: &DiscreteModel<'a, N, Self>) -> Vec<Vec<u8>>
        where N: DiscreteNode<'a>;
}

pub trait ContinuousSampler<'a>: Sampler {
    /// Return a matrix of t x k dimension samples (t = steeps; k = number of vars).
    fn get_samples<N>(self, state_space: &ContModel<'a, N, Self>) -> Vec<Vec<f64>>
        where N: ContNode<'a>;
}

//trait MixedSampler {}
