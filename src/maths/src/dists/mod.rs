mod categorical;

mod beta;
mod cauchy;
mod chisquared;
mod exponential;
mod fdist;
mod gamma;
mod logistic;
mod lognormal;
mod normal;
mod pareto;
mod tdist;

mod transforms;

use RGSLRng;

pub use self::categorical::{Bernoulli, RelaxedBernoulli};
pub use self::categorical::Categorical;

pub use self::beta::Beta;
pub use self::cauchy::Cauchy;
pub use self::chisquared::ChiSquared;
pub use self::exponential::Exponential;
pub use self::fdist::FDist;
pub use self::gamma::Gamma;
pub use self::logistic::Logistic;
pub use self::lognormal::LogNormal;
pub use self::normal::Normal;
pub use self::pareto::Pareto;
pub use self::tdist::TDist;

pub use self::transforms::{AsContinuous, Gaussianization, GumbelSoftmax, GSRelaxation};

/// Draw a random variate from a (continuous) distribution.
pub trait Sample {
    fn sample(&self, rng: &mut RGSLRng) -> f64;
}

/// Compute the cumulative density function and its inverse for a given distribution.
pub trait CDF {
    fn cdf(&self, x: f64) -> f64;
    fn inverse_cdf(&self, x: f64) -> f64;
}
