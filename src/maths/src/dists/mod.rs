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

pub use self::categorical::Binomial;
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

pub use self::transforms::Gaussianization;

trait Invertible {}

pub trait Sample {
    fn sample(&self, rng: &mut RGSLRng) -> f64;
}

pub trait CDF {
    fn cdf(&self, x: f64) -> f64;
    fn inverse_cdf(&self, x: f64) -> f64;
}
