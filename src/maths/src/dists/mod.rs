mod categorical;

mod beta;
mod cauchy;
mod chisquared;
mod exponential;
mod fdist;
mod gamma;
mod normal;
mod studentt;

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
pub use self::normal::Normal;
pub use self::studentt::StudentT;

pub use self::transforms::Gaussianization;

trait Invertible {}

pub trait Sample {
    fn sample(&self, rng: &mut RGSLRng) -> f64;
}

pub trait InverseCDF {
    fn inverse_cdf(&self, x: f64) -> f64;
}

pub trait InverseDensity {
    fn inverse_density(&self, p: f64) -> f64;
}
