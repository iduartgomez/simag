mod normal;
mod exponential;
mod categorical;
mod transforms;

use RGSLRng;

pub use self::normal::Normal;
pub use self::exponential::Exponential;
pub use self::categorical::Categorical;
pub use self::categorical::Binomial;

pub use self::transforms::Gaussianization;

trait Invertible {}

pub trait Sample {
    fn sample(&self, rng: &mut RGSLRng) -> f64;
}

pub trait InverseCDF {
    fn inverse_cdf(&self, x: f64) -> f64;
}

pub trait Inverse {
    fn inverse(&self, p: f64) -> f64;
}
