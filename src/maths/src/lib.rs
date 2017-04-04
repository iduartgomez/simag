//! Support mathematical methods library for the `simAG` framework

extern crate float_cmp;
extern crate itertools;
extern crate rand;
extern crate uuid;
extern crate rgsl;
//extern crate ndarray;

pub mod model;
pub mod sampling;
pub mod dists;

mod rng;
mod err;

//const FLOAT_EQ_ULPS: i64 = 2;

pub use model::DiscreteModel;
pub use rng::RGSLRng;
