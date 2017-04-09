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

#[allow(dead_code)]
const FLOAT_EQ_ULPS: i64 = 2;
#[allow(dead_code)]
const FLOAT_EQ_RATIO: f64 = 0.1;

pub use rng::RGSLRng;
