//! Support mathematical methods, utilities and tools library for the `simAG` framework

extern crate float_cmp;
extern crate itertools;
extern crate rand;
extern crate rgsl;
extern crate uuid;
//extern crate statrs;
//extern crate ndarray;

pub mod dists;
pub mod model;
pub mod sampling;

mod err;
mod rng;

const FLOAT_EQ_ULPS: i64 = 2;
#[allow(dead_code)]
const FLOAT_EQ_RATIO: f64 = 0.1;

pub use rng::RGSLRng;
