//! Support mathematical methods library for the `simAG` framework

extern crate float_cmp;
//extern crate ndarray;
extern crate itertools;
extern crate rand;

pub mod model;
mod sampling;
pub mod dists;

const FLOAT_EQ_ULPS: i64 = 2;

/// Probability type
pub type P = f64;

pub use model::DiscreteModel;
