//! Core functionality for the simAG library

// clippy lints config:
#![allow(unknown_lints)]

#[macro_use]
extern crate nom;
extern crate chrono;
extern crate float_cmp;
extern crate num_cpus;
extern crate rayon;

mod agent;
mod static_var_map;

pub use self::agent::{Agent, Answer, QueryErr};

const TIME_EQ_DIFF: i64 = 1;
const FLOAT_EQ_ULPS: i32 = 2;
