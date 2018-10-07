//! Core functionality for the simAG library

// clippy lints config:
#![allow(unknown_lints)]
#![deny(float_cmp)]
#![allow(or_fun_call)]

#[macro_use]
extern crate nom;
extern crate chrono;
extern crate float_cmp;
extern crate rayon;

pub(crate) mod agent;
pub(crate) mod lang;
pub mod utils;

pub use agent::{Agent, Answer, QueryErr};

const TIME_EQ_DIFF: i64 = 1;
const FLOAT_EQ_ULPS: i32 = 2;
