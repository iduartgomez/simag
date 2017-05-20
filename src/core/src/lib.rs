//! Core functionality for the simAG library

// clippy lints config:
#![deny(float_cmp)]
#![allow(or_fun_call)]
#![allow(unknown_lints)]

#[macro_use] extern crate nom;
extern crate chrono;
extern crate rayon;
extern crate float_cmp;

mod lang;
mod agent;
pub mod utils;

pub use agent::{Agent, Answer, QueryErr};

const TIME_EQ_DIFF: i64 = 1;
const FLOAT_EQ_ULPS: i32 = 2;
