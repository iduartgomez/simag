//! Core functionality for the simAG library

// clippy lints config:
#![allow(unknown_lints)]

#[macro_use]
extern crate nom;

mod agent;
mod var_map;
#[cfg(feature = "persistence")]
mod storage;

pub use self::agent::{Agent, Answer, QueryErr};

const TIME_EQ_DIFF: i64 = 1;
const FLOAT_EQ_ULPS: i32 = 2;
