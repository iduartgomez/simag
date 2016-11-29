// clippy lints config:
#![deny(float_cmp)]
#![allow(or_fun_call)]
#![allow(unknown_lints)]

#[macro_use]
extern crate nom;
extern crate chrono;
extern crate scoped_threadpool;
extern crate float_cmp;

mod lang;
mod agent;

pub use agent::Agent;
