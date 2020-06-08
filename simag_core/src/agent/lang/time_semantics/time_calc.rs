use crate::agent::lang::Var;

/// Special built-in function for time calculus.
#[derive(Debug, Clone)]
pub(in crate::agent) struct TimeCalc {}

impl TimeCalc {
    pub fn contains_var(&self, var: &Var) -> bool {
        todo!()
    }
}
