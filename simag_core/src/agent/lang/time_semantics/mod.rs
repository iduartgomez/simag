mod time_arg;
mod time_calc;
mod time_ops;

use super::ParseErrF;
pub(super) use errors::TimeFnErr;
pub(super) use time_arg::{TimeArg, TimeFn};
pub(in crate::agent) use time_calc::TimeCalcFn;
pub(in crate::agent) use time_ops::TimeOps;

mod errors {
    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    pub enum TimeFnErr {
        NotAssignment,
        WrongDef,
        WrongFormat(String),
        IsNotVar,
        InsufArgs,
        IllegalSubstitution,
        OperatorNotValid,
    }

    impl Into<ParseErrF> for TimeFnErr {
        fn into(self) -> ParseErrF {
            ParseErrF::TimeFnErr(self)
        }
    }
}
