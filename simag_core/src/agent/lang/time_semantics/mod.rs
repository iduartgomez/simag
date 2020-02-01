mod time_arg;
mod time_ops;

use super::ParseErrF;
pub(super) use errors::TimeFnErr;
pub(super) use time_arg::{TimeArg, TimeFn};
pub(super) use time_ops::TimeOps;

mod errors {
    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    pub enum TimeFnErr {
        MultiAssign,
        NotAssignment,
        WrongFormat(String),
        IsNotVar,
        InsufArgs,
        IllegalSubstitution,
    }

    impl Into<ParseErrF> for TimeFnErr {
        fn into(self) -> ParseErrF {
            ParseErrF::TimeFnErr(self)
        }
    }
}
