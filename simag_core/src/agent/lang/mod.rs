mod built_ins;
mod cls_decl;
mod common;
mod fn_decl;
mod gr_func;
mod gr_memb;
mod logsent;
mod parser;
mod skolem;
pub(self) mod space_semantics;
mod terminal;
pub(self) mod time_semantics;
pub(self) mod typedef;
mod var;

use chrono::{DateTime, Utc};

pub(self) use self::built_ins::BuiltIns;
pub(super) use self::cls_decl::ClassDecl;
pub(super) use self::common::{
    Assert, FreeClassMembership, FreeClsMemb, Grounded, GroundedRef, Predicate,
};
pub(super) use self::errors::ParseErrF;
pub(super) use self::fn_decl::FuncDecl;
pub(super) use self::gr_func::GroundedFunc;
pub(super) use self::gr_memb::GroundedMemb;
pub(super) use self::logsent::{LogSentence, ProofResContext, SentID, SentVarReq};
pub(super) use self::parser::Parser;
pub(super) use self::parser::{Operator, ParseTree};
pub(super) use self::skolem::Skolem;
pub(super) use self::terminal::{GrTerminalKind, Terminal};
pub(super) use self::time_semantics::TimeOps;
pub(super) use self::var::{Var, VarKind};
use built_ins::{MOVE_FN, TIME_CALC_FN};

pub type Time = DateTime<Utc>;

#[inline]
fn reserved(s: &[u8]) -> bool {
    match s {
        b"let" | b"exists" | b"fn" | b"ow" | b"this" | b"none" | b"in" | b"and" | b"where" | b"is" | b"as"  
        // time kw
        | TIME_CALC_FN | b"time" | b"since" | b"until" | b"at"
        // space kw
        | MOVE_FN | b"space" | b"to" | b"from" | b"loc" => true,
        _ => false,
    }
}

pub(in crate::agent) trait OpArgsOps {
    fn get_op_args(&self) -> Option<&[common::OpArg]>;
}

mod errors {
    use super::logsent::LogSentErr;
    use super::parser::ParseErrB;
    use super::{space_semantics::SpaceFnErr, time_semantics::TimeFnErr};
    use crate::agent::kb::bms::BmsError;

    use std::fmt;

    #[derive(Debug, PartialEq)]
    pub enum ParseErrF {
        ReservedKW(String),
        IUVal(f32), // illegal value in a truth value assignment/comparison
        IUValNone,  // no truth value found, but was required
        IUValComp,  // a comparison operator was used, but it should have been an assignment
        ExprWithVars(String),
        BothAreVars,
        ClassIsVar,
        RFuncWrongArgs,
        WrongArgNumb,
        WrongDef,
        NotBuiltin,
        TypeUnsupported,
        FailedConversion(&'static str),
        LogSentErr(LogSentErr),
        TimeFnErr(TimeFnErr),
        SpaceFnErr(SpaceFnErr),
        SyntaxErr(String),
    }

    impl fmt::Display for ParseErrF {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let msg;
            let t = match *self {
                ParseErrF::ReservedKW(ref kw) => {
                    msg = format!("use of reserved keyword: {}", kw);
                    msg.as_str()
                }
                ParseErrF::SyntaxErr(ref msg) => msg.as_str(),
                _ => "parse error",
            };
            write!(f, "simag parser: {}", t)
        }
    }

    impl<'a> From<ParseErrB<'a>> for ParseErrF {
        fn from(err: ParseErrB<'a>) -> ParseErrF {
            ParseErrF::SyntaxErr(format!("{}", err))
        }
    }

    impl From<BmsError> for ParseErrF {
        fn from(_err: BmsError) -> ParseErrF {
            ParseErrF::TimeFnErr(TimeFnErr::IllegalSubstitution)
        }
    }
}
