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
use std::collections::VecDeque;

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
pub(super) use self::parser::{Operator, ParseTree};
pub(super) use self::skolem::Skolem;
pub(super) use self::terminal::{GrTerminalKind, Terminal};
pub(super) use self::time_semantics::TimeOps;
pub(super) use self::var::{Var, VarKind};

/// Takes an owned String and returns the corresponding structure representing
/// object program for the logic function. It can parse several statements
/// at the same time, separated by newlines and/or curly braces.
///
/// It includes a a scanner and parser for the synthatical analysis which translate
/// to the **program** in form of parse trees to be feed to an Agent.
pub(in crate::agent) fn logic_parser(
    source: &str,
    tell: bool,
    thread_pool: &rayon::ThreadPool,
) -> Result<VecDeque<parser::ParseTree>, ParseErrF> {
    parser::Parser::parse(source, tell, thread_pool)
}

pub type Time = DateTime<Utc>;

#[inline]
fn reserved(s: &[u8]) -> bool {
    match s {
        b"let" | b"exists" | b"fn" | b"time" | b"time_calc" | b"ow" | b"this" | b"none" | b"in"
        | b"and" | b"where" | b"from" | b"to" | b"is" | b"as" | b"at" | b"since" | b"until" => true,
        _ => false,
    }
}

pub(in crate::agent) trait OpArgsOps {
    fn get_op_args(&self) -> Option<&[common::OpArg]>;
}

mod errors {
    use super::logsent::LogSentErr;
    use super::parser::ParseErrB;
    use super::time_semantics::TimeFnErr;
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
        TypeUnsupported,
        FailedConversion(&'static str),
        LogSentErr(LogSentErr),
        TimeFnErr(TimeFnErr),
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
