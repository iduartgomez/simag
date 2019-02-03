mod common;
mod logsent;
mod parser;

use chrono::{DateTime, Utc};
use std::collections::VecDeque;

pub(super) use self::common::{
    Assert, ClassDecl, FreeClsMemb, FreeClsOwner, FuncDecl, Grounded, GroundedFunc, GroundedMemb,
    GroundedRef, Predicate, Terminal, Var, VarKind,
};
pub(super) use self::errors::ParseErrF;
pub(super) use self::logsent::{LogSentence, ProofResContext, SentID};
pub(super) use self::parser::{CompOperator, ParseTree};

/// Takes an owned String and returns the corresponding structured representing
/// object program for the logic function. It can parse several statements
/// at the same time, separated by newlines and/or curly braces.
///
/// It includes a a scanner and parser for the synthatical analysis which translate
/// to the **program** in form of a `ParseResult` to be feed to an Agent.
pub(in crate::agent) fn logic_parser(
    source: &str,
    tell: bool,
    thread_num: usize,
) -> Result<VecDeque<parser::ParseTree>, ParseErrF> {
    parser::Parser::parse(source, tell, thread_num)
}

pub type Time = DateTime<Utc>;

mod errors {
    use super::common::TimeFnErr;
    use super::logsent::LogSentErr;
    use super::parser::ParseErrB;
    use crate::agent::bms::BmsError;

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
