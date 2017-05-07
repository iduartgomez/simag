mod common;
mod parser;
mod logsent;

pub use self::common::*;
pub use self::errors::ParseErrF;
pub use self::parser::{CompOperator, ParseTree};
pub use self::logsent::{LogSentence, SentID, ProofResContext};

use chrono::{DateTime, UTC};

use std::collections::VecDeque;

/// Takes an owned String and returns the corresponding structured representing
/// object program for the logic function. It can parse several statements
/// at the same time, separated by newlines and/or curly braces.
///
/// It includes a a scanner and parser for the synthatical analysis which translate
/// to the **program** in form of a `ParseResult` to be feed to an Agent.
pub fn logic_parser(source: String, tell: bool) -> Result<VecDeque<ParseTree>, ParseErrF> {
    self::parser::Parser::parse(source, tell)
}

pub type Time = DateTime<UTC>;

mod errors {
    use super::parser::ParseErrB;
    use super::common::TimeFnErr;
    use super::logsent::LogSentErr;

    use std::fmt;

    #[derive(Debug, PartialEq)]
    pub enum ParseErrF {
        ReservedKW(String),
        IUVal(f32), // illegal value in a truth value assignment/comparison
        IUValNone, // no truth value found, but was required
        IUValComp, // a comparison operator was used, but it should have been an assignment
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
            let msg = match *self {
                ParseErrF::ReservedKW(ref kw) => format!("use of reserved keyword: {}", kw),
                _ => { unimplemented!() }
            };
            write!(f, "{}", msg)
        }
    }

    impl<'a> From<ParseErrB<'a>> for ParseErrF {
        fn from(err: ParseErrB<'a>) -> ParseErrF {
            ParseErrF::SyntaxErr(format!("{}", err))
        }
    }
}
