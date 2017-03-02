mod common;
mod parser;
mod logsent;

pub use self::common::*;
pub use self::errors::ParseErrF;
pub use self::parser::{CompOperator, ParseTree};
pub use self::logsent::LogSentence;

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

pub type Date = DateTime<UTC>;

mod errors {
    use super::parser::ParseErrB;
    use super::common::TimeFnErr;
    use super::logsent::LogSentErr;

    #[derive(Debug, PartialEq)]
    pub enum ParseErrF {
        Msg(String),
        ReservedKW(String),
        IUVal(f32),
        IUValComp,
        ExprWithVars(String),
        BothAreVars,
        ClassIsVar,
        RFuncWrongArgs,
        WrongArgNumb,
        WrongDef,
        LogSentErr(LogSentErr),
        TimeFnErr(TimeFnErr),
        None,
    }

    impl ParseErrF {
        #[allow(dead_code)]
        fn format(_err: ParseErrB) -> String {
            unimplemented!()
        }
    }

    impl<'a> From<ParseErrB<'a>> for ParseErrF {
        fn from(err: ParseErrB<'a>) -> ParseErrF {
            match err {
                ParseErrB::SyntaxErrorPos(arr) => {
                    use std::str;
                    println!("@err: {:?}", unsafe { str::from_utf8_unchecked(arr) });
                }
                _ => {}
            }
            ParseErrF::Msg(String::from("failed"))
        }
    }
}
