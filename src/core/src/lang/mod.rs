#![allow(dead_code)]

mod logsent;
mod parser;
mod common;

use std::collections::VecDeque;

pub use self::parser::{ParseErrF, ParseTree};
pub use self::logsent::LogSentence;
pub use self::common::*;

/// Takes an owned String and returns the corresponding structured representing
/// object program for the logic function. It can parse several statements
/// at the same time, separated by newlines and/or curly braces.
///
/// It includes a a scanner and parser for the synthatical analysis which translate
/// to the `program` in form of a ParseResult to be feed to an Agent.
pub fn logic_parser<'a>(source: String) -> Result<VecDeque<ParseTree>, ParseErrF> {
    self::parser::Parser::parse(source)
}
