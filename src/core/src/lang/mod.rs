#![allow(dead_code)]

mod logsent;
mod parser;
mod common;

use self::parser::{Parser, ParseTree, ParseErrF};

pub enum ParserState {
    Ask,
    Tell,
}

impl ParserState {
    fn is_tell(&self) -> bool {
        match *self {
            ParserState::Tell => true,
            ParserState::Ask => false,
        }
    }
}

/// Takes an owned String and returns the corresponding structured representing
/// object program for the logic function. It can parse several statements
/// at the same time, separated by newlines and/or curly braces.
///
/// It includes a a scanner and parser for the synthatical analysis which translate
/// to the `program` in form of a ParseResult to be feed to an Agent.
pub fn logic_parser<'a>(source: String, tell: bool) -> Result<Vec<ParseTree>, ParseErrF> {
    let _parser_state = match tell {
        false => ParserState::Ask,
        true => ParserState::Tell,
    };
    Parser::parse(source)
}
