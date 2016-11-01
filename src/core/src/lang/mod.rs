#![allow(dead_code)]
#![allow(unused_variables)]

mod log_sentence;
mod parser;

use self::parser::{Parser, ParseTree, FinalError, IsTerm};
use self::log_sentence::Particle;

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
fn logic_parser<'a, T, U, P>(source: String, tell: bool) -> Result<ParseTree<U, P>, FinalError>
    where U: IsTerm,
          P: Particle
{
    let parser_state = match tell {
        false => ParserState::Ask,
        true => ParserState::Tell,
    };
    Parser::parse(source, &parser_state)
}
