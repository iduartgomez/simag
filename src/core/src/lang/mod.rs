#![allow(dead_code)]
#![allow(unused_variables)]

mod log_sentence;
mod parser;

use self::log_sentence::{Particle, LogSentence};
use self::parser::{Parser};

struct ParseResult<T, P>
    where T: IsTerm,
          P: Particle
{
    assert_memb: Option<Vec<T>>,
    assert_rel: Option<Vec<LogSentence<P>>>,
    assert_rules: Option<Vec<Rule>>,
    assert_cogs: Option<Vec<Cognition>>,
}

impl<T, P> ParseResult<T, P>
    where T: IsTerm,
          P: Particle
{
    fn new() -> ParseResult<T, P> {
        ParseResult {
            assert_memb: None,
            assert_rel: None,
            assert_rules: None,
            assert_cogs: None,
        }
    }
}

enum ParserState {
    Ask,
    Tell,
}

#[derive(Debug)]
pub enum ParseErr<'a> {
    Failure,
    None,
    EmptyScope,
    Comments,
    NotScope(&'a [u8]),
    UnbalancedDelimiter(&'a [u8]),
    IllegalChain(&'a [u8]),
    NonTerminal(&'a [u8]),
    NonNumber(&'a [u8]),
    UnclosedComment(&'a [u8])
}

/// Takes a string and returns the corresponding structured representing
/// object program for the logic function. It can parse several statements
/// at the same time, separated by newlines and/or curly braces. It includes
/// a scanner and parser for the synthatical analysis which translate to the
/// `program` in form of an object.
fn logic_parser<T, U, P>(source: &T, tell: bool) -> Result<ParseResult<U, P>, ParseErr>
    where T: AsBytesSlice, // this should be a T convertible to a &[u8]
          U: IsTerm,
          P: Particle
{
    let parser_state = match tell {
        false => ParserState::Ask,
        true => ParserState::Tell,
    };
    let source: &[u8] = source.as_bytes();
    let results = ParseResult::new();
    let ast = Parser::parse(source);
    Ok(results)
}

trait IsTerm {}

struct Rule {}

struct Cognition {}

struct LogPredicate {}

struct GroundedTerm {}

impl IsTerm for GroundedTerm {}

struct FreeTerm {}

impl IsTerm for FreeTerm {}

pub trait AsBytesSlice {
    fn as_bytes(&self) -> &[u8];
}
