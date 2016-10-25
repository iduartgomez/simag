#![allow(dead_code)]
#![allow(unused_variables)]

mod log_sentence;
mod parser;

use self::log_sentence::{Particle, LogSentence};

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

enum ParseErr {
    Fail,
    None,
}

/// Takes a string and returns the corresponding structured representing
/// object program for the logic function. It can parse several statements
/// at the same time, separated by newlines and/or curly braces. It includes
/// a scanner and parser for the synthatical analysis which translate to the
/// `program` in form of an object.
fn logic_parser<T, U, P>(source: T, tell: bool) -> Result<ParseResult<U, P>, ParseErr>
    where T: Into<String>,
          U: IsTerm,
          P: Particle
{
    let parser_state = match tell {
        false => ParserState::Ask,
        true => ParserState::Tell,
    };
    let source: String = <T>::into(source);
    let mut results = ParseResult::new();
    let ast = AST::parse(&mut results, source);
    Ok(results)
}

enum AST {
    Expr,
    Stmt,
    Rule,
    Assertion,
    Query,
}

impl AST {
    fn parse<T: IsTerm, P: Particle>(_: &mut ParseResult<T, P>,
                                     _: String)
                                     -> Result<AST, ParseErr> {
        // parse and walk the AST:
        let ast = AST::Expr;
        Ok(ast)
    }
}

trait IsTerm {}

struct Rule {}

struct Cognition {}

struct LogPredicate {}

struct GroundedTerm {}

impl IsTerm for GroundedTerm {}

struct FreeTerm {}

impl IsTerm for FreeTerm {}
