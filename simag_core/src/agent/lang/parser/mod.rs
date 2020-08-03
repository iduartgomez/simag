//! # Grammar for the SIMAG declarative language
//! ```BNF
//! @comments           ::  regex: \/\*(.*)\*\/ (multiline)
//! @eol_comments       ::  /#.*?$/
//!
//! scope = '(' ${var_decl}* [and ${skol_decl}*] in
//!             (class_decl | func_decl | scope | multiple)
//!             logic_op (class_decl | func_decl | scope | multiple)
//!         ')' ;
//! multiple = '(' ${(func_decl | class_decl)} (or_op | and_op) + ')' ;
//! var_decl = 'let ' ${term [':' op_arg]','* ;
//! skol_decl = 'exists ' ${term [':' op_arg]}','* ;
//! class_decl = term ['(' ${op_arg}','+ ')'] args ;
//! func_decl = 'fn::' term ['(' ${op_arg}','+ ')'] args
//!             | 'fn::' term '(' ${op_arg}','+ ')' ;
//! args = '[' ${ arg }','+ ']';
//! arg = term [uval] ;
//! uval = comp_op number ;
//! op_arg = (string|term) [comp_op (string|term)] ;
//! icond_op    =   ':=' ;
//! and_op      =   'and' ;
//! or_op       =   'or' ;
//! logic_op    =    'equiv'
//!             |    'implies'
//!             |    or_op
//!             |    and_op ;
//! comp_op = ('=' | '<' | '>' | '>=' | '<=' ) ;
//! term = regex: \$?[a-zA-Z0-9_]+ ;
//! number = regex: -?[0-9\.]+ ;
//! string = regex: ".*?"|'.*?' ;
//! ```
use std::collections::VecDeque;
use std::fmt;
use std::str;

use nom::{
    character::{is_alphabetic, is_alphanumeric},
    error::{ErrorKind, ParseError},
};
use rayon::prelude::*;

pub(self) mod args;
pub(self) mod assertion;
pub(self) mod ast;
pub(self) mod numbers;
pub(self) mod operators;
pub(self) mod scope;
#[cfg(test)]
mod test;

use super::ParseErrF;
pub(super) use args::{ArgBorrowed, OpArgBorrowed, UVal, UnconstraintArg};
pub(super) use assertion::*;
pub(super) use ast::ASTNode;
pub(in crate::agent) use ast::ParseTree;
pub(super) use numbers::Number;
pub(in crate::agent) use operators::{LogicOperator, Operator};
pub(super) use scope::Scope;

type IResult<'a, I, O, E = I> = nom::IResult<I, O, ParseErrB<'a, E>>;

// Symbols
const ICOND_OP: &[u8] = b":=";
const AND_OP: &[u8] = b"and";
const OR_OP: &[u8] = b"or";
const IFF_OP: &[u8] = b"equiv";
const IMPL_OP: &[u8] = b"implies";

const EMPTY: &[u8] = b"";

/// Lexerless (mostly) recursive descent parser. Takes a string and outputs a correct ParseTree.
pub(in crate::agent) struct Parser;

impl Parser {
    /// Takes an str representing the code and returns the corresponding structure
    /// representing object program for the logic function. It can parse several blocks
    /// of code separated by parantheses.
    ///
    /// It includes a a scanner and parser for the synthatical analysis which translate
    /// to the **program** in form of parse trees to be feed to an agent, which will then
    /// translate to the appropiate data structures for further use.
    pub fn parse(
        input: &str,
        tell: bool,
        tpool: &rayon::ThreadPool,
    ) -> Result<VecDeque<ParseTree>, ParseErrF> {
        // store is a vec where the sequence of characters after cleaning up comments
        // will be stored, both have to be extended to 'static lifetime so they can be
        let (_, clean) = Self::remove_comments(input.as_bytes()).unwrap();
        let scopes = match Self::get_blocks(&clean) {
            Ok(scopes) => scopes.into_par_iter(),
            Err(err) => return Err(ParseErrF::from(err)),
        };
        // walk the AST output and, if correct, output a final parse tree
        let parse_trees: Vec<Result<ParseTree, ParseErrF>> = tpool.install(|| {
            scopes
                .map(|ast| ParseTree::process_ast(&ast, tell))
                .collect()
        });
        let mut results = VecDeque::new();
        for res in parse_trees {
            match res {
                Ok(parsed) => results.push_back(parsed),
                Err(err) => results.push_back(ParseTree::ParseErr(err)),
            }
        }
        Ok(results)
    }

    /// Removes comments.
    fn remove_comments(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
        let mut non_comment = Vec::with_capacity(input.len());

        let mut in_comment = false;
        let mut in_comment_block = false;
        let mut finished_comment = false;
        for (pos, c) in input.iter().enumerate() {
            match c {
                b'#' => in_comment = true,
                b'\n' if in_comment => {
                    in_comment = false;
                    finished_comment = true;
                }
                b'/' if !in_comment && !in_comment_block => {
                    if let Some(next) = input.get(pos + 1) {
                        if next == &b'*' {
                            in_comment_block = true;
                        }
                    } else {
                        return Err(nom::Err::Failure(ParseErrB::SyntaxError));
                    }
                }
                b'/' if in_comment_block => {
                    if let Some(next) = input.get(pos - 1) {
                        if next == &b'*' {
                            in_comment_block = false;
                            finished_comment = true;
                        }
                    } else {
                        return Err(nom::Err::Failure(ParseErrB::SyntaxError));
                    }
                }
                _ => {}
            }

            if !in_comment && !in_comment_block && !finished_comment {
                non_comment.push(*c)
            }
            finished_comment = false;
        }

        Ok((EMPTY, non_comment))
    }

    /// Get the different input blocks, each block is it's own issolated sequence of expressions (or program),
    /// and output the AST for it.
    fn get_blocks<'b>(input: &'b [u8]) -> Result<Vec<ASTNode<'b>>, ParseErrB<'b>> {
        let mut results: Vec<ASTNode> = Vec::new();
        let mut rest = input;
        while rest != EMPTY {
            match Scope::parse_scope(rest) {
                Ok((r, done)) => {
                    rest = r;
                    results.push(done)
                }
                _ => return Err(ParseErrB::SyntaxError),
            }
        }

        if results.is_empty() {
            Err(ParseErrB::NotScope(input))
        } else {
            Ok(results)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(in crate::agent) struct TypeDefBorrowed<'a>(pub &'a [u8]);

// string = (\".*?\")|('.*?) ;
fn string(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input[0] == b'\'' {
        delimited!(input, char!('\''), is_not!("'"), char!('\''))
    } else if input[0] == b'"' {
        delimited!(input, char!('"'), is_not!("\""), char!('"'))
    } else {
        Err(nom::Err::Error(ParseErrB::NotStr(input)))
    }
}

// terminal = [a-zA-Z0-9_]+ ;
#[derive(PartialEq, Clone, Copy)]
pub(in crate::agent) struct TerminalBorrowed<'a>(pub &'a [u8]);

impl<'a> From<&'a [u8]> for TerminalBorrowed<'a> {
    fn from(i: &'a [u8]) -> TerminalBorrowed<'a> {
        TerminalBorrowed(i)
    }
}

impl<'a> PartialEq<&[u8]> for TerminalBorrowed<'a> {
    fn eq(&self, other: &&[u8]) -> bool {
        self.0 == *other
    }
}

impl<'a> std::fmt::Debug for TerminalBorrowed<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Term({})", str::from_utf8(self.0).unwrap())
    }
}

fn terminal(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input.is_empty() {
        return Err(nom::Err::Error(ParseErrB::NotTerminal(EMPTY, input)));
    }

    let mut idx = 0_usize;
    for (i, c) in input.iter().enumerate() {
        if (is_alphabetic(*c) & (i == 0))
            | (is_alphanumeric(*c) & (i != 0))
            | (*c == b'_')
            | ((*c == b'$') & (i == 0))
        {
            idx = i + 1;
        } else if idx > 0 {
            break;
        } else {
            return Err(nom::Err::Error(ParseErrB::NotTerminal(
                &input[i..],
                &input[..i],
            )));
        }
    }
    if (input[0] == b'$' && input[1..idx].is_empty()) || super::reserved(&input[..idx]) {
        Err(nom::Err::Error(ParseErrB::NotTerminal(
            &input[idx..],
            &input[..idx],
        )))
    } else {
        Ok((&input[idx..], &input[0..idx]))
    }
}

fn is_keyword(input: &[u8]) -> IResult<&[u8], &[u8]> {
    match terminal(input) {
        Err(nom::Err::Error(ParseErrB::NotTerminal(rest, kw))) => {
            if super::reserved(kw) {
                Ok((rest, kw))
            } else {
                Err(nom::Err::Error(ParseErrB::NotTerminal(rest, kw)))
            }
        }
        Err(err) => Err(err),
        Ok(_) => Err(nom::Err::Error(ParseErrB::NotSpecialFunc(input))),
    }
}

fn is_type(input: &[u8]) -> bool {
    match input {
        b"time" | b"location" => true,
        _ => false,
    }
}

#[derive(Debug)]
pub(in crate::agent) enum ParseErrB<'a, I = &'a [u8]> {
    Nom(I, nom::error::ErrorKind),
    SyntaxError,
    NotScope(&'a [u8]),
    NotTerminal(&'a [u8], &'a [u8]),
    NotStr(&'a [u8]),
    NotNumber(&'a [u8]),
    NotOperator(&'a [u8]),
    NotSpecialFunc(&'a [u8]),
}

impl<'a, I> ParseError<I> for ParseErrB<'a, I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        ParseErrB::Nom(input, kind)
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}

impl<'a> fmt::Display for ParseErrB<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match *self {
            ParseErrB::SyntaxError => "syntax error".to_string(),
            ParseErrB::NotScope(arr) => format!(
                "syntax error, scope is invalid or not found:\n{}",
                str::from_utf8(arr).unwrap()
            ),
            ParseErrB::NotTerminal(_, arr) => format!(
                "syntax error, illegal character in terminal position:\n{}",
                str::from_utf8(arr).unwrap()
            ),
            _ => todo!(),
        };
        write!(f, "{}", msg)
    }
}
