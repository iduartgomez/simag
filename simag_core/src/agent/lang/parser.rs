//! # Grammar for the SIMAG declarative language
//! ```BNF
//! @comments           ::  regex: \/\*(.*)\*\/ (multiline)
//! @eol_comments       ::  /#.*?$/
//!
//! scope = '(' ${var_decl}* ${skol_decl}*
//!             (class_decl | func_decl | scope | multiple)
//!             logic_op (class_decl | func_decl | scope | multiple)
//!         ')' ;
//! multiple = '(' ${(func_decl | class_decl)} (or_op | and_op) + ')' ;
//! var_decl = '(' 'let ' ${term [':' op_arg]','+ ')' ;
//! skol_decl = '(' 'exists ' ${term [':' op_arg]}','+ ')' ;
//! class_decl = term ['(' ${op_arg}','+ ')'] args ;
//! func_decl = 'fn::' term ['(' ${op_arg}','+ ')'] args
//!             | 'fn::' term '(' ${op_arg}','+ ')' ;
//! args = '[' ${ arg }';'+ ']';
//! arg = term [',' uval] ;
//! uval = 'u' comp_op number ;
//! op_arg = (string|term) [comp_op (string|term)] ;
//! icond_op    =   ':=' ;
//! and_op      =   '&&' ;
//! or_op       =   '||' ;
//! logic_op    =    '<=>'
//!             |    '=>'
//!             |    or_op
//!             |    and_op ;
//! comp_op = ('=' | '<' | '>' | '>=' | '<=' ) ;
//! term = regex: \$?[a-zA-Z0-9_]+ ;
//! number = regex: -?[0-9\.]+ ;
//! string = regex: ".*?"|'.*?' ;
//! ```

use nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::{is_alphanumeric, is_digit, is_space},
    error::{ErrorKind, ParseError},
    multi::many1,
};
use rayon;
use rayon::prelude::*;
use std::collections::VecDeque;
use std::fmt;
use std::str;
use std::str::FromStr;

use super::ParseErrF;
use super::{
    cls_decl::ClassDecl,
    common::Assert,
    fn_decl::FuncDecl,
    logsent::{LogSentence, ParseContext, SentKind},
};

type IResult<'a, I, O, E = I> = nom::IResult<I, O, ParseErrB<'a, E>>;

const ICOND_OP: &[u8] = b":=";
const AND_OP: &[u8] = b"&&";
const OR_OP: &[u8] = b"||";
const IFF_OP: &[u8] = b"<=>";
const IMPL_OP: &[u8] = b"=>";

const EMPTY: &[u8] = b" ";

pub(in crate::agent) struct Parser;
impl Parser {
    /// Lexerless (mostly) recursive descent parser. Takes a string and outputs a correct ParseTree.
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
    fn remove_comments<'a>(input: &[u8]) -> IResult<'a, &'a [u8], Vec<u8>> {
        let mut non_comment = Vec::with_capacity(input.len());

        let mut in_comment = false;
        let mut in_comment_block = false;
        let mut finished_comment = false;
        for (pos, c) in input.into_iter().enumerate() {
            match c {
                b'#' => in_comment = true,
                b'\n' if in_comment => {
                    in_comment = false;
                    finished_comment = true;
                }
                b'/' if !in_comment => {
                    if let Some(next) = input.get(pos + 1) {
                        if next == &b'*' {
                            in_comment_block = true;
                        }
                    } else {
                        return Err(nom::Err::Failure(ParseErrB::SyntaxErrorU));
                    }
                }
                b'/' if in_comment_block => {
                    if let Some(next) = input.get(pos - 1) {
                        if next == &b'*' {
                            in_comment_block = false;
                            finished_comment = true;
                        }
                    } else {
                        return Err(nom::Err::Failure(ParseErrB::SyntaxErrorU));
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

    /// Parse different code blocks and output a complete AST
    fn get_blocks<'b>(input: &'b [u8]) -> Result<Vec<ASTNode<'b>>, ParseErrB<'b>> {
        let (_, scopes) = get_blocks(input).map_err(|err| match err {
            nom::Err::Error(err) => err,
            _ => ParseErrB::SyntaxErrorU,
        })?;
        Ok(scopes)
    }
}

#[derive(Debug)]
pub(in crate::agent) enum ParseErrB<'a, I = &'a [u8]> {
    Nom(I, nom::error::ErrorKind),
    SyntaxErrorU,
    //SyntaxError(Box<ParseErrB<'a>>),
    SyntaxErrorPos(&'a [u8]),
    NotScope(&'a [u8]),
    ImbalDelim(&'a [u8]),
    IllegalChain(&'a [u8]),
    NonTerminal(&'a [u8]),
    NonNumber(&'a [u8]),
    UnclosedComment,
    IsNotStr(&'a [u8]),
    IsNotNumber(&'a [u8]),
    IsNotOperator(&'a [u8]),
    Eof,
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
            ParseErrB::SyntaxErrorU => "syntax error".to_string(),
            //ParseErrB::SyntaxError(Box<ParseErrB<'a>>) => {}
            ParseErrB::SyntaxErrorPos(arr) => {
                format!("syntax error at:\n{}", str::from_utf8(arr).unwrap())
            }
            ParseErrB::NotScope(arr) => format!(
                "syntax error, scope is invalid or not found:\n{}",
                str::from_utf8(arr).unwrap()
            ),
            ParseErrB::ImbalDelim(arr) => format!(
                "syntax error, 
                             open delimiters:\n{}",
                str::from_utf8(arr).unwrap()
            ),
            ParseErrB::IllegalChain(arr) => format!(
                "syntax error,
                             incomplete operator chain:\n{}",
                str::from_utf8(arr).unwrap()
            ),
            ParseErrB::NonTerminal(arr) => format!(
                "syntax error,
                             illegal character in terminal position:\n{}",
                str::from_utf8(arr).unwrap()
            ),
            ParseErrB::NonNumber(arr) => format!(
                "syntax error,
                             illegal character found when parsing a number:v{}",
                str::from_utf8(arr).unwrap()
            ),
            ParseErrB::UnclosedComment => "syntax error, open comment delimiter".to_string(),
            _ => todo!(),
        };
        write!(f, "{}", msg)
    }
}

#[derive(Debug)]
pub(in crate::agent) enum ParseTree {
    Assertion(Vec<Assert>),
    IExpr(LogSentence),
    Expr(LogSentence),
    ParseErr(ParseErrF),
}

impl ParseTree {
    fn process_ast(input: &ASTNode, tell: bool) -> Result<ParseTree, ParseErrF> {
        let mut context = ParseContext::new();
        context.in_assertion = true;
        context.is_tell = tell;
        if let Ok(Some(tree)) = input.is_assertion(&mut context) {
            return Ok(tree);
        }
        // it's an expression, make logic sentence from nested expressions
        let mut context = ParseContext::new();
        context.is_tell = tell;
        match LogSentence::try_new(&input, &mut context) {
            Ok(sent) => match context.stype {
                SentKind::IExpr => Ok(ParseTree::IExpr(sent)),
                SentKind::Expr if context.is_tell => {
                    Err(ParseErrF::ExprWithVars(format!("{}", sent)))
                }
                SentKind::Rule | SentKind::Expr => Ok(ParseTree::Expr(sent)),
            },
            Err(err) => Err(ParseErrF::LogSentErr(err)),
        }
    }

    #[allow(dead_code)]
    pub fn is_err(&self) -> bool {
        match *self {
            ParseTree::ParseErr(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub(in crate::agent) enum ASTNode<'a> {
    Assert(AssertBorrowed<'a>),
    Scope(Box<Scope<'a>>),
    Chain(Vec<ASTNode<'a>>),
    None,
}

impl<'a> ASTNode<'a> {
    #[inline]
    fn from_assert(input: AssertBorrowed) -> ASTNode {
        ASTNode::Assert(input)
    }

    pub fn get_op(&self) -> LogicOperator {
        match *self {
            ASTNode::Scope(ref node) => *node.logic_op.as_ref().unwrap(),
            ASTNode::Assert(_) | ASTNode::Chain(_) | ASTNode::None => unreachable!(),
        }
    }

    pub fn would_assert(&self) -> bool {
        match *self {
            ASTNode::Assert(_) => true,
            _ => false,
        }
    }

    fn is_assertion(&self, context: &mut ParseContext) -> Result<Option<ParseTree>, ParseErrF> {
        context.depth += 1;
        let tree_node = match *self {
            ASTNode::Assert(ref decl) => match *decl {
                // perform potential variable substitution
                AssertBorrowed::ClassDecl(ref decl) => {
                    let mut cls = ClassDecl::from(decl, context)?;
                    if context.in_assertion && context.is_tell {
                        cls.var_substitution()?;
                    }
                    Ok(Some(ParseTree::Assertion(vec![Assert::ClassDecl(cls)])))
                }
                AssertBorrowed::FuncDecl(ref decl) => {
                    let mut func = FuncDecl::from(decl, context)?;
                    if context.in_assertion && context.is_tell {
                        func.var_substitution()?;
                    }
                    Ok(Some(ParseTree::Assertion(vec![Assert::FuncDecl(func)])))
                }
            },
            ASTNode::Chain(ref multi_decl) => {
                let mut v0: Vec<Assert> = Vec::with_capacity(multi_decl.len());
                // chek that indeed all elements are indeed assertions
                // avoid creating declarations prematurely
                for decl in multi_decl {
                    let d = decl.is_assertion(context);
                    match d {
                        Err(err) => return Err(err),
                        Ok(Some(ParseTree::Assertion(mut inner))) => {
                            for e in inner.drain(..) {
                                v0.push(e)
                            }
                        }
                        _ => return Ok(None),
                    }
                }
                Ok(Some(ParseTree::Assertion(v0)))
            }
            ASTNode::Scope(ref node) => {
                let a: Result<Option<ParseTree>, ParseErrF> = (**node).is_assertion(context);
                match a {
                    Err(err) => Err(err),
                    Ok(Some(ParseTree::Assertion(assert))) => {
                        Ok(Some(ParseTree::Assertion(assert)))
                    }
                    _ => Ok(None),
                }
            }
            ASTNode::None => Ok(None),
        };
        context.depth -= 1;
        tree_node
    }
}

#[derive(Debug)]
pub(in crate::agent) struct Scope<'a> {
    pub vars: Option<Vec<VarDeclBorrowed<'a>>>,
    pub logic_op: Option<LogicOperator>,
    pub next: ASTNode<'a>,
}

impl<'a> Scope<'a> {
    fn is_assertion(&self, context: &mut ParseContext) -> Result<Option<ParseTree>, ParseErrF> {
        match self.logic_op {
            Some(LogicOperator::And) | None => {}
            _ => return Ok(None),
        }
        if context.depth == 1 && self.vars.is_some() {
            self.vars
                .as_ref()
                .unwrap()
                .iter()
                .map(|x| context.push_var(x))
                .collect::<Result<Vec<_>, _>>()?;
            self.next.is_assertion(context)
        } else {
            self.next.is_assertion(context)
        }
    }
}

#[derive(Debug)]
pub(in crate::agent) enum AssertBorrowed<'a> {
    FuncDecl(FuncDeclBorrowed<'a>),
    ClassDecl(ClassDeclBorrowed<'a>),
}

#[derive(Debug, Clone)]
pub(in crate::agent) enum VarDeclBorrowed<'a> {
    Var(VarBorrowed<'a>),
    Skolem(SkolemBorrowed<'a>),
}

/// Get the different input blocks, each block is it's own issolated sequence of expressions (or program).
fn get_blocks(input: &[u8]) -> IResult<&[u8], Vec<ASTNode>> {
    // find the positions of the closing delimiters and try until it fails
    let mut mcd = VecDeque::new();
    let mut lp = 0;
    let mut rp = 0;
    let mut slp = -1_i64;
    for (i, c) in input.iter().enumerate() {
        if *c == b'(' {
            lp += 1;
            if slp < 0 {
                slp = i as i64;
            }
        } else if *c == b')' {
            rp += 1;
            if rp == lp {
                if i + 1 < input.len() {
                    mcd.push_back((slp as usize, i + 1));
                } else {
                    mcd.push_back((slp as usize, input.len()));
                }
                slp = -1;
            }
        }
    }
    if lp != rp {
        return Err(nom::Err::Error(ParseErrB::ImbalDelim(input)));
    } else if mcd.is_empty() {
        return Err(nom::Err::Error(ParseErrB::NotScope(input)));
    }

    let mut results: Vec<ASTNode> = Vec::new();
    for _ in 0..mcd.len() {
        let (lp, rp) = mcd.pop_front().unwrap();
        match parse_scope(&input[lp..rp]) {
            Ok((_, done)) => results.push(done),
            Err(err) => return Err(err),
        }
    }
    if results.is_empty() {
        Err(nom::Err::Error(ParseErrB::NotScope(input)))
    } else {
        Ok((EMPTY, results))
    }
}

fn parse_scope(input: &[u8]) -> IResult<&[u8], ASTNode> {
    let sentence = alt!(
        input,
        map!(
            do_parse!(
                rm_spaces
                    >> tag!("(")
                    >> vars: opt!(scope_var_decl)
                    >> decl: decl_alt
                    >> op: opt!(map!(logic_operator, LogicOperator::from_bytes))
                    >> next: alt!(assertions | parse_scope)
                    >> tag!(")")
                    >> rm_spaces
                    >> (vars, decl, op, next, true)
            ),
            declaration
        ) | map!(
            do_parse!(
                rm_spaces
                    >> tag!("(")
                    >> vars: opt!(scope_var_decl)
                    >> next: alt!(assertions | parse_scope)
                    >> op: opt!(map!(logic_operator, LogicOperator::from_bytes))
                    >> decl: decl_alt
                    >> tag!(")")
                    >> rm_spaces
                    >> (vars, decl, op, next, false)
            ),
            declaration
        ) | map!(
            do_parse!(
                rm_spaces
                    >> tag!("(")
                    >> vars: opt!(scope_var_decl)
                    >> lhs: alt!(assertions | parse_scope)
                    >> op: map!(logic_operator, LogicOperator::from_bytes)
                    >> rhs: alt!(assertions | parse_scope)
                    >> tag!(")")
                    >> rm_spaces
                    >> (vars, lhs, op, rhs)
            ),
            logic_cond
        ) | map!(
            do_parse!(
                rm_spaces
                    >> tag!("(")
                    >> vars: opt!(scope_var_decl)
                    >> next: opt!(alt!(assertions | parse_scope))
                    >> tag!(")")
                    >> rm_spaces
                    >> (vars, next)
            ),
            empty_scope
        ) | map!(map!(decl_alt, ASTNode::from_assert), |decl| {
            Ok((EMPTY, decl))
        })
    );
    if let Ok(sentence) = sentence {
        Ok(sentence.1?)
    } else {
        do_parse!(input, expr: assertions >> (expr))
    }
}

type ScopeOutA<'a> = (
    Option<DeclVars<'a>>,
    AssertBorrowed<'a>,
    Option<IResult<'a, &'a [u8], LogicOperator>>,
    ASTNode<'a>,
    bool,
);

/// Assertion optionally followed by an other assertion:
///     e.g.1: (let x in abc[x=1])
///     e.g.2: (let y in abc[y=2] && ...)
fn declaration(input: ScopeOutA) -> IResult<&[u8], ASTNode> {
    let (vars, decl, op, next, is_lhs) = input;
    let op = match op {
        Some(Ok((_, val))) => Some(val),
        Some(Err(err)) => return Err(err),
        None => None,
    };
    let assert = ASTNode::Assert(decl);
    let chained = if is_lhs {
        ASTNode::Chain(vec![assert, next])
    } else {
        ASTNode::Chain(vec![next, assert])
    };
    let curr = Scope {
        next: chained,
        vars: flat_vars(vars),
        logic_op: op,
    };
    let curr = ASTNode::Scope(Box::new(curr));
    Ok((EMPTY, curr))
}

type ScopeOutB<'a> = (
    Option<DeclVars<'a>>,
    ASTNode<'a>,
    IResult<'a, &'a [u8], LogicOperator>,
    ASTNode<'a>,
);

/// Logic condition on two expressions, e.g.: (... expr1 || expr2)
fn logic_cond(input: ScopeOutB) -> IResult<&[u8], ASTNode> {
    let (vars, lhs, op, rhs) = input;
    let next = Scope {
        vars: flat_vars(vars),
        logic_op: Some(op?.1),
        next: ASTNode::Chain(vec![lhs, rhs]),
    };
    Ok((EMPTY, ASTNode::Scope(Box::new(next))))
}

/// An scope devoid of any expressions except optionally variable declarations.
/// e.g.: (let x, y (...))
fn empty_scope<'a>(
    input: (Option<DeclVars<'a>>, Option<ASTNode<'a>>),
) -> IResult<'a, &'a [u8], ASTNode<'a>> {
    let (vars, next) = input;
    if let Some(vars) = flat_vars(vars) {
        if let Some(next) = next {
            Ok((
                EMPTY,
                ASTNode::Scope(Box::new(Scope {
                    vars: Some(vars),
                    logic_op: None,
                    next,
                })),
            ))
        } else {
            Ok((EMPTY, ASTNode::None))
        }
    } else if let Some(next) = next {
        Ok((EMPTY, next))
    } else {
        Ok((EMPTY, ASTNode::None))
    }
}

/// Concatenated assertions, e.g.: (let x, y in abc[x=1] && def[x=2] && ...)
fn assertions(input: &[u8]) -> IResult<&[u8], ASTNode> {
    let res = do_parse!(
        input,
        rm_spaces
            >> tag!("(")
            >> rm_spaces
            >> vars: opt!(scope_var_decl)
            >> rm_spaces
            >> decl: many0!(map!(
                do_parse!(
                    decl: decl_alt
                        >> op: map!(logic_operator, LogicOperator::from_bytes)
                        >> (op?.1, decl)
                ),
                assert_one
            ))
            >> rm_spaces
            >> last: map!(decl_alt, ASTNode::from_assert)
            >> rm_spaces
            >> tag!(")")
            >> (vars, decl, last)
    )?;
    assert_many(res.1)
}

type AssertOne<'a> = (LogicOperator, AssertBorrowed<'a>);

#[inline]
fn assert_one(input: AssertOne) -> IResult<&[u8], ASTNode> {
    let (op, assertion) = input;
    Ok((
        EMPTY,
        ASTNode::Scope(Box::new(Scope {
            next: ASTNode::Assert(assertion),
            vars: None,
            logic_op: Some(op),
        })),
    ))
}

type AssertMany<'a> = (
    Option<DeclVars<'a>>,
    Vec<IResult<'a, &'a [u8], ASTNode<'a>>>,
    ASTNode<'a>,
);

#[inline]
fn assert_many(input: AssertMany) -> IResult<&[u8], ASTNode> {
    let (vars, mut decl, last) = input;

    let mut fd = vec![];
    for e in decl.drain(..) {
        match e {
            Ok((_, e)) => fd.push(e),
            err => return err,
        };
    }

    let last = ASTNode::Scope(Box::new(Scope {
        vars: None,
        logic_op: None,
        next: last,
    }));

    if !fd.is_empty() {
        fd.push(last);
        let f = if let Some(vars) = flat_vars(vars) {
            ASTNode::Scope(Box::new(Scope {
                vars: Some(vars),
                logic_op: None,
                next: ASTNode::Chain(fd),
            }))
        } else {
            ASTNode::Chain(fd)
        };
        Ok((EMPTY, f))
    } else {
        let f = if let Some(vars) = flat_vars(vars) {
            ASTNode::Scope(Box::new(Scope {
                vars: Some(vars),
                logic_op: None,
                next: last,
            }))
        } else {
            last
        };
        Ok((EMPTY, f))
    }
}

#[inline]
fn decl_alt(input: &[u8]) -> IResult<&[u8], AssertBorrowed> {
    let (clean, _) = rm_spaces(input)?;
    alt!(
        clean,
        map!(class_decl, ClassDeclBorrowed::convert_to_assert)
            | map!(func_decl, FuncDeclBorrowed::convert_to_assert)
    )
}

type DeclVars<'a> = Vec<Vec<VarDeclBorrowed<'a>>>;

#[inline]
fn scope_var_decl(input: &[u8]) -> IResult<&[u8], DeclVars> {
    let mut var_or_skolem = many1(alt((variable, skolem)));
    var_or_skolem(input)
}

#[inline]
fn flat_vars(input: Option<DeclVars>) -> Option<Vec<VarDeclBorrowed>> {
    if let Some(vars) = input {
        Some(
            vars.into_iter()
                .flat_map(|x| x.into_iter())
                .collect::<Vec<_>>(),
        )
    } else {
        None
    }
}

// skol_decl = '(' 'exists' $(term[':'op_arg]),+ ')' ;
#[derive(Debug, Clone)]
pub(in crate::agent) struct SkolemBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub op_arg: Option<OpArgBorrowed<'a>>,
}

fn skolem(input: &[u8]) -> IResult<&[u8], Vec<VarDeclBorrowed>, &[u8]> {
    do_parse!(
        input,
        tag!("(")
            >> rm_spaces
            >> tag!("exists ")
            >> vars: fold_many1!(
                do_parse!(
                    rm_spaces
                        >> name: terminal
                        >> oa: opt!(do_parse!(tag!(":") >> oa: op_arg >> (oa)))
                        >> rm_spaces
                        >> opt!(tag!(","))
                        >> (name, oa)
                ),
                Vec::new(),
                |mut vec: Vec<_>, (name, oa)| {
                    let v = SkolemBorrowed {
                        name: TerminalBorrowed::from_slice(name),
                        op_arg: oa,
                    };
                    vec.push(VarDeclBorrowed::Skolem(v));
                    vec
                }
            )
            >> rm_spaces
            >> tag!(")")
            >> (vars)
    )
}

// var_decl = '(' 'let' $(term[':'op_arg]),+ ')' ;
#[derive(Debug, PartialEq, Clone)]
pub(in crate::agent) struct VarBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub op_arg: Option<OpArgBorrowed<'a>>,
}

fn variable(input: &[u8]) -> IResult<&[u8], Vec<VarDeclBorrowed>, &[u8]> {
    do_parse!(
        input,
        tag!("(")
            >> rm_spaces
            >> tag!("let ")
            >> vars: fold_many1!(
                do_parse!(
                    rm_spaces
                        >> name: terminal
                        >> oa: opt!(do_parse!(tag!(":") >> oa: op_arg >> (oa)))
                        >> rm_spaces
                        >> opt!(tag!(","))
                        >> (name, oa)
                ),
                Vec::new(),
                |mut vec: Vec<_>, (name, oa)| {
                    let v = VarBorrowed {
                        name: TerminalBorrowed::from_slice(name),
                        op_arg: oa,
                    };
                    vec.push(VarDeclBorrowed::Var(v));
                    vec
                }
            )
            >> rm_spaces
            >> tag!(")")
            >> (vars)
    )
}

// func_decl = 'fn::' term ['(' op_args ')'] args
// 			 | 'fn::' term '(' op_args ')' ;
#[derive(Debug, PartialEq)]
pub(in crate::agent) struct FuncDeclBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub args: Option<Vec<ArgBorrowed<'a>>>,
    pub op_args: Option<Vec<OpArgBorrowed<'a>>>,
    pub variant: FuncVariants,
}

impl<'a> FuncDeclBorrowed<'a> {
    fn convert_to_assert(decl: FuncDeclBorrowed<'a>) -> AssertBorrowed<'a> {
        AssertBorrowed::FuncDecl(decl)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(in crate::agent) enum FuncVariants {
    Relational,
    NonRelational,
}

impl FuncVariants {
    pub fn is_relational(self) -> bool {
        match self {
            FuncVariants::Relational => true,
            _ => false,
        }
    }
}

fn func_decl(input: &[u8]) -> IResult<&[u8], FuncDeclBorrowed> {
    if let Ok(relational) = do_parse!(
        input,
        tag!("fn::")
            >> name: map!(terminal, TerminalBorrowed::from_slice)
            >> op1: opt!(op_args)
            >> a1: args
            >> (FuncDeclBorrowed {
                name,
                args: Some(a1),
                op_args: op1,
                variant: FuncVariants::Relational
            })
    ) {
        Ok(relational)
    } else {
        do_parse!(
            input,
            tag!("fn::")
                >> name: map!(terminal, TerminalBorrowed::from_slice)
                >> op1: op_args
                >> (FuncDeclBorrowed {
                    name,
                    args: None,
                    op_args: Some(op1),
                    variant: FuncVariants::NonRelational
                })
        )
    }
}

// class_decl = term ['(' op_args ')'] args ;
#[derive(Debug, PartialEq)]
pub(in crate::agent) struct ClassDeclBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub args: Vec<ArgBorrowed<'a>>,
    pub op_args: Option<Vec<OpArgBorrowed<'a>>>,
}

impl<'a> ClassDeclBorrowed<'a> {
    fn convert_to_assert(decl: ClassDeclBorrowed<'a>) -> AssertBorrowed<'a> {
        AssertBorrowed::ClassDecl(decl)
    }
}

fn class_decl(input: &[u8]) -> IResult<&[u8], ClassDeclBorrowed> {
    do_parse!(
        input,
        rm_spaces
            >> name: map!(terminal, TerminalBorrowed::from_slice)
            >> rm_spaces
            >> op_args: opt!(op_args)
            >> rm_spaces
            >> a1: args
            >> (ClassDeclBorrowed {
                name,
                op_args,
                args: a1
            })
    )
}

// arg	= term [',' uval] ;
#[derive(Debug, PartialEq)]
pub(in crate::agent) struct ArgBorrowed<'a> {
    pub term: TerminalBorrowed<'a>,
    pub uval: Option<UVal>,
}

fn arg(input: &[u8]) -> IResult<&[u8], ArgBorrowed> {
    do_parse!(
        input,
        rm_spaces
            >> term: map!(terminal, TerminalBorrowed::from_slice)
            >> rm_spaces
            >> u0: opt!(do_parse!(char!(',') >> u1: uval >> (u1)))
            >> ({ ArgBorrowed { term, uval: u0 } })
    )
}

// args	= '[' arg $(arg);+ ']';
fn args(input: &[u8]) -> IResult<&[u8], Vec<ArgBorrowed>> {
    delimited!(
        input,
        char!('['),
        alt!(separated_list1!(char!(';'), arg) | map!(arg, to_arg_vec)),
        char!(']')
    )
}

fn to_arg_vec(arg: ArgBorrowed) -> Vec<ArgBorrowed> {
    vec![arg]
}

// op_arg =	(string|term) [comp_op (string|term)] ;
#[derive(Debug, PartialEq, Clone)]
pub(in crate::agent) struct OpArgBorrowed<'a> {
    pub term: OpArgTermBorrowed<'a>,
    pub comp: Option<(CompOperator, OpArgTermBorrowed<'a>)>,
}

fn op_arg(input: &[u8]) -> IResult<&[u8], OpArgBorrowed> {
    alt!(
        input,
        do_parse!(
            rm_spaces
                >> term: alt!(
                    map!(string, OpArgTermBorrowed::is_string)
                        | map!(terminal, OpArgTermBorrowed::is_terminal)
                )
                >> rm_spaces
                >> c1: opt!(do_parse!(
                    c2: map!(
                        alt!(tag!(">=") | tag!("<=") | tag!("=") | tag!(">") | tag!("<")),
                        CompOperator::from_chars
                    ) >> rm_spaces
                        >> term: alt!(
                            map!(string, OpArgTermBorrowed::is_string)
                                | map!(terminal, OpArgTermBorrowed::is_terminal)
                        )
                        >> (c2, term)
                ))
                >> (OpArgBorrowed { term, comp: c1 })
        ) | do_parse!(
            rm_spaces
                >> tag!("@")
                >> rm_spaces
                >> from: map!(terminal, OpArgTermBorrowed::is_terminal)
                >> rm_spaces
                >> to: opt!(do_parse!(
                    tag!("->") >> term: map!(terminal, OpArgTermBorrowed::is_terminal) >> (term)
                ))
                >> (OpArgBorrowed {
                    term: from,
                    comp: CompOperator::from_time_op(to),
                })
        )
    )
}

// op_args = $(op_arg),* ;
fn op_args(input: &[u8]) -> IResult<&[u8], Vec<OpArgBorrowed>> {
    delimited!(
        input,
        char!('('),
        separated_list0!(char!(','), op_arg),
        char!(')')
    )
}

#[derive(PartialEq, Clone)]
pub(in crate::agent) enum OpArgTermBorrowed<'a> {
    Terminal(&'a [u8]),
    String(&'a [u8]),
}

impl<'a> std::fmt::Debug for OpArgTermBorrowed<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            OpArgTermBorrowed::Terminal(r) => {
                write!(f, "OpArg::Term({})", str::from_utf8(r).unwrap())
            }
            OpArgTermBorrowed::String(r) => write!(f, "OpArg::Str({})", str::from_utf8(r).unwrap()),
        }
    }
}

impl<'a> OpArgTermBorrowed<'a> {
    fn is_string(i: &'a [u8]) -> OpArgTermBorrowed {
        OpArgTermBorrowed::String(i)
    }

    fn is_terminal(i: &'a [u8]) -> OpArgTermBorrowed {
        OpArgTermBorrowed::Terminal(i)
    }
}

// uval = 'u' comp_op number;
#[derive(Debug, PartialEq, Clone, Copy)]
pub(in crate::agent) struct UVal {
    pub op: CompOperator,
    pub val: Number,
}

fn uval(input: &[u8]) -> IResult<&[u8], UVal> {
    do_parse!(
        input,
        rm_spaces
            >> char!('u')
            >> rm_spaces
            >> op: map!(
                alt!(tag!(">=") | tag!("<=") | tag!("=") | tag!(">") | tag!("<")),
                CompOperator::from_chars
            )
            >> rm_spaces
            >> val: number
            >> rm_spaces
            >> (UVal { op, val })
    )
}

// number = -?[0-9\.]+
#[derive(Debug, PartialEq, Clone, Copy)]
pub(in crate::agent) enum Number {
    SignedFloat(f32),
    UnsignedFloat(f32),
    SignedInteger(i32),
    UnsignedInteger(u32),
}

fn number(input: &[u8]) -> IResult<&[u8], Number> {
    let mut float = false;
    let mut idx = 0_usize;
    let rest = if (input[0] == b'-') | (input[0] == b'+') {
        &input[1..]
    } else {
        input
    };
    for (x, c) in rest.iter().enumerate() {
        if is_digit(*c) | (*c == b'.') {
            if *c == b'.' {
                float = true;
            }
            idx = x + 1;
        } else if idx > 0 {
            break;
        } else {
            return Err(nom::Err::Error(ParseErrB::IsNotNumber(input)));
        }
    }
    if float && (input[0] == b'-') {
        Ok((
            &input[idx + 1..],
            Number::SignedFloat(<f32>::from_str(str::from_utf8(&input[0..=idx]).unwrap()).unwrap()),
        ))
    } else if !float && (input[0] == b'-') {
        Ok((
            &input[idx + 1..],
            Number::SignedInteger(
                <i32>::from_str(str::from_utf8(&input[0..=idx]).unwrap()).unwrap(),
            ),
        ))
    } else if float {
        Ok((
            &input[idx..],
            Number::UnsignedFloat(
                <f32>::from_str(str::from_utf8(&input[0..idx]).unwrap()).unwrap(),
            ),
        ))
    } else {
        Ok((
            &input[idx..],
            Number::UnsignedInteger(
                <u32>::from_str(str::from_utf8(&input[0..idx]).unwrap()).unwrap(),
            ),
        ))
    }
}

// string = (\".*?\")|('.*?) ;
fn string(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input[0] == b'\'' {
        delimited!(input, char!('\''), is_not!("'"), char!('\''))
    } else if input[0] == b'"' {
        delimited!(input, char!('"'), is_not!("\""), char!('"'))
    } else {
        Err(nom::Err::Error(ParseErrB::IsNotStr(input)))
    }
}

// terminal = [a-zA-Z0-9_]+ ;
#[derive(PartialEq, Clone)]
pub(in crate::agent) struct TerminalBorrowed<'a>(pub &'a [u8]);

impl<'a> TerminalBorrowed<'a> {
    pub fn from_slice(i: &'a [u8]) -> TerminalBorrowed<'a> {
        TerminalBorrowed(i)
    }
}

impl<'a> std::fmt::Debug for TerminalBorrowed<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Term({})", str::from_utf8(self.0).unwrap())
    }
}

fn terminal(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut idx = 0_usize;
    for (x, c) in input.iter().enumerate() {
        if is_alphanumeric(*c) | (*c == b'_') | ((*c == b'$') & (x == 0)) {
            idx = x + 1;
        } else if idx > 0 {
            break;
        } else {
            return Err(nom::Err::Error(ParseErrB::NonTerminal(input)));
        }
    }
    if input[0] == b'$' && input[1..idx].is_empty() {
        Err(nom::Err::Error(ParseErrB::NonTerminal(input)))
    } else {
        Ok((&input[idx..], &input[0..idx]))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[allow(dead_code)]
pub(in crate::agent) enum CompOperator {
    // equality operators:
    Equal,
    Less,
    More,
    MoreEqual,
    LessEqual,
    // time operators:
    Until,
    FromUntil,
    At,
}

impl CompOperator {
    fn from_chars(c: &[u8]) -> CompOperator {
        if c == b"<" {
            CompOperator::Less
        } else if c == b">" {
            CompOperator::More
        } else if c == b"=" {
            CompOperator::Equal
        } else if c == b"<=" {
            CompOperator::LessEqual
        } else {
            CompOperator::MoreEqual
        }
    }

    fn from_time_op(t: Option<OpArgTermBorrowed>) -> Option<(CompOperator, OpArgTermBorrowed)> {
        if let Some(term) = t {
            Some((CompOperator::FromUntil, term))
        } else {
            Some((CompOperator::At, OpArgTermBorrowed::String(b"")))
        }
    }

    #[inline]
    pub fn is_equal(self) -> bool {
        match self {
            CompOperator::Equal => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_more(self) -> bool {
        match self {
            CompOperator::More => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_less(self) -> bool {
        match self {
            CompOperator::Less => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_more_eq(self) -> bool {
        match self {
            CompOperator::MoreEqual => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_less_eq(self) -> bool {
        match self {
            CompOperator::LessEqual => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_time_assignment(self) -> bool {
        match self {
            CompOperator::Equal
            | CompOperator::Until
            | CompOperator::At
            | CompOperator::FromUntil => true,
            _ => false,
        }
    }

    pub fn generate_uid(self, id: &mut Vec<u8>) {
        match self {
            CompOperator::Equal => id.push(1),
            CompOperator::Less => id.push(2),
            CompOperator::More => id.push(3),
            CompOperator::MoreEqual => id.push(4),
            CompOperator::LessEqual => id.push(5),
            CompOperator::Until => id.push(6),
            CompOperator::At => id.push(7),
            CompOperator::FromUntil => id.push(8),
        }
    }
}

impl std::fmt::Display for CompOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompOperator::Equal => write!(f, "="),
            CompOperator::Less => write!(f, "<"),
            CompOperator::More => write!(f, ">"),
            CompOperator::MoreEqual => write!(f, ">="),
            CompOperator::LessEqual => write!(f, "<="),
            CompOperator::Until => write!(f, "->"),
            CompOperator::At => write!(f, "@"),
            CompOperator::FromUntil => write!(f, "<->"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(in crate::agent) enum LogicOperator {
    Entail,
    And,
    Or,
    Implication,
    Biconditional,
}

impl LogicOperator {
    fn from_bytes(m: &[u8]) -> IResult<&[u8], LogicOperator> {
        match m {
            ICOND_OP => Ok((EMPTY, LogicOperator::Entail)),
            AND_OP => Ok((EMPTY, LogicOperator::And)),
            OR_OP => Ok((EMPTY, LogicOperator::Or)),
            IFF_OP => Ok((EMPTY, LogicOperator::Biconditional)),
            IMPL_OP => Ok((EMPTY, LogicOperator::Implication)),
            _ => Err(nom::Err::Error(ParseErrB::IsNotOperator(m))), // will never happen
        }
    }

    pub fn is_and(self) -> bool {
        match self {
            LogicOperator::And => true,
            _ => false,
        }
    }

    pub fn is_or(self) -> bool {
        match self {
            LogicOperator::Or => true,
            _ => false,
        }
    }
}

fn logic_operator(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (clean, _) = rm_spaces(input)?;
    let mut operator = alt((tag(":="), tag("&&"), tag("||"), tag("=>"), tag("<=>")));
    operator(clean)
}

#[inline]
fn rm_spaces(input: &[u8]) -> IResult<&[u8], &[u8]> {
    match take_while(is_space)(input) {
        Ok((rest, _)) => Ok((rest, EMPTY)),
        Err(err) => Err(err),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use super::{class_decl, func_decl};
    use nom;

    #[test]
    fn remove_comments() -> Result<(), nom::Err<ParseErrB<'static>>> {
        // remove comments:
        let source = b"
            # one line comment
            ( # first scope
                ( # second scope
                    (let x, y)
                    professor[$Lucy, u=1]
                )
            )
            /*
                multi line
                comment
            */
        ";
        let clean = Parser::remove_comments(source)?;

        let expected = "(((letx,y)professor[$Lucy,u=1]))";
        assert_eq!(
            str::from_utf8(&clean.1)
                .unwrap()
                .split_ascii_whitespace()
                .collect::<String>(),
            expected
        );
        Ok(())
    }

    #[test]
    fn parser_ast_output() -> Result<(), nom::Err<ParseErrB<'static>>> {
        // split per scopes and declarations
        let source = b"
            ( american[x,u=1] && ( weapon[y,u=1] && hostile[z,u=1] ) )
        ";
        let (_, clean) = Parser::remove_comments(source)?;
        let scanned = Parser::get_blocks(&clean);
        assert!(scanned.is_ok());

        // let source = b"
        //     ( ( american[x,u=1] && hostile[z,u=1] ) && hostile[z,u=1] )
        // ";
        // let (_, clean) = Parser::remove_comments(source)?;
        // let scanned = Parser::get_blocks(clean);
        // assert!(scanned.is_ok());

        // let source = b"
        //     ( american[x,u=1] && hostile[z,u=1] && ( weapon[y,u=1]) )
        // ";
        // let (_, clean) = Parser::remove_comments(source)?;
        // let scanned = Parser::get_blocks(clean);
        // assert!(scanned.is_err());

        // let source = b"
        //     ( ( american[x,u=1] ) && hostile[z,u=1] && weapon[y,u=1] )
        // ";
        // let (_, clean) = Parser::remove_comments(source)?;
        // let scanned = Parser::get_blocks(clean);
        // assert!(scanned.is_err());

        // let source = b"
        //     ( ( ( american[x,u=1] ) ) && hostile[z,u=1] && ( ( weapon[y,u=1] ) ) )
        // ";
        // let (_, clean) = Parser::remove_comments(source)?;
        // let scanned = Parser::get_blocks(clean);
        // assert!(scanned.is_err());

        // let source = b"
        //     ( american[x,u=1] && ( ( hostile[z,u=1] ) ) && weapon[y,u=1] )
        // ";
        // let (_, clean) = Parser::remove_comments(source)?;
        // let scanned = Parser::get_blocks(clean);
        // assert!(scanned.is_err());

        // let source = b"
        // ((let x y) (american[x,u=1] && hostile[z,u=1]) := criminal[x,u=1])
        // ((let x y) ((american[x,u=1] && hostile[z,u=1]) := criminal[x,u=1]))
        // ((let x y) (american[x,u=1] && hostile[z,u=1]) := criminal[x,u=1])
        // ";
        // let (_, clean) = Parser::remove_comments(source)?;
        // let scanned = Parser::get_blocks(clean);
        // assert!(scanned.is_ok());
        // let out = scanned.unwrap();
        // assert_eq!(out.len(), 3);

        Ok(())
    }

    macro_rules! assert_done_or_err {
        ($i:ident) => {{
            assert!(!$i.is_err());
        }};
    }

    #[test]
    #[allow(clippy::cognitive_complexity)]
    fn parser_predicate() {
        let s1 = b"professor[$Lucy,u=1]";
        let s1_res = class_decl(s1);
        assert_done_or_err!(s1_res);
        let s1_res = s1_res.unwrap().1;
        assert_eq!(s1_res.name, TerminalBorrowed(b"professor"));
        assert_eq!(s1_res.args[0].term, TerminalBorrowed(b"$Lucy"));
        assert!(s1_res.args[0].uval.is_some());

        let s2 = b"missile[$M1,u>-1.5]";
        let s2_res = class_decl(s2);
        assert_done_or_err!(s2_res);
        let s2_res = s2_res.unwrap().1;
        assert_eq!(s2_res.name, TerminalBorrowed(b"missile"));
        assert_eq!(s2_res.args[0].term, TerminalBorrowed(b"$M1"));
        let s2_uval = s2_res.args[0].uval.as_ref().unwrap();
        assert_eq!(s2_uval.op, CompOperator::More);
        assert_eq!(s2_uval.val, Number::SignedFloat(-1.5_f32));

        let s3 = b"dean(t1=\"now\",t2=t1)[$John,u=0]";
        let s3_res = class_decl(s3);
        assert_done_or_err!(s3_res);
        let s3_res = s3_res.unwrap().1;
        assert_eq!(s3_res.name, TerminalBorrowed(b"dean"));
        assert_eq!(s3_res.args[0].term, TerminalBorrowed(b"$John"));
        assert!(s3_res.args[0].uval.is_some());
        assert_eq!(
            s3_res.op_args.as_ref().unwrap(),
            &vec![
                OpArgBorrowed {
                    term: OpArgTermBorrowed::Terminal(b"t1"),
                    comp: Some((CompOperator::Equal, OpArgTermBorrowed::String(b"now"))),
                },
                OpArgBorrowed {
                    term: OpArgTermBorrowed::Terminal(b"t2"),
                    comp: Some((CompOperator::Equal, OpArgTermBorrowed::Terminal(b"t1"))),
                },
            ]
        );

        let s4 = b"animal(t=\"2015.07.05.11.28\")[cow, u=1; brown, u=0.5]";
        let s4_res = class_decl(s4);
        assert_done_or_err!(s4_res);
        let s4_res = s4_res.unwrap().1;
        assert_eq!(s4_res.args[1].term, TerminalBorrowed(b"brown"));
        assert!(s4_res.op_args.is_some());
        assert_eq!(
            s4_res.op_args.as_ref().unwrap(),
            &vec![OpArgBorrowed {
                term: OpArgTermBorrowed::Terminal(b"t"),
                comp: Some((
                    CompOperator::Equal,
                    OpArgTermBorrowed::String(b"2015.07.05.11.28"),
                )),
            }]
        );

        let s5 = b"happy(time=t1, @t1, ow)[x,u>=0.5]";
        let s5_res = class_decl(s5);
        assert_done_or_err!(s5_res);
        let s5_res = s5_res.unwrap().1;
        assert!(s5_res.op_args.is_some());
        assert_eq!(
            &s5_res.op_args.as_ref().unwrap()[0],
            &OpArgBorrowed {
                term: OpArgTermBorrowed::Terminal(b"time"),
                comp: Some((CompOperator::Equal, OpArgTermBorrowed::Terminal(b"t1"),)),
            }
        );
        assert_eq!(
            &s5_res.op_args.as_ref().unwrap()[1],
            &OpArgBorrowed {
                term: OpArgTermBorrowed::Terminal(b"t1"),
                comp: Some((CompOperator::At, OpArgTermBorrowed::String(b""))),
            }
        );

        let s6 = b"happy(time=t1, @t1->t2, ow)[x,u<=0.5]";
        let s6_res = class_decl(s6);
        assert_done_or_err!(s6_res);
        let s6_res = s6_res.unwrap().1;
        assert!(s6_res.op_args.is_some());
        assert_eq!(
            &s6_res.op_args.as_ref().unwrap()[1],
            &OpArgBorrowed {
                term: OpArgTermBorrowed::Terminal(b"t1"),
                comp: Some((CompOperator::FromUntil, OpArgTermBorrowed::Terminal(b"t2"),)),
            }
        );
    }

    #[test]
    fn parser_function() {
        let s1 = b"fn::criticize(t=\"now\")[$John,u=1;$Lucy]";
        let s1_res = func_decl(s1);
        assert_done_or_err!(s1_res);
        assert_eq!(s1_res.unwrap().1.variant, FuncVariants::Relational);

        let s2 = b"fn::takes[$analysis,u>0;$Bill]";
        let s2_res = func_decl(s2);
        assert_done_or_err!(s2_res);
        let s2_res = s2_res.unwrap().1;
        assert_eq!(s2_res.name, TerminalBorrowed(b"takes"));
        assert_eq!(s2_res.variant, FuncVariants::Relational);

        let s3 = b"fn::loves[cow, u=1; bull ]";
        let s3_res = func_decl(s3);
        assert_done_or_err!(s3_res);
        assert_eq!(s3_res.unwrap().1.variant, FuncVariants::Relational);

        let s4 = b"fn::time_calc(t1<t2)";
        let s4_res = func_decl(s4);
        assert_done_or_err!(s4_res);
        assert_eq!(s4_res.unwrap().1.variant, FuncVariants::NonRelational);
    }
}
