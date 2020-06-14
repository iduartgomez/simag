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
    bytes::complete::tag,
    character::{complete::multispace0, is_alphabetic, is_alphanumeric, is_digit},
    combinator::{map, opt},
    error::{ErrorKind, ParseError},
    sequence::tuple,
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

// Symbols
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
    fn remove_comments(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
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
            return Err(ParseErrB::ImbalDelim(input));
        } else if mcd.is_empty() {
            return Err(ParseErrB::NotScope(input));
        }

        let mut results: Vec<ASTNode> = Vec::new();
        for _ in 0..mcd.len() {
            let (lp, rp) = mcd.pop_front().unwrap();
            match parse_scope(&input[lp..rp]) {
                Ok((_, done)) => results.push(done),
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

#[derive(Debug)]
pub(in crate::agent) enum ParseErrB<'a, I = &'a [u8]> {
    Nom(I, nom::error::ErrorKind),
    SyntaxError,
    NotScope(&'a [u8]),
    ImbalDelim(&'a [u8]),
    NonTerminal(&'a [u8]),
    IsNotStr(&'a [u8]),
    IsNotNumber(&'a [u8]),
    IsNotOperator(&'a [u8]),
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
            ParseErrB::ImbalDelim(arr) => format!(
                "syntax error, 
                             open delimiters:\n{}",
                str::from_utf8(arr).unwrap()
            ),
            ParseErrB::NonTerminal(arr) => format!(
                "syntax error,
                             illegal character in terminal position:\n{}",
                str::from_utf8(arr).unwrap()
            ),
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

fn parse_scope(input: &[u8]) -> IResult<&[u8], ASTNode> {
    if let Ok((rest, sentence)) = parse_sentence(input) {
        return Ok((rest, sentence));
    } else {
        multi_asserts(input)
    }
}

fn parse_sentence(input: &[u8]) -> IResult<&[u8], ASTNode> {
    let (i, _) = multispace0(input)?;
    let (i, _) = tag("(")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, vars) = opt(scope_var_decl)(i)?;
    let (i, _) = multispace0(i)?;

    let node;
    let next_input;

    if let Ok((rest, (decl, op, next))) = lhs(i) {
        // is lhs
        if let Ok((_, decl)) = declaration(vars, decl, op, next, true) {
            node = decl;
            next_input = rest;
        } else {
            panic!()
        }
    } else if let Ok((rest, (decl, op, next))) = rhs(i) {
        // is rhs
        match declaration(vars, decl, op, next, false) {
            Ok((_, decl)) => {
                node = decl;
                next_input = rest;
            }
            Err(err) => return Err(err),
        }
    } else if let Ok((rest, (lhs, op, rhs))) = joint_scopes(i) {
        // joint scopes
        match logic_cond(vars, lhs, op, rhs) {
            Ok((_, decl)) => {
                node = decl;
                next_input = rest;
            }
            Err(err) => return Err(err),
        }
    } else if let Ok((rest, next)) = empty_scope(vars, i) {
        node = next;
        next_input = rest;
    } else {
        return Err(nom::Err::Failure(ParseErrB::SyntaxError));
    }

    // eprintln!("input: {}", str::from_utf8(input).unwrap());
    let (i, _) = multispace0(next_input)?;
    let (i, _) = tag(")")(i)?;
    let (rest, _) = multispace0(i)?;
    Ok((rest, node))
}

#[inline]
fn lhs(i: &[u8]) -> IResult<&[u8], (AssertBorrowed, Option<LogicOperator>, ASTNode)> {
    let (i, decl) = decl_knowledge(i)?;
    let (i, _) = multispace0(i)?;
    let (i, op) = opt(LogicOperator::from_bytes)(i)?;
    let (i, _) = multispace0(i)?;
    let (rest, next) = alt((multi_asserts, parse_scope))(i)?;
    Ok((rest, (decl, op, next)))
}

#[inline]
fn rhs(i: &[u8]) -> IResult<&[u8], (AssertBorrowed, Option<LogicOperator>, ASTNode)> {
    let (i, next) = alt((multi_asserts, parse_scope))(i)?;
    let (i, _) = multispace0(i)?;
    let (i, op) = opt(LogicOperator::from_bytes)(i)?;
    let (i, _) = multispace0(i)?;
    let (rest, decl) = decl_knowledge(i)?;
    Ok((rest, (decl, op, next)))
}

#[inline]
fn joint_scopes(i: &[u8]) -> IResult<&[u8], (ASTNode, LogicOperator, ASTNode)> {
    let (i, lhs) = alt((multi_asserts, parse_scope))(i)?;
    let (i, _) = multispace0(i)?;
    let (i, op) = LogicOperator::from_bytes(i)?;
    let (i, _) = multispace0(i)?;
    let (rest, rhs) = alt((multi_asserts, parse_scope))(i)?;
    Ok((rest, (lhs, op, rhs)))
}

/// An scope devoid of any expressions except optionally variable declarations.
/// e.g.: (let x, y (...))
#[inline]
fn empty_scope<'a>(
    vars: Option<DeclVars<'a>>,
    input: &'a [u8],
) -> IResult<'a, &'a [u8], ASTNode<'a>> {
    let (rest, next) = opt(alt((multi_asserts, parse_scope)))(input)?;
    if let Some(vars) = vars {
        if let Some(next) = next {
            Ok((
                rest,
                ASTNode::Scope(Box::new(Scope {
                    vars: Some(vars),
                    logic_op: None,
                    next,
                })),
            ))
        } else {
            Ok((rest, ASTNode::None))
        }
    } else if let Some(next) = next {
        Ok((rest, next))
    } else {
        Ok((rest, ASTNode::None))
    }
}

/// Assertion optionally followed by an other assertion:
///     e.g.1: (let x in abc[x=1])
///     e.g.2: (let y in abc[y=2] && ...)
fn declaration<'a>(
    vars: Option<DeclVars<'a>>,
    decl: AssertBorrowed<'a>,
    op: Option<LogicOperator>,
    next: ASTNode<'a>,
    is_lhs: bool,
) -> IResult<'a, &'static [u8], ASTNode<'a>> {
    // let (vars, decl, op, next, is_lhs) = input;
    let assert = ASTNode::Assert(decl);
    let chained = if is_lhs {
        ASTNode::Chain(vec![assert, next])
    } else {
        ASTNode::Chain(vec![next, assert])
    };
    let curr = Scope {
        next: chained,
        vars,
        logic_op: op,
    };
    let curr = ASTNode::Scope(Box::new(curr));
    Ok((EMPTY, curr))
}

/// Logic condition on two expressions, e.g.: (... expr1 || expr2)
fn logic_cond<'a>(
    vars: Option<DeclVars<'a>>,
    lhs: ASTNode<'a>,
    op: LogicOperator,
    rhs: ASTNode<'a>,
) -> IResult<'a, &'static [u8], ASTNode<'a>> {
    // let (vars, lhs, op, rhs) = input;
    let next = Scope {
        vars,
        logic_op: Some(op),
        next: ASTNode::Chain(vec![lhs, rhs]),
    };
    Ok((EMPTY, ASTNode::Scope(Box::new(next))))
}

/// One or multiple concatenated assertions, e.g.: (let x, y in abc[x=1] && def[x=2] && ...)
fn multi_asserts(input: &[u8]) -> IResult<&[u8], ASTNode> {
    let (rest, asserts) = do_parse!(
        input,
        multispace0
            >> tag!("(")
            >> multispace0
            >> vars: opt!(scope_var_decl)
            >> multispace0
            >> decl: many0!(map!(
                do_parse!(
                    multispace0
                        >> decl: decl_knowledge
                        >> multispace0
                        >> op: map!(logic_operator, LogicOperator::from_bytes)
                        >> (op?.1, decl)
                ),
                assert_one
            ))
            >> multispace0
            >> last: map!(decl_knowledge, ASTNode::from_assert)
            >> multispace0
            >> tag!(")")
            >> multispace0
            >> (vars, decl, last)
    )?;
    let (_, asserts) = assert_many(asserts)?;
    Ok((rest, asserts))
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
        let f = if let Some(vars) = vars {
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
        let f = if let Some(vars) = vars {
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
fn decl_knowledge(input: &[u8]) -> IResult<&[u8], AssertBorrowed> {
    alt!(
        input,
        map!(class_decl, ClassDeclBorrowed::convert_to_assert)
            | map!(func_decl, FuncDeclBorrowed::convert_to_assert)
    )
}

type DeclVars<'a> = Vec<VarDeclBorrowed<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub(in crate::agent) struct TypeDefBorrowed<'a>(pub &'a [u8]);

/// only var: let a, b in
/// only existential: exist c, d in
/// both: let a, b and exist c, d in
#[inline]
fn scope_var_decl(i: &[u8]) -> IResult<&[u8], DeclVars> {
    fn get_vars(mut input: &[u8], var: bool) -> IResult<&[u8], Vec<VarDeclBorrowed>> {
        let mut vars = vec![];
        let mut seps = 0;
        loop {
            let (i, _) = multispace0(input)?;
            let (i, name) = terminal(i)?;
            if name == b"in" || name == b"and" {
                input = i;
                break;
            }
            let (i, ty, val) = {
                match opt(tuple((
                    tag(":"),
                    multispace0,
                    terminal,
                    multispace0,
                    tag("="),
                    multispace0,
                    OpArgTermBorrowed::get,
                )))(i)?
                {
                    (rest, Some((.., ty, _, _, _, val))) => (rest, TypeDefBorrowed(ty), Some(val)),
                    (rest, None) => (rest, TypeDefBorrowed(b""), None),
                }
            };
            let (i, s) = opt(tag(","))(i)?;
            if s.is_some() {
                seps += 1;
            }
            if var {
                vars.push(VarDeclBorrowed::Var(VarBorrowed {
                    name: TerminalBorrowed::from_slice(name),
                    ty,
                    val,
                }));
            } else {
                vars.push(VarDeclBorrowed::Skolem(SkolemBorrowed {
                    name: TerminalBorrowed::from_slice(name),
                    ty,
                    val,
                }));
            }
            input = i;
        }

        if vars.len() - 1 == seps {
            Ok((input, vars))
        } else {
            Err(nom::Err::Error(ParseErrB::SyntaxError))
        }
    }

    let mut scope_vars = vec![];
    let (i, _) = multispace0(i)?;

    let (mut i, let_kw) = opt(tag("let "))(i)?;
    if let_kw.is_some() {
        let (rest, vars) = get_vars(i, true)?;
        i = rest;
        scope_vars.extend(vars);
    }
    let (i, _) = multispace0(i)?;
    let (i, and_kw) = opt(tag("and"))(i)?;
    let (i, _) = multispace0(i)?;

    let (mut i, exists_kw) = opt(tag("exists "))(i)?;
    // check that the combination of keywords is correct:
    match (let_kw, and_kw, exists_kw) {
        (Some(_), None, None) => {}
        (None, None, Some(_)) => {}
        (Some(_), Some(_), Some(_)) => {}
        _ => return Err(nom::Err::Error(ParseErrB::SyntaxError)),
    };

    if exists_kw.is_some() {
        let (rest, vars) = get_vars(i, false)?;
        i = rest;
        scope_vars.extend(vars);
    }

    Ok((i, scope_vars))
}

// skol_decl = '(' 'exists' $(term[':'op_arg]),+ ')' ;
#[derive(Debug, Clone)]
pub(in crate::agent) struct SkolemBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub ty: TypeDefBorrowed<'a>,
    pub val: Option<OpArgTermBorrowed<'a>>,
}

// var_decl = '(' 'let' $(term[':'op_arg]),+ ')' ;
#[derive(Debug, PartialEq, Clone)]
pub(in crate::agent) struct VarBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub ty: TypeDefBorrowed<'a>,
    pub val: Option<OpArgTermBorrowed<'a>>,
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
    #[inline]
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

#[inline]
fn func_decl(input: &[u8]) -> IResult<&[u8], FuncDeclBorrowed> {
    if let Ok(relational) = do_parse!(
        input,
        multispace0
            >> tag!("fn::")
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
            multispace0
                >> tag!("fn::")
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
    #[inline]
    fn convert_to_assert(decl: ClassDeclBorrowed<'a>) -> AssertBorrowed<'a> {
        AssertBorrowed::ClassDecl(decl)
    }
}

#[inline]
fn class_decl(input: &[u8]) -> IResult<&[u8], ClassDeclBorrowed> {
    do_parse!(
        input,
        multispace0
            >> name: map!(terminal, TerminalBorrowed::from_slice)
            >> op_args: opt!(op_args)
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
        multispace0
            >> term: map!(terminal, TerminalBorrowed::from_slice)
            >> multispace0
            >> u0: opt!(uval)
            >> (ArgBorrowed { term, uval: u0 })
    )
}

// args	= '[' arg $(arg);+ ']';
fn args(input: &[u8]) -> IResult<&[u8], Vec<ArgBorrowed>> {
    delimited!(
        input,
        char!('['),
        alt!(separated_list1!(char!(','), arg) | map!(arg, to_arg_vec)),
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

fn op_arg(i: &[u8]) -> IResult<&[u8], OpArgBorrowed> {
    fn normal_arg(orig: &[u8]) -> IResult<&[u8], OpArgBorrowed> {
        let (i, term) = OpArgTermBorrowed::get(orig)?;
        if term.is_reserved() && term != b"ow" {
            return Err(nom::Err::Error(ParseErrB::NonTerminal(orig)));
        }
        let (i, _) = multispace0(i)?;
        let (i, op) = opt(alt((
            tag(">="),
            tag("<="),
            tag("="),
            tag(">"),
            tag("<"),
            tag("is"),
        )))(i)?;
        if let Some(op) = op {
            let comp = CompOperator::from_chars(op);
            let (i, _) = multispace0(i)?;
            let (i, term2) = OpArgTermBorrowed::get(i)?;
            if term2.is_reserved() && term2 != b"ow" {
                return Err(nom::Err::Error(ParseErrB::NonTerminal(orig)));
            }
            let (i, _) = multispace0(i)?;
            Ok((
                i,
                OpArgBorrowed {
                    term,
                    comp: Some((comp?.1, term2)),
                },
            ))
        } else {
            Ok((i, OpArgBorrowed { term, comp: None }))
        }
    }

    fn time_arg(i: &[u8]) -> IResult<&[u8], OpArgBorrowed> {
        let (i, _) = tag("since")(i)?;
        let (i, _) = multispace0(i)?;
        let (i, since) = map(terminal, OpArgTermBorrowed::is_terminal)(i)?;
        let (i, _) = multispace0(i)?;
        let (i, until) = opt(tuple((
            tag("until"),
            multispace0,
            map(terminal, OpArgTermBorrowed::is_terminal),
            multispace0,
        )))(i)?;

        let term = if let Some((.., term, _)) = until {
            Some(term)
        } else {
            None
        };

        Ok((
            i,
            OpArgBorrowed {
                term: since,
                comp: CompOperator::from_time_op(term),
            },
        ))
    }

    fn var_assignment(orig: &[u8]) -> IResult<&[u8], OpArgBorrowed> {
        // let s3 = b"dean(where t1 is \"now\", t2 is t1)[$John=0]";
        // let s5 = b"happy(where this.time is 'now', since t1, ow)[x>=0.5]";
        let (i, _) = tag("where")(orig)?;
        let (i, _) = multispace0(i)?;

        let (i, this0) = {
            let r = opt(tag("this."))(i)?;
            (r.0, r.1.is_some())
        };
        let (i, v0) = OpArgTermBorrowed::get(i)?;
        if v0 == OpArgTermBorrowed::ThisTime && !this0 {
            return Err(nom::Err::Error(ParseErrB::SyntaxError));
        }

        let (i, _) = multispace0(i)?;
        let (i, _) = tag("is")(i)?;
        let (i, _) = multispace0(i)?;

        let (i, this1) = {
            let r = opt(tag("this."))(i)?;
            (r.0, r.1.is_some())
        };
        let (i, v1) = OpArgTermBorrowed::get(i)?;
        if v1 == OpArgTermBorrowed::ThisTime && !this1 {
            return Err(nom::Err::Error(ParseErrB::SyntaxError));
        }
        Ok((
            i,
            OpArgBorrowed {
                term: v0,
                comp: Some((CompOperator::Assignment, v1)),
            },
        ))
    }

    let (i, _) = multispace0(i)?;

    if let Ok((rest, arg)) = var_assignment(i) {
        return Ok((rest, arg));
    }

    if let Ok((rest, arg)) = normal_arg(i) {
        return Ok((rest, arg));
    }

    match time_arg(i) {
        Ok((rest, arg)) => Ok((rest, arg)),
        Err(err) => Err(err),
    }
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
    ThisTime,
}

impl<'a, T> PartialEq<T> for OpArgTermBorrowed<'a>
where
    T: AsRef<[u8]>,
{
    fn eq(&self, other: &T) -> bool {
        let other: &[u8] = other.as_ref();
        match self {
            OpArgTermBorrowed::Terminal(r) => *r == other,
            OpArgTermBorrowed::String(r) => *r == other,
            OpArgTermBorrowed::ThisTime => false,
        }
    }
}

impl<'a> std::fmt::Debug for OpArgTermBorrowed<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            OpArgTermBorrowed::Terminal(r) => {
                write!(f, "OpArg::Term({})", str::from_utf8(r).unwrap())
            }
            OpArgTermBorrowed::String(r) => write!(f, "OpArg::Str({})", str::from_utf8(r).unwrap()),
            OpArgTermBorrowed::ThisTime => write!(f, "OpArg::ThisTime"),
        }
    }
}

impl<'a> OpArgTermBorrowed<'a> {
    fn get(i: &[u8]) -> IResult<&[u8], OpArgTermBorrowed> {
        alt((
            map(string, OpArgTermBorrowed::is_string),
            map(terminal, OpArgTermBorrowed::is_terminal),
        ))(i)
    }

    fn is_string(i: &'a [u8]) -> OpArgTermBorrowed {
        OpArgTermBorrowed::String(i)
    }

    fn is_terminal(i: &'a [u8]) -> OpArgTermBorrowed {
        match i {
            b"time" => OpArgTermBorrowed::ThisTime,
            _ => OpArgTermBorrowed::Terminal(i),
        }
    }

    fn is_reserved(&self) -> bool {
        match self {
            OpArgTermBorrowed::Terminal(r) => super::reserved(r),
            OpArgTermBorrowed::ThisTime => true,
            OpArgTermBorrowed::String(_) => false,
        }
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
        multispace0
            //>> opt!(tuple((char(','), multispace0, char('u'))))
            >> multispace0
            >> op: map!(
                alt!(tag!(">=") | tag!("<=") | tag!("=") | tag!(">") | tag!("<")),
                CompOperator::from_chars
            )
            >> multispace0
            >> val: number
            >> (UVal { op: op?.1, val })
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
    if input.is_empty() {
        return Err(nom::Err::Error(ParseErrB::NonTerminal(input)));
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
pub(in crate::agent) enum CompOperator {
    // equality operators:
    Equal,
    Less,
    More,
    MoreEqual,
    LessEqual,
    // time operators:
    Since,
    Until,
    SinceUntil,
    // other:
    Assignment,
}

impl CompOperator {
    fn from_chars(c: &[u8]) -> IResult<&[u8], CompOperator> {
        if c == b"<" {
            Ok((EMPTY, CompOperator::Less))
        } else if c == b">" {
            Ok((EMPTY, CompOperator::More))
        } else if c == b"=" {
            Ok((EMPTY, CompOperator::Equal))
        } else if c == b"<=" {
            Ok((EMPTY, CompOperator::LessEqual))
        } else if c == b">=" {
            Ok((EMPTY, CompOperator::MoreEqual))
        } else if c == b"is" {
            Ok((EMPTY, CompOperator::Assignment))
        } else {
            Err(nom::Err::Error(ParseErrB::IsNotOperator(c)))
        }
    }

    fn from_time_op(t: Option<OpArgTermBorrowed>) -> Option<(CompOperator, OpArgTermBorrowed)> {
        if let Some(term) = t {
            Some((CompOperator::SinceUntil, term))
        } else {
            Some((CompOperator::Since, OpArgTermBorrowed::String(b"")))
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
            CompOperator::Until
            | CompOperator::Since
            | CompOperator::SinceUntil
            | CompOperator::Assignment => true,
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
            CompOperator::Since => id.push(7),
            CompOperator::SinceUntil => id.push(8),
            CompOperator::Assignment => id.push(9),
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
            CompOperator::Since => write!(f, "@"),
            CompOperator::SinceUntil => write!(f, "<->"),
            CompOperator::Assignment => write!(f, "=>"),
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
    fn from_bytes(i: &[u8]) -> IResult<&[u8], LogicOperator> {
        let mut find_op = alt::<_, _, ParseErrB, _>((
            tag(ICOND_OP),
            tag(AND_OP),
            tag(OR_OP),
            tag(IFF_OP),
            tag(IMPL_OP),
        ));
        match find_op(i) {
            Ok((rest, ICOND_OP)) => Ok((rest, LogicOperator::Entail)),
            Ok((rest, AND_OP)) => Ok((rest, LogicOperator::And)),
            Ok((rest, OR_OP)) => Ok((rest, LogicOperator::Or)),
            Ok((rest, IFF_OP)) => Ok((rest, LogicOperator::Biconditional)),
            Ok((rest, IMPL_OP)) => Ok((rest, LogicOperator::Implication)),
            _ => Err(nom::Err::Error(ParseErrB::IsNotOperator(i))),
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

#[inline]
fn logic_operator(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut operator = alt((tag(":="), tag("&&"), tag("||"), tag("=>"), tag("<=>")));
    operator(input)
}

#[cfg(test)]
mod test {
    use super::*;
    use nom;

    #[test]
    fn remove_comments() -> Result<(), nom::Err<ParseErrB<'static>>> {
        // remove comments:
        let source = b"
            # one line comment
            ( # first scope
                ( # second scope
                    let x, y in
                    professor[$Lucy=1]
                )
            )
            /*
                multi line
                comment
            */
        ";
        let clean = Parser::remove_comments(source)?;

        let expected = "((letx,yinprofessor[$Lucy=1]))";
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
    fn parse_statements() -> Result<(), nom::Err<ParseErrB<'static>>> {
        let source = b"
            ( american[x=1] )
        ";
        multi_asserts(source)?;
        Ok(())
    }

    #[test]
    fn parse_variables() -> Result<(), nom::Err<ParseErrB<'static>>> {
        let source = b"let a, b in";
        scope_var_decl(source)?;
        let source = b"exists a, b in";
        scope_var_decl(source)?;
        let source = b"let a, b and exist c, d in";
        scope_var_decl(source)?;

        let source = b"let a, b";
        assert!(scope_var_decl(source).is_err());
        let source = b"let a b in";
        assert!(scope_var_decl(source).is_err());
        let source = b"let a, b exist c, d in";
        assert!(scope_var_decl(source).is_err());

        Ok(())
    }

    #[test]
    fn parse_sentences() -> Result<(), nom::Err<ParseErrB<'static>>> {
        let source = b"
            ( american[x=1] && ( weapon[y=1] && hostile[z=1] ) )
        ";
        parse_sentence(source)?;

        let source = b"
            ( ( american[x=1] && hostile[z=1] ) && hostile[z=1] )
        ";
        parse_sentence(source)?;

        let source = b"
            ( american[x=1] && hostile[z=1] && ( weapon[y=1]) )
        ";
        let scanned = parse_sentence(source);
        assert!(scanned.is_err());

        let source = b"
            ( ( american[x=1] ) && hostile[z=1] && weapon[y=1] )
        ";
        let scanned = parse_sentence(source);
        assert!(scanned.is_err());

        let source = b"
            ( ( ( american[x=1] ) ) && hostile[z=1] && ( ( weapon[y=1] ) ) )
        ";
        let scanned = parse_sentence(source);
        assert!(scanned.is_err());

        let source = b"
            ( american[x=1] && ( ( hostile[z=1] ) ) && weapon[y=1] )
        ";
        let scanned = parse_sentence(source);
        assert!(scanned.is_err());

        let source = b"
        (let x, y in (american[x=1] && hostile[z=1]) := criminal[x=1])
        (let x, y in ((american[x=1] && hostile[z=1]) := criminal[x=1]))
        (let x, y in (american[x=1] && hostile[z=1]) := criminal[x=1])
        ";
        let (_, clean) = Parser::remove_comments(source)?;
        let scanned = Parser::get_blocks(&clean).unwrap();
        assert_eq!(scanned.len(), 3);

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
        let s1 = b"professor[$Lucy=1]";
        let s1_res = class_decl(s1);
        assert_done_or_err!(s1_res);
        let s1_res = s1_res.unwrap().1;
        assert_eq!(s1_res.name, TerminalBorrowed(b"professor"));
        assert_eq!(s1_res.args[0].term, TerminalBorrowed(b"$Lucy"));
        assert!(s1_res.args[0].uval.is_some());

        let s2 = b"missile[$M1 > -1.5]";
        let s2_res = class_decl(s2);
        assert_done_or_err!(s2_res);
        let s2_res = s2_res.unwrap().1;
        assert_eq!(s2_res.name, TerminalBorrowed(b"missile"));
        assert_eq!(s2_res.args[0].term, TerminalBorrowed(b"$M1"));
        let s2_uval = s2_res.args[0].uval.as_ref().unwrap();
        assert_eq!(s2_uval.op, CompOperator::More);
        assert_eq!(s2_uval.val, Number::SignedFloat(-1.5_f32));

        // non-sensical, but can parse:
        let s3 = b"dean(where t1 is \"now\", t2 is t1)[$John=0]";
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
                    comp: Some((CompOperator::Assignment, OpArgTermBorrowed::String(b"now"))),
                },
                OpArgBorrowed {
                    term: OpArgTermBorrowed::Terminal(b"t2"),
                    comp: Some((CompOperator::Assignment, OpArgTermBorrowed::Terminal(b"t1"))),
                },
            ]
        );

        // non-sensical, but can parse:
        let s4 = b"animal(where t1 is '2015.07.05.11.28')[cow, brown=0.5]";
        let s4_res = class_decl(s4);
        assert_done_or_err!(s4_res);
        let s4_res = s4_res.unwrap().1;
        assert_eq!(s4_res.args[1].term, TerminalBorrowed(b"brown"));
        assert!(s4_res.op_args.is_some());
        assert_eq!(
            s4_res.op_args.as_ref().unwrap(),
            &vec![OpArgBorrowed {
                term: OpArgTermBorrowed::Terminal(b"t1"),
                comp: Some((
                    CompOperator::Assignment,
                    OpArgTermBorrowed::String(b"2015.07.05.11.28"),
                )),
            }]
        );

        let s5 = b"happy(where this.time is 'now', since t1, ow)[x>=0.5]";
        let s5_res = class_decl(s5);
        assert_done_or_err!(s5_res);
        let s5_res = s5_res.unwrap().1;
        assert!(s5_res.op_args.is_some());
        assert_eq!(
            &s5_res.op_args.as_ref().unwrap()[0],
            &OpArgBorrowed {
                term: OpArgTermBorrowed::ThisTime,
                comp: Some((CompOperator::Assignment, OpArgTermBorrowed::String(b"now"),)),
            }
        );
        assert_eq!(
            &s5_res.op_args.as_ref().unwrap()[1],
            &OpArgBorrowed {
                term: OpArgTermBorrowed::Terminal(b"t1"),
                comp: Some((CompOperator::Since, OpArgTermBorrowed::String(b""))),
            }
        );

        // all valid forms:
        // happy(where this.time is t1, happens since t1 until t2, ow)[$John]
        // happy(where t1 is this.time)[$John]
        // happy(since t1)[$John]
        let s6 = b"happy(where this.time is t1, since t1 until t2, ow)[x<=0.5]";
        let s6_res = class_decl(s6);
        assert_done_or_err!(s6_res);
        let s6_res = s6_res.unwrap().1;
        assert!(s6_res.op_args.is_some());
        assert_eq!(
            &s6_res.op_args.as_ref().unwrap()[1],
            &OpArgBorrowed {
                term: OpArgTermBorrowed::Terminal(b"t1"),
                comp: Some((CompOperator::SinceUntil, OpArgTermBorrowed::Terminal(b"t2"),)),
            }
        );

        /*
        //TODO: add a way to define a 'record', conversedly can be used for querying
        //this is an entity and all the classes memberships in one go, e.g.:
        $john = {
            fast=0,
            slow=0.5,
            dog, // ellided =1
            from "now",
        }
        // defining more than one entity with similar values:
        [$john, $mary] = {
            ...
        }
        */
    }

    #[test]
    fn parser_function() {
        let s5 = b"fn::criticize(time='2018-04-01T00:00:00Z')[$John=1,$Lucy]";
        let s5_res = func_decl(s5);
        assert!(s5_res.is_err());

        let s1 = b"fn::criticize(t1 is 'now')[$John=1,$Lucy]";
        let s1_res = func_decl(s1);
        assert_done_or_err!(s1_res);
        assert_eq!(s1_res.unwrap().1.variant, FuncVariants::Relational);

        let s2 = b"fn::takes[$analysis>0,$Bill]";
        let s2_res = func_decl(s2);
        assert_done_or_err!(s2_res);
        let s2_res = s2_res.unwrap().1;
        assert_eq!(s2_res.name, TerminalBorrowed(b"takes"));
        assert_eq!(s2_res.variant, FuncVariants::Relational);

        let s3 = b"fn::loves[cow=1,bull]";
        let s3_res = func_decl(s3);
        assert_done_or_err!(s3_res);
        assert_eq!(s3_res.unwrap().1.variant, FuncVariants::Relational);

        let s4 = b"fn::time_calc(t1<t2)";
        let s4_res = func_decl(s4);
        assert_done_or_err!(s4_res);
        assert_eq!(s4_res.unwrap().1.variant, FuncVariants::NonRelational);
    }
}
