//! # Grammar for the SIMAG declarative language
//! ```BNF
//! @comments			::	regex: \/\*(.*)\*\/ (multiline)
//! @eol_comments 		::	/#.*?$/
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
//! 		  | 'fn::' term '(' ${op_arg}','+ ')' ;
//! args = '[' ${ arg }';'+ ']';
//! arg = term [',' uval] ;
//! uval = 'u' comp_op number ;
//! op_arg = (string|term) [comp_op (string|term)] ;
//! icond_op    =	':=' ;
//! and_op      =	'&&' ;
//! or_op		=	'||' ;
//! logic_op	=    '<=>'
//! 		    |    '=>'
//!             |    or_op
//!             |	 and_op ;
//! comp_op	= ('=' | '<' | '>' | '>=' | '<=' ) ;
//! term = regex: \$?[a-zA-Z0-9_]+ ;
//! number = regex: -?[0-9\.]+ ;
//! string = regex: ".*?"|'.*?' ;
//! ```

use std::collections::{VecDeque, HashMap};
use std::fmt;
use std::str;
use std::str::FromStr;

use nom;
use nom::{is_alphanumeric, is_digit};
use nom::{ErrorKind, IResult};

use rayon::prelude::*;
use rayon;

use lang::common::*;
use lang::errors::ParseErrF;
use lang::logsent::*;

const ICOND_OP: &[u8] = b":=";
const AND_OP: &[u8] = b"&&";
const OR_OP: &[u8] = b"||";
const IFF_OP: &[u8] = b"<=>";
const IMPL_OP: &[u8] = b"=>";

const EMPTY: &[u8] = b" ";

pub(crate) struct Parser;
impl Parser {
    /// Lexerless (mostly) recursive descent parser. Takes a string and outputs a correct ParseTree.
    pub fn parse(
        input: &str,
        tell: bool,
        num_threads: usize,
    ) -> Result<VecDeque<ParseTree>, ParseErrF> {
        // store is a vec where the sequence of characters after cleaning up comments
        // will be stored, both have to be extended to 'static lifetime so they can be
        // TODO: use rayon::iter::IntoParallelIterator;
        let mut clean = vec![];
        let scopes = match Self::p1(input.as_bytes(), &mut clean) {
            Ok(scopes) => scopes.into_par_iter(),
            Err(err) => return Err(ParseErrF::from(err)),
        };
        // walk the AST output and, if correct, output a final parse tree
        let tpool = rayon::ThreadPoolBuilder::new()
            .num_threads(num_threads)
            .build()
            .unwrap();
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

    /// Tokenize and output an AST
    fn p1<'b: 'a, 'a>(
        input: &'a [u8],
        p2: &'b mut Vec<u8>,
    ) -> Result<Vec<ASTNode<'b>>, ParseErrB<'a>> {
        // clean up every comment to facilitate further parsing
        // TODO: clean up or ignore comments without having to collect over the initial slice
        let p1 = match remove_comments(input) {
            IResult::Done(_, done) => done,
            IResult::Error(nom::Err::Position(_, _)) => return Err(ParseErrB::UnclosedComment),
            _ => return Err(ParseErrB::SyntaxErrorU),
        };
        for s in p1 {
            for c in s {
                p2.push(c.clone());
            }
        }
        let scopes = get_blocks(&p2[..]);
        if scopes.is_err() {
            match scopes.unwrap_err() {
                nom::Err::Position(t, p) => match (t, p) {
                    (ErrorKind::Custom(0), p) => Err(ParseErrB::NonTerminal(p)),
                    (ErrorKind::Custom(1), p) => Err(ParseErrB::NonNumber(p)),
                    (ErrorKind::Custom(11), p) => Err(ParseErrB::NotScope(p)),
                    (ErrorKind::Custom(12), p) => Err(ParseErrB::ImbalDelim(p)),
                    (ErrorKind::Custom(13), p) => Err(ParseErrB::IllegalChain(p)),
                    (_, p) => Err(ParseErrB::SyntaxErrorPos(p)),
                },
                _ => Err(ParseErrB::SyntaxErrorU),
            }
        } else {
            let (_, scopes) = scopes.unwrap();
            Ok(scopes)
        }
    }
}

#[derive(Debug)]
pub(crate) enum ParseErrB<'a> {
    SyntaxErrorU,
    //SyntaxError(Box<ParseErrB<'a>>),
    SyntaxErrorPos(&'a [u8]),
    NotScope(&'a [u8]),
    ImbalDelim(&'a [u8]),
    IllegalChain(&'a [u8]),
    NonTerminal(&'a [u8]),
    NonNumber(&'a [u8]),
    UnclosedComment,
}

impl<'a> fmt::Display for ParseErrB<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::str;
        let msg = unsafe {
            match *self {
                ParseErrB::SyntaxErrorU => "syntax error".to_string(),
                //ParseErrB::SyntaxError(Box<ParseErrB<'a>>) => {}
                ParseErrB::SyntaxErrorPos(arr) => {
                    format!("syntax error at:\n{}", str::from_utf8_unchecked(arr))
                }
                ParseErrB::NotScope(arr) => format!(
                    "syntax error, scope is invalid or not found:\n{}",
                    str::from_utf8_unchecked(arr)
                ),
                ParseErrB::ImbalDelim(arr) => format!(
                    "syntax error, 
                             open delimiters:\n{}",
                    str::from_utf8_unchecked(arr)
                ),
                ParseErrB::IllegalChain(arr) => format!(
                    "syntax error,
                             incomplete operator chain:\n{}",
                    str::from_utf8_unchecked(arr)
                ),
                ParseErrB::NonTerminal(arr) => format!(
                    "syntax error,
                             illegal character in terminal position:\n{}",
                    str::from_utf8_unchecked(arr)
                ),
                ParseErrB::NonNumber(arr) => format!(
                    "syntax error,
                             illegal character found when parsing a number:v{}",
                    str::from_utf8_unchecked(arr)
                ),
                ParseErrB::UnclosedComment => "syntax error, open comment delimiter".to_string(),
            }
        };
        write!(f, "{}", msg)
    }
}

#[derive(Debug)]
pub(crate) enum ParseTree {
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
        match LogSentence::new(&input, &mut context) {
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
pub(crate) enum ASTNode<'a> {
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
            ASTNode::Assert(_) | ASTNode::Chain(_) | ASTNode::None => panic!(),
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
                AssertBorrowed::ClassDecl(ref decl) => {
                    let cls = ClassDecl::from(decl, context)?;
                    Ok(Some(ParseTree::Assertion(vec![Assert::ClassDecl(cls)])))
                }
                AssertBorrowed::FuncDecl(ref decl) => {
                    let func = FuncDecl::from(decl, context)?;
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
            },
            ASTNode::None => Ok(None),
        };
        context.depth -= 1;
        tree_node
    }
}

#[derive(Debug)]
pub(crate) struct Scope<'a> {
    pub vars: Option<Vec<VarDeclBorrowed<'a>>>,
    pub logic_op: Option<LogicOperator>,
    pub next: ASTNode<'a>,
}

impl<'a> Scope<'a> {
    fn is_assertion(&self, context: &mut ParseContext) -> Result<Option<ParseTree>, ParseErrF> {
        if self.vars.is_some() && context.depth != 1 && !context.is_tell {
            return Ok(None);
        }
        match self.logic_op {
            Some(LogicOperator::And) | None => {}
            _ => return Ok(None),
        }
        if context.depth == 1 && context.is_tell {
            // embedded defined variables in this assertion, extract values and perform substitution
            self.vars.as_ref().unwrap().iter().map(|x| context.push_var(x)).collect::<Result<Vec<_>, _>>()?;
            match self.next.is_assertion(context) {
                Ok(Some(ParseTree::Assertion(asserts))) => {
                    let mut assignments = HashMap::new(); 
                    for x in context.vars.iter().filter(|x| x.is_time_var()) {
                        assignments.insert(x.clone(), x.get_times());
                    }
                    unimplemented!()
                }
                Err(err) => Err(err),
                _ => Err(ParseErrF::WrongDef),
            }
        } else {
            self.next.is_assertion(context)
        }
    }
}

#[derive(Debug)]
pub(crate) enum AssertBorrowed<'a> {
    FuncDecl(FuncDeclBorrowed<'a>),
    ClassDecl(ClassDeclBorrowed<'a>),
}

#[derive(Debug)]
pub(crate) enum VarDeclBorrowed<'a> {
    Var(VarBorrowed<'a>),
    Skolem(SkolemBorrowed<'a>),
}

fn get_blocks(input: &[u8]) -> IResult<&[u8], Vec<ASTNode>> {
    let input = match remove_multispace(input) {
        Some(view) => view,
        None => return IResult::Done(EMPTY, vec![]),
    };
    // find the positions of the closing delimiters and try until it fails
    let mut mcd = ::std::collections::VecDeque::new();
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
        return IResult::Error(nom::Err::Position(ErrorKind::Custom(12), input));
    } else if mcd.is_empty() {
        return IResult::Error(nom::Err::Position(ErrorKind::Custom(11), input));
    }

    let mut results: Vec<ASTNode> = Vec::new();
    for _ in 0..mcd.len() {
        let (lp, rp) = mcd.pop_front().unwrap();
        match scope0(&input[lp..rp]) {
            IResult::Done(_, done) => match done {
                IResult::Done(_, done) => results.push(done),
                IResult::Error(err) => return IResult::Error(err),
                IResult::Incomplete(_) => return IResult::Error(nom::Err::Code(ErrorKind::Count)),
            },
            IResult::Error(err) => return IResult::Error(err),
            IResult::Incomplete(_) => return IResult::Error(nom::Err::Code(ErrorKind::Count)),
        }
    }
    if results.is_empty() {
        return IResult::Error(nom::Err::Position(ErrorKind::Custom(11), input));
    }
    IResult::Done(EMPTY, results)
}

named!(scope0(&[u8]) -> IResult<&[u8], ASTNode>, ws!(
    alt!(
        map!(
            do_parse!(
                tag!("(") >>
                vars: opt!(scope_var_decl) >>
                decl: decl_alt >>
                op: opt!(map!(logic_operator, LogicOperator::from_bytes)) >>
                next: alt!(assertions | scope0) >>
                tag!(")") >>
                (vars, decl, op, next, true)
            ), expr0
        ) | 
        map!(
            do_parse!(
                tag!("(") >>
                vars: opt!(scope_var_decl) >>
                next: alt!(assertions | scope0) >>
                op: opt!(map!(logic_operator, LogicOperator::from_bytes)) >>
                decl: decl_alt >>
                tag!(")") >>
                (vars, decl, op, next, false)
            ), expr0
        ) |
        map!(
            do_parse!(
                tag!("(") >>
                vars: opt!(scope_var_decl) >>
                lhs: alt!(assertions | scope0) >> 
                op: map!(logic_operator, LogicOperator::from_bytes) >> 
                rhs: alt!(assertions | scope0) >>
                tag!(")") >>
                (vars, lhs, op, rhs)
            ), expr1 
        ) |
        map!(
            do_parse!(
                tag!("(") >>
                vars: opt!(scope_var_decl) >>
                next: opt!(alt!(assertions | scope0)) >>
                tag!(")") >>
                (vars, next)
            ), empty_scope 
        ) |
        map!(
            map!(decl_alt, ASTNode::from_assert), 
            |decl| {
                IResult::Done(EMPTY, decl)
            }
        ) |
        do_parse!(expr: assertions >> (expr))
    ) 
));

type ScopeOutA<'a> = (
    Option<DeclVars<'a>>,
    AssertBorrowed<'a>,
    Option<LogicOperator>,
    IResult<&'a [u8], ASTNode<'a>>,
    bool,
);

fn expr0(input: ScopeOutA) -> IResult<&[u8], ASTNode> {
    let (vars, decl, op, next, is_lhs) = input;
    let (rest, next) = match next {
        IResult::Done(rest, next) => (rest, next),
        IResult::Error(err) => return IResult::Error(err),
        IResult::Incomplete(_) => return IResult::Error(nom::Err::Code(ErrorKind::Custom(11))),
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
    IResult::Done(rest, curr)
}

type ScopeOutB<'a> = (
    Option<DeclVars<'a>>,
    IResult<&'a [u8], ASTNode<'a>>,
    LogicOperator,
    IResult<&'a [u8], ASTNode<'a>>,
);

fn expr1(input: ScopeOutB) -> IResult<&[u8], ASTNode> {
    let (vars, lhs, op, rhs) = input;
    let lhs = match lhs {
        IResult::Done(_, next) => next,
        IResult::Error(err) => return IResult::Error(err),
        IResult::Incomplete(_) => return IResult::Error(nom::Err::Code(ErrorKind::Custom(11))),
    };
    let rhs = match rhs {
        IResult::Done(_, next) => next,
        IResult::Error(err) => return IResult::Error(err),
        IResult::Incomplete(_) => return IResult::Error(nom::Err::Code(ErrorKind::Custom(11))),
    };
    let next = Scope {
        vars: flat_vars(vars),
        logic_op: Some(op),
        next: ASTNode::Chain(vec![lhs, rhs]),
    };
    IResult::Done(EMPTY, ASTNode::Scope(Box::new(next)))
}

fn empty_scope<'a>(
    input: (Option<DeclVars<'a>>, Option<IResult<&'a [u8], ASTNode<'a>>>),
) -> IResult<&'a [u8], ASTNode<'a>> {
    let (vars, next) = input;
    if let Some(vars) = flat_vars(vars) {
        if let Some(next) = next {
            let (rest, next) = match next {
                IResult::Done(rest, next) => (rest, next),
                IResult::Error(err) => return IResult::Error(err),
                IResult::Incomplete(_) => {
                    return IResult::Error(nom::Err::Code(ErrorKind::Custom(11)))
                }
            };
            IResult::Done(
                rest,
                ASTNode::Scope(Box::new(Scope {
                    vars: Some(vars),
                    logic_op: None,
                    next,
                })),
            )
        } else {
            IResult::Done(EMPTY, ASTNode::None)
        }
    } else if next.is_some() {
        match next {
            Some(IResult::Done(rest, next)) => IResult::Done(rest, next),
            Some(IResult::Error(err)) => return IResult::Error(err),
            Some(IResult::Incomplete(_)) => {
                return IResult::Error(nom::Err::Code(ErrorKind::Custom(11)))
            }
            None => IResult::Done(EMPTY, ASTNode::None),
        }
    } else {
        IResult::Done(EMPTY, ASTNode::None)
    }
}

named!(assertions(&[u8]) -> IResult<&[u8], ASTNode>,
    map!(ws!(
        do_parse!(
            tag!("(") >>
            vars: opt!(scope_var_decl) >>
            decl: many0!(
                map!(
                    do_parse!(
                        decl: decl_alt >> 
                        op: map!(logic_operator, LogicOperator::from_bytes) >>
                        (op, decl)
                    ),  assert_one
                )
            ) >>
            last: map!(decl_alt, ASTNode::from_assert) >>
            tag!(")") >>
            (vars, decl, last)
        )
    ), assert_many)
);

type AssertOne<'a> = (LogicOperator, AssertBorrowed<'a>);

#[inline]
fn assert_one(input: AssertOne) -> IResult<&[u8], ASTNode> {
    let (op, assertion) = input;
    IResult::Done(
        EMPTY,
        ASTNode::Scope(Box::new(Scope {
            next: ASTNode::Assert(assertion),
            vars: None,
            logic_op: Some(op),
        })),
    )
}

type AssertMany<'a> = (
    Option<DeclVars<'a>>,
    Vec<IResult<&'a [u8], ASTNode<'a>>>,
    ASTNode<'a>,
);

#[inline]
fn assert_many(input: AssertMany) -> IResult<&[u8], ASTNode> {
    let (vars, mut decl, last) = input;

    let mut fd = vec![];
    for e in decl.drain(..) {
        match e {
            IResult::Done(_, e) => fd.push(e),
            IResult::Error(err) => return IResult::Error(err),
            IResult::Incomplete(_) => return IResult::Error(nom::Err::Code(ErrorKind::Custom(13))),
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
        IResult::Done(EMPTY, f)
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
        IResult::Done(EMPTY, f)
    }
}

#[inline]
fn decl_alt(input: &[u8]) -> IResult<&[u8], AssertBorrowed> {
    alt!(
        input,
        map!(class_decl, ClassDeclBorrowed::convert_to_assert)
            | map!(func_decl, FuncDeclBorrowed::convert_to_assert)
    )
}

type DeclVars<'a> = Vec<Vec<VarDeclBorrowed<'a>>>;

#[inline]
named!(scope_var_decl(&[u8]) -> DeclVars, 
    ws!(many1!(alt!(variable | skolem)))
);

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
#[derive(Debug)]
pub(crate) struct SkolemBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub op_arg: Option<OpArgBorrowed<'a>>,
}

named!(skolem(&[u8]) -> Vec<VarDeclBorrowed>, do_parse!(
    tag!("(") >> 
    opt!(take_while!(is_multispace)) >>
    tag!("exists ") >> 
    vars: fold_many1!(
        do_parse!(
            opt!(take_while!(is_multispace)) >>
            name: terminal >>
            oa: opt!(do_parse!(
                tag!(":") >> 
                oa: op_arg >>
                (oa) 
            )) >>
            opt!(take_while!(is_multispace)) >>
            opt!(tag!(",")) >>
            (name, oa)
        ), Vec::new(), |mut vec: Vec<_>, (name, oa)| {
            let v = SkolemBorrowed {
                name: TerminalBorrowed::from_slice(name),
                op_arg: oa
            };
            vec.push(VarDeclBorrowed::Skolem(v));
            vec
        }
    ) >>
    opt!(take_while!(is_multispace)) >>
    tag!(")") >> 
    (vars)
));

// var_decl = '(' 'let' $(term[':'op_arg]),+ ')' ;
#[derive(Debug, PartialEq)]
pub(crate) struct VarBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub op_arg: Option<OpArgBorrowed<'a>>,
}

named!(variable(&[u8]) -> Vec<VarDeclBorrowed>, do_parse!(
    tag!("(") >> 
    opt!(take_while!(is_multispace)) >>
    tag!("let ") >>
    vars: fold_many1!(
        do_parse!(
            opt!(take_while!(is_multispace)) >>
            name: terminal >>
            oa: opt!(do_parse!(
                tag!(":") >> 
                oa: op_arg >>
                (oa) 
            )) >>
            opt!(take_while!(is_multispace)) >>
            opt!(tag!(",")) >>
            (name, oa)
        ), Vec::new(), |mut vec: Vec<_>, (name, oa)| {
            let v = VarBorrowed {
                name: TerminalBorrowed::from_slice(name),
                op_arg: oa
            };
            vec.push(VarDeclBorrowed::Var(v));
            vec
        }
    ) >> 
    opt!(take_while!(is_multispace)) >>
    tag!(")") >>
    (vars)
));

// func_decl = 'fn::' term ['(' op_args ')'] args
// 			 | 'fn::' term '(' op_args ')' ;
#[derive(Debug, PartialEq)]
pub(crate) struct FuncDeclBorrowed<'a> {
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
pub(crate) enum FuncVariants {
    Relational,
    NonRelational,
    TimeCalc,
}

impl FuncVariants {
    pub fn is_relational(self) -> bool {
        match self {
            FuncVariants::Relational => true,
            _ => false,
        }
    }
}

named!(func_decl(&[u8]) -> FuncDeclBorrowed,
    ws!(alt_complete!(
        do_parse!(
            tag!("fn::") >> 
            name: map!(terminal, TerminalBorrowed::from_slice) >>
            op1: opt!(op_args) >>
            a1: args >>
            (FuncDeclBorrowed {
                name,
                args: Some(a1),
                op_args: op1,
                variant: FuncVariants::Relational
            })
        ) |
        do_parse!(
            tag!("fn::") >> 
            name: map!(terminal, TerminalBorrowed::from_slice) >>
            op1: op_args >>
            (FuncDeclBorrowed {
                name,
                args: None,
                op_args: Some(op1),
                variant: FuncVariants::NonRelational
            })
        )
    ))
);

// class_decl = term ['(' op_args ')'] args ;
#[derive(Debug, PartialEq)]
pub(crate) struct ClassDeclBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub args: Vec<ArgBorrowed<'a>>,
    pub op_args: Option<Vec<OpArgBorrowed<'a>>>,
}

impl<'a> ClassDeclBorrowed<'a> {
    fn convert_to_assert(decl: ClassDeclBorrowed<'a>) -> AssertBorrowed<'a> {
        AssertBorrowed::ClassDecl(decl)
    }
}

named!(class_decl(&[u8]) -> ClassDeclBorrowed, ws!(do_parse!(
    name: map!(terminal, TerminalBorrowed::from_slice) >> 
    op1: opt!(op_args) >>
    a1: args >>
    (ClassDeclBorrowed{name, op_args: op1, args: a1})
)));

// arg	= term [',' uval] ;
#[derive(Debug, PartialEq)]
pub(crate) struct ArgBorrowed<'a> {
    pub term: TerminalBorrowed<'a>,
    pub uval: Option<UVal>,
}

named!(
    arg<ArgBorrowed>,
    ws!(do_parse!(
        term: map!(terminal, TerminalBorrowed::from_slice)
            >> u0: opt!(do_parse!(char!(',') >> u1: uval >> (u1))) >> ({
            ArgBorrowed {
                term,
                uval: u0,
            }
        })
    ))
);

// args	= '[' arg $(arg);* ']';
named!(args(&[u8]) -> Vec<ArgBorrowed>, delimited!(
        char!('['),
        alt!(separated_list!(char!(';'), arg) | map!(arg, to_arg_vec)),
        char!(']')
    )
);

fn to_arg_vec(arg: ArgBorrowed) -> Vec<ArgBorrowed> {
    vec![arg]
}

// op_arg =	(string|term) [comp_op (string|term)] ;
#[derive(Debug, PartialEq)]
pub(crate) struct OpArgBorrowed<'a> {
    pub term: OpArgTermBorrowed<'a>,
    pub comp: Option<(CompOperator, OpArgTermBorrowed<'a>)>,
}

named!(
    op_arg<OpArgBorrowed>,
    ws!(alt!(
        do_parse!(
            term: alt!(
                map!(string, OpArgTermBorrowed::is_string)
                | map!(terminal, OpArgTermBorrowed::is_terminal)) >> 
            c1: opt!(do_parse!(
                c2: map!(
                    alt!(tag!(">=") | tag!("<=") | tag!("=") | tag!(">") | tag!("<")),
                    CompOperator::from_chars) >> 
                term: alt!(
                    map!(string, OpArgTermBorrowed::is_string)
                    | map!(terminal, OpArgTermBorrowed::is_terminal)) >> 
                (c2, term))) >>
            (OpArgBorrowed {
                term,
                comp: c1,
            })) |
        do_parse!(
            tag!("@") >>
            from: map!(terminal, OpArgTermBorrowed::is_terminal) >>
            to: opt!(do_parse!(
                tag!("->") >>
                term: map!(terminal, OpArgTermBorrowed::is_terminal) >>
                (term))) >> 
            (OpArgBorrowed {
                term: from,
                comp: CompOperator::from_time_op(to),
            })   
        )
    ))
);

// op_args = $(op_arg),+ ;
named!(op_args(&[u8]) -> Vec<OpArgBorrowed>, delimited!(
        char!('('),
        alt!(separated_list!(char!(','), op_arg) | map!(op_arg, to_op_arg_vec)),
        char!(')')
    )
);

fn to_op_arg_vec(a: OpArgBorrowed) -> Vec<OpArgBorrowed> {
    vec![a]
}

#[derive(Debug, PartialEq)]
pub(crate) enum OpArgTermBorrowed<'a> {
    Terminal(&'a [u8]),
    String(&'a [u8]),
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
pub(crate) struct UVal {
    pub op: CompOperator,
    pub val: Number,
}

named!(
    uval<UVal>,
    ws!(do_parse!(
        char!('u')
            >> op: map!(
                alt!(tag!(">=") | tag!("<=") | tag!("=") | tag!(">") | tag!("<")),
                CompOperator::from_chars
            ) >> val: number >> (UVal { op, val })
    ))
);

// number = -?[0-9\.]+
#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum Number {
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
            return IResult::Error(nom::Err::Position(ErrorKind::Custom(1), input));
        }
    }
    if float && (input[0] == b'-') {
        IResult::Done(
            &input[idx + 1..],
            Number::SignedFloat(
                <f32>::from_str(str::from_utf8(&input[0..=idx]).unwrap()).unwrap(),
            ),
        )
    } else if !float && (input[0] == b'-') {
        IResult::Done(
            &input[idx + 1..],
            Number::SignedInteger(
                <i32>::from_str(str::from_utf8(&input[0..=idx]).unwrap()).unwrap(),
            ),
        )
    } else if float {
        IResult::Done(
            &input[idx..],
            Number::UnsignedFloat(
                <f32>::from_str(str::from_utf8(&input[0..idx]).unwrap()).unwrap(),
            ),
        )
    } else {
        IResult::Done(
            &input[idx..],
            Number::UnsignedInteger(
                <u32>::from_str(str::from_utf8(&input[0..idx]).unwrap()).unwrap(),
            ),
        )
    }
}

// string = (\".*?\")|('.*?) ;
fn string(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if input[0] == b'\'' {
        delimited!(input, char!('\''), is_not!("'"), char!('\''))
    } else if input[0] == b'"' {
        delimited!(input, char!('"'), is_not!("\""), char!('"'))
    } else {
        IResult::Error(nom::Err::Position(ErrorKind::IsNotStr, input))
    }
}

// terminal = [a-zA-Z0-9_]+ ;
#[derive(PartialEq)]
pub(crate) struct TerminalBorrowed<'a>(pub &'a [u8]);

impl<'a> TerminalBorrowed<'a> {
    pub fn from_slice(i: &'a [u8]) -> TerminalBorrowed<'a> {
        TerminalBorrowed(i)
    }
}

impl<'a> ::std::fmt::Debug for TerminalBorrowed<'a> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "Term({})", unsafe { str::from_utf8_unchecked(self.0) })
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
            return IResult::Error(nom::Err::Position(ErrorKind::Custom(0), input));
        }
    }
    if input[0] == b'$' && input[1..idx].is_empty() {
        return IResult::Error(nom::Err::Position(ErrorKind::Custom(0), input));
    }
    IResult::Done(&input[idx..], &input[0..idx])
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub(crate) enum CompOperator {
    // equality operators:
    Equal,
    Less,
    More,
    MoreEqual,
    LessEqual,
    // time operators:
    Until,
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
            Some((CompOperator::Until, term))
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
            CompOperator::Equal | CompOperator::Until | CompOperator::At => true,
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
            CompOperator::At => id.push(7)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum LogicOperator {
    Entail,
    And,
    Or,
    Implication,
    Biconditional,
}

impl LogicOperator {
    fn from_bytes(m: &[u8]) -> LogicOperator {
        if m == ICOND_OP {
            LogicOperator::Entail
        } else if m == AND_OP {
            LogicOperator::And
        } else if m == OR_OP {
            LogicOperator::Or
        } else if m == IFF_OP {
            LogicOperator::Biconditional
        } else if m == IMPL_OP {
            LogicOperator::Implication
        } else {
            panic!() // will never happen
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

named!(
    logic_operator,
    ws!(alt!(
        tag!(":=") | tag!("&&") | tag!("||") | tag!("=>") | tag!("<=>")
    ))
);

// comment parsing tools:
named!(remove_comments(&[u8]) -> Vec<&[u8]>,
    many1!(
        do_parse!(
            non_comment: comment_tag >>
            opt!(alt!(
                recognize!(delimited!(char!('#'), is_not!("\n"), alt!(is_a!("\n") | eof ))) |
                recognize!(delimited!(tag!("/*"), take_until!("*/"), tag!("*/")))
            )) >>
            (non_comment)
        )
    )
);

#[inline]
fn eof(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if !input.is_empty() {
        IResult::Done(input, input)
    } else {
        IResult::Error(nom::Err::Position(ErrorKind::Eof, input))
    }
}

fn comment_tag(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut comment = false;
    let mut idx = 0_usize;
    for (i, c) in input.iter().enumerate() {
        if *c == b'#' {
            idx = i;
            break;
        } else if *c == b'/' {
            comment = true;
        } else if comment {
            if *c != b'*' {
                comment = false;
            } else {
                idx = i - 1;
                break;
            }
        }
    }
    if idx == 0 {
        IResult::Done(EMPTY, &input[0..])
    } else {
        IResult::Done(&input[idx..], &input[0..idx])
    }
}

// white spaces and newlines parsing tools:
fn remove_multispace(input: &[u8]) -> Option<&[u8]> {
    let trimmed = take_while!(input, is_multispace);
    match trimmed {
        IResult::Done(r, _) => Some(r),
        _ => None,
    }
}

#[inline]
fn is_multispace(chr: u8) -> bool {
    chr == b' ' || chr == b'\t' || chr == b'\r' || chr == b'\n'
}

#[cfg(test)]
mod test {
    #![allow(cyclomatic_complexity)]
    
    use super::*;
    use super::{class_decl, func_decl};
    use std::str;

    use nom;
    use nom::IResult;

    #[test]
    fn parser_ast_output() {
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
        let mut clean = vec![];
        let scanned = Parser::p1(source, &mut clean);
        assert!(scanned.is_ok());

        // split per scopes and declarations
        let source = b"
            ( american[x,u=1] && ( weapon[y,u=1] && hostile[z,u=1] ) )
        ";
        let mut clean = vec![];
        let scanned = Parser::p1(source, &mut clean);
        assert!(scanned.is_ok());

        let source = b"
            ( ( american[x,u=1] && hostile[z,u=1] ) && hostile[z,u=1] )
        ";
        let mut clean = vec![];
        let scanned = Parser::p1(source, &mut clean);
        assert!(scanned.is_ok());

        let source = b"
            ( american[x,u=1] && hostile[z,u=1] && ( weapon[y,u=1]) )
        ";
        let mut clean = vec![];
        let scanned = Parser::p1(source, &mut clean);
        assert!(scanned.is_err());

        let source = b"
            ( ( american[x,u=1] ) && hostile[z,u=1] && weapon[y,u=1] )
        ";
        let mut clean = vec![];
        let scanned = Parser::p1(source, &mut clean);
        assert!(scanned.is_err());

        let source = b"
            ( ( ( american[x,u=1] ) ) && hostile[z,u=1] && ( ( weapon[y,u=1] ) ) )
        ";
        let mut clean = vec![];
        let scanned = Parser::p1(source, &mut clean);
        assert!(scanned.is_err());

        let source = b"
            ( american[x,u=1] && ( ( hostile[z,u=1] ) ) && weapon[y,u=1] )
        ";
        let mut clean = vec![];
        let scanned = Parser::p1(source, &mut clean);
        assert!(scanned.is_err());

        let source = b"
        ((let x y) (american[x,u=1] && hostile[z,u=1]) := criminal[x,u=1])
        ((let x y) ((american[x,u=1] && hostile[z,u=1]) := criminal[x,u=1]))
        ((let x y) (american[x,u=1] && hostile[z,u=1]) := criminal[x,u=1])
        ";
        let mut clean = vec![];
        let scanned = Parser::p1(source, &mut clean);
        assert!(scanned.is_ok());
        let out = scanned.unwrap();
        assert_eq!(out.len(), 3);
    }

    macro_rules! assert_done_or_err {
        ($i:ident) => {{
            if let IResult::Error(nom::Err::Position(ref t, ref v)) = $i {
                println!("\n@error Err::{:?}: {:?}", t, unsafe {
                    str::from_utf8_unchecked(v)
                });
            }
            assert!(!$i.is_err());
        }};
    }

    #[test]
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
                comp: Some((
                    CompOperator::Equal,
                    OpArgTermBorrowed::Terminal(b"t1"),
                )),
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
                comp: Some((
                    CompOperator::Until,
                    OpArgTermBorrowed::Terminal(b"t2"),
                )),
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
