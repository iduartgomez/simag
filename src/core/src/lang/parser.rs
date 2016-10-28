use std::str;
use std::str::FromStr;

use nom::{IResult, ErrorKind};
use nom::{is_digit, is_alphanumeric, eof};
use nom;


use super::ParseErr;

const ICOND_OP: &'static [u8] = b"|>";
const AND_OP: &'static [u8] = b"&&";
const OR_OP: &'static [u8] = b"||";
const IFF_OP: &'static [u8] = b"<=>";
const IMPL_OP: &'static [u8] = b"=>";

pub struct Parser;
impl Parser {
    pub fn parse(input: &[u8]) -> Result<AST, ParseErr> {
        // let mut p2 = Vec::new();
        // Lexer::scan(input, &mut p2);
        // parse and walk the AST:
        let ast = AST::Stmt;
        Ok(ast)
    }
}

#[derive(Debug)]
pub enum AST {
    Stmt,
    Rule,
    Assertion,
    Query,
    Expr,
}

struct Lexer;

impl Lexer {
    /// Checks that the input is valid and removes unnecessary tokens.
    fn scan<'a>(input: &'a [u8], p2: &'a mut Vec<u8>) -> Result<NextScope<'a>, ParseErr<'a>> {
        // TODO: when stable on the std lib, check that bytes can be encoded as correct chars
        let p1 = match remove_comments(input) {
            IResult::Done(_, done) => done,
            IResult::Error(nom::Err::Position(_, _)) => return Err(ParseErr::Comments),
            _ => return Err(ParseErr::Failure),
        };
        for v in p1 {
            for b in v {
                p2.push(*b)
            }
        }
        // separate by scopes; everything inside a scope is either an other scope,
        // a var declaration, an operator or a terminal node;
        // scopes are linked by logical operators, and the terminal nodes contain
        // function and/or class declarations (which can be chained by operators)
        match scope(&p2[..]) {
            IResult::Done(_, d) => Ok(d),
            IResult::Error(nom::Err::Position(t, p)) => {
                match (t, p) {
                    (ErrorKind::Custom(0), p) => Err(ParseErr::NonTerminal(p)),
                    (ErrorKind::Custom(1), p) => Err(ParseErr::NonNumber(p)),
                    (ErrorKind::Custom(4), p) => Err(ParseErr::UnclosedComment(p)),
                    (ErrorKind::Custom(11), p) => Err(ParseErr::NotScope(p)),
                    (ErrorKind::Custom(12), p) => Err(ParseErr::UnbalancedDelimiter(p)),
                    (ErrorKind::Custom(15), p) => Err(ParseErr::IllegalChain(p)),
                    _ => Err(ParseErr::Failure),
                }
            }
            _ => Err(ParseErr::Failure),
        }
    }
}

#[derive(Debug)]
struct Scope<'a> {
    next: NextScope<'a>,
    vars: Option<Vec<VarDecl<'a>>>,
    logic_op: Option<LogicOperator>,
}

#[derive(Debug)]
enum NextScope<'a> {
    EndNode(EndNode<'a>),
    Scope(Box<Scope<'a>>),
    Chain(Vec<NextScope<'a>>),
    None,
}

#[derive(Debug)]
enum EndNode<'a> {
    FuncDecl(FuncDecl<'a>),
    ClassDecl(ClassDecl<'a>),
}

#[derive(Debug)]
enum VarDecl<'a> {
    Var(Var<'a>),
    Skolem(Skolem<'a>),
}

// scope disambiguation infrastructure:
fn scope<'a>(input: &'a [u8]) -> IResult<&[u8], NextScope<'a>> {
    // check that is actually an scope else fail
    let ot = tuple!(input, opt!(take_while!(is_multispace)), char!('('));
    let input = match ot {
        IResult::Done(rest, _) => rest,
        _ => return IResult::Error(nom::Err::Position(ErrorKind::Custom(11), input)),
    };
    // try to get scope vars
    let (offset, vars) = take_vars(input);
    // check for inner scopes, fail if unbalanced
    let (rest, rest_l, rest_r, is_endnode) = match check_balanced(offset, input) {
        Err(err) => return IResult::Error(err),
        Ok((rest, rest_l, rest_r, is_endnode)) => (rest, rest_l, rest_r, is_endnode),
    };
    // try to get terminal nodes from input[0..], check if it's lhs
    let subnodes: IResult<&[u8], Vec<Scope>> = many1!(rest, expand_side);
    match subnodes {
        IResult::Done(r, mut d) => {
            // lhs node, check there is nothing on the rhs or fail
            if is_endnode == false {
                let subnodes_r: IResult<&[u8], Vec<Scope>> = many1!(rest_r, expand_side);
                if subnodes_r.is_done() {
                    return IResult::Error(nom::Err::Position(ErrorKind::Custom(15), rest_r));
                }
            }
            if d.len() > 1 {
                // multiple declarations, cannot have inner scopes
                let rest_l = remove_multispace(rest_l);
                if rest_l.len() > 0 {
                    return IResult::Error(nom::Err::Position(ErrorKind::Custom(15), rest_l));
                }
                let mut v0 = Vec::new();
                for e in d.drain(..) {
                    v0.push(NextScope::Scope(Box::new(e)))
                }
                return IResult::Done(r, NextScope::Chain(v0));
            } else {
                // single decl, test for rest
                let decl = d.pop().unwrap();
                let rest_l = remove_multispace(rest_l);
                if rest_l.len() > 0 {
                    let rhs = scope(rest_l);
                    match rhs {
                        IResult::Done(r, next) => {
                            let d = NextScope::Chain(vec![NextScope::Scope(Box::new(decl)), next]);
                            return IResult::Done(r, d);
                        }
                        IResult::Error(err) => return IResult::Error(err),
                        IResult::Incomplete(err) => return IResult::Incomplete(err),
                    }
                } else {
                    return IResult::Done(r, NextScope::Scope(Box::new(decl)));
                }
            }
        }
        _ => {}
    }
    // parsing from beginning failed... check if it's a rhs
    // try to get terminal nodes from input[pcd..] // pcd = previous closing delimiter
    let subnodes: IResult<&[u8], Vec<Scope>> = many1!(rest_r, expand_side);
    match subnodes {
        IResult::Done(r0, mut d) => {
            // rhs node
            if d.len() > 1 {
                // multiple declarations, cannot have inner scopes
                let trial = scope(rest_l);
                if trial.is_done() {
                    return IResult::Error(nom::Err::Position(ErrorKind::Custom(15), rest_l));
                }
                let mut v0 = Vec::new();
                for e in d.drain(..) {
                    v0.push(NextScope::Scope(Box::new(e)))
                }
                return IResult::Done(r0, NextScope::Chain(v0));
            } else {
                let decl = d.pop().unwrap();
                // single decl, test for lhs
                let (r1, next) = match scope(rest_l) {
                    IResult::Done(r, d) => {
                        (r, NextScope::Chain(vec![d, NextScope::Scope(Box::new(decl))]))
                    }
                    IResult::Error(nom::Err::Position(ErrorKind::Custom(11), _)) => {
                        (r0, NextScope::Scope(Box::new(decl)))
                    }
                    IResult::Error(err) => return IResult::Error(err),
                    IResult::Incomplete(err) => return IResult::Incomplete(err),
                };
                return IResult::Done(r1, next);
            }
        }
        _ => {}
    }
    // not a terminal node, keep going
    let rest = remove_multispace(rest);
    if rest.len() == 0 {
        // empty scope, will ignore further down
        return IResult::Done(rest,
                             NextScope::Scope(Box::new(Scope {
                                 next: NextScope::None,
                                 vars: None,
                                 logic_op: None,
                             })));
    }
    let (rest, next) = match scope(rest_l) {
        IResult::Done(r, n) => (r, n),
        IResult::Error(err) => return IResult::Error(err),
        _ => return IResult::Error(nom::Err::Position(ErrorKind::Complete, input)),
    };
    IResult::Done(rest,
                  NextScope::Scope(Box::new(Scope {
                      next: next,
                      vars: vars,
                      logic_op: None,
                  })))
}

fn take_vars(input: &[u8]) -> (usize, Option<Vec<VarDecl>>) {
    named!(tv(&[u8]) -> Vec<Vec<VarDecl>>, many1!(chain!(
        take_while!(is_multispace)? ~
        v1: alt!(variable | skolem) ,
        || { v1 }
    )));
    let output = tv(input);
    let vars;
    let mut offset = 0_usize;
    if output.is_done() {
        let (r, mut v) = output.unwrap();
        offset = input.len() - r.len();
        let mut v0 = Vec::new();
        // flat vec
        for ref mut v1 in v.drain(..) {
            for v2 in v1.drain(..) {
                v0.push(v2);
            }
        }
        vars = Some(v0);
    } else {
        vars = None;
    }
    (offset, vars)
}

fn check_balanced(offset: usize,
                  input: &[u8])
                  -> Result<(&[u8], &[u8], &[u8], bool), nom::Err<&[u8]>> {
    let mut lp = 0;
    let mut rp = 0;
    let mut cd = 0;
    let mut nod = 0;
    let mut pcd = 0;
    let mut in_func = false;
    for (i, c) in input.iter().enumerate() {
        if *c == b'(' {
            if is_term_char(input[i - 1]) {
                in_func = true;
                continue;
            }
            lp += 1;
            if lp == 1 {
                nod = i;
            }
        }
        if *c == b')' && !in_func {
            rp += 1;
            pcd = cd;
            cd = i;
        } else if *c == b')' && in_func {
            in_func = false;
        }
    }
    if (lp != (rp - 1)) && !(lp == 0 && rp == 0) {
        return Err(nom::Err::Position(ErrorKind::Custom(12), input));
    }
    let is_endnode;
    if offset > 0 {
        // scope has var decl
        lp -= 1;
        rp -= 1;
    }
    if (lp == 0 && rp == 0) | (lp == 0 && rp == 1) {
        is_endnode = true;
    } else {
        is_endnode = false;
    }
    // if there were vars, offset input slice to ignore them
    let rest_r;
    let rest_l;
    let rest;
    if offset > 0 {
        rest = &input[offset..cd];
        if offset < pcd {
            rest_r = &input[pcd + 1..cd - 1];
        } else {
            rest_r = &input[offset..cd - 1];
        }
        if offset < nod {
            rest_l = &input[nod..pcd + 1];
        } else {
            rest_l = &input[offset..pcd + 1];
        }
    } else {
        if nod > pcd {
            return Err(nom::Err::Position(ErrorKind::Custom(15), input))
        }
        rest = &input[0..cd];
        rest_r = &input[pcd + 1..cd - 1];
        rest_l = &input[nod..pcd + 1];
    }
    Ok((rest, rest_l, rest_r, is_endnode))
}

fn expand_side<'a>(input: &'a [u8]) -> IResult<&[u8], Scope<'a>> {
    let input = remove_multispace(input);
    let out1 = logic_operator(input);
    if out1.is_done() {
        let (i2, op) = out1.unwrap();
        let out2 = is_end_node(i2);
        if out2.is_done() {
            // is 'op' + 'decl'
            let (r, decl) = out2.unwrap();
            let result = Scope {
                next: decl,
                logic_op: Some(LogicOperator::from_bytes(op)),
                vars: None,
            };
            return IResult::Done(r, result);
        } else {
            return IResult::Error(nom::Err::Position(ErrorKind::Alt, input));
        }
    } else {
        let out1 = is_end_node(input);
        if out1.is_done() {
            let (i2, decl) = out1.unwrap();
            let out2 = logic_operator(i2);
            if out2.is_done() {
                // is 'decl' + 'op'
                let (r, op) = out2.unwrap();
                let result = Scope {
                    next: decl,
                    logic_op: Some(LogicOperator::from_bytes(op)),
                    vars: None,
                };
                return IResult::Done(r, result);
            } else {
                // is 'decl'
                let result = Scope {
                    next: decl,
                    logic_op: None,
                    vars: None,
                };
                return IResult::Done(i2, result);
            }
        }
    }
    IResult::Error(nom::Err::Position(ErrorKind::Alt, input))
}

fn is_end_node(input: &[u8]) -> IResult<&[u8], NextScope> {
    let f = func_decl(input);
    if f.is_done() {
        let (r, fun) = f.unwrap();
        return IResult::Done(r, NextScope::EndNode(EndNode::FuncDecl(fun)));
    }
    let c = class_decl(input);
    if c.is_done() {
        let (r, cls) = c.unwrap();
        return IResult::Done(r, NextScope::EndNode(EndNode::ClassDecl(cls)));
    }
    match f {
        IResult::Error(err) => IResult::Error(err),
        IResult::Incomplete(err) => IResult::Incomplete(err),
        _ => panic!(),
    }
}

// skol_decl = '(' 'exists' $(term[':'op_arg]),+ ')' ;
#[derive(Debug)]
struct Skolem<'a> {
    name: Terminal<'a>,
    op_arg: Option<OpArg<'a>>,
}

named!(skolem(&[u8]) -> Vec<VarDecl>, chain!(
    tag!("(") ~
    take_while!(is_multispace)? ~
    tag!("exists ") ~
    vars: fold_many1!(
        chain!(
            take_while!(is_multispace)? ~
            name: terminal ~
            oa: chain!(tag!(":") ~ oa: op_arg, ||{oa})? ~
            take_while!(is_multispace)? ~
            tag!(",")?,
            || { (name, oa) }
        ), Vec::new(), |mut vec: Vec<_>, (name, oa)| {
            let v = Skolem {
                name: Terminal::from_slice(name),
                op_arg: oa
            };
            vec.push(VarDecl::Skolem(v));
            vec
        }
    ) ~
    take_while!(is_multispace)? ~
    tag!(")") ,
    || { vars }
));

// var_decl = '(' 'let' $(term[':'op_arg]),+ ')' ;
#[derive(Debug)]
struct Var<'a> {
    name: Terminal<'a>,
    op_arg: Option<OpArg<'a>>,
}

named!(variable(&[u8]) -> Vec<VarDecl>, chain!(
    tag!("(") ~
    take_while!(is_multispace)? ~
    tag!("let ") ~
    vars: fold_many1!(
        chain!(
            take_while!(is_multispace)? ~
            name: terminal ~
            oa: chain!(tag!(":") ~ oa: op_arg, ||{oa})? ~
            take_while!(is_multispace)? ~
            tag!(",")?,
            || { (name, oa) }
        ), Vec::new(), |mut vec: Vec<_>, (name, oa)| {
            let v = Var {
                name: Terminal::from_slice(name),
                op_arg: oa
            };
            vec.push(VarDecl::Var(v));
            vec
        }
    ) ~
    take_while!(is_multispace)? ~
    tag!(")") ,
    || { vars }
));

// func_decl = 'fn::' term ['(' op_args ')'] args
// 			 | 'fn::' term '(' op_args ')' ;
#[derive(Debug, PartialEq)]
struct FuncDecl<'a> {
    name: Terminal<'a>,
    args: Option<Vec<Arg<'a>>>,
    op_args: Option<Vec<OpArg<'a>>>,
    variant: FuncVariants,
}

#[derive(Debug, PartialEq, Eq)]
enum FuncVariants {
    Relational,
    NonRelational,
}

named!(func_decl(&[u8]) -> FuncDecl,
    alt_complete!(
        chain!(
            take_while!(is_multispace)? ~
            tag!("fn::") ~
            name: map!(terminal, Terminal::from_slice) ~
            op1: op_args? ~
            a1: args,
            || {
                FuncDecl {
                    name: name,
                    args: Some(a1),
                    op_args: op1,
                    variant: FuncVariants::Relational
                }
            }
        ) |
        chain!(
            take_while!(is_multispace)? ~
            tag!("fn::") ~
            name: map!(terminal, Terminal::from_slice) ~
            op1: op_args,
            || {
                FuncDecl {
                    name: name,
                    args: None,
                    op_args: Some(op1),
                    variant: FuncVariants::NonRelational
                }
            }
        )
    )
);

// class_decl = term ['(' op_args ')'] args ;
#[derive(Debug, PartialEq)]
struct ClassDecl<'a> {
    name: Terminal<'a>,
    args: Vec<Arg<'a>>,
    op_args: Option<Vec<OpArg<'a>>>,
}

named!(class_decl(&[u8]) -> ClassDecl, chain!(
    name: map!(terminal, Terminal::from_slice) ~
    op1: op_args? ~
    a1: args ,
    || { ClassDecl{name: name, op_args: op1, args: a1} }
));

// arg	= term [',' uval] ;
#[derive(Debug, PartialEq)]
struct Arg<'a> {
    term: Terminal<'a>,
    uval: Option<UVal>,
}

named!(arg <Arg>, chain!(
    take_while!(is_multispace)? ~
    term: map!(terminal, Terminal::from_slice) ~
    u0: chain!(
        take_while!(is_multispace)? ~
        char!(',') ~
        take_while!(is_multispace)? ~
        u1: uval ,
        ||{u1}
    )? ~
    take_while!(is_multispace)? ,
    || { Arg{term: term, uval: u0} }
));

// args	= '[' arg $(arg);* ']';
named!(args(&[u8]) -> Vec<Arg>, delimited!(
        char!('['),
        alt!(separated_list!(char!(';'), arg) | map!(arg, to_arg_vec)),
        char!(']')
    )
);

fn to_arg_vec(a: Arg) -> Vec<Arg> {
    vec![a]
}

// op_arg =	(string|term) [comp_op (string|term)] ;
#[derive(Debug, PartialEq)]
struct OpArg<'a> {
    term: TorS<'a>,
    comp: Option<(CompOperator, TorS<'a>)>,
}

named!(op_arg <OpArg>, chain!(
    take_while!(is_multispace)? ~
    term: alt!(
        map!(string, TorS::is_string) |
        map!(terminal, TorS::is_terminal )
    ) ~
    c1: chain!(
        take_while!(is_multispace)? ~
        c2: map!(one_of!("=<>"), CompOperator::from_char) ~
        take_while!(is_multispace)? ~
        term: alt!(
            map!(string, TorS::is_string) |
            map!(terminal, TorS::is_terminal )
        ) ~
        take_while!(is_multispace)? ,
        || { (c2, term) }
    )? ,
    || { OpArg{term: term, comp: c1} }
));

// op_args = $(op_arg),+ ;
named!(op_args(&[u8]) -> Vec<OpArg>, delimited!(
        char!('('),
        alt!(separated_list!(char!(','), op_arg) | map!(op_arg, to_op_arg_vec)),
        char!(')')
    )
);

fn to_op_arg_vec(a: OpArg) -> Vec<OpArg> {
    vec![a]
}

#[derive(Debug, PartialEq)]
enum TorS<'a> {
    Terminal(&'a [u8]),
    String(&'a [u8]),
}

impl<'a> TorS<'a> {
    fn is_string(i: &'a [u8]) -> TorS {
        TorS::String(i)
    }

    fn is_terminal(i: &'a [u8]) -> TorS {
        TorS::Terminal(i)
    }
}

// uval = 'u' comp_op number;
#[derive(Debug, PartialEq)]
struct UVal {
    op: CompOperator,
    val: Number,
}

named!(uval <UVal>, chain!(
    take_while!(is_multispace)? ~
    char!('u') ~
    take_while!(is_multispace)? ~
    op: map!(
        one_of!("=<>"),
        CompOperator::from_char
    ) ~
    take_while!(is_multispace)? ~
    val: number ,
    || { UVal{op: op, val: val} }
));

// number = -?[0-9\.]+
#[derive(Debug, PartialEq)]
enum Number {
    SignedFloat(f32),
    UnsignedFloat(f32),
    SignedInteger(i32),
    UnsignedInteger(u32),
}

fn number(input: &[u8]) -> IResult<&[u8], Number> {
    let mut signed = false;
    let mut float = false;
    let mut idx = 0_usize;
    let mut rest = input;
    if (input[0] == b'-') | (input[0] == b'+') {
        signed = true;
        rest = &input[1..];
    }
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
    match (signed, float) {
        (true, true) => {
            IResult::Done(&input[idx + 1..],
                          Number::SignedFloat(<f32>::from_str(str::from_utf8(&input[0..idx + 1])
                                  .unwrap())
                              .unwrap()))
        }
        (true, false) => {
            IResult::Done(&input[idx + 1..],
                          Number::SignedInteger(<i32>::from_str(str::from_utf8(&input[0..idx +
                                                                                         1])
                                  .unwrap())
                              .unwrap()))
        }
        (false, true) => {
            IResult::Done(&input[idx..],
                          Number::UnsignedFloat(<f32>::from_str(str::from_utf8(&input[0..idx])
                                  .unwrap())
                              .unwrap()))
        }
        (false, false) => {
            IResult::Done(&input[idx..],
                          Number::UnsignedInteger(<u32>::from_str(str::from_utf8(&input[0..idx])
                                  .unwrap())
                              .unwrap()))
        }
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
#[derive(Debug, PartialEq, Eq)]
struct Terminal<'a>(&'a [u8]);

impl<'a> Terminal<'a> {
    fn from_slice(i: &'a [u8]) -> Terminal<'a> {
        Terminal(i)
    }
}

fn terminal(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut idx = 0_usize;
    for (x, c) in input.iter().enumerate() {
        if (is_alphanumeric(*c) == true) | (*c == b'_') | ((*c == b'$') & (x == 0)) {
            idx = x + 1;
        } else if idx > 0 {
            break;
        } else {
            return IResult::Error(nom::Err::Position(ErrorKind::Custom(0), input));
        }
    }
    IResult::Done(&input[idx..], &input[0..idx])
}

fn is_term_char(c: u8) -> bool {
    if (is_alphanumeric(c) == true) | (c == b'_') | (c == b'$') {
        true
    } else {
        false
    }
}

#[derive(Debug, PartialEq, Eq)]
enum CompOperator {
    Equal,
    Less,
    More,
}

impl CompOperator {
    fn from_char(c: char) -> CompOperator {
        if c == '<' {
            CompOperator::Less
        } else if c == '>' {
            CompOperator::More
        } else {
            CompOperator::Equal
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum LogicOperator {
    ICond,
    And,
    Or,
    Implication,
    Biconditional,
}

impl LogicOperator {
    fn from_bytes(m: &[u8]) -> LogicOperator {
        if m == ICOND_OP {
            LogicOperator::ICond
        } else if m == AND_OP {
            LogicOperator::And
        } else if m == OR_OP {
            LogicOperator::Or
        } else if m == IFF_OP {
            LogicOperator::Biconditional
        } else if m == IMPL_OP {
            LogicOperator::Implication
        } else {
            panic!()
        }
    }
}

named!(logic_operator, chain!(
    take_while!(is_multispace)? ~
    op: alt!(tag!("|>") | tag!("&&") | tag!("||") | tag!("=>") | tag!("<=>")) ~
    take_while!(is_multispace)? ,
    || { op }
));

// comment parsing tools:
named!(remove_comments(&[u8]) -> Vec<&[u8]>,
    many1!(
        chain!(
            before: comment_tag ~
            comment: alt!(
                recognize!(delimited!(char!('#'), is_not!("\n"), alt!(is_a!("\n") | eof ))) |
                recognize!(delimited!(tag!("/*"), take_until_bytes!(b"*/"), tag!("*/")))
            ) ? ,
            || {
                before
             }
        )
    )
);

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
        IResult::Done(&input[0..], &input[..])
    } else {
        IResult::Done(&input[idx..], &input[0..idx])
    }
}

// white spaces and newlines parsing tools:
fn remove_multispace(input: &[u8]) -> &[u8] {
    let trimmed = take_while!(input, is_multispace);
    match trimmed {
        IResult::Done(r, _) => r,
        _ => input,
    }
}

fn is_multispace(chr: u8) -> bool {
    if chr == b' ' || chr == b'\t' || chr == b'\r' || chr == b'\n' {
        true
    } else {
        false
    }
}

#[test]
fn scanner() {
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
    let mut data = Vec::new();
    let scanned = Lexer::scan(source, &mut data);
    assert!(scanned.is_ok());

    let source = b"
        ( american[x,u=1] && ( weapon[y,u=1] && hostile[z,u=1 ) )
    ";
    let mut data = Vec::new();
    let scanned = Lexer::scan(source, &mut data);
    assert!(scanned.is_ok());

    let source = b"
        ( american[x,u=1] && hostile[z,u=1] && ( weapon[y,u=1]) )
    ";
    let mut data = Vec::new();
    let scanned = Lexer::scan(source, &mut data);
    assert!(scanned.is_err());

    let source = b"
        ( ( american[x,u=1] && hostile[z,u=1 ) && fn::criticize(t=\"now\")[$John,u=1;$Lucy] )
    ";
    let mut data = Vec::new();
    let scanned = Lexer::scan(source, &mut data);
    assert!(scanned.is_ok());

    let source = b"
        ( ( american[x,u=1] ) && fn::criticize(t=\"now\")[$John,u=1;$Lucy] && weapon[y,u=1] )
    ";
    let mut data = Vec::new();
    let scanned = Lexer::scan(source, &mut data);
    assert!(scanned.is_err());

    let source = b"
        ( ( ( american[x,u=1] ) ) && hostile[z,u=1] && ( ( weapon[y,u=1] ) ) )
    ";
    let mut data = Vec::new();
    let scanned = Lexer::scan(source, &mut data);
    assert!(scanned.is_err());
    println!("{:?}", scanned);

    let source = b"
        ( american[x,u=1] && ( ( hostile[z,u=1] ) ) && weapon[y,u=1] )
    ";
    let mut data = Vec::new();
    let scanned = Lexer::scan(source, &mut data);
    assert!(scanned.is_err());
    println!("{:?}", scanned);

    // println!("\n@error: {:?}", unsafe{ str::from_utf8_unchecked(&input[..]) });
    let source = b"
        (   ( let x y z )
            (
                ( american[x,u=1] && weapon[y,u=1] && fn::sells[y,u>0.5;x;z] && hostile[z,u=1] )
                |> criminal[x,u=1]
            )
        )
    ";
    let mut data = Vec::new();
    let scanned = Lexer::scan(source, &mut data);
    assert!(scanned.is_ok());
    let scanned = scanned.unwrap();
    match scanned {
        NextScope::Scope(s0) => {
            assert!(s0.vars.is_some());
            match s0.next {
                NextScope::Chain(s1) => {
                    assert_eq!(s1.len(), 2);
                    match s1[0] {
                        NextScope::Chain(ref s2_0) => {
                            assert_eq!(s2_0.len(), 4);
                        }
                        _ => panic!(),
                    }
                    match s1[1] {
                        NextScope::Scope(ref s2_1) => {
                            assert_eq!(s2_1.logic_op.as_ref().unwrap(),
                                       &LogicOperator::ICond);
                            match s2_1.next {
                                NextScope::EndNode(EndNode::ClassDecl(_)) => {}
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        }
        _ => panic!(),
    }
}

macro_rules! assert_done_or_err {
    ($i:ident) => {{
        match $i {
            IResult::Error(nom::Err::Position(ref t, ref v)) => {
                println!("\n@error Err::{:?}: {:?}", t, unsafe{str::from_utf8_unchecked(v)});
            },
            _ => {}
        }
        assert!(!$i.is_err());
    }}
}

#[test]
fn parse_predicate() {
    let s1 = b"professor[$Lucy,u=1]";
    let s1_res = class_decl(s1);
    assert_done_or_err!(s1_res);
    let s1_res = s1_res.unwrap().1;
    assert_eq!(s1_res.name, Terminal(b"professor"));
    assert_eq!(s1_res.args[0].term, Terminal(b"$Lucy"));
    assert!(s1_res.args[0].uval.is_some());

    let s2 = b"missile[$M1,u>-1.5]";
    let s2_res = class_decl(s2);
    assert_done_or_err!(s2_res);
    let s2_res = s2_res.unwrap().1;
    assert_eq!(s2_res.name, Terminal(b"missile"));
    assert_eq!(s2_res.args[0].term, Terminal(b"$M1"));
    let s2_uval = s2_res.args[0].uval.as_ref().unwrap();
    assert_eq!(s2_uval.op, CompOperator::More);
    assert_eq!(s2_uval.val, Number::SignedFloat(-1.5_f32));

    let s3 = b"dean(t1=\"now\",t2=t1)[$John,u=0]";
    let s3_res = class_decl(s3);
    assert_done_or_err!(s3_res);
    let s3_res = s3_res.unwrap().1;
    assert_eq!(s3_res.name, Terminal(b"dean"));
    assert_eq!(s3_res.args[0].term, Terminal(b"$John"));
    assert!(s3_res.args[0].uval.is_some());
    assert_eq!(
        s3_res.op_args.as_ref().unwrap(),
        &vec![OpArg{term: TorS::Terminal(b"t1"),
                    comp: Some((CompOperator::Equal, TorS::String(b"now")))},
              OpArg{term: TorS::Terminal(b"t2"),
                    comp: Some((CompOperator::Equal, TorS::Terminal(b"t1")))}]
    );

    let s4 = b"animal(t=\"2015.07.05.11.28\")[cow, u=1; brown, u=0.5]";
    let s4_res = class_decl(s4);
    assert_done_or_err!(s4_res);
    let s4_res = s4_res.unwrap().1;
    assert_eq!(s4_res.args[1].term, Terminal(b"brown"));
    assert!(s4_res.op_args.is_some());
    assert_eq!(s4_res.op_args.as_ref().unwrap(),
        &vec![OpArg{term: TorS::Terminal(b"t"),
                    comp: Some((CompOperator::Equal, TorS::String(b"2015.07.05.11.28")))}]);
}

#[test]
fn parse_function() {
    let s1 = b"fn::criticize(t=\"now\")[$John,u=1;$Lucy]";
    let s1_res = func_decl(s1);
    assert_done_or_err!(s1_res);
    assert_eq!(s1_res.unwrap().1.variant, FuncVariants::Relational);

    let s2 = b"fn::takes[$analysis,u>0;$Bill]";
    let s2_res = func_decl(s2);
    assert_done_or_err!(s2_res);
    let s2_res = s2_res.unwrap().1;
    assert_eq!(s2_res.name, Terminal(b"takes"));
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
