use std::str;
use std::str::FromStr;

use nom::{IResult, Err, ErrorKind, Needed};
use nom::{is_digit, is_alphanumeric, eof};

// ErrorKind::Custom(0) -> Error::NonTerminal
// ErrorKind::Custom(1) -> Error::NonNumber
// ErrorKind::Custom(3) -> Error::InputError
// ErrorKind::Custom(4) -> Error::Unclosed comment delimiter

use super::ParseErr;

const ICOND_OP: &'static [u8] = b"|>";
const AND_OP: &'static [u8] = b"&&";
const OR_OP: &'static [u8] = b"||";
const LOGIC_OP: (&'static [u8], &'static [u8], &'static [u8], &'static [u8]) =
    (b"<=>", b"=>", OR_OP, AND_OP);
const COMP_OP: &'static [u8] = b"=<>";

pub struct Parser;
impl Parser {
    pub fn parse(input: &[u8]) -> Result<AST, ParseErr> {
        let mut p2 = Vec::new();
        LexerData::scan(input, &mut p2);
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

#[derive(Debug)]
struct LexerData<'a>(NextScope<'a>);

impl<'a> LexerData<'a> {
    /// Checks that the input is valid and removes unnecessary tokens.
    fn scan(input: &'a [u8], p2: &'a mut Vec<u8>) -> Result<LexerData<'a>, ParseErr> {
        // TODO: when stable on the std lib, check that bytes can be encoded as correct chars
        let p1 = match remove_comments(input) {
            IResult::Done(_, done) => done,
            IResult::Error(Err::Position(_, _)) => return Err(ParseErr::Comments),
            _ => return Err(ParseErr::Failure),
        };
        for v in p1 {
            for b in v {
                p2.push(*b)
            }
        }
        // separate by scopes; everything is either a scope or a var definition
        let first = scope(&p2[..]).unwrap();
        Ok(LexerData(first))
    }
}

#[derive(Debug)]
struct Scope<'a> {
    next: NextScope<'a>,
    vars: Option<Vec<Var<'a>>>,
    skol: Option<Vec<Skolem<'a>>>,
}

fn scope<'a>(input: &'a [u8]) -> Result<NextScope<'a>, ParseErr> {
    named!(take_vars(&[u8]) -> (Option<Vec<Var>>, Option<Vec<Skolem>>), chain!(
        take_while!(is_multispace)? ~
        v1: variable ? ~
        take_while!(is_multispace)? ~
        v2: skolem ? ~
        take_while!(is_multispace)? ,
        || { (v1, v2) }
    ));
    let ot = tuple!(input,
        opt!(take_while!(is_multispace)),
        tag!("("),
        opt!(take_while!(is_multispace))
    );
    let input = match ot {
        IResult::Done(rest, _) => rest,
        _ => return Err(ParseErr::Failure),
    };
    let output = take_vars(input);
    let (rest, vars, skolem) = match output {
        IResult::Done(r, (v1, v2)) => (r, v1, v2),
        _ => return Err(ParseErr::Failure),
    };
    println!("@rep: {:?}", unsafe {str::from_utf8_unchecked(&rest[..])});
    let output = is_end_node(rest);
    if output.is_done() {
        let next = match output {
            IResult::Done(_, n) => n,
            _ => return Err(ParseErr::Failure),
        };
        Ok(NextScope::Scope(Box::new(Scope {
            next: next,
            vars: vars,
            skol: skolem,
        })))
    } else {
        if input.len() == 0 {
            return Err(ParseErr::EmptyScope);
        }
        let next = match scope(rest) {
            Ok(n) => n,
            Err(err) => return Err(err),
        };
        Ok(NextScope::Scope(Box::new(Scope {
            next: next,
            vars: vars,
            skol: skolem,
        })))
    }
}

#[derive(Debug)]
enum NextScope<'a> {
    TerminalNode(EndNode<'a>),
    Scope(Box<Scope<'a>>),
}

#[derive(Debug)]
enum EndNode<'a> {
    FuncDecl(FuncDecl<'a>),
    ClassDecl(ClassDecl<'a>),
}

fn is_end_node(input: &[u8]) -> IResult<&[u8], NextScope> {
    // trim all whitespaces
    let f = func_decl(input);
    if f.is_done() {
        let (r, fun) = f.unwrap();
        return IResult::Done(r, NextScope::TerminalNode(EndNode::FuncDecl(fun)));
    }
    let c = class_decl(input);
    if c.is_done() {
        let (r, cls) = c.unwrap();
        return IResult::Done(r, NextScope::TerminalNode(EndNode::ClassDecl(cls)));
    }
    IResult::Incomplete(Needed::Unknown)
}

// skol_decl = '(' 'exists' $(term[':'op_arg]),+ ')' ;
#[derive(Debug)]
struct Skolem<'a> {
    name: Terminal<'a>,
    op_arg: Option<OpArg<'a>>,
}

named!(skolem(&[u8]) -> Vec<Skolem>, chain!(
    tag!("(") ~
    take_while!(is_multispace)? ~
    tag!("exists ") ~
    vars: fold_many1!(
            chain!(
            take_while!(is_multispace)? ~
            name: terminal ~
            oa: chain!(tag!(":") ~ oa: op_arg, ||{oa})? ~
            take_while!(is_multispace)? ~
            tag!(","),
            || { (name, oa) }
        ), Vec::new(), |mut vec: Vec<_>, (name, oa)| {
            let v = Skolem {
                name: Terminal::from_slice(name),
                op_arg: oa
            };
            vec.push(v);
            vec
        }
    ) ~
    tag!(")") ,
    || { vars }
));

// var_decl = '(' 'let' $(term[':'op_arg]),+ ')' ;
#[derive(Debug)]
struct Var<'a> {
    name: Terminal<'a>,
    op_arg: Option<OpArg<'a>>,
}

named!(variable(&[u8]) -> Vec<Var>, chain!(
    tag!("(") ~
    take_while!(is_multispace)? ~
    tag!("let ") ~
    vars: fold_many1!(
            chain!(
            take_while!(is_multispace)? ~
            name: terminal ~
            oa: chain!(tag!(":") ~ oa: op_arg, ||{oa})? ~
            take_while!(is_multispace)? ~
            tag!(","),
            || { (name, oa) }
        ), Vec::new(), |mut vec: Vec<_>, (name, oa)| {
            let v = Var {
                name: Terminal::from_slice(name),
                op_arg: oa
            };
            vec.push(v);
            vec
        }
    ) ~
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
            tag!("fn::") ~
            name: map!(terminal, Terminal::from_slice) ~
            op1: op_args? ~
            a1: args,
            || {
                FuncDecl{
                    name: name,
                    args: Some(a1),
                    op_args: op1,
                    variant: FuncVariants::Relational
                }
            }
        ) |
        chain!(
            tag!("fn::") ~
            name: map!(terminal, Terminal::from_slice) ~
            op1: op_args,
            || {
                FuncDecl{
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
    term: map!(terminal, Terminal::from_slice) ~
    u0: chain!(char!(',') ~ u1: uval, ||{u1})? ,
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

named!(op_arg <OpArg>, chain!(
    term: alt!(
        map!(string, TorS::is_string) |
        map!(terminal, TorS::is_terminal )
    ) ~
    c1: chain!(
        c2: map!(one_of!("=<>"), CompOperator::from_char) ~
        term: alt!(
            map!(string, TorS::is_string) |
            map!(terminal, TorS::is_terminal )
        ),
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

// uval = 'u' comp_op digit;
#[derive(Debug, PartialEq)]
struct UVal {
    op: CompOperator,
    val: Number,
}

named!(uval <UVal>, chain!(
    char!('u') ~
    op: map!(
        one_of!("=<>"),
        CompOperator::from_char
    ) ~
    val: number,
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
            return IResult::Error(Err::Position(ErrorKind::Custom(1), input));
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
        IResult::Error(Err::Position(ErrorKind::IsNotStr, input))
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
            return IResult::Error(Err::Position(ErrorKind::Custom(0), input));
        }
    }
    IResult::Done(&input[idx..], &input[0..idx])
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

named!(remove_comments(&[u8]) -> Vec<&[u8]>,
    many1!(
        chain!(
            before: comment_tag ~
            comment: alt!(
                recognize!(delimited!(char!('#'), is_not!("\n"), alt!(is_a!("\n") | eof ))) |
                recognize!(delimited!(tag!("/*"), take_until_bytes!(b"*/"), tag!("*/")))
            ) ? ,
            || {
                if before.len() > 0 {
                    before
                } else {
                    &b"\n"[..]
                }
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
    IResult::Done(&input[idx..], &input[0..idx])
}

fn is_multispace(chr: u8) -> bool {
    if chr == b' ' || chr == b'\t' || chr == b'\r' || chr == b'\n' {
        true
    } else {
        false
    }
}

macro_rules! assert_done_or_err {
    ($i:ident) => {{
        match $i {
            IResult::Error(Err::Position(ref t, ref v)) => {
                println!("\n@error Err::{:?}: {:?}", t, unsafe{str::from_utf8_unchecked(v)});
            },
            _ => {}
        }
        assert!(!$i.is_err());
    }}
}

#[test]
fn scanner() {
    let source = b"
        # one line comment
        ( # first scope
            ( # second scope
                (let x, y)
                professor[$Lucy,u=1]
            )
        )
        /*
            multi line
            comment
        */
    ";
    let parsed = b"{abcdfghi8763}";
    let mut data = Vec::new();
    let scanned = LexerData::scan(source, &mut data);
    // println!("\n@error: {:?}", unsafe{ str::from_utf8_unchecked(&scanned[..]) });
    //
    // for (i, chr) in scanned.iter().enumerate() {
    // let chr = **chr;
    // assert_eq!(chr, parsed[i]);
    // }
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

    let s4 = b"animal(t=\"2015.07.05.11.28\")[cow,u=1;brown,u=0.5]";
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

    let s3 = b"fn::loves[cow,u=1;bull]";
    let s3_res = func_decl(s3);
    assert_done_or_err!(s3_res);
    assert_eq!(s3_res.unwrap().1.variant, FuncVariants::Relational);

    let s4 = b"fn::time_calc(t1<t2)";
    let s4_res = func_decl(s4);
    assert_done_or_err!(s4_res);
    assert_eq!(s4_res.unwrap().1.variant, FuncVariants::NonRelational);
}
