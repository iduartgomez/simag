use std::str;
use std::str::FromStr;

use nom::{IResult, Err, ErrorKind};
use nom::{is_digit, is_alphanumeric};

const ICOND_OP: &'static [u8] = b"|>";
const AND_OP: &'static [u8] = b"&&";
const OR_OP: &'static [u8] = b"||";
const LOGIC_OP: (&'static [u8], &'static [u8], &'static [u8], &'static [u8]) =
    (b"<=>", b"=>", OR_OP, AND_OP);
const COMP_OP: &'static [u8] = b"=<>";

enum AST {
    Stmt,
    Rule,
    Assertion,
    Query,
}

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
// ErrorKind::Custom(1) -> Error::NonNumber
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

// ErrorKind::Custom(0) -> Error::NonTerminal
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

struct Scanner;
impl Scanner {
    /// Checks that the input is valid and removes unnecessary tokens.
    fn scan<'b>(input: &'b [u8]) -> Vec<&'b u8> {
        // TODO: when unstable on the std lib, check that bytes can be encoded as correct chars
        let mut comment = false;
        let remember = move |chr: &&u8| {
            let chr = **chr;
            if chr == b'\n' && comment {
                comment = false;
                false
            } else if chr == b' ' || chr == b'\t' || chr == b'\r' || chr == b'\n' {
                false
            } else if chr == b'#' && !comment {
                comment = true;
                false
            } else if !comment {
                true
            } else {
                false
            }
        };
        input.iter().filter(remember).collect::<Vec<_>>()
    }
}

struct Parser;
impl Parser {
    fn parse(input: &[u8]) -> AST {
        Scanner::scan(input);
        AST::Assertion
    }
}

#[test]
fn scanner() {
    let source = b"
        # one line comment
        abcd
        fghi
        8763
    ";
    let parsed = b"abcdfghi8763";
    let scanned: Vec<_> = Scanner::scan(source);
    // println!("\n@error: {:?}", unsafe{ str::from_utf8_unchecked(&scanned[..]) });
    for (i, chr) in scanned.iter().enumerate() {
        let chr = **chr;
        assert_eq!(chr, parsed[i]);
    }
}
