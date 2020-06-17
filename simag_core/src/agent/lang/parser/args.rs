use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::{map, opt},
    sequence::tuple,
};

use super::numbers::number;
use super::*;
use crate::agent::lang::reserved;

// arg	= term [',' uval] ;
#[derive(Debug, PartialEq)]
pub(in crate::agent) struct ArgBorrowed<'a> {
    pub term: TerminalBorrowed<'a>,
    pub uval: Option<UVal>,
}

pub(super) fn arg(input: &[u8]) -> IResult<&[u8], ArgBorrowed> {
    let res = do_parse!(
        input,
        multispace0
            >> term: map!(terminal, TerminalBorrowed::from_slice)
            >> multispace0
            >> u0: opt!(uval)
            >> (ArgBorrowed { term, uval: u0 })
    );
    res
}

// args	= '[' arg $(arg);+ ']';
pub(super) fn args(input: &[u8]) -> IResult<&[u8], Vec<ArgBorrowed>> {
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
    pub term: UnconstraintArg<'a>,
    pub comp: Option<(Operator, UnconstraintArg<'a>)>,
}

pub(super) fn op_arg(i: &[u8]) -> IResult<&[u8], OpArgBorrowed> {
    fn normal_arg(orig: &[u8]) -> IResult<&[u8], OpArgBorrowed> {
        let (i, term) = UnconstraintArg::get(orig)?;
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
            let comp = Operator::from_chars(op);
            let (i, _) = multispace0(i)?;
            let (i, term2) = UnconstraintArg::get(i)?;
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
        let (i, since) = UnconstraintArg::get(i)?;
        let (i, _) = multispace0(i)?;
        let (i, until) = opt(tuple((
            tag("until"),
            multispace0,
            UnconstraintArg::get,
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
                comp: Operator::from_time_op(term),
            },
        ))
    }

    fn var_assignment(orig: &[u8]) -> IResult<&[u8], OpArgBorrowed> {
        let (i, _) = tag("where")(orig)?;
        let (i, _) = multispace0(i)?;

        let (i, this0) = {
            let r = opt(tag("this."))(i)?;
            (r.0, r.1.is_some())
        };
        let (i, v0) = UnconstraintArg::get(i)?;
        if v0 == UnconstraintArg::ThisTime && !this0 {
            return Err(nom::Err::Error(ParseErrB::SyntaxError));
        }

        let (i, _) = multispace0(i)?;
        let (i, _) = tag("is")(i)?;
        let (i, _) = multispace0(i)?;

        let (i, this1) = {
            let r = opt(tag("this."))(i)?;
            (r.0, r.1.is_some())
        };
        let (i, v1) = UnconstraintArg::get(i)?;
        if v1 == UnconstraintArg::ThisTime && !this1 {
            return Err(nom::Err::Error(ParseErrB::SyntaxError));
        }
        Ok((
            i,
            OpArgBorrowed {
                term: v0,
                comp: Some((Operator::Assignment, v1)),
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
pub(super) fn op_args(input: &[u8]) -> IResult<&[u8], Vec<OpArgBorrowed>> {
    delimited!(
        input,
        char!('('),
        separated_list0!(char!(','), op_arg),
        char!(')')
    )
}

#[derive(PartialEq, Clone)]
pub(in crate::agent) enum UnconstraintArg<'a> {
    Terminal(&'a [u8]),
    String(&'a [u8]),
    ThisTime,
}

impl<'a, T> PartialEq<T> for UnconstraintArg<'a>
where
    T: AsRef<[u8]>,
{
    fn eq(&self, other: &T) -> bool {
        let other: &[u8] = other.as_ref();
        match self {
            UnconstraintArg::Terminal(r) => *r == other,
            UnconstraintArg::String(r) => *r == other,
            UnconstraintArg::ThisTime => false,
        }
    }
}

impl<'a> std::fmt::Debug for UnconstraintArg<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            UnconstraintArg::Terminal(r) => {
                write!(f, "OpArg::Term({})", str::from_utf8(r).unwrap())
            }
            UnconstraintArg::String(r) => write!(f, "OpArg::Str({})", str::from_utf8(r).unwrap()),
            UnconstraintArg::ThisTime => write!(f, "OpArg::ThisTime"),
        }
    }
}

impl<'a> UnconstraintArg<'a> {
    pub(super) fn get(i: &[u8]) -> IResult<&[u8], UnconstraintArg> {
        alt((
            map(string, UnconstraintArg::is_string),
            map(terminal, UnconstraintArg::is_terminal),
        ))(i)
    }

    fn is_string(i: &'a [u8]) -> UnconstraintArg {
        UnconstraintArg::String(i)
    }

    fn is_terminal(i: &'a [u8]) -> UnconstraintArg {
        match i {
            b"time" => UnconstraintArg::ThisTime,
            _ => UnconstraintArg::Terminal(i),
        }
    }

    fn is_reserved(&self) -> bool {
        match self {
            UnconstraintArg::Terminal(r) => reserved(r),
            UnconstraintArg::ThisTime => true,
            UnconstraintArg::String(_) => false,
        }
    }
}

// uval = 'u' comp_op number;
#[derive(Debug, PartialEq, Clone, Copy)]
pub(in crate::agent) struct UVal {
    pub op: Operator,
    pub val: Number,
}

fn uval(input: &[u8]) -> IResult<&[u8], UVal> {
    do_parse!(
        input,
        multispace0
            >> multispace0
            >> op: map!(
                alt!(tag!(">=") | tag!("<=") | tag!("=") | tag!(">") | tag!("<")),
                Operator::from_chars
            )
            >> multispace0
            >> val: number
            >> (UVal { op: op?.1, val })
    )
}
