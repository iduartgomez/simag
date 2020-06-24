use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    character::complete::multispace0,
    combinator::{map, opt},
    sequence::tuple,
};

use super::numbers::number;
use super::*;
use operators::OperatorKind;

// op_args = $(op_arg),* ;
pub(super) fn op_args(input: &[u8]) -> IResult<&[u8], Vec<OpArgBorrowed>> {
    let (mut prev, _) = tuple((multispace0, char('('), multispace0))(input)?;
    let mut args = vec![];
    loop {
        let rest = if let Ok((rest, arg)) = op_arg(prev) {
            args.push(arg);
            rest
        } else {
            let (rest, margs) = var_assign_arg(prev)?;
            args.extend(margs);
            rest
        };
        match char::<_, ParseErrB>(',')(rest) {
            Ok((rest, _)) => prev = rest,
            Err(_) => {
                prev = rest;
                break;
            }
        }
    }
    let (i, _) = tuple((multispace0, char(')'), multispace0))(prev)?;
    Ok((i, args))
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

// arg	= term [',' uval] ;
#[derive(Clone, Debug, PartialEq)]
pub(in crate::agent) struct ArgBorrowed<'a> {
    pub term: TerminalBorrowed<'a>,
    pub uval: Option<UVal>,
}

pub(super) fn arg(input: &[u8]) -> IResult<&[u8], ArgBorrowed> {
    do_parse!(
        input,
        multispace0
            >> term: map!(terminal, TerminalBorrowed::from_slice)
            >> multispace0
            >> u0: opt!(uval)
            >> (ArgBorrowed { term, uval: u0 })
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
            return Err(nom::Err::Error(ParseErrB::NonTerminal(EMPTY, orig)));
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
                return Err(nom::Err::Error(ParseErrB::NonTerminal(orig, i)));
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
        let (i, first_tag) = alt((tag("since"), tag("at")))(i)?;
        let (i, _) = multispace0(i)?;
        let (i, since) = UnconstraintArg::get(i)?;
        let (i, _) = multispace0(i)?;
        let (i, until) = opt(tuple((
            tag("until"),
            multispace0,
            UnconstraintArg::get,
            multispace0,
        )))(i)?;

        if first_tag == b"at" && until.is_some() {
            return Err(nom::Err::Error(ParseErrB::SyntaxError));
        }

        let comp = if let Some((.., term, _)) = until {
            Operator::from_time_op(Some(term), OperatorKind::TimeFn)
        } else if first_tag == b"since" {
            Operator::from_time_op(None, OperatorKind::TimeFn)
        } else {
            Some((Operator::Until, UnconstraintArg::String(EMPTY)))
        };

        Ok((i, OpArgBorrowed { term: since, comp }))
    }

    fn space_arg(i: &[u8]) -> IResult<&[u8], OpArgBorrowed> {
        let (i, from) = opt(tuple((
            tag("from"),
            multispace0,
            UnconstraintArg::get,
            multispace0,
        )))(i)?;
        let (i, _) = tag("to")(i)?;
        let (i, (_, to, ..)) = tuple((multispace0, UnconstraintArg::get, multispace0))(i)?;

        let term = if let Some((.., term, _)) = from {
            Some(term)
        } else {
            None
        };

        Ok((
            i,
            OpArgBorrowed {
                term: to,
                comp: Operator::from_time_op(term, OperatorKind::SpaceFn),
            },
        ))
    }

    let (i, _) = multispace0(i)?;
    if let Ok((rest, arg)) = normal_arg(i) {
        return Ok((rest, arg));
    }

    if let Ok((rest, arg)) = space_arg(i) {
        return Ok((rest, arg));
    }

    match time_arg(i) {
        Ok((rest, arg)) => Ok((rest, arg)),
        Err(err) => Err(err),
    }
}

fn var_assign_arg(orig: &[u8]) -> IResult<&[u8], Vec<OpArgBorrowed>> {
    let mut args = vec![];
    let (mut rest, _) = tag("where")(orig)?;
    loop {
        let (i, _) = multispace0(rest)?;
        let (i, v0) = UnconstraintArg::get(i)?;
        let (i, _) = tuple((multispace0, tag("is"), multispace0))(i)?;
        let (i, (.., v1)) = tuple((tag("this."), UnconstraintArg::get))(i)?;
        let op;
        match v1 {
            UnconstraintArg::Keyword(b"time") => op = Operator::TimeAssignment,
            UnconstraintArg::Keyword(b"loc") => op = Operator::SpaceAssignment,
            _ => return Err(nom::Err::Error(ParseErrB::SyntaxError)),
        }

        args.push(OpArgBorrowed {
            term: v0,
            comp: Some((op, v1)),
        });

        match tuple((multispace0, tag("and"), multispace0))(i) {
            Ok((i, _)) => rest = i,
            Err::<_, nom::Err<ParseErrB>>(_) => {
                rest = i;
                break;
            }
        }
    }
    Ok((rest, args))
}

#[derive(PartialEq, Clone)]
pub(in crate::agent) enum UnconstraintArg<'a> {
    Keyword(&'a [u8]),
    Terminal(&'a [u8]),
    String(&'a [u8]),
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
            UnconstraintArg::Keyword(kw) => *kw == other,
        }
    }
}

impl<'a> std::fmt::Debug for UnconstraintArg<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            UnconstraintArg::Terminal(r) => write!(f, "Arg::Term({})", str::from_utf8(r).unwrap()),
            UnconstraintArg::String(r) => write!(f, "Arg::Str({})", str::from_utf8(r).unwrap()),
            UnconstraintArg::Keyword(r) => {
                write!(f, "Arg::Keyword({})", str::from_utf8(r).unwrap())
            }
        }
    }
}

impl<'a> UnconstraintArg<'a> {
    pub(super) fn get(i: &[u8]) -> IResult<&[u8], UnconstraintArg> {
        alt((
            map(terminal, UnconstraintArg::Terminal),
            map(string, UnconstraintArg::String),
            map(is_keyword, UnconstraintArg::Keyword),
        ))(i)
    }

    fn is_reserved(&self) -> bool {
        match self {
            UnconstraintArg::Keyword(_) => true,
            UnconstraintArg::String(_) | UnconstraintArg::Terminal(_) => false,
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
