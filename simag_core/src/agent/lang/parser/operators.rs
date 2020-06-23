use super::*;
use nom::{branch::alt, bytes::complete::tag};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[allow(unused)]
pub(in crate::agent) enum Operator {
    // equality operators:
    Equal,
    Less,
    More,
    MoreEqual,
    LessEqual,
    // time operators:
    /// used for asserting some time variants holds
    Since,
    Until,
    SinceUntil,
    /// used whenever a time value is assigned in a declaration from `this`
    TimeAssignment,
    // space operators:
    /// used whenever a space value is assigned in a declaration from `this`
    SpaceAssignment,
}

impl Operator {
    pub(super) fn from_chars(c: &[u8]) -> IResult<&[u8], Operator> {
        match c {
            b"<" => Ok((EMPTY, Operator::Less)),
            b">" => Ok((EMPTY, Operator::More)),
            b"=" => Ok((EMPTY, Operator::Equal)),
            b"<=" => Ok((EMPTY, Operator::LessEqual)),
            b">=" => Ok((EMPTY, Operator::MoreEqual)),
            b"is" => Ok((EMPTY, Operator::TimeAssignment)),
            b"at" => Ok((EMPTY, Operator::SpaceAssignment)),
            _ => Err(nom::Err::Error(ParseErrB::IsNotOperator(c))),
        }
    }

    pub(super) fn from_time_op(t: Option<UnconstraintArg>) -> Option<(Operator, UnconstraintArg)> {
        if let Some(term) = t {
            Some((Operator::SinceUntil, term))
        } else {
            Some((Operator::Since, UnconstraintArg::String(b"")))
        }
    }

    #[inline]
    pub fn is_equal(self) -> bool {
        match self {
            Operator::Equal => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_more(self) -> bool {
        match self {
            Operator::More => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_less(self) -> bool {
        match self {
            Operator::Less => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_more_eq(self) -> bool {
        match self {
            Operator::MoreEqual => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_less_eq(self) -> bool {
        match self {
            Operator::LessEqual => true,
            _ => false,
        }
    }

    pub fn generate_uid(self, id: &mut Vec<u8>) {
        match self {
            Operator::Equal => id.push(1),
            Operator::Less => id.push(2),
            Operator::More => id.push(3),
            Operator::MoreEqual => id.push(4),
            Operator::LessEqual => id.push(5),
            Operator::Until => id.push(6),
            Operator::Since => id.push(7),
            Operator::SinceUntil => id.push(8),
            Operator::TimeAssignment => id.push(9),
            Operator::SpaceAssignment => id.push(10),
        }
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operator::Equal => write!(f, "="),
            Operator::Less => write!(f, "<"),
            Operator::More => write!(f, ">"),
            Operator::MoreEqual => write!(f, ">="),
            Operator::LessEqual => write!(f, "<="),
            Operator::SpaceAssignment => write!(f, "@"),
            Operator::Until => write!(f, "<-"),
            Operator::Since => write!(f, "->"),
            Operator::SinceUntil => write!(f, "<->"),
            Operator::TimeAssignment => write!(f, "=>"),
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
    pub(super) fn from_bytes(i: &[u8]) -> IResult<&[u8], LogicOperator> {
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

pub(super) fn logic_operator(input: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        tag(ICOND_OP),
        tag(AND_OP),
        tag(OR_OP),
        tag(IMPL_OP),
        tag(IFF_OP),
    ))(input)
}
