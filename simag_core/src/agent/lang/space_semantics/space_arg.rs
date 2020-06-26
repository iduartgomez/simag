use std::{convert::TryFrom, sync::Arc};

use super::SpaceFnErr;
use crate::agent::{
    lang::{
        common::ConstraintValue,
        logsent::ParseContext,
        parser::{OpArgBorrowed, UnconstraintArg},
        Operator, Var,
    },
    ParseErrF,
};
use smallvec::SmallVec;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) struct Point(u64, u64, u64);

impl TryFrom<&str> for Point {
    type Error = ParseErrF;

    fn try_from(input: &str) -> Result<Self, Self::Error> {
        let p: Result<SmallVec<[u64; 3]>, _> = input
            .split('.')
            .map(|p| p.parse())
            .collect::<Result<_, _>>();
        match p {
            Ok(val) if val.len() == 3 => Ok(Point(val[0], val[1], val[2])),
            _ => Err(ParseErrF::WrongDef),
        }
    }
}

impl TryFrom<&[u8]> for Point {
    type Error = ParseErrF;

    fn try_from(input: &[u8]) -> Result<Self, Self::Error> {
        let input_str = std::str::from_utf8(input).unwrap();
        Self::try_from(input_str)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum SpaceArg {
    AssignThisToVar(Arc<Var>),
    /// is a place declaration
    DeclSpace(Point),
    FromVarToVar(Arc<Var>, Arc<Var>),
    FromVarToVal(Arc<Var>, Point),
    FromValToVar(Point, Arc<Var>),
    FromValToVal(Point, Point),
    ToVar(Arc<Var>),
    ToVal(Point),
}
use SpaceArg::*;

impl SpaceArg {
    pub fn generate_uid(&self) -> Vec<u8> {
        vec![0]
    }
}

impl<'a> TryFrom<(&'a OpArgBorrowed<'a>, &'a ParseContext)> for SpaceArg {
    type Error = ParseErrF;

    fn try_from(input: (&'a OpArgBorrowed<'a>, &'a ParseContext)) -> Result<Self, Self::Error> {
        let mut var0 = None;
        let (other, context) = input;
        let term0 = match ConstraintValue::try_from((&other.term, context)) {
            Ok(ConstraintValue::String(space_val)) => {
                if let Some((Operator::At, _)) = other.comp {
                    // first argument is a place decl, e.g. '0.0.0'
                    Some(DeclSpace(Point::try_from(space_val.as_str())?))
                } else {
                    return Err(ParseErrF::WrongDef);
                }
            }
            Ok(ConstraintValue::Terminal(val)) => {
                // first argument is a variable
                var0 = Some(val.get_var());
                None
            }
            Ok(ConstraintValue::SpacePayload) => None,
            _ => return Err(ParseErrF::WrongDef),
        };

        let (op, term1) = match &other.comp {
            Some((Operator::At, UnconstraintArg::String(b""))) => (Operator::At, None),
            Some((Operator::SpaceAssignment, UnconstraintArg::Keyword(b"loc"))) => {
                (Operator::SpaceAssignment, None)
            }
            Some((Operator::FromTo, UnconstraintArg::Terminal(var))) => {
                let t = &UnconstraintArg::Terminal(*var);
                match ConstraintValue::try_from((t, context)) {
                    Ok(val) => (Operator::FromTo, Some(val)),
                    Err(err) => return Err(err),
                }
            }
            Some((Operator::To, UnconstraintArg::String(b""))) => (Operator::To, None),
            Some((Operator::To, _)) | Some((Operator::FromTo, _)) | Some((Operator::At, _)) => {
                return Err(SpaceFnErr::WrongDef.into())
            }
            _ => return Err(ParseErrF::WrongDef),
        };

        match (term0, var0, op, term1) {
            (Some(DeclSpace(val)), None, Operator::At, None) => {
                // at <val>
                Ok(DeclSpace(val))
            }
            (None, Some(var), Operator::SpaceAssignment, None) => {
                // <var> is this.loc
                Ok(AssignThisToVar(var))
            }
            (Some(DeclSpace(val)), None, Operator::To, None) => {
                // to <val>
                Ok(ToVal(val))
            }
            (None, Some(var), Operator::To, None) => {
                // to <var>
                Ok(ToVar(var))
            }
            (Some(DeclSpace(val0)), None, Operator::FromTo, Some(val1)) => {
                // from <val> to <val|var>
                match val1 {
                    ConstraintValue::String(val) => {
                        Ok(FromValToVal(val0, Point::try_from(val.as_str())?))
                    }
                    ConstraintValue::Terminal(var) => Ok(FromValToVar(val0, var.get_var())),
                    _ => Err(SpaceFnErr::WrongDef.into()),
                }
            }
            (None, Some(var0), Operator::FromTo, Some(val1)) => {
                // from <var> to <val|var>
                match val1 {
                    ConstraintValue::String(val) => {
                        Ok(FromVarToVal(var0, Point::try_from(val.as_str())?))
                    }
                    ConstraintValue::Terminal(var1) => Ok(FromVarToVar(var0, var1.get_var())),
                    _ => Err(SpaceFnErr::WrongDef.into()),
                }
            }
            _ => Err(SpaceFnErr::WrongDef.into()),
        }
    }
}
