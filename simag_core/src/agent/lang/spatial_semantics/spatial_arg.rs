use super::{Point, SpatialFnErr};
use crate::agent::{
    lang::{
        common::ConstraintValue,
        logsent::ParseContext,
        parser::{OpArgBorrowed, UnconstraintArg},
        Operator, Var,
    },
    ParseErrF,
};
use std::{convert::TryFrom, iter::FromIterator};

#[cfg(feature = "persistence")]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
pub(in crate::agent) enum SpatialArg {
    AssignThisToVar(Var),
    /// is a place declaration
    DeclLocation(Point),
    FromVarToVar(Var, Var),
    FromVarToVal(Var, Point),
    FromValToVar(Point, Var),
    FromValToVal(Point, Point),
    ToVar(Var),
    ToVal(Point),
}
use SpatialArg::*;

impl SpatialArg {
    pub fn generate_uid(&self) -> Vec<u8> {
        let mut d = Vec::from_iter(b"spatial_arg".iter().cloned());
        let rest = match self {
            AssignThisToVar(v) => {
                d.extend(b"_0<");
                v.generate_uid()
            }
            DeclLocation(p) => {
                d.extend(b"_1<");
                p.generate_uid()
            }
            FromVarToVar(v0, v1) => {
                d.extend(b"_2<");
                let mut v0 = v0.generate_uid();
                let v1 = v1.generate_uid();
                v0.extend(v1);
                v0
            }
            FromVarToVal(v0, v1) => {
                d.extend(b"_3<");
                let mut v0 = v0.generate_uid();
                let v1 = v1.generate_uid();
                v0.extend(v1);
                v0
            }
            FromValToVar(v0, v1) => {
                d.extend(b"_4<");
                let mut v0 = v0.generate_uid();
                let v1 = v1.generate_uid();
                v0.extend(v1);
                v0
            }
            FromValToVal(v0, v1) => {
                d.extend(b"_5<");
                let mut v0 = v0.generate_uid();
                let v1 = v1.generate_uid();
                v0.extend(v1);
                v0
            }
            ToVar(v0) => {
                d.extend(b"_6<");
                v0.generate_uid()
            }
            ToVal(p) => {
                d.extend(b"_7<");
                p.generate_uid()
            }
        };
        d.extend(rest);
        d.push(b'>');
        d
    }

    pub fn has_origin(&self) -> bool {
        match self {
            FromVarToVar(_, _) | FromVarToVal(_, _) | FromValToVar(_, _) | FromValToVal(_, _) => {
                true
            }
            _ => false,
        }
    }
}

impl<'a> TryFrom<(&'a OpArgBorrowed<'a>, &'a ParseContext)> for SpatialArg {
    type Error = ParseErrF;

    fn try_from(input: (&'a OpArgBorrowed<'a>, &'a ParseContext)) -> Result<Self, Self::Error> {
        let mut var0 = None;
        let (other, context) = input;
        let term0 = match ConstraintValue::try_from((&other.term, context)) {
            Ok(ConstraintValue::String(location_val)) => {
                if let Some((Operator::At, _)) = other.comp {
                    // first argument is a place decl, e.g. '0.0.0'
                    Some(DeclLocation(Point::try_from(location_val.as_str())?))
                } else {
                    return Err(ParseErrF::WrongDef);
                }
            }
            Ok(ConstraintValue::Terminal(val)) => {
                // first argument is a variable
                var0 = Some(val.get_var());
                None
            }
            Ok(ConstraintValue::SpatialPayload(_)) => return Err(ParseErrF::WrongDef),
            _ => return Err(ParseErrF::WrongDef),
        };

        let (op, term1) = match &other.comp {
            Some((Operator::At, UnconstraintArg::String(b""))) => (Operator::At, None),
            Some((Operator::SpatialAssignment, UnconstraintArg::Keyword(b"loc"))) => {
                (Operator::SpatialAssignment, None)
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
                return Err(SpatialFnErr::WrongDef.into())
            }
            _ => return Err(ParseErrF::WrongDef),
        };

        match (term0, var0, op, term1) {
            (Some(DeclLocation(val)), None, Operator::At, None) => {
                // at <val>
                Ok(DeclLocation(val))
            }
            (None, Some(var), Operator::SpatialAssignment, None) => {
                // <var> is this.loc
                Ok(AssignThisToVar(var))
            }
            (Some(DeclLocation(val)), None, Operator::To, None) => {
                // to <val>
                Ok(ToVal(val))
            }
            (None, Some(var), Operator::To, None) => {
                // to <var>
                Ok(ToVar(var))
            }
            (Some(DeclLocation(val0)), None, Operator::FromTo, Some(val1)) => {
                // from <val> to <val|var>
                match val1 {
                    ConstraintValue::String(val) => {
                        Ok(FromValToVal(val0, Point::try_from(val.as_str())?))
                    }
                    ConstraintValue::Terminal(var) => Ok(FromValToVar(val0, var.get_var())),
                    _ => Err(SpatialFnErr::WrongDef.into()),
                }
            }
            (None, Some(var0), Operator::FromTo, Some(val1)) => {
                // from <var> to <val|var>
                match val1 {
                    ConstraintValue::String(val) => {
                        Ok(FromVarToVal(var0, Point::try_from(val.as_str())?))
                    }
                    ConstraintValue::Terminal(var1) => Ok(FromVarToVar(var0, var1.get_var())),
                    _ => Err(SpatialFnErr::WrongDef.into()),
                }
            }
            _ => Err(SpatialFnErr::WrongDef.into()),
        }
    }
}
