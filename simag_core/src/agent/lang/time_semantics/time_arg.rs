use std::collections::HashMap;
use std::convert::TryFrom;
use std::sync::Arc;

use super::*;
use crate::agent::kb::bms::BmsWrapper;
use crate::agent::lang::{
    common::ConstraintValue,
    logsent::ParseContext,
    parser::{Operator, UnconstraintArg},
    *,
};
use chrono::{DateTime, Utc};
use parser::OpArgBorrowed;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum TimeArg {
    TimeVar,
    /// is a time declaration
    TimeDecl(TimeFn),
    TimeVarAssign(Arc<Var>),
    TimeVarSince(Arc<Var>),
    // TimeVarUntil(Arc<Var>),
    TimeVarSinceUntil(Arc<Var>, Arc<Var>),
}
use TimeArg::*;

impl TimeArg {
    #[inline]
    pub fn contains_payload(&self) -> bool {
        match self {
            TimeDecl(_) | TimeVarAssign(_) => true,
            _ => false,
        }
    }

    pub fn get_time_payload(
        &self,
        assignments: &HashMap<&Var, Arc<BmsWrapper>>,
        value: Option<f32>,
    ) -> BmsWrapper {
        let bms = BmsWrapper::new(false);
        match self {
            TimeDecl(TimeFn::Since(payload)) => {
                bms.new_record(Some(*payload), value, None);
            }
            TimeDecl(TimeFn::Now) => {
                bms.new_record(None, value, None);
            }
            TimeDecl(TimeFn::Interval(time0, time1)) => {
                bms.new_record(Some(*time0), value, None);
                bms.new_record(Some(*time1), None, None);
            }
            TimeVarAssign(var) | TimeVarSince(var) => {
                let assignment = &**(assignments.get(&**var).unwrap());
                return assignment.clone();
            }
            _ => unreachable!(format!(
                "SIMAG - {}:{} - unreachable: can't get time payload from a free variable",
                file!(),
                line!()
            )),
        }
        bms
    }

    pub fn time_payload_value(
        other: Option<&(Operator, UnconstraintArg)>,
        context: &ParseContext,
    ) -> Result<ConstraintValue, ParseErrF> {
        match other {
            None => Ok(ConstraintValue::TimePayload(TimeFn::IsVar)),
            Some(&(ref op, ref term)) => {
                if !op.is_time_assignment() {
                    return Err(TimeFnErr::NotAssignment.into());
                }
                match term {
                    UnconstraintArg::String(slice) => {
                        let time = TimeFn::from_str(slice, *op)?;
                        Ok(ConstraintValue::TimePayload(time))
                    }
                    UnconstraintArg::Terminal(_) => {
                        let var = ConstraintValue::try_from((term, context))?;
                        if var.is_var() {
                            Ok(var)
                        } else {
                            Err(TimeFnErr::IsNotVar.into())
                        }
                    }
                    UnconstraintArg::Keyword(_) => Err(TimeFnErr::IsNotVar.into()),
                }
            }
        }
    }

    pub fn contains_var(&self, var: &Var) -> bool {
        match self {
            TimeVarAssign(this_var) | TimeVarSince(this_var) => var == &**this_var,
            TimeVarSinceUntil(v0, v1) => &**v0 == var || &**v1 == var,
            _ => false,
        }
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        match self {
            TimeDecl(decl) => decl.generate_uid(),
            TimeVarAssign(var) | TimeVarSince(var) => {
                format!("{:?}", var.as_ref() as *const Var).into_bytes()
            }
            TimeVarSinceUntil(v0, v1) => {
                let mut id = format!("{:?}", v0.as_ref() as *const Var).into_bytes();
                id.append(&mut format!("{:?}", v1.as_ref() as *const Var).into_bytes());
                id
            }
            TimeVar => vec![2],
        }
    }

    pub fn var_substitution(&mut self) -> Result<(), ParseErrF> {
        match self {
            TimeVarSinceUntil(var0, var1) => {
                let mut var0_time = var0.get_times();
                let var1_time = var1.get_times();
                var0_time.merge_from_until(&var1_time)?;
                let mut assignment = TimeDecl(TimeFn::from_bms(&var0_time)?);
                std::mem::swap(&mut assignment, self);
            }
            TimeVarSince(var0) => {
                let var0_time = var0.get_times();
                let mut assignment = TimeDecl(TimeFn::from_bms(&var0_time)?);
                std::mem::swap(&mut assignment, self);
            }
            _ => {}
        }
        Ok(())
    }
}

impl From<Arc<Var>> for TimeArg {
    fn from(var: Arc<Var>) -> Self {
        TimeArg::TimeVarSince(var)
    }
}

impl From<(Arc<Var>, Arc<Var>)> for TimeArg {
    fn from(vars: (Arc<Var>, Arc<Var>)) -> Self {
        TimeArg::TimeVarSinceUntil(vars.0, vars.1)
    }
}
impl<'a> TryFrom<(&'a OpArgBorrowed<'a>, &'a ParseContext)> for TimeArg {
    type Error = ParseErrF;

    fn try_from(input: (&'a OpArgBorrowed<'a>, &'a ParseContext)) -> Result<Self, Self::Error> {
        let (other, context) = input;
        match ConstraintValue::try_from((&other.term, context)) {
            Ok(ConstraintValue::TimePayload(TimeFn::ThisTime)) => {
                let val = TimeArg::time_payload_value(other.comp.as_ref(), context)?;
                if val.is_var() {
                    Ok(TimeArg::TimeVarAssign(val.get_var()))
                } else {
                    match val {
                        ConstraintValue::TimePayload(TimeFn::IsVar) => Ok(TimeArg::TimeVar),
                        ConstraintValue::TimePayload(load) => Ok(TimeArg::TimeDecl(load)),
                        _ => Err(ParseErrF::WrongDef),
                    }
                }
            }
            Ok(ConstraintValue::String(time_val)) => {
                if let Some((op, _)) = other.comp {
                    let t = TimeFn::from_str(time_val.as_bytes(), op)?;
                    Ok(TimeArg::TimeDecl(t))
                } else {
                    Err(TimeFnErr::IsNotVar.into())
                }
            }
            Ok(ConstraintValue::TimePayload(load)) => Ok(TimeArg::TimeDecl(load)),
            Ok(_) => Err(TimeFnErr::IsNotVar.into()),
            Err(err) => Err(err),
        }
    }
}

/// Used for instantiating time values during runtime.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum TimeFn {
    ThisTime,
    /// Instantiate to current value.
    Now,
    /// Applies since the declared instant in time.
    Since(Time),
    /// Time interval for value decl, in the form of [t0,t1).
    Interval(Time, Time),
    /// Used whenever a variable is qualified as a time type variable. This variable will be replaced
    /// by a value during rule evaluation according to the definition of the rule.
    IsVar,
}

impl TimeFn {
    pub fn from_str(slice: &[u8], op: Operator) -> Result<TimeFn, ParseErrF> {
        if slice == b"now" {
            Ok(TimeFn::Now)
        } else {
            let s = std::str::from_utf8(slice).unwrap();
            if let Ok(time) = DateTime::parse_from_rfc3339(s) {
                match op {
                    Operator::Since => Ok(TimeFn::Since(time.with_timezone(&Utc))),
                    _ => Err(TimeFnErr::OperatorNotValid.into()),
                }
            } else {
                Err(TimeFnErr::WrongFormat(s.to_owned()).into())
            }
        }
    }

    /// Get a time interval from a bmswrapper, ie. created with the
    /// merge_from_until method.
    pub fn from_bms(rec: &BmsWrapper) -> Result<TimeFn, ParseErrF> {
        let values: Vec<_> = rec.iter_values().map(|(t, _)| t).collect();
        if values.len() != 2 {
            return Err(ParseErrF::TimeFnErr(TimeFnErr::IllegalSubstitution));
        }
        Ok(TimeFn::Interval(values[0], values[1]))
    }

    pub fn get_time_payload(&self, value: Option<f32>) -> BmsWrapper {
        let bms = BmsWrapper::new(false);
        match *self {
            TimeFn::Since(ref payload) => {
                bms.new_record(Some(*payload), value, None);
            }
            TimeFn::Now => {
                bms.new_record(None, value, None);
            }
            _ => unreachable!(),
        }
        bms
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        let mut id = vec![];
        match self {
            TimeFn::Since(time) => {
                id.push(0);
                id.append(&mut format!("{}", time).into_bytes());
            }
            TimeFn::Interval(time0, time1) => {
                id.push(1);
                id.append(&mut format!("{}", time0).into_bytes());
                id.append(&mut format!("{}", time1).into_bytes());
            }
            TimeFn::Now => id.push(2),
            TimeFn::IsVar => id.push(3),
            TimeFn::ThisTime => id.push(4),
        }
        id
    }
}

impl<'a> TryFrom<&'a ConstraintValue> for TimeFn {
    type Error = ParseErrF;

    fn try_from(val: &'a ConstraintValue) -> Result<Self, Self::Error> {
        match val {
            ConstraintValue::TimePayload(val) => Ok(val.clone()),
            _ => Err(ParseErrF::FailedConversion("Time OpArg")),
        }
    }
}
