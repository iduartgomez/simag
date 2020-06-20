use std::collections::HashMap;
use std::convert::TryFrom;
use std::sync::Arc;

use super::*;
use crate::agent::kb::bms::BmsWrapper;
use crate::agent::lang::{common::ConstraintValue, logsent::ParseContext, parser::Operator, *};
use chrono::{DateTime, Utc};
use parser::OpArgBorrowed;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum TimeArg {
    VarAssign(Arc<Var>),
    /// is a time declaration
    DeclTime(TimeFn),
    SinceVar(Arc<Var>),
    // UntilVar(Arc<Var>),
    SinceVarUntilVar(Arc<Var>, Arc<Var>),
    SinceVarUntilTime(Arc<Var>, Time),
    // SinceTimeUntilVar(Arc<Var>, Time),
    // SinceTimeUntilTime(Arc<Var>, Time),
}
use TimeArg::*;

impl TimeArg {
    #[inline]
    pub fn contains_payload(&self) -> bool {
        match self {
            DeclTime(_) => true,
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
            DeclTime(TimeFn::Since(payload)) => {
                bms.new_record(Some(*payload), value, None);
            }
            DeclTime(TimeFn::Now) => {
                bms.new_record(None, value, None);
            }
            DeclTime(TimeFn::Interval(time0, time1)) => {
                bms.new_record(Some(*time0), value, None);
                bms.new_record(Some(*time1), None, None);
            }
            SinceVar(var) => {
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

    pub fn contains_var(&self, var: &Var) -> bool {
        match self {
            SinceVar(this_var) => var == &**this_var,
            SinceVarUntilVar(v0, v1) => &**v0 == var || &**v1 == var,
            _ => false,
        }
    }

    pub fn var_substitution(&mut self) -> Result<(), ParseErrF> {
        match self {
            SinceVarUntilVar(var0, var1) => {
                let mut var0_time = var0.get_times();
                let var1_time = var1.get_times();
                var0_time.merge_from_until(&var1_time)?;
                let mut assignment = DeclTime(TimeFn::from_bms(&var0_time)?);
                std::mem::swap(&mut assignment, self);
            }
            SinceVar(var0) => {
                let var0_time = var0.get_times();
                let mut assignment = DeclTime(TimeFn::from_bms(&var0_time)?);
                std::mem::swap(&mut assignment, self);
            }
            _ => {}
        }
        Ok(())
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        match self {
            VarAssign(var) => format!("{:?}", var.as_ref() as *const Var).into_bytes(),
            DeclTime(decl) => decl.generate_uid(),
            SinceVar(var) => format!("{:?}", var.as_ref() as *const Var).into_bytes(),
            SinceVarUntilVar(v0, v1) => {
                let mut id = format!("{:?}", v0.as_ref() as *const Var).into_bytes();
                id.append(&mut format!("{:?}", v1.as_ref() as *const Var).into_bytes());
                id
            }
            SinceVarUntilTime(v0, val) => {
                let mut id = format!("{:?}", v0.as_ref() as *const Var).into_bytes();
                id.append(&mut format!("{}", val).into_bytes());
                id
            }
        }
    }
}

impl<'a> TryFrom<(&'a OpArgBorrowed<'a>, &'a ParseContext)> for TimeArg {
    type Error = ParseErrF;

    fn try_from(input: (&'a OpArgBorrowed<'a>, &'a ParseContext)) -> Result<Self, Self::Error> {
        let (other, context) = input;

        let mut var0 = None;
        let t0 = match ConstraintValue::try_from((&other.term, context)) {
            Ok(ConstraintValue::TimePayload(TimeFn::ThisTime)) => None, // first argument is: this.time
            Ok(ConstraintValue::TimePayload(_)) => unreachable!(),
            Ok(ConstraintValue::String(time_val)) => {
                if let Some((op, _)) = other.comp {
                    // first argument is a time decl, e.g. 'now'
                    let t = TimeFn::from_str(time_val.as_bytes(), op)?;
                    Some(TimeArg::DeclTime(t))
                } else {
                    return Err(TimeFnErr::IsNotVar.into());
                }
            }
            Ok(ConstraintValue::Terminal(val)) => {
                // first argument is a variable
                var0 = Some(val.get_var());
                None
            }
            Err(err) => return Err(err),
        };

        let (op, t1) = match &other.comp {
            Some((op, unc_arg)) => match ConstraintValue::try_from((unc_arg, context)) {
                Ok(t) => (*op, Some(t)),
                Err(ParseErrF::ReservedKW(kw)) => return Err(ParseErrF::ReservedKW(kw)),
                Err(err) => return Err(err),
            },
            None => (Operator::Since, None),
        };

        match (t0, var0, op, t1) {
            // where this.time is <val|var>:
            (None, None, Operator::Since, Some(val)) => {
                if val.is_var() {
                    Ok(TimeArg::SinceVar(val.get_var()))
                } else {
                    match val {
                        ConstraintValue::String(val) => {
                            let val = get_time(val)?;
                            Ok(TimeArg::DeclTime(TimeFn::Since(val)))
                        }
                        _ => Err(TimeFnErr::IsNotVar.into()),
                    }
                }
            }
            (None, None, Operator::Until, Some(_val)) => unimplemented!(),
            // since var0 until <val|var>:
            (None, Some(var0), Operator::SinceUntil, Some(val)) => {
                if val.is_var() {
                    Ok(SinceVarUntilVar(var0, val.get_var()))
                } else {
                    match val {
                        ConstraintValue::String(val) => {
                            let val = get_time(val)?;
                            Ok(SinceVarUntilTime(var0, val))
                        }
                        _ => Err(TimeFnErr::IsNotVar.into()),
                    }
                }
            }
            (None, Some(var0), Operator::Since, Some(val)) => {
                if let ConstraintValue::TimePayload(TimeFn::ThisTime) = val {
                    // where var0 is this.time, declaration of time type variable: `let <var>: time`
                    Ok(VarAssign(var0))
                } else {
                    unimplemented!()
                }
            }
            (None, Some(_var0), Operator::Until, Some(_val)) => unimplemented!(),
            _ => Err(TimeFnErr::OperatorNotValid.into()),
        }
    }
}

/// Used for instantiating time values during runtime.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum TimeFn {
    /// Instantiate to current value.
    Now,
    /// Applies since the declared instant in time.
    Since(Time),
    /// Time interval for value decl, in the form of [t0,t1).
    Interval(Time, Time),
    ThisTime,
}

fn get_time<T: AsRef<str>>(slice: T) -> Result<Time, ParseErrF> {
    if let Ok(time) = DateTime::parse_from_rfc3339(slice.as_ref()) {
        Ok(time.with_timezone(&Utc))
    } else {
        Err(TimeFnErr::WrongFormat(slice.as_ref().to_owned()).into())
    }
}

impl TimeFn {
    pub fn from_str(slice: &[u8], op: Operator) -> Result<TimeFn, ParseErrF> {
        if slice == b"now" {
            Ok(TimeFn::Now)
        } else {
            let s = std::str::from_utf8(slice).unwrap();
            let time = get_time(s)?;
            match op {
                Operator::Since => Ok(TimeFn::Since(time.with_timezone(&Utc))),
                _ => Err(TimeFnErr::OperatorNotValid.into()),
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
