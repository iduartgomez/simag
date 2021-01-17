use std::collections::HashMap;
use std::convert::TryFrom;
use std::sync::Arc;

use super::*;
use crate::agent::kb::bms::{BmsWrapper, IsTimeData};
use crate::agent::lang::{common::ConstraintValue, logsent::ParseContext, parser::Operator, *};
use chrono::{DateTime, Utc};
use parser::OpArgBorrowed;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum TimeArg {
    AssignThisToVar(Arc<Var>),
    /// is a time declaration
    DeclTime(TimeFn),
    SinceVar(Arc<Var>),
    // UntilVar(Arc<Var>),
    SinceVarUntilVar(Arc<Var>, Arc<Var>),
    SinceVarUntilTime(Arc<Var>, TimeFn),
    SinceTimeUntilVar(TimeFn, Arc<Var>),
    SinceTimeUntilTime(TimeFn, TimeFn),
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
        assignments: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
        value: Option<f32>,
    ) -> BmsWrapper<IsTimeData> {
        match self {
            DeclTime(time_fn) => time_fn.payload_from_time_arg(value),
            SinceVar(var) => {
                let assignment = &**(assignments.get(&**var).unwrap());
                assignment.clone()
            }
            SinceVarUntilVar(v0, v1) => {
                let mut v0 = (&**assignments.get(&**v0).unwrap()).clone();
                let v1 = &**(assignments.get(&**v1).unwrap());
                v0.merge_since_until(v1).unwrap();
                v0
            }
            SinceVarUntilTime(v0, t1) => {
                let mut v0 = (&**assignments.get(&**v0).unwrap()).clone();
                let t1 = t1.payload_from_time_arg(value);
                v0.merge_since_until(&t1).unwrap();
                v0
            }
            SinceTimeUntilVar(t0, v1) => {
                let mut t0 = t0.payload_from_time_arg(value);
                let v1 = (&**assignments.get(&**v1).unwrap()).clone();
                t0.merge_since_until(&v1).unwrap();
                t0
            }
            SinceTimeUntilTime(t0, t1) => {
                let mut t0 = t0.payload_from_time_arg(value);
                let t1 = t1.payload_from_time_arg(value);
                t0.merge_since_until(&t1).unwrap();
                t0
            }
            _ => unreachable!("SIMAG - can't get time payload from a free variable"),
        }
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
                let mut var0_time = var0.get_time();
                let var1_time = var1.get_time();
                var0_time.merge_since_until(&var1_time)?;
                let mut assignment = DeclTime(TimeFn::from_bms(&var0_time)?);
                std::mem::swap(&mut assignment, self);
            }
            SinceVar(var0) => {
                let var0_time = var0.get_time();
                let mut assignment = DeclTime(TimeFn::from_bms(&var0_time)?);
                std::mem::swap(&mut assignment, self);
            }
            _ => {}
        }
        Ok(())
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        let mut id = Vec::from(b"time_arg<".as_ref());
        let other = match self {
            AssignThisToVar(var) => var.generate_uid(),
            DeclTime(decl) => decl.generate_uid(),
            SinceVar(var) => var.generate_uid(),
            SinceVarUntilVar(v0, v1) => {
                let mut id = v0.generate_uid();
                id.append(&mut v1.generate_uid());
                id
            }
            SinceVarUntilTime(v0, val) => {
                let mut id = v0.generate_uid();
                id.append(&mut format!("{:?}", val).into_bytes());
                id
            }
            _ => unimplemented!(),
        };
        id.extend(other);
        id.push(b'>');
        id
    }

    pub fn is_interval(&self) -> bool {
        match self {
            SinceTimeUntilTime(_, _)
            | SinceTimeUntilVar(_, _)
            | SinceVarUntilTime(_, _)
            | SinceVarUntilVar(_, _) => true,
            _ => false,
        }
    }
}

impl<'a> TryFrom<(&'a OpArgBorrowed<'a>, &'a ParseContext)> for TimeArg {
    type Error = ParseErrF;

    fn try_from(input: (&'a OpArgBorrowed<'a>, &'a ParseContext)) -> Result<Self, Self::Error> {
        let (other, context) = input;

        let mut var0 = None;
        let term0 = match ConstraintValue::try_from((&other.term, context)) {
            Ok(ConstraintValue::TimePayload(TimeFn::ThisTime)) => None, // first argument is: this.time
            Ok(ConstraintValue::TimePayload(_)) => unreachable!(),
            Ok(ConstraintValue::String(time_val)) => {
                if let Some((Operator::Since, _)) = other.comp {
                    // first argument is a time decl, e.g. 'now'
                    let t = TimeFn::from_str(time_val.as_bytes(), Operator::Since)?;
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
            Ok(ConstraintValue::SpatialPayload(_)) => return Err(TimeFnErr::IsNotVar.into()),
            Err(err) => return Err(err),
        };

        let (op, term1) = match &other.comp {
            Some((op, unc_arg)) => match ConstraintValue::try_from((unc_arg, context)) {
                Ok(t) => (*op, Some(t)),
                // Err(ParseErrF::ReservedKW(kw)) => return Err(ParseErrF::ReservedKW(kw)),
                Err(err) => return Err(err),
            },
            None => (Operator::TimeAssignment, None),
        };

        match (term0, var0, op, term1) {
            (Some(val), None, Operator::Since, Some(empty)) => {
                // since <val>
                match empty {
                    ConstraintValue::String(empty) if empty == "" => Ok(val),
                    _ => Err(TimeFnErr::WrongDef.into()),
                }
            }
            (None, Some(var), Operator::Since, Some(empty)) => {
                // since <var>
                match empty {
                    ConstraintValue::String(empty) if empty == "" => Ok(SinceVar(var)),
                    _ => Err(TimeFnErr::WrongDef.into()),
                }
            }
            (None, Some(_var), Operator::Until, Some(_empty)) => {
                // until <var>
                unimplemented!()
            }
            (Some(_val), None, Operator::Until, Some(_empty)) => {
                // until <val>
                unimplemented!()
            }
            (None, Some(var0), Operator::SinceUntil, Some(val)) => {
                // since <var> until <val|var>
                if val.is_var() {
                    Ok(SinceVarUntilVar(var0, val.get_var()))
                } else {
                    match val {
                        ConstraintValue::String(val) => Ok(SinceVarUntilTime(
                            var0,
                            TimeFn::from_str(val.as_bytes(), Operator::Until)
                                .map_err(|_| Into::<ParseErrF>::into(TimeFnErr::WrongDef))?,
                        )),
                        _ => Err(TimeFnErr::WrongDef.into()),
                    }
                }
            }
            (Some(val0), None, Operator::SinceUntil, Some(val1)) => {
                // since <val> until <var|val>
                match val0 {
                    TimeArg::DeclTime(TimeFn::Since(t0)) => {
                        if val1.is_var() {
                            Ok(SinceTimeUntilVar(TimeFn::Since(t0), val1.get_var()))
                        } else {
                            match val1 {
                                ConstraintValue::String(val) => Ok(SinceTimeUntilTime(
                                    TimeFn::Since(t0),
                                    TimeFn::from_str(val.as_bytes(), Operator::Until).map_err(
                                        |_| Into::<ParseErrF>::into(TimeFnErr::WrongDef),
                                    )?,
                                )),
                                _ => Err(TimeFnErr::WrongDef.into()),
                            }
                        }
                    }
                    _ => Err(TimeFnErr::WrongDef.into()),
                }
            }
            (None, Some(var0), Operator::TimeAssignment, Some(val)) => {
                if let ConstraintValue::TimePayload(TimeFn::ThisTime) = val {
                    // where var0 is this.time, declaration of time type variable: `let <var>: time`
                    Ok(AssignThisToVar(var0))
                } else {
                    // where var0 is val
                    unimplemented!()
                }
            }
            (None, None, Operator::TimeAssignment, Some(val)) => {
                // where this.time is <val|var>
                if val.is_var() {
                    Ok(SinceVar(val.get_var()))
                } else {
                    match val {
                        ConstraintValue::String(val) => {
                            let val = get_time(val)
                                .map_err(|_| Into::<ParseErrF>::into(TimeFnErr::WrongDef))?;
                            Ok(DeclTime(TimeFn::Since(val)))
                        }
                        _ => Err(TimeFnErr::WrongDef.into()),
                    }
                }
            }
            (None, None, Operator::Until, Some(_val)) => {
                // where this.time until <val|var>:
                unimplemented!()
            }
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
    Until(Time),
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
                Operator::Until => Ok(TimeFn::Until(time.with_timezone(&Utc))),
                _ => Err(TimeFnErr::OperatorNotValid.into()),
            }
        }
    }

    /// Get a time interval from a bmswrapper, e.g. created with the merge_from_until method.
    pub fn from_bms(rec: &BmsWrapper<IsTimeData>) -> Result<TimeFn, ParseErrF> {
        let values = rec
            .get_time_interval()
            .map_err(|_| ParseErrF::TimeFnErr(TimeFnErr::WrongDef))?;
        Ok(TimeFn::Interval(values[0].0, values[1].0))
    }

    pub fn get_time_payload(&self, value: Option<f32>) -> BmsWrapper<IsTimeData> {
        match *self {
            TimeFn::Since(ref payload) => BmsWrapper::<IsTimeData>::new(Some(*payload), value),
            TimeFn::Now => BmsWrapper::<IsTimeData>::new(None, value),
            _ => unreachable!(),
        }
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        let mut id = vec![];
        match self {
            TimeFn::Since(time) => {
                id.push(0);
                id.append(&mut format!("{}", time).into_bytes());
            }
            TimeFn::Until(time) => {
                id.push(3);
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

    fn payload_from_time_arg(&self, value: Option<f32>) -> BmsWrapper<IsTimeData> {
        match self {
            TimeFn::Since(payload) => BmsWrapper::<IsTimeData>::new(Some(*payload), value),
            TimeFn::Now => BmsWrapper::<IsTimeData>::new(None, value),
            TimeFn::Interval(time0, time1) => {
                let mut t0 = BmsWrapper::<IsTimeData>::new(Some(*time0), value);
                let t1 = &BmsWrapper::<IsTimeData>::new(Some(*time1), None);
                t0.merge_since_until(t1)
                    .unwrap_or_else(|_| unreachable!("SIMAG - illegal merge"));
                t0
            }
            _ => unreachable!(),
        }
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
