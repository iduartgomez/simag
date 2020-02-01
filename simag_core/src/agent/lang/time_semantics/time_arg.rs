use std::collections::HashMap;
use std::convert::TryFrom;
use std::sync::Arc;

use super::*;
use crate::agent::kb::bms::BmsWrapper;
use crate::agent::lang::{
    common::{OpArg, OpArgTerm},
    logsent::ParseContext,
    parser::{CompOperator, OpArgTermBorrowed},
    *,
};
use crate::TIME_EQ_DIFF;
use chrono::{DateTime, Duration, Utc};

pub(in crate::agent::lang) struct TimeArg<'a>(&'a OpArg);

impl<'a> TimeArg<'a> {
    pub fn compare_time_args(&self, assignments: &HashMap<&Var, Arc<BmsWrapper>>) -> bool {
        let (term, op, comp) = match *self.0 {
            OpArg::Generic(ref term, Some((ref op, ref comp))) => (term, op, comp),
            _ => return false,
        };

        let var0 = term.get_var_ref();
        let var1 = comp.get_var_ref();
        let arg0 = assignments.get(&*var0).unwrap().get_last_date();
        let arg1 = assignments.get(&*var1).unwrap().get_last_date();

        match *op {
            CompOperator::Equal => {
                let comp_diff = Duration::seconds(TIME_EQ_DIFF);
                let lower_bound = arg0 - comp_diff;
                let upper_bound = arg0 + comp_diff;
                !((arg1 < lower_bound) || (arg1 > upper_bound))
            }
            CompOperator::More => arg0 > arg1,
            CompOperator::Less => arg0 < arg1,
            CompOperator::MoreEqual => {
                let comp_diff = Duration::seconds(TIME_EQ_DIFF);
                let lower_bound = arg0 - comp_diff;
                let upper_bound = arg0 + comp_diff;
                !((arg1 < lower_bound) || (arg1 > upper_bound)) || arg0 > arg1
            }
            CompOperator::LessEqual => {
                let comp_diff = Duration::seconds(TIME_EQ_DIFF);
                let lower_bound = arg0 - comp_diff;
                let upper_bound = arg0 + comp_diff;
                !((arg1 < lower_bound) || (arg1 > upper_bound)) || arg0 < arg1
            }
            CompOperator::Until | CompOperator::At | CompOperator::FromUntil => unreachable!(),
        }
    }

    pub fn get_time_payload(
        &self,
        assignments: &HashMap<&Var, Arc<BmsWrapper>>,
        value: Option<f32>,
    ) -> BmsWrapper {
        let bms = BmsWrapper::new(false);
        match self.0 {
            OpArg::TimeDecl(TimeFn::Time(payload)) => {
                bms.new_record(Some(*payload), value, None);
            }
            OpArg::TimeDecl(TimeFn::Now) => {
                bms.new_record(None, value, None);
            }
            OpArg::TimeDecl(TimeFn::Interval(time0, time1)) => {
                bms.new_record(Some(*time0), value, None);
                bms.new_record(Some(*time1), None, None);
            }
            OpArg::TimeVarAssign(var) => {
                let assignment = &**(assignments.get(&**var).unwrap());
                return assignment.clone();
            }
            _ => unimplemented!(),
        }
        bms
    }

    pub fn time_payload(
        other: Option<&(CompOperator, OpArgTermBorrowed<'a>)>,
        context: &ParseContext,
    ) -> Result<(CompOperator, OpArgTerm), ParseErrF> {
        match other {
            None => Ok((CompOperator::Equal, OpArgTerm::TimePayload(TimeFn::IsVar))),
            Some(&(ref op, ref term)) => {
                if !op.is_time_assignment() {
                    return Err(TimeFnErr::NotAssignment.into());
                }
                match *term {
                    OpArgTermBorrowed::String(slice) => {
                        let time = TimeFn::from_str(slice)?;
                        Ok((CompOperator::Equal, OpArgTerm::TimePayload(time)))
                    }
                    OpArgTermBorrowed::Terminal(_) => {
                        let var = common::op_arg_term_from_borrowed(&term, context)?;
                        if var.is_var() {
                            Ok((CompOperator::Equal, var))
                        } else {
                            Err(TimeFnErr::IsNotVar.into())
                        }
                    }
                }
            }
        }
    }
}

impl<'a> TryFrom<&'a OpArg> for TimeArg<'a> {
    type Error = &'static str;

    fn try_from(op_arg: &'a OpArg) -> Result<Self, Self::Error> {
        match op_arg {
            OpArg::TimeVar |
            OpArg::TimeDecl(_) |
            OpArg::TimeVarAssign(_) |
            OpArg::TimeVarFrom(_) |
            // TimeVarUntil(_),
            OpArg::TimeVarFromUntil(_, _) => Ok(TimeArg(op_arg)),
            _ => Err("not valid")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum TimeFn {
    Now,
    Time(Time),
    /// Time interval for value decl, in the form of [t0,t1)
    Interval(Time, Time),
    IsVar,
}

impl TimeFn {
    pub fn from_str(slice: &[u8]) -> Result<TimeFn, ParseErrF> {
        if slice == b"now" {
            Ok(TimeFn::Now)
        } else {
            let s = std::str::from_utf8(slice).unwrap();
            match DateTime::parse_from_rfc3339(s) {
                Err(_e) => Err(TimeFnErr::WrongFormat(s.to_owned()).into()),
                Ok(time) => Ok(TimeFn::Time(time.with_timezone(&Utc))),
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
            TimeFn::Time(ref payload) => {
                bms.new_record(Some(*payload), value, None);
            }
            TimeFn::Now => {
                bms.new_record(None, value, None);
            }
            _ => panic!(),
        }
        bms
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        let mut id = vec![];
        match self {
            TimeFn::Time(time) => {
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
        }
        id
    }
}
