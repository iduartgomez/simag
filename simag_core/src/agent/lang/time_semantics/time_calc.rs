use std::collections::HashMap;
use std::sync::Arc;

use super::TimeFnErr;
use crate::agent::{
    kb::bms::BmsWrapper,
    lang::{
        common::{ConstraintValue, OpArg},
        logsent::ParseContext,
        parser::{FuncDeclBorrowed, TerminalBorrowed},
        CompOperator, Var,
    },
    ParseErrF,
};
use crate::TIME_EQ_DIFF;
use chrono::Duration;

/// Special built-in function for time calculus.
#[derive(Debug, Clone)]
pub(in crate::agent) struct TimeCalc {
    var0: ConstraintValue,
    var1: ConstraintValue,
    op: CompOperator,
}

impl TimeCalc {
    #[inline]
    fn new(other: &FuncDeclBorrowed, context: &mut ParseContext) -> Result<TimeCalc, ParseErrF> {
        if other.args.is_some() || other.op_args.is_none() {
            return Err(ParseErrF::WrongDef);
        }
        match other.op_args {
            Some(ref oargs) => {
                // a time_calc func can only be formed from a comparison arguments formed by two vars
                // e.g.: (v0 = v1)
                let oa = &oargs[0];
                let arg = OpArg::from(oa, context)?;
                let (var0, op, var1) = match arg {
                    OpArg::Generic(v0, Some((op, v1))) => {
                        if (!v0.is_var() | !v1.is_var())
                            || (!v0.get_var_ref().is_time_var() | !v1.get_var_ref().is_time_var())
                        {
                            return Err(TimeFnErr::IsNotVar.into());
                        } else {
                            (v0, op, v1)
                        }
                    }
                    _ => return Err(TimeFnErr::InsufArgs.into()),
                };

                Ok(TimeCalc { var0, var1, op })
            }
            None => Err(TimeFnErr::InsufArgs.into()),
        }
    }

    pub fn contains_var(&self, var: &Var) -> bool {
        for arg in &[&self.var0, &self.var1] {
            if arg.contains_var(var) {
                return true;
            }
        }
        false
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        let mut uid = b"time_calc".to_vec();
        uid.extend(self.var0.generate_uid());
        self.op.generate_uid(&mut uid);
        uid.extend(self.var1.generate_uid());
        uid
    }

    pub fn time_resolution(&self, assignments: &HashMap<&Var, Arc<BmsWrapper>>) -> bool {
        let var0 = self.var0.get_var_ref();
        let var1 = self.var1.get_var_ref();
        let arg0 = assignments.get(&*var0).unwrap().get_last_date();
        let arg1 = assignments.get(&*var1).unwrap().get_last_date();

        match self.op {
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
            CompOperator::Until
            | CompOperator::Since
            | CompOperator::SinceUntil
            | CompOperator::Assignment => unreachable!(),
        }
    }
}

impl<'a> std::convert::TryFrom<(&'a FuncDeclBorrowed<'a>, &'a mut ParseContext)> for TimeCalc {
    type Error = &'a FuncDeclBorrowed<'a>;

    fn try_from(decl: (&'a FuncDeclBorrowed, &mut ParseContext)) -> Result<Self, Self::Error> {
        let (other, context) = decl;

        if let TerminalBorrowed(b"time_calc") = other.name {
            Ok(Self::new(other, context).map_err(|_| other)?)
        } else {
            Err(other)
        }
    }
}
