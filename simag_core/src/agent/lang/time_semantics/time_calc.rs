use std::collections::HashMap;
use std::convert::TryFrom;
use std::sync::Arc;

use super::TimeFnErr;
use crate::agent::{
    kb::bms::{BmsWrapper, IsTimeData},
    lang::{
        built_ins::TIME_CALC_FN,
        common::OpArg,
        logsent::ParseContext,
        parser::{FuncDeclBorrowed, TerminalBorrowed},
        Operator, Var,
    },
    ParseErrF,
};
use crate::TIME_EQ_DIFF;
use chrono::Duration;

/// Special built-in function for time calculus.
#[derive(Debug, Clone)]
pub(in crate::agent) struct TimeCalcFn {
    var0: Arc<Var>,
    var1: Arc<Var>,
    op: Operator,
}

impl TimeCalcFn {
    fn new(other: &FuncDeclBorrowed, context: &mut ParseContext) -> Result<TimeCalcFn, ParseErrF> {
        if other.args.is_some() || other.op_args.is_none() {
            return Err(ParseErrF::WrongDef);
        }
        match other.op_args {
            Some(ref oargs) => {
                // a time_calc func can only be formed from a comparison arguments formed by two vars
                // e.g.: (v0 = v1)
                let oa = &oargs[0];
                let arg = OpArg::try_from((oa, &*context))?;
                let (var0, op, var1) = match arg {
                    OpArg::Generic(v0, Some((op, v1))) => {
                        if (!v0.is_var() | !v1.is_var())
                            || (!v0.get_var_ref().is_time_var() | !v1.get_var_ref().is_time_var())
                        {
                            return Err(TimeFnErr::IsNotVar.into());
                        } else {
                            (v0.get_var(), op, v1.get_var())
                        }
                    }
                    _ => return Err(TimeFnErr::InsufArgs.into()),
                };

                Ok(TimeCalcFn { var0, var1, op })
            }
            None => Err(TimeFnErr::InsufArgs.into()),
        }
    }

    pub fn contains_var(&self, var: &Var) -> bool {
        for arg in [&*self.var0, &*self.var1].iter() {
            if *arg == var {
                return true;
            }
        }
        false
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        let mut uid = TIME_CALC_FN.to_vec();
        uid.extend(self.var0.generate_uid());
        self.op.generate_uid(&mut uid);
        uid.extend(self.var1.generate_uid());
        uid
    }

    pub fn time_resolution(
        &self,
        assignments: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
    ) -> bool {
        let arg0 = assignments.get(&*self.var0).unwrap().get_last_date();
        let arg1 = assignments.get(&*self.var1).unwrap().get_last_date();

        match self.op {
            Operator::Equal => {
                let comp_diff = Duration::seconds(TIME_EQ_DIFF);
                let lower_bound = arg0 - comp_diff;
                let upper_bound = arg0 + comp_diff;
                !((arg1 < lower_bound) || (arg1 > upper_bound))
            }
            Operator::More => arg0 > arg1,
            Operator::Less => arg0 < arg1,
            Operator::MoreEqual => {
                let comp_diff = Duration::seconds(TIME_EQ_DIFF);
                let lower_bound = arg0 - comp_diff;
                let upper_bound = arg0 + comp_diff;
                !((arg1 < lower_bound) || (arg1 > upper_bound)) || arg0 > arg1
            }
            Operator::LessEqual => {
                let comp_diff = Duration::seconds(TIME_EQ_DIFF);
                let lower_bound = arg0 - comp_diff;
                let upper_bound = arg0 + comp_diff;
                !((arg1 < lower_bound) || (arg1 > upper_bound)) || arg0 < arg1
            }
            _ => unreachable!(),
        }
    }
}

impl<'a> std::convert::TryFrom<(&'a FuncDeclBorrowed<'a>, &'a mut ParseContext)> for TimeCalcFn {
    type Error = ParseErrF;

    fn try_from(decl: (&'a FuncDeclBorrowed, &mut ParseContext)) -> Result<Self, Self::Error> {
        let (other, context) = decl;

        if let TerminalBorrowed(TIME_CALC_FN) = other.name {
            TimeCalcFn::new(other, context)
        } else {
            Err(ParseErrF::NotBuiltin)
        }
    }
}
