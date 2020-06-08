use std::collections::HashMap;
use std::convert::TryFrom;
use std::sync::Arc;

use super::{TimeArg, TimeFnErr};
use crate::agent::{
    kb::bms::BmsWrapper,
    lang::{
        common::OpArg,
        logsent::ParseContext,
        parser::{FuncDeclBorrowed, TerminalBorrowed},
        Var,
    },
    ParseErrF,
};

/// Special built-in function for time calculus.
#[derive(Debug, Clone)]
pub(in crate::agent) struct TimeCalc {
    pub op_args: Vec<OpArg>,
}

impl TimeCalc {
    #[inline]
    fn new(other: &FuncDeclBorrowed, context: &mut ParseContext) -> Result<TimeCalc, ParseErrF> {
        if other.args.is_some() || other.op_args.is_none() {
            return Err(ParseErrF::WrongDef);
        }
        match other.op_args {
            Some(ref oargs) => {
                let mut v0 = Vec::with_capacity(oargs.len());
                for e in oargs {
                    let arg = match OpArg::from(e, context) {
                        Err(err) => return Err(err),
                        Ok(a) => a,
                    };
                    match arg {
                        // Generic(OpArgTerm, Option<(CompOperator, OpArgTerm)>)
                        OpArg::Generic(ref v0, Some((_, ref v1))) => {
                            if (!v0.is_var() | !v1.is_var())
                                || (!v0.get_var_ref().is_time_var()
                                    | !v1.get_var_ref().is_time_var())
                            {
                                return Err(TimeFnErr::IsNotVar.into());
                            }
                        }
                        _ => return Err(TimeFnErr::InsufArgs.into()),
                    }
                    v0.push(arg);
                }
                Ok(TimeCalc { op_args: v0 })
            }
            None => Err(TimeFnErr::InsufArgs.into()),
        }
    }

    pub fn contains_var(&self, var: &Var) -> bool {
        for arg in &self.op_args {
            if arg.contains_var(var) {
                return true;
            }
        }
        false
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        let mut uid = b"time_calc".to_vec();
        for arg in &self.op_args {
            uid.extend(arg.generate_uid());
        }
        uid
    }

    pub fn time_resolution(&self, assignments: &HashMap<&Var, Arc<BmsWrapper>>) -> Option<bool> {
        for arg in &self.op_args {
            if let Ok(arg) = TimeArg::try_from(arg) {
                let not_time_eq = !arg.compare_time_args(assignments);
                if not_time_eq {
                    return Some(false);
                }
            }
        }
        Some(true)
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
