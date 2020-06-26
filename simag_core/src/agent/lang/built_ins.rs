use std::{collections::HashMap, sync::Arc};

use super::{
    logsent::ParseContext, parser::FuncDeclBorrowed, space_semantics::MoveFn,
    time_semantics::TimeCalc, Var,
};
use crate::agent::kb::bms::BmsWrapper;

pub const MOVE_FN: &[u8] = b"move";
pub const TIME_CALC_FN: &[u8] = b"time_calc";

/// Special built-in functions callable in logical sentences.
#[derive(Debug, Clone)]
pub(in crate::agent) enum BuiltIns {
    TimeCalculus(TimeCalc),
    MoveFn(MoveFn),
}

impl BuiltIns {
    pub fn generate_uid(&self) -> Vec<u8> {
        match self {
            BuiltIns::TimeCalculus(f) => f.generate_uid(),
            BuiltIns::MoveFn(f) => f.generate_uid(),
        }
    }

    pub fn get_name(&self) -> &str {
        match self {
            BuiltIns::TimeCalculus(_) => "time_calc",
            BuiltIns::MoveFn(_) => "move",
        }
    }

    #[inline]
    pub fn grounded_eq(&self, time_assign: &HashMap<&Var, Arc<BmsWrapper>>) -> Option<bool> {
        match self {
            BuiltIns::TimeCalculus(f) => Some(f.time_resolution(time_assign)),
            BuiltIns::MoveFn(f) => todo!(),
        }
    }
}

impl<'a> std::convert::TryFrom<(&'a FuncDeclBorrowed<'a>, &'a mut ParseContext)> for BuiltIns {
    type Error = &'a FuncDeclBorrowed<'a>;

    fn try_from(input: (&'a FuncDeclBorrowed, &mut ParseContext)) -> Result<Self, Self::Error> {
        let (decl, ctxt) = input;
        if let Ok(f) = TimeCalc::try_from((decl, &mut *ctxt)) {
            Ok(BuiltIns::TimeCalculus(f))
        } else if let Ok(f) = MoveFn::try_from((decl, &mut *ctxt)) {
            Ok(BuiltIns::MoveFn(f))
        } else {
            Err(decl)
        }
    }
}
