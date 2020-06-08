use std::{collections::HashMap, sync::Arc};

use super::{logsent::ParseContext, parser::FuncDeclBorrowed, time_semantics::TimeCalc, Var};
use crate::agent::kb::bms::BmsWrapper;

/// Special built-in functions callable in logical sentences.
#[derive(Debug, Clone)]
pub(in crate::agent) enum BuiltIns {
    TimeCalculus(TimeCalc),
}

impl BuiltIns {
    pub fn generate_uid(&self) -> Vec<u8> {
        match self {
            BuiltIns::TimeCalculus(f) => f.generate_uid(),
        }
    }

    pub fn get_name(&self) -> &str {
        match self {
            BuiltIns::TimeCalculus(_) => "time_calc",
        }
    }

    #[inline]
    pub fn grounded_eq(&self, time_assign: &HashMap<&Var, Arc<BmsWrapper>>) -> Option<bool> {
        match self {
            BuiltIns::TimeCalculus(f) => f.time_resolution(time_assign),
        }
    }
}

impl<'a> std::convert::TryFrom<(&'a FuncDeclBorrowed<'a>, &'a mut ParseContext)> for BuiltIns {
    type Error = &'a FuncDeclBorrowed<'a>;

    fn try_from(decl: (&'a FuncDeclBorrowed, &mut ParseContext)) -> Result<Self, Self::Error> {
        if let Ok(f) = TimeCalc::try_from((decl.0, decl.1)) {
            Ok(BuiltIns::TimeCalculus(f))
        } else {
            Err(decl.0)
        }
    }
}
