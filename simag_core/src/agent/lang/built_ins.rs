use super::{
    logsent::ParseContext,
    parser::FuncDeclBorrowed,
    spatial_semantics::{LocFn, MoveFn},
    time_semantics::TimeCalcFn,
    ParseErrF, Var,
};
use crate::agent::kb::bms::{BmsWrapper, IsSpatialData, IsTimeData};
use std::{collections::HashMap, sync::Arc};

#[cfg(feature = "persistence")]
use serde::{Deserialize, Serialize};

pub const MOVE_FN: &[u8] = b"move";
pub const LOCATION_FN: &[u8] = b"location";
pub const TIME_CALC_FN: &[u8] = b"time_calc";

/// Special built-in functions callable in logical sentences.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
pub(in crate::agent) enum BuiltIns<T = String>
where
    T: AsRef<str>,
{
    // time semantics funcs:
    TimeCalculus(TimeCalcFn),
    // spatial semantics funcs:
    Move(MoveFn),
    Location(LocFn<T>),
}

impl<T: AsRef<str>> From<LocFn<T>> for BuiltIns<T> {
    fn from(func: LocFn<T>) -> Self {
        BuiltIns::Location(func)
    }
}

impl<T: AsRef<str>> BuiltIns<T> {
    pub fn generate_uid(&self) -> Vec<u8> {
        match self {
            BuiltIns::TimeCalculus(f) => f.generate_uid(),
            BuiltIns::Move(f) => f.generate_uid(),
            BuiltIns::Location(_) => unreachable!(),
        }
    }

    pub fn get_name(&self) -> &str {
        match self {
            BuiltIns::TimeCalculus(_) => "time_calc",
            BuiltIns::Move(_) => "move",
            BuiltIns::Location(_) => "location",
        }
    }

    #[inline]
    pub fn grounded_eq(
        &self,
        time_assign: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
        loc_assign: &HashMap<&Var, Arc<BmsWrapper<IsSpatialData>>>,
    ) -> Option<bool> {
        match self {
            BuiltIns::TimeCalculus(f) => Some(f.time_resolution(time_assign)),
            BuiltIns::Location(f) => Some(f.loc_resolution(loc_assign)),
            BuiltIns::Move(_) => unreachable!(), // makes no sense in this context
        }
    }
}

impl<'a> std::convert::TryFrom<(&'a FuncDeclBorrowed<'a>, &mut ParseContext)> for BuiltIns {
    type Error = ParseErrF;

    fn try_from(input: (&FuncDeclBorrowed<'a>, &mut ParseContext)) -> Result<Self, Self::Error> {
        let (decl, ctxt) = input;
        match decl.name.0 {
            MOVE_FN => Ok(BuiltIns::Move(MoveFn::try_from((decl, &mut *ctxt))?)),
            TIME_CALC_FN => Ok(BuiltIns::TimeCalculus(TimeCalcFn::try_from((
                decl, &mut *ctxt,
            ))?)),
            _ => Err(ParseErrF::WrongDef),
        }
    }
}
