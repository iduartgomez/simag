use super::{
    common::ConstraintValue,
    logsent::ParseContext,
    parser::{TerminalBorrowed, UnconstraintArg, VarBorrowed},
    spatial_semantics::Point,
    time_semantics::{TimeFn, TimeFnErr},
    typedef::TypeDef,
    Operator, ParseErrF,
};
use crate::{
    agent::kb::bms::{BmsWrapper, IsTimeData},
    var_map::{IdToken, TableData, VariableMap},
};
use once_cell::sync::Lazy;
use std::{
    convert::TryFrom,
    hash::Hash,
    sync::atomic::{AtomicUsize, Ordering},
};

#[cfg(feature = "persistence")]
use serde::{Deserialize, Serialize};

static VAR_STORAGE: Lazy<TableData<str>> = Lazy::new(TableData::init_static);
static VAR_TABLE: Lazy<VariableMap<str>> = Lazy::new(|| VariableMap::new(&*VAR_STORAGE));

static NEXT_VARIABLE: AtomicUsize = AtomicUsize::new(0);

/// Variable equality is bassed on physical address, to compare term equality use the
/// `name_eq` method.
#[derive(Clone)]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
pub(in crate::agent) struct Var {
    pub ty: TypeDef,
    token_id: IdToken,
    var_id: usize,
    #[cfg_attr(feature = "persistence", serde(skip_serializing, skip_deserializing))]
    assigned_val: Option<ConstraintValue>,
}

impl<'a> From<&'a str> for Var {
    fn from(name: &'a str) -> Self {
        let id = VAR_TABLE.push(name.to_owned());
        let var_id = NEXT_VARIABLE.fetch_add(1, Ordering::SeqCst);
        Var {
            token_id: id,
            var_id,
            ty: TypeDef::Erased,
            assigned_val: None,
        }
    }
}

impl<'a> std::convert::TryFrom<(&'a VarBorrowed<'a>, &'a ParseContext)> for Var {
    type Error = ParseErrF;

    fn try_from(input: (&'a VarBorrowed<'a>, &'a ParseContext)) -> Result<Var, ParseErrF> {
        let VarBorrowed {
            name: TerminalBorrowed(name),
            ref ty,
            ref val,
        } = input.0;

        let (ty, assigned_val) = match (ty, val) {
            (def, Some(val)) if def.0 == b"time" => match val {
                UnconstraintArg::String(slice) => {
                    let time = TimeFn::from_str(slice, Operator::Since)?;
                    (TypeDef::TimeDecl, Some(ConstraintValue::TimePayload(time)))
                }
                _ => return Err(TimeFnErr::InsufArgs.into()),
            },
            (def, None) if def.0 == b"time" => (TypeDef::Time, None),
            (def, Some(val)) if def.0 == b"location" => match val {
                UnconstraintArg::String(slice) => {
                    let loc = Point::try_from(*slice)?;
                    (TypeDef::LocDecl, Some(ConstraintValue::SpatialPayload(loc)))
                }
                _ => return Err(TimeFnErr::InsufArgs.into()),
            },
            (def, None) if def.0 == b"location" => (TypeDef::Location, None),
            (_, None) => (TypeDef::Erased, None),
            _ => return Err(ParseErrF::TypeUnsupported),
        };

        let name = std::str::from_utf8(name).expect("should be valid utf8");
        if super::reserved(name.as_bytes()) {
            return Err(ParseErrF::ReservedKW(name.to_owned()));
        }
        let token_id = VAR_TABLE.push(name.to_owned());
        let var_id = NEXT_VARIABLE.fetch_add(1, Ordering::SeqCst);
        Ok(Var {
            token_id,
            var_id,
            ty,
            assigned_val,
        })
    }
}

impl Var {
    pub fn get_time(&self) -> BmsWrapper<IsTimeData> {
        self.assigned_val
            .as_ref()
            .map(|arg| TimeFn::try_from(arg).unwrap())
            .unwrap()
            .get_time_payload(None)
    }

    pub fn get_location(&self) -> Point {
        todo!()
    }

    pub fn is_time_var(&self) -> bool {
        match self.ty {
            TypeDef::Time | TypeDef::TimeDecl => true,
            _ => false,
        }
    }

    pub fn name_eq<T: VarEq>(&self, other: T) -> bool {
        other.name_eq(self)
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        self.var_id
            .to_le_bytes()
            .iter()
            .copied()
            .collect::<Vec<_>>()
    }

    pub fn get_name(&self) -> &str {
        VAR_TABLE.get(&self.token_id).unwrap()
    }
}

impl std::cmp::PartialEq for Var {
    fn eq(&self, other: &Var) -> bool {
        self.token_id == other.token_id
    }
}

impl std::cmp::Eq for Var {}

impl Hash for Var {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.token_id.hash(state);
    }
}

impl std::fmt::Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let name = self.get_name();
        if let Some(a) = &self.assigned_val {
            let a = format!("{:?}", a);
            write!(f, "{}: {} = {}", name, self.ty, a)
        } else {
            write!(f, "{}: {}", name, self.ty)
        }
    }
}

pub(in crate::agent) trait VarEq {
    fn name_eq(&self, var: &Var) -> bool;
}

impl VarEq for Var {
    #[inline]
    fn name_eq(&self, var: &Var) -> bool {
        self.token_id == var.token_id
    }
}

impl<'a> VarEq for &'a Var {
    #[inline]
    fn name_eq(&self, var: &Var) -> bool {
        self.token_id == var.token_id
    }
}

impl<'a> VarEq for &'a str {
    fn name_eq(&self, var: &Var) -> bool {
        self == &var.get_name()
    }
}
