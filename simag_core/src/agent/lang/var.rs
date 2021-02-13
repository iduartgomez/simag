use super::{
    common::ConstraintValue,
    logsent::ParseContext,
    parser::{TerminalBorrowed, UnconstraintArg, VarBorrowed},
    spatial_semantics::Point,
    time_semantics::{TimeFn, TimeFnErr},
    typedef::TypeDef,
    Operator, ParseErrF,
};
use crate::agent::kb::bms::{BmsWrapper, IsTimeData};
use std::{
    convert::TryFrom,
    sync::atomic::{AtomicUsize, Ordering},
};

static NEXT_ADDRESS: AtomicUsize = AtomicUsize::new(0);

/// Obtain a new address from the virtual address space.
#[inline]
fn get_new_address() -> usize {
    NEXT_ADDRESS.fetch_add(1, Ordering::SeqCst)
}

/// Variable equality is bassed on physical address, to compare term equality use the
/// `name_eq` method.
#[derive(Clone)]
pub(in crate::agent) struct Var {
    pub name: String,
    pub ty: TypeDef,
    pub address: usize,
    assigned_val: Option<ConstraintValue>,
}

impl<'a> From<&'a str> for Var {
    fn from(name: &'a str) -> Self {
        Var {
            name: name.to_owned(),
            ty: TypeDef::Erased,
            assigned_val: None,
            address: get_new_address(),
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

        let name = std::str::from_utf8(name).unwrap().to_owned();
        if super::reserved(name.as_bytes()) {
            return Err(ParseErrF::ReservedKW(name));
        }
        Ok(Var {
            name,
            ty,
            assigned_val,
            address: get_new_address(),
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

    pub fn name_eq(&self, other: &Var) -> bool {
        self.name == other.name
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        self.address.to_le_bytes().iter().copied().collect()
    }
}

impl std::cmp::PartialEq for Var {
    fn eq(&self, other: &Var) -> bool {
        self.address == other.address
    }
}

impl std::cmp::Eq for Var {}

impl std::hash::Hash for Var {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.address.hash(state);
    }
}

impl std::fmt::Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(a) = &self.assigned_val {
            let a = format!("{:?}", a);
            write!(f, "{}: {} = {}", self.name, self.ty, a)
        } else {
            write!(f, "{}: {}", self.name, self.ty)
        }
    }
}
