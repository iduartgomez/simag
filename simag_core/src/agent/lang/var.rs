use super::{
    common::ConstraintValue,
    logsent::ParseContext,
    parser::{TerminalBorrowed, UnconstraintArg, VarBorrowed},
    time_semantics::{TimeFn, TimeFnErr},
    typedef::TypeDef,
    ParseErrF,
};
use crate::agent::kb::bms::BmsWrapper;
use std::convert::TryFrom;

/// Variable equality is bassed on physical address, to compare term equality use the
/// `name_eq` method.
#[derive(Clone)]
pub(in crate::agent) struct Var {
    pub name: String,
    pub kind: VarKind,
    ty: TypeDef,
    assigned_val: Option<ConstraintValue>,
}

impl<'a> From<&'a str> for Var {
    fn from(name: &'a str) -> Self {
        Var {
            name: name.to_owned(),
            kind: VarKind::Normal,
            ty: TypeDef::Erased,
            assigned_val: None,
        }
    }
}

#[derive(Clone, PartialEq)]
pub(in crate::agent) enum VarKind {
    Normal,
    Time,
    TimeDecl,
}

impl std::fmt::Debug for VarKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for VarKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VarKind::Normal => write!(f, "Normal"),
            VarKind::Time => write!(f, "Time"),
            VarKind::TimeDecl => write!(f, "TimeDecl"),
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

        let mut kind = VarKind::Normal;
        let (ty, assigned_val) = match (ty, val) {
            (def, Some(val)) if def.0 == b"time" => match val {
                UnconstraintArg::String(slice) => {
                    let time = TimeFn::from_str(slice)?;
                    kind = VarKind::TimeDecl;
                    (TypeDef::Time, Some(ConstraintValue::TimePayload(time)))
                }
                _ => return Err(TimeFnErr::InsufArgs.into()),
            },
            (def, None) if def.0 == b"time" => {
                kind = VarKind::Time;
                (
                    TypeDef::Time,
                    Some(ConstraintValue::TimePayload(TimeFn::IsVar)),
                )
            }
            (_, None) => (TypeDef::Erased, None),
            _ => return Err(ParseErrF::TypeUnsupported),
        };

        let name = std::str::from_utf8(name).unwrap().to_owned();
        if super::reserved(name.as_bytes()) {
            return Err(ParseErrF::ReservedKW(name));
        }
        Ok(Var {
            name,
            kind,
            ty,
            assigned_val,
        })
    }
}

impl Var {
    pub fn get_times(&self) -> BmsWrapper {
        self.assigned_val
            .as_ref()
            .map(|arg| TimeFn::try_from(arg).unwrap())
            .unwrap()
            .get_time_payload(None)
    }

    pub fn is_time_var(&self) -> bool {
        match self.kind {
            VarKind::Time | VarKind::TimeDecl => true,
            _ => false,
        }
    }

    pub fn name_eq(&self, other: &Var) -> bool {
        self.name == other.name
    }
}

impl std::cmp::PartialEq for Var {
    fn eq(&self, other: &Var) -> bool {
        (self as *const Var) == (other as *const Var)
    }
}

impl std::cmp::Eq for Var {}

impl std::hash::Hash for Var {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let address = &*self as *const Var as usize;
        address.hash(state);
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
            write!(f, "{}: {} = {}", self.name, self.kind, a)
        } else {
            write!(f, "{}: {}", self.name, self.kind)
        }
    }
}
