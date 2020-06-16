use std::convert::TryFrom;

use super::{
    common::ConstraintValue,
    logsent::ParseContext,
    parser::{SkolemBorrowed, TerminalBorrowed, UnconstraintArg},
    time_semantics::{TimeFn, TimeFnErr},
    typedef::TypeDef,
    ParseErrF,
};

#[derive(Debug, PartialEq, Eq, Hash)]
pub(in crate::agent) struct Skolem {
    pub name: String,
    ty: TypeDef,
    assigned_val: Option<ConstraintValue>,
}

impl Skolem {
    pub fn name_eq(&self, other: &Skolem) -> bool {
        self.name == other.name
    }
}

impl<'a> TryFrom<(&SkolemBorrowed<'a>, &ParseContext)> for Skolem {
    type Error = ParseErrF;

    fn try_from(input: (&SkolemBorrowed<'a>, &ParseContext)) -> Result<Skolem, ParseErrF> {
        let (input, _) = input;
        let &SkolemBorrowed {
            name: TerminalBorrowed(name),
            ref ty,
            ref val,
        } = input;

        let (ty, assigned_val) = match (ty, val) {
            (def, Some(val)) if def.0 == b"time" => match val {
                UnconstraintArg::String(slice) => {
                    let time = TimeFn::from_str(slice)?;
                    (TypeDef::Time, Some(ConstraintValue::TimePayload(time)))
                }
                _ => return Err(TimeFnErr::InsufArgs.into()),
            },
            (def, None) if def.0 == b"time" => (
                TypeDef::Time,
                Some(ConstraintValue::TimePayload(TimeFn::IsVar)),
            ),
            (_def, None) => (TypeDef::Erased, None),
            _ => return Err(ParseErrF::TypeUnsupported),
        };

        let name = std::str::from_utf8(name).unwrap().to_owned();
        if super::reserved(name.as_bytes()) {
            return Err(ParseErrF::ReservedKW(name));
        }
        Ok(Skolem {
            name,
            ty,
            assigned_val,
        })
    }
}
