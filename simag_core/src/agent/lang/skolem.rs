use super::{
    common::{op_arg_term_from_borrowed, OpArgTerm},
    logsent::ParseContext,
    parser::{OpArgTermBorrowed, SkolemBorrowed, TerminalBorrowed},
    time_semantics::{TimeFn, TimeFnErr},
    typedef::TypeDef,
    ParseErrF,
};

#[derive(Debug, PartialEq, Eq, Hash)]
pub(in crate::agent) struct Skolem {
    pub name: String,
    ty: TypeDef,
    assigned_val: Option<OpArgTerm>,
}

impl Skolem {
    pub fn from<'a>(
        input: &SkolemBorrowed<'a>,
        context: &ParseContext,
    ) -> Result<Skolem, ParseErrF> {
        let &SkolemBorrowed {
            name: TerminalBorrowed(name),
            ref ty,
            ref val,
        } = input;

        let (ty, assigned_val) = match (ty, val) {
            (def, Some(val)) if def.0 == b"time" => match val {
                OpArgTermBorrowed::String(slice) => {
                    let time = TimeFn::from_str(slice)?;
                    (TypeDef::Time, Some(OpArgTerm::TimePayload(time)))
                }
                _ => return Err(TimeFnErr::InsufArgs.into()),
            },
            (def, None) if def.0 == b"time" => {
                (TypeDef::Time, Some(OpArgTerm::TimePayload(TimeFn::IsVar)))
            }
            (def, None) => (TypeDef::Erased, None),
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

    pub fn name_eq(&self, other: &Skolem) -> bool {
        self.name == other.name
    }
}
