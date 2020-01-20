use super::{
    common::OpArg,
    logsent::ParseContext,
    parser::{SkolemBorrowed, TerminalBorrowed},
    ParseErrF,
};

#[derive(Debug, PartialEq, Eq, Hash)]
pub(in crate::agent) struct Skolem {
    pub name: String,
    op_arg: Option<OpArg>,
}

impl Skolem {
    pub fn from<'a>(
        input: &SkolemBorrowed<'a>,
        context: &ParseContext,
    ) -> Result<Skolem, ParseErrF> {
        let &SkolemBorrowed {
            name: TerminalBorrowed(name),
            ref op_arg,
        } = input;
        let op_arg = match *op_arg {
            Some(ref op_arg) => {
                let t = OpArg::from(op_arg, context)?;
                Some(t)
            }
            None => None,
        };
        let name = std::str::from_utf8(name).unwrap().to_owned();
        if super::reserved(&name) {
            return Err(ParseErrF::ReservedKW(name));
        }
        Ok(Skolem { name, op_arg })
    }

    pub fn name_eq(&self, other: &Skolem) -> bool {
        self.name == other.name
    }
}
