use crate::agent::{
    lang::{
        built_ins::MOVE_FN,
        logsent::ParseContext,
        parser::{FuncDeclBorrowed, TerminalBorrowed},
        Var,
    },
    ParseErrF,
};

#[derive(Debug, Clone)]
pub(in crate::agent) struct MoveFn;

impl MoveFn {
    fn new(delc: &FuncDeclBorrowed, context: &mut ParseContext) -> Result<Self, ParseErrF> {
        Ok(MoveFn)
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        Vec::from(b"move".as_ref())
    }

    pub fn contains_var(&self, var: &Var) -> bool {
        // for arg in &[&self.var0, &self.var1] {
        //     if arg.contains_var(var) {
        //         return true;
        //     }
        // }
        // false
        todo!()
    }
}

impl<'a> std::convert::TryFrom<(&'a FuncDeclBorrowed<'a>, &'a mut ParseContext)> for MoveFn {
    type Error = &'a FuncDeclBorrowed<'a>;

    fn try_from(decl: (&'a FuncDeclBorrowed, &mut ParseContext)) -> Result<Self, Self::Error> {
        let (other, context) = decl;

        if let TerminalBorrowed(MOVE_FN) = other.name {
            Ok(Self::new(other, context).map_err(|_| other)?)
        } else {
            Err(other)
        }
    }
}
