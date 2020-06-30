use std::convert::TryFrom;
use std::sync::Arc;

use super::SpatialArg;
use crate::agent::{
    lang::{
        built_ins::MOVE_FN,
        logsent::ParseContext,
        parser::{FuncDeclBorrowed, TerminalBorrowed},
        time_semantics::TimeArg,
        Terminal, Var,
    },
    ParseErrF,
};
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub(in crate::agent) struct MoveFn {
    // this rarely will contain more than 2 vars
    vars: SmallVec<[Arc<Var>; 2]>,
    spatial_arg: SpatialArg,
    time_arg: Option<TimeArg>,
}

impl MoveFn {
    fn new(decl: &FuncDeclBorrowed, context: &mut ParseContext) -> Result<Self, ParseErrF> {
        let mut vars = SmallVec::new();
        if let Some(args) = &decl.args {
            for arg in args {
                match Terminal::from(&arg.term, context)? {
                    Terminal::FreeTerm(var) => vars.push(var),
                    _ => return Err(ParseErrF::WrongDef),
                }
            }
        } else {
            return Err(ParseErrF::WrongArgNumb);
        }

        let mut spatial_arg = None;
        let mut time_arg = None;
        if let Some(args) = &decl.op_args {
            for arg in args {
                if let Ok(arg) = SpatialArg::try_from((arg, &*context)) {
                    if spatial_arg.is_some() {
                        return Err(ParseErrF::WrongArgNumb);
                    }
                    spatial_arg = Some(arg);
                    continue;
                }

                if let Ok(arg) = TimeArg::try_from((arg, &*context)) {
                    if time_arg.is_some() {
                        return Err(ParseErrF::WrongArgNumb);
                    }
                    time_arg = Some(arg);
                    continue;
                }
            }
        }
        vars.shrink_to_fit();
        if let Some(spatial_arg) = spatial_arg {
            Ok(MoveFn {
                vars,
                spatial_arg,
                time_arg,
            })
        } else {
            Err(ParseErrF::WrongArgNumb)
        }
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        Vec::from(b"move".as_ref())
    }

    pub fn contains_var(&self, var: &Var) -> bool {
        for arg in &self.vars {
            if &**arg == var {
                return true;
            }
        }
        false
    }
}

impl<'a> std::convert::TryFrom<(&'a FuncDeclBorrowed<'a>, &'a mut ParseContext)> for MoveFn {
    type Error = ParseErrF;

    fn try_from(decl: (&'a FuncDeclBorrowed, &mut ParseContext)) -> Result<Self, Self::Error> {
        let (other, context) = decl;

        if let TerminalBorrowed(MOVE_FN) = other.name {
            Self::new(other, context)
        } else {
            Err(ParseErrF::NotBuiltin)
        }
    }
}
