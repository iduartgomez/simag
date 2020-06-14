use super::{
    common::OpArg,
    logsent::ParseContext,
    parser::{TerminalBorrowed, VarBorrowed},
    time_semantics::TimeArg,
    ParseErrF,
};
use crate::agent::kb::bms::BmsWrapper;
use std::collections::HashMap;

/// Variable equality is bassed on physical address, to compare term equality use the
/// `name_eq` method.
#[derive(Clone)]
pub(in crate::agent) struct Var {
    pub name: String,
    pub op_arg: Option<OpArg>,
    pub kind: VarKind,
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

impl Var {
    pub fn from<'a>(input: &VarBorrowed<'a>, context: &ParseContext) -> Result<Var, ParseErrF> {
        let &VarBorrowed {
            name: TerminalBorrowed(name),
            ref op_arg,
        } = input;
        let mut kind = VarKind::Normal;
        let op_arg = match *op_arg {
            Some(ref op_arg) => {
                let t = OpArg::from(op_arg, context)?;
                match t {
                    OpArg::TimeDecl(_) => {
                        kind = VarKind::TimeDecl;
                    }
                    OpArg::TimeVar => {
                        kind = VarKind::Time;
                    }
                    _ => return Err(ParseErrF::WrongDef),
                }
                Some(t)
            }
            None => None,
        };
        let name = std::str::from_utf8(name).unwrap().to_owned();
        if super::reserved(name.as_bytes()) {
            return Err(ParseErrF::ReservedKW(name));
        }
        Ok(Var { name, op_arg, kind })
    }

    pub fn get_times(&self) -> BmsWrapper {
        use std::convert::TryFrom;
        let h = HashMap::new();
        self.op_arg
            .as_ref()
            .map(|arg| TimeArg::try_from(arg).unwrap())
            .unwrap()
            .get_time_payload(&h, None)
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
        if let Some(a) = &self.op_arg {
            let a = format!("{:?}", a);
            write!(f, "{}: {} = {}", self.name, self.kind, a)
        } else {
            write!(f, "{}: {}", self.name, self.kind)
        }
    }
}
