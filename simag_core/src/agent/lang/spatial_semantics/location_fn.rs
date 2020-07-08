use std::{collections::HashMap, sync::Arc};

use super::Point;
use crate::agent::{
    kb::bms::{BmsWrapper, IsSpatialData},
    lang::{
        built_ins::LOCATION_FN,
        logsent::ParseContext,
        parser::{FuncDeclBorrowed, OpArgBorrowed, TerminalBorrowed},
        GrTerminalKind, Operator, Terminal, Var,
    },
    ParseErrF,
};

#[derive(Debug, Clone)]
pub(in crate::agent) struct LocFn<T>
where
    T: AsRef<str>,
{
    locations: Vec<(GrTerminalKind<T>, ObjLocation)>,
}

#[derive(Debug, Clone)]
enum ObjLocation {
    Reified(Point),
    Free(Arc<Var>),
}
use ObjLocation::*;

impl<T: AsRef<str>> LocFn<T> {
    pub fn generate_id(&self) -> Vec<u8> {
        todo!()
    }

    pub fn contains_var(&self, var: &Var) -> bool {
        self.locations.iter().any(|e| match &e.1 {
            ObjLocation::Free(c_var) => &**c_var == var,
            _ => false,
        })
    }

    pub fn loc_resolution(
        &self,
        _loc_assign: &HashMap<&Var, Arc<BmsWrapper<IsSpatialData>>>,
    ) -> bool {
        todo!()
    }

    pub fn objects_to_update(self) -> impl Iterator<Item = (GrTerminalKind<T>, Point)>
    where
        T: 'static,
    {
        self.locations.into_iter().filter_map(|(t, l)| match l {
            ObjLocation::Reified(loc) => Some((t, loc)),
            _ => None,
        })
    }

    pub fn iter(&self) -> impl Iterator<Item = (&GrTerminalKind<T>, &Point)> {
        self.locations.iter().filter_map(|(t, l)| match l {
            ObjLocation::Reified(loc) => Some((t, loc)),
            _ => None,
        })
    }
}

impl<'a> std::convert::TryFrom<(&'a FuncDeclBorrowed<'a>, &'a mut ParseContext)> for LocFn<String> {
    type Error = ParseErrF;

    fn try_from(input: (&'a FuncDeclBorrowed, &'a mut ParseContext)) -> Result<Self, Self::Error> {
        let (other, context) = input;
        if other.name != LOCATION_FN {
            return Err(ParseErrF::ToDo);
        }
        if let Some(args) = &other.op_args {
            let mut locations = Vec::with_capacity(args.len());
            for arg in args {
                let OpArgBorrowed { term, comp } = arg;
                if let Some((Operator::At, loc)) = comp {
                    let loc = if let Ok(p) = Point::try_from(&**loc) {
                        Reified(p)
                    } else {
                        match Terminal::from(&TerminalBorrowed::from(&**loc), context)? {
                            Terminal::FreeTerm(var) => Reified(var.get_location()),
                            _ => return Err(ParseErrF::ToDo),
                        }
                    };
                    let term = GrTerminalKind::from(&**term);
                    locations.push((term, loc));
                } else {
                    return Err(ParseErrF::ToDo);
                }
            }
            Ok(LocFn { locations })
        } else {
            unreachable!(format!(
                "SIMAG - {}:{} - unreachable: a LocFn decl must have op args",
                file!(),
                line!()
            ))
        }
    }
}
