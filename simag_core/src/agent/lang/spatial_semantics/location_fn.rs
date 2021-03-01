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
use std::{collections::HashMap, sync::Arc};

#[cfg(feature = "persistence")]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
pub(in crate::agent) struct LocFn<T>
where
    T: AsRef<str>,
{
    /// (terminal, is_var?, location)
    locations: Vec<(Arc<GrTerminalKind<T>>, bool, ObjLocation)>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
enum ObjLocation {
    Reified(Point),
    #[allow(dead_code)]
    Free(Var),
}
use ObjLocation::*;

impl<T: AsRef<str>> LocFn<T> {
    pub fn contains_var(&self, var: &Var) -> bool {
        self.locations.iter().any(|e| match &e.2 {
            ObjLocation::Free(c_var) => c_var == var,
            _ => false,
        })
    }

    pub fn loc_resolution(
        &self,
        _loc_assign: &HashMap<&Var, Arc<BmsWrapper<IsSpatialData>>>,
    ) -> bool {
        todo!()
    }

    pub fn objects_to_update(self) -> impl Iterator<Item = (Arc<GrTerminalKind<T>>, Point)>
    where
        T: 'static,
    {
        self.locations.into_iter().filter_map(|(t, _, l)| match l {
            ObjLocation::Reified(loc) => Some((t, loc)),
            _ => None,
        })
    }

    pub fn locate_objects(&self) -> impl Iterator<Item = (&Arc<GrTerminalKind<T>>, &Point)> {
        self.locations.iter().filter_map(|(t, is_var, l)| match l {
            ObjLocation::Reified(loc) if !is_var => Some((t, loc)),
            _ => None,
        })
    }

    pub fn free_locations(&self) -> impl Iterator<Item = (&Arc<GrTerminalKind<T>>, &Point)> {
        self.locations.iter().filter_map(|(t, is_var, l)| match l {
            ObjLocation::Reified(loc) if *is_var => Some((t, loc)),
            _ => None,
        })
    }
}

impl<T: AsRef<str>> From<(GrTerminalKind<T>, Point)> for LocFn<T> {
    fn from(input: (GrTerminalKind<T>, Point)) -> Self {
        let (term, loc) = input;
        LocFn {
            locations: vec![(Arc::new(term), false, ObjLocation::Reified(loc))],
        }
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
                    let mut is_var = false;
                    for v in &context.vars {
                        if v.name_eq(Into::<&str>::into(&term)) {
                            is_var = true;
                            break;
                        }
                    }
                    locations.push((Arc::new(term), is_var, loc));
                } else {
                    return Err(ParseErrF::ToDo);
                }
            }
            Ok(LocFn { locations })
        } else {
            unreachable!("SIMAG - a LocFn decl must have op args")
        }
    }
}
