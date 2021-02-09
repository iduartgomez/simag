use std::convert::TryFrom;
use std::{collections::HashMap, sync::Arc};

use super::SpatialArg;
use crate::agent::{
    kb::{
        bms::{
            add_loc_from_move_fn, add_loc_from_spatial_data, BmsWrapper, IsSpatialData, IsTimeData,
            RecordHistory,
        },
        VarAssignment,
    },
    lang::{
        built_ins::MOVE_FN,
        logsent::ParseContext,
        parser::{FuncDeclBorrowed, TerminalBorrowed},
        time_semantics::TimeArg,
        GrTerminalKind, ProofResContext, Terminal, Var,
    },
    ParseErrF, Representation,
};
use smallvec::SmallVec;
use SpatialArg::*;

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
            if let Some(time_arg) = &time_arg {
                if time_arg.is_interval() && !spatial_arg.has_origin() {
                    return Err(ParseErrF::WrongDef);
                }
            }
            Ok(MoveFn {
                vars,
                spatial_arg,
                time_arg,
            })
        } else {
            Err(ParseErrF::WrongArgNumb)
        }
    }

    pub fn contains_var(&self, var: &Var) -> bool {
        for arg in &self.vars {
            if &**arg == var {
                return true;
            }
        }
        false
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        let mut id = Vec::from(b"move_fn<".as_ref());
        for a in &self.vars {
            id.extend(a.generate_uid());
        }
        id.extend(self.spatial_arg.generate_uid());
        id.push(b'>');
        id
    }

    pub fn get_location(
        &self,
        loc_assign: &HashMap<&Var, Arc<BmsWrapper<IsSpatialData>>>,
    ) -> Result<BmsWrapper<IsSpatialData>, ()> {
        match &self.spatial_arg {
            FromVarToVar(v0, v1) => {
                let mut l0 = (&*loc_assign[&**v0]).clone();
                let l1 = &loc_assign[&**v1];
                if l0.merge_from_to(l1).is_ok() {
                    Ok(l0)
                } else {
                    Err(())
                }
            }
            FromVarToVal(v0, l1) => {
                let mut l0 = (&*loc_assign[&**v0]).clone();
                let l1 = BmsWrapper::<IsSpatialData>::new(Some(l1.clone()));
                if l0.merge_from_to(&l1).is_ok() {
                    Ok(l0)
                } else {
                    Err(())
                }
            }
            FromValToVar(l0, v1) => {
                let mut l0 = BmsWrapper::<IsSpatialData>::new(Some(l0.clone()));
                let l1 = &loc_assign[&**v1];
                if l0.merge_from_to(&l1).is_ok() {
                    Ok(l0)
                } else {
                    Err(())
                }
            }
            FromValToVal(l0, l1) => {
                let mut l0 = BmsWrapper::<IsSpatialData>::new(Some(l0.clone()));
                let l1 = BmsWrapper::<IsSpatialData>::new(Some(l1.clone()));
                if l0.merge_from_to(&l1).is_ok() {
                    Ok(l0)
                } else {
                    Err(())
                }
            }
            ToVar(v0) => {
                let l0 = &loc_assign[&**v0];
                Ok((&**l0).clone())
            }
            ToVal(l0) => Ok(BmsWrapper::<IsSpatialData>::new(Some(l0.clone()))),
            _ => Err(()),
        }
    }

    pub fn substitute<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
        loc_assign: &HashMap<&Var, Arc<BmsWrapper<IsSpatialData>>>,
        context: &mut T,
    ) {
        let add_rec = |bms: &BmsWrapper<RecordHistory>, loc: BmsWrapper<IsSpatialData>| {
            if let Some(time_arg) = &self.time_arg {
                let t = time_arg.get_time_payload(time_assign, None);
                let new = t.merge_spatial_data(loc).unwrap();
                add_loc_from_move_fn(context, bms, new);
            } else {
                add_loc_from_spatial_data(context, bms, loc);
            }
        };

        if let Some(assignments) = assignments {
            let loc = self
                .get_location(loc_assign)
                .unwrap_or_else(|_| unreachable!("SIMAG - location not assigned in move fn"));

            let classes = agent.classes.get();
            let entities = agent.entities.get();

            for var in &self.vars {
                let assigned = assignments[&**var];
                match assigned.name {
                    GrTerminalKind::Class(cls) => {
                        let c = classes.get(cls).unwrap();
                        add_rec(&c.location, loc.clone());
                    }
                    GrTerminalKind::Entity(ent) => {
                        let e = entities.get(ent).unwrap();
                        add_rec(&e.location, loc.clone());
                    }
                }
            }
        } else {
            let (_l0, _l1) = match &self.spatial_arg {
                FromValToVal(l0, l1) => (Some(l0.clone()), l1.clone()),
                ToVal(l0) => (None, l0.clone()),
                _ => unreachable!(),
            };
            todo!()
        }
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
