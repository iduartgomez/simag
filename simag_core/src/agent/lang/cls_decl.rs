use super::{
    common::*,
    logsent::{LogSentResolution, ParseContext},
    parser::ClassDeclBorrowed,
    time_semantics::{TimeArg, TimeOps},
    *,
};
use crate::agent::kb::{
    bms::{BmsWrapper, IsSpatialData, IsTimeData, OverwriteBms, RecordHistory},
    repr::Representation,
    VarAssignment,
};
use spatial_semantics::{SpatialArg, SpatialOps};
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::{iter::FromIterator, sync::Arc};

#[cfg(feature = "persistence")]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
pub(in crate::agent) struct ClassDecl {
    name: Terminal,
    args: Vec<Predicate>,
    pub op_args: Option<Vec<OpArg>>,
}

impl<'a> TryFrom<(&ClassDeclBorrowed<'a>, &mut ParseContext)> for ClassDecl {
    type Error = ParseErrF;

    fn try_from(input: (&ClassDeclBorrowed, &mut ParseContext)) -> Result<Self, Self::Error> {
        let (other, context) = input;
        let class_name = Terminal::from(&other.name, context)?;
        let op_args = match other.op_args {
            Some(ref oargs) => {
                let mut v0 = Vec::with_capacity(oargs.len());
                let mut found_time_arg = false;
                let mut found_spatial_arg = false;
                for e in oargs {
                    let a = OpArg::try_from((e, &*context))?;
                    match a {
                        OpArg::Spatial(_) => {
                            if found_spatial_arg {
                                // only one allowed
                                return Err(ParseErrF::WrongArgNumb);
                            } else {
                                found_spatial_arg = true
                            }
                        }
                        OpArg::Time(_) => {
                            if found_time_arg {
                                // only one allowed
                                return Err(ParseErrF::WrongArgNumb);
                            } else {
                                found_time_arg = true
                            }
                        }
                        _ => {}
                    }
                    v0.push(a);
                }
                Some(v0)
            }
            None => None,
        };
        let args = {
            let mut v0 = Vec::with_capacity(other.args.len());
            for arg in &other.args {
                let pred = Predicate::from(arg, context, &class_name, None)?;
                v0.push(pred);
            }
            v0
        };

        Ok(ClassDecl {
            name: class_name,
            args,
            op_args,
        })
    }
}

impl ClassDecl {
    pub fn get_args(&self) -> &[Predicate] {
        &self.args
    }

    pub fn get_args_mut(&mut self) -> &mut [Predicate] {
        &mut self.args
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        match &self.name {
            Terminal::FreeTerm(var) => var.get_name(),
            Terminal::GroundedTerm(name) => name,
        }
    }

    #[inline]
    pub fn get_parent(&self) -> &Terminal {
        &self.name
    }

    pub(in crate::agent::lang) fn generate_uid(&self) -> Vec<u8> {
        let mut id = Vec::from_iter(b"cls_decl<".iter().cloned());
        id.append(&mut self.name.generate_uid());
        for a in &self.args {
            let mut id_2 = a.generate_uid();
            id.append(&mut id_2)
        }
        if let Some(ref args) = self.op_args {
            for a in args {
                let mut id_2 = a.generate_uid();
                id.append(&mut id_2)
            }
        }
        id.push(b'>');
        id
    }

    pub(in crate::agent::lang) fn contains_var(&self, var: &Var) -> bool {
        for a in &self.args {
            match *a {
                Predicate::FreeMembershipToClass(ref term) if &term.term == var => return true,
                _ => continue,
            }
        }
        if self.op_args.is_some() {
            for a in self.op_args.as_ref().unwrap() {
                if a.contains_var(var) {
                    return true;
                }
            }
        }
        false
    }

    pub(in crate::agent::lang) fn parent_is_grounded(&self) -> bool {
        match self.name {
            Terminal::GroundedTerm(_) => true,
            _ => false,
        }
    }

    /// While constructing an assertion in a tell context performs variable
    /// substitution whenever is possible, variables must be declared.
    pub fn var_substitution(&mut self) -> Result<(), ParseErrF> {
        if let Some(op_args) = &mut self.op_args {
            for op_arg in op_args {
                op_arg.var_substitution()?;
            }
        }
        Ok(())
    }

    fn get_assignment<T>(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> Option<Arc<T>>
    where
        T: for<'a> TryFrom<&'a BmsWrapper<RecordHistory>>,
    {
        let arg = &self.args[0];
        match *arg {
            Predicate::FreeMembershipToClass(ref free) => {
                var_assign?;
                if let Some(entity) = var_assign.as_ref().unwrap().get(&free.term) {
                    if let Some(grounded) = entity.get_class(free.parent.get_name()) {
                        if free.grounded_eq(grounded) {
                            return grounded.bms.as_ref().map(|bms| {
                                Arc::new(
                                    (&**bms).try_into().unwrap_or_else(|_| {
                                        unreachable!("SIMAG - illegal conversion")
                                    }),
                                )
                            });
                        }
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            }
            Predicate::GroundedMemb(ref compare) => {
                let entity = agent.get_obj_from_class(self.get_name(), &compare.term);
                if let Some(grounded) = entity {
                    if grounded.compare_ignoring_times(compare) {
                        return grounded.bms.as_ref().map(|bms| {
                            Arc::new(
                                (&**bms)
                                    .try_into()
                                    .unwrap_or_else(|_| unreachable!("SIMAG - illegal conversion")),
                            )
                        });
                    }
                } else {
                    return None;
                }
            }
            _ => return None, // this path won't be taken in any program
        }
        None
    }
}

impl OpArgsOps for ClassDecl {
    fn get_op_args(&self) -> Option<&[common::OpArg]> {
        self.op_args.as_deref()
    }
}

impl TimeOps for ClassDecl {
    fn get_times(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> Option<Arc<BmsWrapper<IsTimeData>>> {
        self.get_assignment(agent, var_assign)
    }

    fn get_time_payload(&self, value: Option<f32>) -> Option<BmsWrapper<IsTimeData>> {
        self.op_args.as_ref()?;
        for arg in self.op_args.as_ref().unwrap() {
            if let OpArg::Time(TimeArg::DeclTime(ref decl)) = *arg {
                return Some(decl.get_time_payload(value));
            }
        }
        None
    }
}

impl SpatialOps for ClassDecl {
    fn get_spatial_payload(&self) -> Option<BmsWrapper<IsSpatialData>> {
        self.op_args.as_ref()?;
        for arg in self.op_args.as_ref().unwrap() {
            if let OpArg::Spatial(SpatialArg::DeclLocation(loc)) = arg {
                return Some(BmsWrapper::<IsSpatialData>::new(Some(loc.clone())));
            }
        }
        None
    }

    fn get_location(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> Option<Arc<BmsWrapper<IsSpatialData>>> {
        self.get_assignment(agent, var_assign)
    }
}

impl std::iter::IntoIterator for ClassDecl {
    type Item = GroundedMemb;
    type IntoIter = std::vec::IntoIter<GroundedMemb>;

    fn into_iter(mut self) -> Self::IntoIter {
        let mut v = Vec::with_capacity(self.args.len());
        for _ in 0..self.args.len() {
            match self.args.pop() {
                Some(Predicate::GroundedMemb(grfact)) => v.push(grfact),
                Some(_) | None => {}
            }
        }
        v.shrink_to_fit();
        v.into_iter()
    }
}

impl<T: ProofResContext> LogSentResolution<T> for ClassDecl {
    /// Compare each term of a class declaration if they are comparable, and returns
    /// the result of such comparison (or none in case they are not comparable).
    fn grounded_eq(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        _: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
        context: &mut T,
    ) -> Option<bool> {
        for a in &self.args {
            match *a {
                Predicate::FreeMembershipToClass(ref free) => {
                    assignments?;
                    if let Some(entity) = assignments.as_ref().unwrap().get(&free.term) {
                        if let Some(current) = entity.get_class(free.parent.get_name()) {
                            context.push_antecedents(Grounded::Class(Arc::downgrade(
                                &current.clone(),
                            )));
                            if let Some(time) = current
                                .bms
                                .as_ref()
                                .unwrap()
                                .get_newest_date(context.newest_grfact())
                            {
                                context.set_newest_grfact(time);
                            }
                            if !free.grounded_eq(current) {
                                return Some(false);
                            }
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                Predicate::GroundedMemb(ref compare) => {
                    if context.compare_cls(compare) {
                        let cmp = context.has_cls_memb(compare);
                        if let Some(false) = cmp {
                            context.set_inconsistent(true);
                            return Some(false);
                        } else {
                            return cmp;
                        }
                    } else {
                        let entity = agent.get_obj_from_class(self.get_name(), &compare.term);
                        if let Some(current) = entity {
                            let grounded = Grounded::Class(Arc::downgrade(&current.clone()));
                            context.push_antecedents(grounded);
                            if let Some(time) = current
                                .bms
                                .as_ref()
                                .unwrap()
                                .get_newest_date(context.newest_grfact())
                            {
                                context.set_newest_grfact(time);
                            }
                            if !current.compare_ignoring_times(compare) {
                                return Some(false);
                            }
                        } else {
                            return None;
                        }
                    }
                }
                _ => return None, // this path won't be taken in any program
            }
        }
        Some(true)
    }

    fn substitute(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
        context: &mut T,
    ) {
        let time_data = self.get_own_time_data(time_assign, None);
        for a in &self.args {
            let grfact = match *a {
                Predicate::FreeMembershipToClass(ref free) => {
                    if let Some(entity) = assignments.as_ref().unwrap().get(&free.term) {
                        GroundedMemb::from_free(free, &*entity.name)
                    } else {
                        break;
                    }
                }
                Predicate::GroundedMemb(ref grounded) => grounded.clone(),
                _ => return, // this path won't be taken in any program
            };
            let t = time_data.clone();
            t.replace_value(grfact.get_value());
            if let Some(bms) = grfact.bms.as_ref() {
                bms.overwrite_data(t.into()).unwrap();
            };
            #[cfg(debug_assertions)]
            {
                log::trace!("Correct substitution found, updating: {:?}", grfact);
            }
            context.push_grounded_cls(grfact.clone(), grfact.bms.as_ref().unwrap().get_last_time());
            agent.up_membership(&Arc::new(grfact), Some(context))
        }
    }
}
