use std::collections::HashMap;
use std::sync::{Arc, atomic::AtomicBool};

use super::*;
use crate::agent::{
    bms::BmsWrapper,
    kb::{repr::Representation, VarAssignment},
};
use common::*;
use logsent::{ParseContext, LogSentResolution};
use parser::ClassDeclBorrowed;

#[derive(Debug, Clone)]
pub(in crate::agent) struct ClassDecl {
    name: Terminal,
    args: Vec<Predicate>,
    pub op_args: Option<Vec<OpArg>>,
}

impl<'a> ClassDecl {
    pub fn from(
        other: &ClassDeclBorrowed<'a>,
        context: &mut ParseContext,
    ) -> Result<ClassDecl, ParseErrF> {
        let class_name = Terminal::from(&other.name, context)?;
        let op_args = match other.op_args {
            Some(ref oargs) => {
                let mut v0 = Vec::with_capacity(oargs.len());
                for e in oargs {
                    let a = OpArg::from(e, context)?;
                    v0.push(a);
                }
                Some(v0)
            }
            None => None,
        };
        let args = {
            let mut v0 = Vec::with_capacity(other.args.len());
            for arg in &other.args {
                if arg.uval.is_none() {
                    return Err(ParseErrF::IUValNone);
                }
                let pred = Predicate::from(arg, context, &class_name, false)?;
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

    pub fn get_args(&self) -> &[Predicate] {
        &self.args
    }

    pub fn get_args_mut(&mut self) -> &mut [Predicate] {
        &mut self.args
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        match self.name {
            Terminal::FreeTerm(ref var) => &var.name,
            Terminal::GroundedTerm(ref name) => name,
            Terminal::Keyword(_) => panic!(),
        }
    }

    #[inline]
    pub fn get_parent(&self) -> &Terminal {
        &self.name
    }

    /// If the class declaration has an overwrite flag then if there are any previous records
    /// those will be dropped.
    pub fn get_own_time_data(
        &self,
        assignments: &HashMap<&Var, Arc<BmsWrapper>>,
        value: Option<f32>,
    ) -> BmsWrapper {
        if self.op_args.is_none() {
            let bms = BmsWrapper::new(false);
            bms.new_record(None, value, None);
            return bms;
        }
        let mut v = None;
        let mut ow = false;
        for arg in self.op_args.as_ref().unwrap() {
            match *arg {
                OpArg::TimeDecl(_) | OpArg::TimeVarAssign(_) => {
                    v = Some(arg.get_time_payload(assignments, value));
                }
                OpArg::OverWrite => {
                    ow = true;
                }
                _ => {}
            }
        }
        if v.is_none() {
            let bms = BmsWrapper::new(ow);
            bms.new_record(None, value, None);
            bms
        } else {
            let mut bms = v.unwrap();
            bms.overwrite = AtomicBool::new(ow);
            bms
        }
    }

    pub(in crate::agent::lang) fn get_time_decl(&self, var0: &Var) -> bool {
        if self.op_args.is_none() {
            return false;
        }
        for arg in self.op_args.as_ref().unwrap() {
            if let OpArg::TimeVarFrom(ref var1) = *arg {
                return var1.as_ref() == var0;
            }
        }
        false
    }

    pub(in crate::agent::lang) fn get_times(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> Option<Arc<BmsWrapper>> {
        let arg = &self.args[0];
        match *arg {
            Predicate::FreeClsMemb(ref free) => {
                var_assign?;
                if let Some(entity) = var_assign.as_ref().unwrap().get(&*free.term) {
                    if let Some(grounded) = entity.get_class(free.parent.get_name()) {
                        if free.grounded_eq(grounded) {
                            return grounded.bms.clone();
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
                    if *grounded == *compare {
                        return grounded.bms.clone();
                    }
                } else {
                    return None;
                }
            }
            _ => return None, // this path won't be taken in any program
        }
        None
    }

    pub fn get_time_payload(&self, value: Option<f32>) -> Option<BmsWrapper> {
        self.op_args.as_ref()?;
        for arg in self.op_args.as_ref().unwrap() {
            if let OpArg::TimeDecl(ref decl) = *arg {
                return Some(decl.get_time_payload(value));
            }
        }
        None
    }

    pub(in crate::agent::lang) fn generate_uid(&self) -> Vec<u8> {
        let mut id = vec![];
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
        id
    }

    pub(in crate::agent::lang) fn contains_var(&self, var: &Var) -> bool {
        for a in &self.args {
            match *a {
                Predicate::FreeClsMemb(ref term)
                    if &*term.term as *const Var == &*var as *const Var =>
                {
                    return true
                }
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
        _: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) -> Option<bool> {
        for a in &self.args {
            match *a {
                Predicate::FreeClsMemb(ref free) => {
                    assignments?;
                    if let Some(entity) = assignments.as_ref().unwrap().get(&*free.term) {
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
                            if *current != *compare {
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
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) {
        use crate::agent::bms::ReplaceMode;

        let time_data = self.get_own_time_data(time_assign, None);
        for a in &self.args {
            let grfact = match *a {
                Predicate::FreeClsMemb(ref free) => {
                    if let Some(entity) = assignments.as_ref().unwrap().get(&*free.term) {
                        GroundedMemb::from_free(free, entity.name)
                    } else {
                        break;
                    }
                }
                Predicate::GroundedMemb(ref grounded) => grounded.clone(),
                _ => return, // this path won't be taken in any program
            };
            let t = time_data.clone();
            t.replace_value(grfact.get_value(), ReplaceMode::Substitute);
            grfact.overwrite_time_data(&t);
            context.push_grounded_cls(
                grfact.clone(),
                grfact.bms.as_ref().unwrap().get_last_date(),
            );
            agent.up_membership(&Arc::new(grfact), Some(context))
        }
    }
}
