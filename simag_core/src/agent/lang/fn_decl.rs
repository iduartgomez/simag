use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::sync::Arc;

use super::{
    common::*,
    logsent::{LogSentResolution, ParseContext},
    parser::{FuncDeclBorrowed, FuncVariants},
    time_semantics::{TimeArg::*, TimeFn, TimeOps},
    var::Var,
    *,
};
use crate::agent::{
    kb::bms::{BmsWrapper, IsTimeData, RecordHistory},
    kb::{repr::Representation, VarAssignment},
};

#[derive(Debug, Clone)]
pub(in crate::agent) struct FuncDecl {
    pub(in crate::agent::lang) name: Terminal,
    pub(in crate::agent::lang) args: Option<Vec<Predicate>>,
    pub op_args: Option<Vec<OpArg>>,
    pub variant: FuncVariants,
}

impl<'a> FuncDecl {
    pub fn from(
        other: &FuncDeclBorrowed<'a>,
        context: &mut ParseContext,
    ) -> Result<FuncDecl, ParseErrF> {
        let func_name = Terminal::from(&other.name, context)?;
        match other.variant {
            FuncVariants::Relational => FuncDecl::decl_relational_fn(other, context, func_name),
            FuncVariants::NonRelational => {
                FuncDecl::decl_nonrelational_fn(other, context, func_name)
            }
        }
    }

    pub fn is_grounded(&self) -> bool {
        if !self.parent_is_grounded() {
            return false;
        }
        for a in self.args.as_ref().unwrap().iter() {
            match *a {
                Predicate::GroundedMemb(_) => {}
                _ => return false,
            }
        }
        true
    }

    pub fn is_relational(&self) -> bool {
        if let FuncVariants::Relational = self.variant {
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        match self.name {
            Terminal::FreeTerm(ref var) => &var.name,
            Terminal::GroundedTerm(ref name) => name,
        }
    }

    #[inline]
    pub fn get_uval(&self) -> (Operator, f32) {
        let (op, val) = self.args.as_ref().unwrap().get(0).unwrap().get_uval();
        (op.unwrap(), val.unwrap())
    }

    #[inline]
    pub fn get_parent(&self) -> &Terminal {
        &self.name
    }

    pub fn get_args(&self) -> &[Predicate] {
        self.args.as_ref().unwrap()
    }

    pub(in crate::agent::lang) fn generate_uid(&self) -> Vec<u8> {
        let mut id = vec![];
        id.append(&mut self.name.generate_uid());
        if let Some(ref args) = self.args {
            for a in args {
                let mut id_2 = a.generate_uid();
                id.append(&mut id_2)
            }
        }
        if let Some(ref args) = self.op_args {
            for a in args {
                let mut id_2 = a.generate_uid();
                id.append(&mut id_2)
            }
        }
        id
    }

    fn decl_relational_fn(
        other: &FuncDeclBorrowed<'a>,
        context: &mut ParseContext,
        name: Terminal,
    ) -> Result<FuncDecl, ParseErrF> {
        let op_args = match other.op_args {
            Some(ref oargs) => {
                let mut v0 = Vec::with_capacity(oargs.len());
                for e in oargs {
                    let a = OpArg::try_from((e, &*context))?;
                    v0.push(a);
                }
                Some(v0)
            }
            None => None,
        };
        let mut args = Vec::with_capacity(3);
        if let Some(oargs) = other.args.as_ref() {
            if oargs.len() > 3 || oargs.len() < 2 {
                return Err(ParseErrF::WrongArgNumb);
            }
            let mut vars = 0;
            for (i, a) in oargs.iter().enumerate() {
                let pred = Predicate::from(a, context, &name, Some(i))?;
                if pred.has_uval() && (i == 1 || i == 2) {
                    return Err(ParseErrF::RFuncWrongArgs);
                }
                if pred.is_var() {
                    vars += 1;
                }
                args.push(pred);
            }
            if (oargs.len() == vars) && name.is_var() {
                // it's a free fn query, but at least one of the arguments must be grounded
                return Err(ParseErrF::BothAreVars);
            }
        } else {
            return Err(ParseErrF::WrongArgNumb);
        }
        Ok(FuncDecl {
            name,
            args: Some(args),
            op_args,
            variant: FuncVariants::Relational,
        })
    }

    fn decl_nonrelational_fn(
        other: &FuncDeclBorrowed<'a>,
        context: &mut ParseContext,
        name: Terminal,
    ) -> Result<FuncDecl, ParseErrF> {
        let op_args = match other.op_args {
            Some(ref oargs) => {
                let mut v0 = Vec::with_capacity(oargs.len());
                for e in oargs {
                    let a = match OpArg::try_from((e, &*context)) {
                        Err(err) => return Err(err),
                        Ok(a) => a,
                    };
                    v0.push(a);
                }
                Some(v0)
            }
            None => None,
        };
        Ok(FuncDecl {
            name,
            args: None,
            op_args,
            variant: FuncVariants::NonRelational,
        })
    }

    pub(in crate::agent::lang) fn contains_var(&self, var: &Var) -> bool {
        if self.args.is_some() {
            for a in self.args.as_ref().unwrap() {
                if let Predicate::FreeMembershipToClass(ref term) = *a {
                    if &*term.term == var {
                        return true;
                    }
                } else {
                    continue;
                }
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

    pub fn var_substitution(&mut self) -> Result<(), ParseErrF> {
        if let Some(op_args) = &mut self.op_args {
            for op_arg in op_args {
                op_arg.var_substitution()?;
            }
        }
        Ok(())
    }
}

impl Into<GroundedFunc> for FuncDecl {
    /// Assumes all arguments are grounded and converts to a GroundedFunc (panics otherwise).
    fn into(self) -> GroundedFunc {
        let FuncDecl {
            name,
            args,
            op_args,
            ..
        } = self;
        let name = match name {
            Terminal::GroundedTerm(name) => name,
            Terminal::FreeTerm(_) => unreachable!(),
        };
        let mut first = None;
        let mut second = None;
        let mut third = None;
        let mut val = None;
        let mut args = args.unwrap();
        for (i, a) in args.drain(..).enumerate() {
            let mut n_a = match a {
                Predicate::GroundedMemb(term) => term,
                Predicate::FreeMembershipToClass(_) | Predicate::FreeClassMembership(_) => {
                    unreachable!()
                }
            };
            n_a.bms = None;
            if i == 0 {
                val = n_a.get_value();
                first = Some(n_a);
            } else if i == 1 {
                second = Some(n_a);
            } else {
                third = Some(n_a);
            }
        }
        let time_data = BmsWrapper::<RecordHistory>::new();
        let mut ow = false;
        if let Some(mut oargs) = op_args {
            for arg in oargs.drain(..) {
                match arg {
                    OpArg::Time(DeclTime(TimeFn::Since(time))) => {
                        time_data.new_record(Some(time), None, val, None);
                    }
                    OpArg::Time(DeclTime(TimeFn::Interval(t0, t1))) => {
                        time_data.new_record(Some(t0), None, val, None);
                        time_data.new_record(Some(t1), None, None, None);
                    }
                    OpArg::Time(DeclTime(TimeFn::Now)) => {
                        time_data.new_record(Some(Utc::now()), None, val, None);
                    }
                    OpArg::OverWrite => {
                        ow = true;
                    }
                    _ => {}
                }
            }
        }
        if time_data.record_len() == 0 {
            time_data.new_record(None, None, val, None);
        }
        GroundedFunc {
            name,
            args: [first.unwrap(), second.unwrap()],
            third,
            bms: Arc::new(time_data.with_ow_val(ow)),
        }
    }
}

impl OpArgsOps for FuncDecl {
    fn get_op_args(&self) -> Option<&[common::OpArg]> {
        self.op_args.as_deref()
    }
}

impl TimeOps for FuncDecl {
    fn get_times(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> Option<Arc<BmsWrapper<IsTimeData>>> {
        if self.is_grounded() {
            let sbj = self.args.as_ref().unwrap();
            let grfunc = self.clone().into();
            if let Some(relation) = agent.get_relationship(&grfunc, sbj[0].get_name()) {
                Some(Arc::new((&*relation.bms).try_into().unwrap_or_else(|_| {
                    unreachable!(
                        "SIMAG - {}:{} - unreachable: illegal conversion",
                        file!(),
                        line!()
                    )
                })))
            } else {
                None
            }
        } else {
            var_assign?;
            let f = HashMap::new();
            if let Ok(grfunc) = GroundedFunc::from_free(self, var_assign, &f) {
                for arg in self.get_args() {
                    if let Predicate::FreeMembershipToClass(ref arg) = *arg {
                        let assignments = var_assign.as_ref().unwrap();
                        if let Some(entity) = assignments.get(&*arg.term) {
                            if let Some(current) = entity.get_relationship(&grfunc) {
                                return Some(Arc::new((&*current.bms).try_into().unwrap_or_else(
                                    |_| {
                                        unreachable!(
                                            "SIMAG - {}:{} - unreachable: illegal conversion",
                                            file!(),
                                            line!()
                                        )
                                    },
                                )));
                            }
                        }
                    }
                }
            }
            None
        }
    }
}

impl<T: ProofResContext> LogSentResolution<T> for FuncDecl {
    /// Compares two relational functions, if they include free terms variable values
    /// assignments must be provided or will return None or panic in worst case.
    fn grounded_eq(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
        context: &mut T,
    ) -> Option<bool> {
        if self.is_grounded() {
            let sbj = self.args.as_ref().unwrap();
            let grfunc = self.clone().into();
            if context.compare_relation(&grfunc) {
                let cmp = context.has_relationship(&grfunc);
                if let Some(false) = cmp {
                    context.set_inconsistent(true);
                    Some(false)
                } else {
                    cmp
                }
            } else {
                agent.has_relationship(&grfunc, sbj[0].get_name())
            }
        } else {
            let assigned = assignments?;
            if let Ok(grfunc) = GroundedFunc::from_free(self, assignments, time_assign) {
                for arg in self.get_args() {
                    if let Predicate::FreeMembershipToClass(ref arg) = *arg {
                        if let Some(entity) = assigned.get(&*arg.term) {
                            if let Some(current) = entity.get_relationship(&grfunc) {
                                let a = Grounded::Function(Arc::downgrade(&current.clone()));
                                context.push_antecedents(a);
                                if let Some(time) =
                                    current.bms.get_newest_date(context.newest_grfact())
                                {
                                    context.set_newest_grfact(time);
                                }
                                if !current.compare_ignoring_times(&grfunc) {
                                    return Some(false);
                                } else {
                                    return Some(true);
                                }
                            }
                        }
                    }
                }
            }
            None
        }
    }

    fn substitute(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
        context: &mut T,
    ) {
        if let Ok(grfunc) = GroundedFunc::from_free(self, assignments, time_assign) {
            let time_data = self.get_own_time_data(time_assign, None);
            time_data.replace_value(grfunc.get_value());
            grfunc.bms.overwrite_data(time_data);
            #[cfg(debug_assertions)]
            {
                log::trace!("Correct substitution found, updating: {:?}", grfunc);
            }
            context.push_grounded_func(grfunc.clone(), grfunc.bms.get_last_date());
            agent.up_relation(&Arc::new(grfunc), Some(context));
        }
    }
}
