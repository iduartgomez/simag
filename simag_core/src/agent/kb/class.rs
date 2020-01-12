use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use float_cmp::ApproxEqUlps;

use crate::agent::kb::{
    bms::BmsWrapper,
    repr::{lookahead_rules, Representation},
};
use crate::agent::lang::{
    CompOperator, FreeClassMembership, FreeClsMemb, FuncDecl, Grounded, GroundedFunc, GroundedMemb,
    GroundedRef, LogSentence, Predicate, ProofResContext,
};
use crate::FLOAT_EQ_ULPS;

/// A class is a set of entities that share some properties.
/// It can be a subset of others supersets, and viceversa.
///
/// Membership is not binary, but fuzzy, being the extreme cases (0, 1)
/// the classic binary membership. Likewise, membership to a class can be
/// temporal. For more info check `Entity` type documentation.
///
/// All the attributes of a class are inherited by their members
/// (to a fuzzy degree).
#[derive(Debug)]
pub(in crate::agent) struct Class {
    pub name: String,
    classes: RwLock<HashMap<String, Arc<GroundedMemb>>>,
    relations: RwLock<HashMap<String, Vec<Arc<GroundedFunc>>>>,
    pub beliefs: RwLock<HashMap<String, Vec<Arc<LogSentence>>>>,
    pub rules: RwLock<Vec<Arc<LogSentence>>>,
    kind: ClassKind,
    pub(in crate::agent::kb) members: RwLock<Vec<ClassMember>>,
}

impl Class {
    pub(in crate::agent::kb) fn new(name: String, kind: ClassKind) -> Class {
        Class {
            name,
            classes: RwLock::new(HashMap::new()),
            relations: RwLock::new(HashMap::new()),
            beliefs: RwLock::new(HashMap::new()),
            rules: RwLock::new(Vec::new()),
            kind,
            members: RwLock::new(Vec::new()),
        }
    }

    pub(in crate::agent::kb) fn belongs_to_class(
        &self,
        class_name: &str,
        interval: bool,
    ) -> Option<Arc<GroundedMemb>> {
        let lock = self.classes.read().unwrap();
        match lock.get(class_name) {
            Some(r) if r.get_value().is_some() || interval => Some(r.clone()),
            Some(_) | None => None,
        }
    }

    pub(in crate::agent::kb) fn get_class_membership(
        &self,
        compare: &FreeClassMembership,
    ) -> Vec<Arc<GroundedMemb>> {
        let lock = self.classes.read().unwrap();
        lock.values()
            .filter(|x| compare.filter_grounded(&**x))
            .cloned()
            .collect::<Vec<_>>()
    }

    /// Set a superclass of this class
    pub(in crate::agent::kb) fn add_class_membership<T: ProofResContext>(
        &self,
        agent: &Representation,
        grounded: &Arc<GroundedMemb>,
        context: Option<&T>,
    ) -> bool {
        let name = grounded.get_parent();
        if let Some(context) = context {
            if !context.is_substituting()
                && lookahead_rules(agent, name, &GroundedRef::Class(&*grounded))
            {
                return false;
            }
        } else if lookahead_rules(agent, name, &GroundedRef::Class(&*grounded)) {
            return false;
        }
        let stmt_exists = {
            let lock = self.classes.read().unwrap();
            lock.contains_key(name)
        };
        if stmt_exists {
            let lock = self.classes.read().unwrap();
            let current = lock.get(name).unwrap();
            if let Some(context) = context {
                current.update(
                    agent,
                    &*grounded,
                    Some((context.get_id(), context.get_production_time())),
                );
                BmsWrapper::update_producers(
                    &Grounded::Class(Arc::downgrade(&current.clone())),
                    context,
                );
            } else {
                current.update(agent, &*grounded, None);
            }
            false
        } else {
            let mut lock = self.classes.write().unwrap();
            if let Some(context) = context {
                let bms = grounded.bms.as_ref().unwrap();
                bms.set_last_rec_producer(Some((context.get_id(), context.get_production_time())));
                BmsWrapper::update_producers(
                    &Grounded::Class(Arc::downgrade(&grounded.clone())),
                    context,
                );
            }
            lock.insert(name.to_string(), grounded.clone());
            true
        }
    }

    /// Add members of this class, being them other classes or entities.
    pub(in crate::agent::kb) fn add_member(&self, member: ClassMember) {
        self.members.write().unwrap().push(member);
    }

    pub fn get_members(&self, comp: &FreeClsMemb) -> Vec<Arc<GroundedMemb>> {
        let lock = self.members.read().unwrap();
        lock.iter()
            .map(|x| x.unwrap_memb())
            .filter(|m| comp.grounded_eq(m))
            .collect::<Vec<_>>()
    }

    pub(in crate::agent::kb) fn has_relationship(
        &self,
        func: &GroundedFunc,
    ) -> Option<Arc<GroundedFunc>> {
        let lock = self.relations.read().unwrap();
        if let Some(relation_type) = lock.get(func.get_name()) {
            for rel in relation_type {
                if rel.comparable(func) {
                    return Some(rel.clone());
                }
            }
        }
        None
    }

    pub(in crate::agent::kb) fn get_relationships(
        &self,
        pos: usize,
        compare: &Predicate,
    ) -> HashMap<&str, Vec<Arc<GroundedFunc>>> {
        let mut res = HashMap::new();
        let (op, val) = compare.get_uval();
        let lock = self.relations.read().unwrap();
        for functions in lock.values() {
            for f in functions {
                if f.name_in_pos(&*self.name, pos) {
                    // guaranteed this lives as long as self
                    let t = unsafe { &*(&**f as *const GroundedFunc) };
                    let rel_name = t.get_name();
                    match op {
                        None => res
                            .entry(rel_name)
                            .or_insert_with(|| vec![])
                            .push(f.clone()),
                        Some(CompOperator::Equal) => {
                            if f.get_value()
                                .unwrap()
                                .approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS)
                            {
                                res.entry(rel_name)
                                    .or_insert_with(|| vec![])
                                    .push(f.clone())
                            }
                        }
                        Some(CompOperator::More) => {
                            if *val.as_ref().unwrap() < f.get_value().unwrap() {
                                res.entry(rel_name)
                                    .or_insert_with(|| vec![])
                                    .push(f.clone())
                            }
                        }
                        Some(CompOperator::Less) => {
                            if *val.as_ref().unwrap() > f.get_value().unwrap() {
                                res.entry(rel_name)
                                    .or_insert_with(|| vec![])
                                    .push(f.clone())
                            }
                        }
                        Some(CompOperator::LessEqual) => {
                            if *val.as_ref().unwrap() > f.get_value().unwrap()
                                || f.get_value()
                                    .unwrap()
                                    .approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS)
                            {
                                res.entry(rel_name)
                                    .or_insert_with(|| vec![])
                                    .push(f.clone())
                            }
                        }
                        Some(CompOperator::MoreEqual) => {
                            if *val.as_ref().unwrap() < f.get_value().unwrap()
                                || f.get_value()
                                    .unwrap()
                                    .approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS)
                            {
                                res.entry(rel_name)
                                    .or_insert_with(|| vec![])
                                    .push(f.clone())
                            }
                        }
                        Some(CompOperator::Until)
                        | Some(CompOperator::At)
                        | Some(CompOperator::FromUntil) => unreachable!(),
                    }
                }
            }
        }
        res
    }

    pub fn get_funcs(&self, func: &FuncDecl) -> Vec<Arc<GroundedFunc>> {
        let mut res = vec![];
        let lock = self.members.read().unwrap();
        for curr_func in lock.iter() {
            let curr_func = curr_func.unwrap_fn();
            let mut process = true;
            for (i, arg) in func.get_args().iter().enumerate() {
                if !arg.is_var() && (arg.get_name() != curr_func.get_args_names()[i]) {
                    process = false;
                    break;
                }
                if i == 0 {
                    match func.get_uval() {
                        (CompOperator::Equal, val) => {
                            if !val.approx_eq_ulps(&curr_func.get_value().unwrap(), FLOAT_EQ_ULPS) {
                                process = false;
                            }
                        }
                        (CompOperator::More, val) => {
                            if val > curr_func.get_value().unwrap() {
                                process = false;
                            }
                        }
                        (CompOperator::Less, val) => {
                            if val < curr_func.get_value().unwrap() {
                                process = false;
                            }
                        }
                        (CompOperator::LessEqual, val) => {
                            if val < curr_func.get_value().unwrap()
                                || !val
                                    .approx_eq_ulps(&curr_func.get_value().unwrap(), FLOAT_EQ_ULPS)
                            {
                                process = false;
                            }
                        }
                        (CompOperator::MoreEqual, val) => {
                            if val > curr_func.get_value().unwrap()
                                || !val
                                    .approx_eq_ulps(&curr_func.get_value().unwrap(), FLOAT_EQ_ULPS)
                            {
                                process = false;
                            }
                        }
                        (CompOperator::Until, _)
                        | (CompOperator::At, _)
                        | (CompOperator::FromUntil, _) => unreachable!(),
                    }
                }
            }
            if process {
                res.push(curr_func.clone());
            }
        }
        res
    }

    /// Add a relationship this class has with other classes/entities.
    /// Returns 'true' in case the relationship didn't exist,
    /// 'false' otherwise. If it already existed it's value is updated.
    pub(in crate::agent::kb) fn add_relationship<T: ProofResContext>(
        &self,
        agent: &Representation,
        func: &Arc<GroundedFunc>,
        context: Option<&T>,
    ) -> bool {
        let name = func.get_name();
        if let Some(context) = context {
            if !context.is_substituting()
                && lookahead_rules(agent, name, &GroundedRef::Function(&*func))
            {
                return false;
            }
        } else if lookahead_rules(agent, name, &GroundedRef::Function(&*func)) {
            return false;
        }
        let stmt_exists = {
            let lock = self.relations.write().unwrap();
            lock.contains_key(name)
        };
        if stmt_exists {
            let mut found_rel = false;
            {
                let lock = self.relations.read().unwrap();
                let funcs_type = lock.get(name).unwrap();
                for f in funcs_type {
                    if f.comparable(&*func) {
                        if let Some(context) = context {
                            f.update(
                                agent,
                                &*func,
                                Some((context.get_id(), context.get_production_time())),
                            );
                            BmsWrapper::update_producers(
                                &Grounded::Function(Arc::downgrade(&f.clone())),
                                context,
                            );
                        } else {
                            f.update(agent, &*func, None);
                        }
                        found_rel = true;
                        break;
                    }
                }
            }
            if !found_rel {
                let mut lock = self.relations.write().unwrap();
                if let Some(context) = context {
                    func.bms.set_last_rec_producer(Some((
                        context.get_id(),
                        context.get_production_time(),
                    )));
                    BmsWrapper::update_producers(
                        &Grounded::Function(Arc::downgrade(&func.clone())),
                        context,
                    );
                }
                let funcs_type = lock.get_mut(name).unwrap();
                funcs_type.push(func.clone());
                true
            } else {
                false
            }
        } else {
            let mut lock = self.relations.write().unwrap();
            if let Some(context) = context {
                func.bms
                    .set_last_rec_producer(Some((context.get_id(), context.get_production_time())));
                BmsWrapper::update_producers(
                    &Grounded::Function(Arc::downgrade(&func.clone())),
                    context,
                );
            }
            lock.insert(name.to_string(), vec![func.clone()]);
            true
        }
    }

    /// Add a grounded relationship of this kind of relationship
    pub(in crate::agent::kb) fn add_relation_to_class(&self, func: Arc<GroundedFunc>) {
        self.members.write().unwrap().push(ClassMember::Func(func));
    }

    pub(in crate::agent::kb) fn add_belief(&self, belief: Arc<LogSentence>, parent: &str) {
        let sent_exists = self.beliefs.read().unwrap().contains_key(parent);
        if sent_exists {
            if let Some(ls) = self.beliefs.write().unwrap().get_mut(parent) {
                ls.push(belief)
            }
        } else {
            self.beliefs
                .write()
                .unwrap()
                .insert(parent.to_string(), vec![belief]);
        }
    }

    pub(in crate::agent::kb) fn add_rule(&self, rule: Arc<LogSentence>) {
        self.rules.write().unwrap().push(rule);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::agent::kb) enum ClassKind {
    Relationship,
    Membership,
}

#[derive(Debug, Clone)]
pub(in crate::agent::kb) enum ClassMember {
    Entity(Arc<GroundedMemb>),
    Class(Arc<GroundedMemb>),
    Func(Arc<GroundedFunc>),
}

impl ClassMember {
    #[inline]
    fn unwrap_memb(&self) -> Arc<GroundedMemb> {
        match *self {
            ClassMember::Entity(ref obj) | ClassMember::Class(ref obj) => obj.clone(),
            ClassMember::Func(_) => panic!(),
        }
    }

    #[inline]
    fn unwrap_fn(&self) -> Arc<GroundedFunc> {
        match *self {
            ClassMember::Func(ref f) => f.clone(),
            ClassMember::Entity(_) | ClassMember::Class(_) => panic!(),
        }
    }
}