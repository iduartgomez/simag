use std::collections::HashMap;
use std::sync::Arc;

use dashmap::DashMap;
use float_cmp::ApproxEqUlps;
use parking_lot::RwLock;

use crate::agent::kb::{
    bms::BmsWrapper,
    repr::{lookahead_rules, Representation},
};
use crate::agent::lang::{
    FreeClassMembership, FreeClsMemb, FuncDecl, Grounded, GroundedFunc, GroundedMemb, GroundedRef,
    LogSentence, Operator, Predicate, ProofResContext,
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
pub(in crate::agent) struct Class {
    pub name: String,
    classes: DashMap<String, Arc<GroundedMemb>>,
    relations: DashMap<String, Vec<Arc<GroundedFunc>>>,
    pub beliefs: DashMap<String, Vec<Arc<LogSentence>>>,
    pub rules: RwLock<Vec<Arc<LogSentence>>>,
    pub(in crate::agent::kb) members: RwLock<Vec<ClassMember>>,
}

impl Class {
    pub(in crate::agent::kb) fn new(name: String, _kind: ClassKind) -> Class {
        Class {
            name,
            classes: DashMap::new(),
            relations: DashMap::new(),
            beliefs: DashMap::new(),
            rules: RwLock::new(Vec::new()),
            members: RwLock::new(Vec::new()),
        }
    }

    pub(in crate::agent::kb) fn belongs_to_class(
        &self,
        class_name: &str,
        interval: bool,
    ) -> Option<Arc<GroundedMemb>> {
        match self.classes.get(class_name) {
            Some(r) if r.get_value().is_some() || interval => Some(r.clone()),
            Some(_) | None => None,
        }
    }

    pub(in crate::agent::kb) fn get_class_membership(
        &self,
        compare: &FreeClassMembership,
    ) -> Vec<Arc<GroundedMemb>> {
        self.classes
            .iter()
            .filter_map(|x| {
                if compare.filter_grounded(x.value()) {
                    Some(x.value().clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    /// Set or update a superclass of this class. Returns whether this was a new superclass
    /// or already existed.
    pub(in crate::agent::kb) fn add_or_update_class_membership<T: ProofResContext>(
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
        if self.classes.contains_key(name) {
            let current = self.classes.get(name).unwrap();
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
            if let Some(context) = context {
                let bms = grounded.bms.as_ref().unwrap();
                bms.set_last_rec_producer(Some((context.get_id(), context.get_production_time())));
                BmsWrapper::update_producers(
                    &Grounded::Class(Arc::downgrade(&grounded.clone())),
                    context,
                );
            }
            self.classes.insert(name.to_string(), grounded.clone());
            true
        }
    }

    /// Add members of this class, being them other classes or entities.
    pub(in crate::agent::kb) fn add_member(&self, member: ClassMember) {
        self.members.write().push(member);
    }

    pub fn get_members(&self, comp: &FreeClsMemb) -> Vec<Arc<GroundedMemb>> {
        let lock = self.members.read();
        lock.iter()
            .map(|x| x.unwrap_memb())
            .filter(|m| comp.grounded_eq(m))
            .collect::<Vec<_>>()
    }

    pub(in crate::agent::kb) fn has_relationship(
        &self,
        func: &GroundedFunc,
    ) -> Option<Arc<GroundedFunc>> {
        if let Some(relation_type) = self.relations.get(func.get_name()) {
            for rel in relation_type.value() {
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
        // FIXME: check time equality!
        let mut res = HashMap::new();
        let (op, val) = compare.get_uval();
        for functions in self.relations.iter() {
            for f in functions.value() {
                if f.name_in_pos(&*self.name, pos) {
                    // Safety: guaranteed this lives as long as self
                    let t = unsafe { &*(&**f as *const GroundedFunc) };
                    let rel_name = t.get_name();
                    match op {
                        None => res
                            .entry(rel_name)
                            .or_insert_with(|| vec![])
                            .push(f.clone()),
                        Some(Operator::Equal) => {
                            if f.get_value()
                                .unwrap()
                                .approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS)
                            {
                                res.entry(rel_name)
                                    .or_insert_with(|| vec![])
                                    .push(f.clone())
                            }
                        }
                        Some(Operator::More) => {
                            if *val.as_ref().unwrap() < f.get_value().unwrap() {
                                res.entry(rel_name)
                                    .or_insert_with(|| vec![])
                                    .push(f.clone())
                            }
                        }
                        Some(Operator::Less) => {
                            if *val.as_ref().unwrap() > f.get_value().unwrap() {
                                res.entry(rel_name)
                                    .or_insert_with(|| vec![])
                                    .push(f.clone())
                            }
                        }
                        Some(Operator::LessEqual) => {
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
                        Some(Operator::MoreEqual) => {
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
                        _ => unreachable!(),
                    }
                }
            }
        }
        res
    }

    pub fn get_funcs(&self, func: &FuncDecl) -> Vec<Arc<GroundedFunc>> {
        // FIXME: check time equality!
        let mut res = vec![];
        let lock = self.members.read();
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
                        (Operator::Equal, val) => {
                            if !val.approx_eq_ulps(&curr_func.get_value().unwrap(), FLOAT_EQ_ULPS) {
                                process = false;
                            }
                        }
                        (Operator::More, val) => {
                            if val > curr_func.get_value().unwrap() {
                                process = false;
                            }
                        }
                        (Operator::Less, val) => {
                            if val < curr_func.get_value().unwrap() {
                                process = false;
                            }
                        }
                        (Operator::LessEqual, val) => {
                            if val < curr_func.get_value().unwrap()
                                || !val
                                    .approx_eq_ulps(&curr_func.get_value().unwrap(), FLOAT_EQ_ULPS)
                            {
                                process = false;
                            }
                        }
                        (Operator::MoreEqual, val) => {
                            if val > curr_func.get_value().unwrap()
                                || !val
                                    .approx_eq_ulps(&curr_func.get_value().unwrap(), FLOAT_EQ_ULPS)
                            {
                                process = false;
                            }
                        }
                        _ => unreachable!(),
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
        if self.relations.contains_key(name) {
            let mut found_rel = false;
            {
                let funcs_type = self.relations.get(name).unwrap();
                for f in funcs_type.value() {
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
                let mut funcs_type = self.relations.get_mut(name).unwrap();
                funcs_type.push(func.clone());
                true
            } else {
                false
            }
        } else {
            if let Some(context) = context {
                func.bms
                    .set_last_rec_producer(Some((context.get_id(), context.get_production_time())));
                BmsWrapper::update_producers(
                    &Grounded::Function(Arc::downgrade(&func.clone())),
                    context,
                );
            }
            self.relations.insert(name.to_string(), vec![func.clone()]);
            true
        }
    }

    /// Add a grounded relationship of this kind of relationship
    pub(in crate::agent::kb) fn add_relation_to_class(&self, func: Arc<GroundedFunc>) {
        self.members.write().push(ClassMember::Func(func));
    }

    pub(in crate::agent::kb) fn add_belief(&self, belief: Arc<LogSentence>, parent: &str) {
        if self.beliefs.contains_key(parent) {
            if let Some(mut ls) = self.beliefs.get_mut(parent) {
                ls.push(belief)
            }
        } else {
            self.beliefs.insert(parent.to_string(), vec![belief]);
        }
    }

    pub(in crate::agent::kb) fn add_rule(&self, rule: Arc<LogSentence>) {
        self.rules.write().push(rule);
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
            ClassMember::Func(_) => unreachable!(),
        }
    }

    #[inline]
    fn unwrap_fn(&self) -> Arc<GroundedFunc> {
        match *self {
            ClassMember::Func(ref f) => f.clone(),
            ClassMember::Entity(_) | ClassMember::Class(_) => unreachable!(),
        }
    }
}
