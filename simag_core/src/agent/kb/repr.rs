use float_cmp::ApproxEqUlps;

use std::collections::HashMap;
use std::iter::FromIterator;
use std::rc::Rc;
use std::sync::{Arc, RwLock};

use crate::agent::kb::{
    bms::{BmsWrapper, ReplaceMode},
    class::*,
    inference::{
        meet_sent_req, ArgsProduct, GroundedResult, IExprResult, InfResults, Inference, QueryInput,
    },
    VarAssignment,
};
use crate::agent::lang::{
    logic_parser, Assert, ClassDecl, CompOperator, FreeClassMembership, FuncDecl, Grounded,
    GroundedFunc, GroundedMemb, GroundedRef, LogSentence, ParseErrF, ParseTree, Predicate,
    ProofResContext, Var,
};
use crate::FLOAT_EQ_ULPS;

// TODO: find better solution for self-referential borrows escaping self method scopes
// this should remove the use of unsafe in this module which seems a little bit unnecessary?
// Avoid copying/cloning if possible.

/// A container for internal agent's representations.
///
/// An agent can have any number of such representations at any moment,
/// all of which are contained in this object.
///
/// The class includes methods to encode and decode the representations
/// to/from data streams or idioms.
///
/// Attributes:
///     entities -> Unique members (entities) of their own set/class.
///     | Entities are denoted with a $ symbol followed by an alphanumeric literal.
///     classes -> Sets of objects (entities or subclasses) that share a common property.
///     | This includes 'classes of relationships' and other 'functions'.
#[derive(Default, Debug)]
pub struct Representation {
    pub(in crate::agent) entities: RwLock<HashMap<String, Entity>>,
    pub(in crate::agent) classes: RwLock<HashMap<String, Class>>,
    threads: usize,
}

impl Representation {
    pub fn new() -> Representation {
        #[cfg(feature = "tracing")]
        {
            super::tracing::Logger::get_logger();
        }

        Representation {
            entities: RwLock::new(HashMap::new()),
            classes: RwLock::new(HashMap::new()),
            threads: 4,
        }
    }

    /// Parses a sentence (or several of them) into an usable formula
    /// and stores it into the internal representation along with the
    /// corresponding classes. In case the sentence is a predicate,
    /// the objects get declared as members of their classes.
    ///
    /// Accepts first-order logic sentences, both atomic sentences
    /// ('Lucy is a professor') and complex sentences compossed
    /// of different atoms and operators ('If someone is a professor,
    /// then it's a person'). Examples:
    ///
    /// `>>> r.tell("(professor[$Lucy,u=1])")`
    /// will include the individual '$Lucy' in the professor category)
    /// `>>> r.tell("((let x) professor[x,u=1] := person[x,u=1])")`
    /// all the individuals which are professors will be added to the
    /// person category, and the formula will be stored in the professor
    /// class for future use.
    ///
    /// For more examples check the LogSentence type docs.
    pub fn tell(&mut self, source: &str) -> Result<(), Vec<ParseErrF>> {
        let pres = logic_parser(source, true, self.threads);
        if let Ok(mut sentences) = pres {
            let mut errors = Vec::new();
            for _ in 0..sentences.len() {
                match sentences.pop_front().unwrap() {
                    ParseTree::Assertion(assertions) => {
                        for assertion in assertions {
                            if assertion.is_class() {
                                let cls_decl = assertion.unwrap_cls();
                                let f = HashMap::new();
                                let time_data = cls_decl.get_own_time_data(&f, None);
                                for a in cls_decl {
                                    let t = time_data.clone();
                                    t.replace_value(a.get_value(), ReplaceMode::Tell);
                                    a.overwrite_time_data(&t);
                                    if a.is_time_interval() {
                                        a.update_value(None);
                                    }
                                    let x: Option<&IExprResult> = None;
                                    self.up_membership(&Arc::new(a), x)
                                }
                            } else {
                                let a = Arc::new(assertion.unwrap_fn().into_grounded());
                                let x: Option<&IExprResult> = None;
                                self.up_relation(&a, x)
                            }
                        }
                    }
                    ParseTree::IExpr(iexpr) => self.add_belief(&Arc::new(iexpr)),
                    ParseTree::Expr(rule) => self.add_rule(&Arc::new(rule)),
                    ParseTree::ParseErr(err) => errors.push(err),
                }
            }
            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(())
            }
        } else {
            Err(vec![pres.unwrap_err()])
        }
    }

    /// Asks the KB if some fact is true and returns the answer to the query.
    pub fn ask(&self, source: &str) -> Result<Answer, QueryErr> {
        let queries = logic_parser(source, false, self.threads);
        if let Ok(queries) = queries {
            let pres = QueryInput::ManyQueries(queries);
            self.ask_processed(pres, usize::max_value(), false)
        } else {
            Err(QueryErr::ParseErr(queries.unwrap_err()))
        }
    }

    pub(in crate::agent) fn ask_processed(
        &self,
        source: QueryInput,
        depth: usize,
        ignore_current: bool,
    ) -> Result<Answer, QueryErr> {
        let mut inf = match Inference::try_new(self, source, depth, ignore_current, self.threads) {
            Ok(inf) => inf,
            Err(()) => return Err(QueryErr::QueryErr),
        };
        {
            let inf_r = unsafe { &mut *(&mut inf as *mut Inference) };
            inf_r.infer_facts();
        }
        Ok(inf.get_results())
    }

    pub(in crate::agent) fn up_membership<T: ProofResContext>(
        &self,
        assert: &Arc<GroundedMemb>,
        context: Option<&T>,
    ) {
        let parent_exists = self
            .classes
            .read()
            .unwrap()
            .contains_key(assert.get_parent());
        if !parent_exists {
            let class = Class::new(assert.get_parent().to_string(), ClassKind::Membership);
            self.classes
                .write()
                .unwrap()
                .insert(class.name.clone(), class);
        }
        let decl;
        let is_new: bool;
        if (assert.get_name()).starts_with('$') {
            let entity_exists = self
                .entities
                .read()
                .unwrap()
                .contains_key(assert.get_name());
            if entity_exists {
                let lock = self.entities.read().unwrap();
                let entity = lock.get(assert.get_name()).unwrap();
                is_new = entity.add_class_membership(self, assert, context);
                decl = ClassMember::Entity(assert.clone());
            } else {
                let entity = Entity::new(assert.get_name().to_string());
                self.entities
                    .write()
                    .unwrap()
                    .insert(entity.name.clone(), entity);
                let lock = self.entities.read().unwrap();
                let entity = lock.get(assert.get_name()).unwrap();
                is_new = entity.add_class_membership(self, assert, context);
                decl = ClassMember::Entity(assert.clone());
            }
        } else {
            let class_exists = self.classes.read().unwrap().contains_key(assert.get_name());
            if class_exists {
                let lock = self.classes.read().unwrap();
                let class = lock.get(assert.get_name()).unwrap();
                is_new = class.add_class_membership(self, assert, context);
                decl = ClassMember::Class(assert.clone());
            } else {
                let class = Class::new(assert.get_name().to_string(), ClassKind::Membership);
                self.classes
                    .write()
                    .unwrap()
                    .insert(class.name.clone(), class);
                let lock = self.classes.read().unwrap();
                let class = lock.get(assert.get_name()).unwrap();
                is_new = class.add_class_membership(self, assert, context);
                decl = ClassMember::Class(assert.clone());
            }
        }
        if is_new {
            let lock = self.classes.read().unwrap();
            let parent = lock.get(assert.get_parent()).unwrap();
            parent.add_member(decl);
        }
    }

    pub(in crate::agent) fn up_relation<T: ProofResContext>(
        &self,
        assert: &Arc<GroundedFunc>,
        context: Option<&T>,
    ) {
        // it doesn't matter this is overwritten, as if it exists, it exists for all
        let is_new = Rc::new(::std::cell::RefCell::new(true));
        let process_arg = |a: &GroundedMemb| {
            let subject = a.get_name();
            let is_new1;
            if (subject).starts_with('$') {
                let entity_exists = self.entities.read().unwrap().contains_key(subject);
                if entity_exists {
                    let lock = self.entities.read().unwrap();
                    let entity = lock.get(subject).unwrap();
                    is_new1 = entity.add_relationship(self, assert, context);
                } else {
                    let entity = Entity::new(subject.to_string());
                    self.entities
                        .write()
                        .unwrap()
                        .insert(entity.name.clone(), entity);
                    let lock = self.entities.read().unwrap();
                    let entity = lock.get(subject).unwrap();
                    is_new1 = entity.add_relationship(self, assert, context);
                }
            } else {
                let class_exists = self.classes.read().unwrap().contains_key(subject);
                if class_exists {
                    let lock = self.classes.read().unwrap();
                    let class = lock.get(subject).unwrap();
                    is_new1 = class.add_relationship(self, assert, context);
                } else {
                    let class = Class::new(subject.to_string(), ClassKind::Membership);
                    self.classes
                        .write()
                        .unwrap()
                        .insert(class.name.clone(), class);
                    let lock = self.classes.read().unwrap();
                    let class = lock.get(subject).unwrap();
                    is_new1 = class.add_relationship(self, assert, context);
                }
            }
            let new_check = is_new.clone();
            *new_check.borrow_mut() = is_new1;
        };
        let relation_exists = self.classes.read().unwrap().contains_key(assert.get_name());
        if !relation_exists {
            let relationship = Class::new(assert.get_name().to_string(), ClassKind::Relationship);
            self.classes
                .write()
                .unwrap()
                .insert(relationship.name.clone(), relationship);
        }
        for arg in assert.get_args() {
            process_arg(arg);
        }
        if *is_new.borrow() {
            let lock = self.classes.read().unwrap();
            let parent = lock.get(assert.get_name()).unwrap();
            parent.add_relation_to_class(assert.clone());
        }
    }

    fn add_belief(&self, belief: &Arc<LogSentence>) {
        fn update(
            subject: &str,
            name: &str,
            is_entity: bool,
            belief: &Arc<LogSentence>,
            repr: &Representation,
        ) {
            if is_entity {
                let entity_exists = repr.entities.read().unwrap().contains_key(subject);
                if entity_exists {
                    let lock = repr.entities.read().unwrap();
                    let entity = lock.get(subject).unwrap();
                    entity.add_belief(belief.clone(), subject);
                } else {
                    let entity = Entity::new(subject.to_string());
                    entity.add_belief(belief.clone(), name);
                    repr.entities
                        .write()
                        .unwrap()
                        .insert(subject.to_string(), entity);
                }
            } else {
                let class_exists = repr.classes.read().unwrap().contains_key(subject);
                if class_exists {
                    let lock = repr.classes.read().unwrap();
                    let class = lock.get(subject).unwrap();
                    class.add_belief(belief.clone(), name);
                } else {
                    let class = Class::new(subject.to_string(), ClassKind::Membership);
                    class.add_belief(belief.clone(), name);
                    repr.classes
                        .write()
                        .unwrap()
                        .insert(subject.to_string(), class);
                }
            }
        };

        for p in belief.get_all_predicates() {
            match *p {
                Assert::ClassDecl(ref cls_decl) => {
                    let class_exists = self
                        .classes
                        .read()
                        .unwrap()
                        .contains_key(cls_decl.get_name());
                    if class_exists {
                        self.classes
                            .read()
                            .unwrap()
                            .get(cls_decl.get_name())
                            .unwrap()
                            .add_belief(belief.clone(), cls_decl.get_name())
                    } else {
                        let class =
                            Class::new(cls_decl.get_name().to_string(), ClassKind::Membership);
                        class.add_belief(belief.clone(), cls_decl.get_name());
                        self.classes
                            .write()
                            .unwrap()
                            .insert(class.name.clone(), class);
                    }
                    for arg in cls_decl.get_args() {
                        if !arg.is_var() {
                            let subject = arg.get_name();
                            if subject.starts_with('$') {
                                update(subject, cls_decl.get_name(), true, &belief, self)
                            } else {
                                update(subject, cls_decl.get_name(), false, &belief, self)
                            }
                        }
                    }
                }
                Assert::FuncDecl(ref fn_decl) => {
                    if !fn_decl.is_relational() {
                        continue;
                    }
                    let class_exists = self
                        .classes
                        .read()
                        .unwrap()
                        .contains_key(fn_decl.get_name());
                    if class_exists {
                        self.classes
                            .read()
                            .unwrap()
                            .get(fn_decl.get_name())
                            .unwrap()
                            .add_belief(belief.clone(), fn_decl.get_name())
                    } else {
                        let class =
                            Class::new(fn_decl.get_name().to_string(), ClassKind::Relationship);
                        class.add_belief(belief.clone(), fn_decl.get_name());
                        self.classes
                            .write()
                            .unwrap()
                            .insert(class.name.clone(), class);
                    }
                    for arg in fn_decl.get_args() {
                        if !arg.is_var() {
                            let subject = arg.get_name();
                            if subject.starts_with('$') {
                                update(subject, fn_decl.get_name(), true, &belief, self)
                            } else {
                                update(subject, fn_decl.get_name(), false, &belief, self)
                            }
                        }
                    }
                }
            }
        }

        let iter_cls_candidates =
            |cls_decl: &ClassDecl, candidates: &HashMap<&Var, Vec<Arc<VarAssignment>>>| {
                for a in cls_decl.get_args() {
                    match *a {
                        Predicate::FreeClsMemb(ref free) => {
                            if let Some(ls) = candidates.get(free.get_var_ref()) {
                                for entity in ls {
                                    let grfact =
                                        Arc::new(GroundedMemb::from_free(free, entity.name));
                                    self.ask_processed(QueryInput::AskClassMember(grfact), 0, true)
                                        .unwrap();
                                }
                            }
                        }
                        _ => continue,
                    }
                }
            };
        let iter_func_candidates =
            |func_decl: &FuncDecl, candidates: &HashMap<&Var, Vec<Arc<VarAssignment>>>| {
                let mapped = ArgsProduct::product(candidates.clone());
                if let Some(mapped) = mapped {
                    let f = HashMap::new();
                    for args in mapped {
                        let args = HashMap::from_iter(args.iter().map(|&(v, ref a)| (v, &**a)));
                        if let Ok(grfunc) = GroundedFunc::from_free(func_decl, Some(&args), &f) {
                            self.ask_processed(
                                QueryInput::AskRelationalFunc(Arc::new(grfunc)),
                                0,
                                true,
                            )
                            .unwrap();
                        }
                    }
                }
            };

        for var_req in belief.get_lhs_predicates().into_sent_req() {
            if let Some(candidates) = meet_sent_req(self, &var_req) {
                for var in candidates.keys() {
                    let it = belief.get_rhs_predicates();
                    for pred in it.iter().filter(|x| x.contains(&**var)) {
                        match **pred {
                            Assert::ClassDecl(ref cls_decl) => {
                                iter_cls_candidates(cls_decl, &candidates)
                            }
                            Assert::FuncDecl(ref func_decl) => {
                                iter_func_candidates(func_decl, &candidates)
                            }
                        }
                    }
                }
            }
        }
    }

    fn add_rule(&self, rule: &Arc<LogSentence>) {
        let preds = rule.get_all_predicates();
        for p in preds {
            let name = p.get_name();
            let class_exists = self.classes.read().unwrap().contains_key(name);
            if class_exists {
                let lock = self.classes.read().unwrap();
                let class = lock.get(name).unwrap();
                class.add_rule(rule.clone());
            } else {
                let nc = match *p {
                    Assert::ClassDecl(_) => Class::new(name.to_string(), ClassKind::Membership),
                    Assert::FuncDecl(_) => Class::new(name.to_string(), ClassKind::Relationship),
                };
                nc.add_rule(rule.clone());
                self.classes.write().unwrap().insert(name.to_string(), nc);
            }
        }
        rollback_from_rule(self, &rule);
    }

    /// Takes a vector of class names and returns a hash map with those classes as keys
    /// and the memberships to those classes.
    pub(in crate::agent) fn by_class<'a, 'b>(
        &'a self,
        classes: &'b [&str],
    ) -> HashMap<&'b str, Vec<Arc<GroundedMemb>>> {
        let mut dict = HashMap::new();
        let lock = self.classes.read().unwrap();
        for cls in classes {
            let cls_ref = lock.get(*cls);
            if cls_ref.is_none() {
                continue;
            }
            let mut v = vec![];
            for e in &**cls_ref.unwrap().members.read().unwrap() {
                match *e {
                    ClassMember::Class(ref m) | ClassMember::Entity(ref m) => {
                        if m.get_value().is_some() {
                            v.push(m.clone());
                        }
                    }
                    _ => {}
                }
            }
            dict.insert(*cls, v);
        }
        dict
    }

    /// Takes a vector of relation declarations and returns a hash map with those relation
    /// names as keys and a hash map of the objects which have one relation of that kind
    /// as value (with a list of the grounded functions for each object).
    pub(in crate::agent) fn by_relationship<'a, 'b>(
        &'a self,
        funcs: &'b [&'b FuncDecl],
    ) -> HashMap<&'b str, HashMap<&'a str, Vec<Arc<GroundedFunc>>>> {
        let mut dict = HashMap::new();
        let lock = self.classes.read().unwrap();
        for func in funcs {
            let func_ref: Option<&Class> = lock.get(func.get_name());
            if func_ref.is_none() {
                continue;
            }
            let mut m = HashMap::new();
            for e in &**func_ref.unwrap().members.read().unwrap() {
                if let ClassMember::Func(ref f) = *e {
                    if f.get_value().is_some() {
                        for name in f.get_args_names() {
                            let name = unsafe { std::mem::transmute::<&str, &'a str>(name) };
                            let e: &mut Vec<_> = m.entry(name).or_insert_with(|| vec![]);
                            e.push(f.clone());
                        }
                    }
                }
            }
            dict.insert(func.get_name(), m);
        }
        dict
    }

    pub(in crate::agent) fn get_obj_from_class(
        &self,
        class: &str,
        subject: &str,
    ) -> Option<Arc<GroundedMemb>> {
        if subject.starts_with('$') {
            let entity_exists = self.entities.read().unwrap().contains_key(subject);
            if entity_exists {
                let lock = self.entities.read().unwrap();
                match lock.get(subject).unwrap().belongs_to_class(class, false) {
                    Some(r) => Some(r.clone()),
                    None => None,
                }
            } else {
                None
            }
        } else {
            let class_exists = self.classes.read().unwrap().contains_key(subject);
            if class_exists {
                let lock = self.classes.read().unwrap();
                match lock.get(subject).unwrap().belongs_to_class(class, false) {
                    Some(r) => Some(r.clone()),
                    None => None,
                }
            } else {
                None
            }
        }
    }

    /// Takes a grounded predicate from a query and returns the membership truth value.
    /// It checks the truth value based on the time intervals of the predicate.
    pub(in crate::agent) fn class_membership_query(&self, pred: &GroundedMemb) -> Option<bool> {
        let subject = pred.get_name();
        if subject.starts_with('$') {
            if let Some(entity) = self.entities.read().unwrap().get(subject) {
                if let Some(current) = entity.belongs_to_class(pred.get_parent(), true) {
                    return current.compare_at_time_intervals(pred);
                }
            }
        } else if let Some(class) = self.classes.read().unwrap().get(subject) {
            if let Some(current) = class.belongs_to_class(pred.get_parent(), true) {
                return current.compare_at_time_intervals(pred);
            }
        }
        None
    }

    pub(in crate::agent) fn get_class_membership(
        &self,
        subject: &FreeClassMembership,
    ) -> Vec<Arc<GroundedMemb>> {
        let name = subject.get_name();
        if name.starts_with('$') {
            if let Some(entity) = self.entities.read().unwrap().get(name) {
                entity.get_class_membership(subject)
            } else {
                vec![]
            }
        } else if let Some(class) = self.classes.read().unwrap().get(name) {
            class.get_class_membership(subject)
        } else {
            vec![]
        }
    }

    pub(in crate::agent) fn has_relationship(
        &self,
        pred: &GroundedFunc,
        subject: &str,
    ) -> Option<bool> {
        if subject.starts_with('$') {
            if let Some(entity) = self.entities.read().unwrap().get(subject) {
                if let Some(current) = entity.has_relationship(pred) {
                    return current.compare_at_time_intervals(pred);
                }
            }
        } else if let Some(class) = self.classes.read().unwrap().get(subject) {
            if let Some(current) = class.has_relationship(pred) {
                return current.compare_at_time_intervals(pred);
            }
        }
        None
    }

    pub(in crate::agent) fn get_relationship(
        &self,
        pred: &GroundedFunc,
        subject: &str,
    ) -> Option<Arc<GroundedFunc>> {
        if subject.starts_with('$') {
            if let Some(entity) = self.entities.read().unwrap().get(subject) {
                if let Some(current) = entity.has_relationship(pred) {
                    if *current == *pred {
                        return Some(current);
                    } else {
                        return None;
                    }
                }
            }
        } else if let Some(class) = self.classes.read().unwrap().get(subject) {
            if let Some(current) = class.has_relationship(pred) {
                if *current == *pred {
                    return Some(current);
                } else {
                    return None;
                }
            }
        }
        None
    }

    pub(in crate::agent) fn get_relationships(
        &self,
        func: &FuncDecl,
    ) -> HashMap<&str, Vec<Arc<GroundedFunc>>> {
        let mut res = HashMap::new();
        for (pos, arg) in func.get_args().iter().enumerate() {
            if !arg.is_var() {
                let name = arg.get_name();
                if name.starts_with('$') {
                    if let Some(entity) = self.entities.read().unwrap().get(name) {
                        let mut v: HashMap<&str, Vec<Arc<GroundedFunc>>> =
                            entity.get_relationships(pos, arg);
                        for (_, mut funcs) in v.drain() {
                            // guaranteed this lives as long as self
                            let t = unsafe { &*(&*funcs[0] as *const GroundedFunc) };
                            let rel = t.get_name();
                            res.entry(rel).or_insert_with(|| vec![]).append(&mut funcs);
                        }
                    }
                } else if let Some(class) = self.classes.read().unwrap().get(name) {
                    let mut v: HashMap<&str, Vec<Arc<GroundedFunc>>> =
                        class.get_relationships(pos, arg);
                    for (_, mut funcs) in v.drain() {
                        // guaranteed this lives as long as self
                        let t = unsafe { &*(&*funcs[0] as *const GroundedFunc) };
                        let rel = t.get_name();
                        res.entry(rel).or_insert_with(|| vec![]).append(&mut funcs);
                    }
                }
            }
        }
        res
    }

    pub fn with_threads(mut self, threads: usize) -> Self {
        self.threads = threads;
        self
    }

    pub fn set_threads(&mut self, threads: usize) {
        self.threads = threads;
    }
}

/// Error type for query failures.
///
/// A query can fail because it's either incomprehensible, in which case
/// it will return a `QueryErr` variant, or because parsing of the ask request failed
/// in which case it will return a `ParseErr` variant and it's payload.
#[derive(Debug)]
pub enum QueryErr {
    QueryErr,
    ParseErr(ParseErrF),
}

/// Answer to a query and inference results payload.
#[derive(Debug)]
pub struct Answer<'a>(InfResults<'a>);

type ObjName<'a> = &'a str;
type QueryPred = String;

pub struct Membership<'a> {
    name: &'a str,
    value: Option<f32>,
}

impl<'a> Membership<'a> {
    pub fn get_parent(&self) -> &str {
        self.name
    }

    pub fn get_value(&self) -> Option<f32> {
        self.value
    }
}

impl<'a> Answer<'a> {
    pub(super) fn new(results: InfResults<'a>) -> Answer<'a> {
        Answer(results)
    }

    pub fn get_results_single(&self) -> Option<bool> {
        self.0.get_results_single()
    }

    pub fn get_results_multiple(self) -> HashMap<QueryPred, HashMap<String, GroundedResult>> {
        self.0.get_results_multiple()
    }

    pub fn get_memberships(&self) -> HashMap<ObjName<'a>, Vec<Membership<'a>>> {
        self.0
            .get_memberships()
            .iter()
            .map(|(name, memberships)| {
                (
                    *name,
                    memberships
                        .iter()
                        .map(|gr| Membership {
                            value: gr.get_value(),
                            name: gr.get_parent(),
                        })
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<HashMap<_, _>>()
    }

    #[allow(dead_code)]
    pub(super) fn get_relationships(&self) -> HashMap<ObjName<'a>, Vec<&'a GroundedFunc>> {
        self.0.get_relationships()
    }
}

/// An entity is a singleton, the unique member of it's own class.
///
/// Represents an object which can pertain to multiple classes or sets.
/// It's an abstraction owned by an agent, the internal representation
/// of the object, not the object itself.
///
/// An entity inherits the properties of the classes it belongs to,
/// and has some implicit attributes which are unique to itself.
///
/// Membership to a class is denoted (following fuzzy sets) by a real number
/// between 0 and 1. If the number is one, the object will always belong to
/// the set, if it's zero, it will never belong to the set.
///
/// For example, an object can belong to the set "cold" with a degree of
/// 0.9 (in natural language then it would be 'very cold') or 0.1
/// (then it would be "a bit cold", the subjective adjectives are defined
/// in the class itself).
///
/// Attributes:
///     * name: unique name to identify the entity.
///     * classes: categories to which the object belongs. Includes the degree of membership
///       (ie. ("cold", 0.9)).
///     * attr: implicit attributes of the object, unique to itself.
///     * beliefs: hese are the cognitions attributed to the object by the agent owning this
///       representation.
///     * relations: Functions between objects and/or classes.
///
#[derive(Debug)]
pub(crate) struct Entity {
    pub name: String,
    classes: RwLock<HashMap<String, Arc<GroundedMemb>>>,
    relations: RwLock<HashMap<String, Vec<Arc<GroundedFunc>>>>,
    beliefs: RwLock<HashMap<String, Vec<Arc<LogSentence>>>>,
}

impl Entity {
    fn new(name: String) -> Entity {
        Entity {
            name,
            classes: RwLock::new(HashMap::new()),
            relations: RwLock::new(HashMap::new()),
            beliefs: RwLock::new(HashMap::new()),
        }
    }

    fn belongs_to_class(&self, class_name: &str, interval: bool) -> Option<Arc<GroundedMemb>> {
        let lock = self.classes.read().unwrap();
        match lock.get(class_name) {
            Some(r) if r.get_value().is_some() || interval => Some(r.clone()),
            Some(_) | None => None,
        }
    }

    fn get_class_membership(&self, compare: &FreeClassMembership) -> Vec<Arc<GroundedMemb>> {
        let lock = self.classes.read().unwrap();
        lock.values()
            .filter(|x| compare.filter_grounded(&**x))
            .cloned()
            .collect::<Vec<_>>()
    }

    fn add_class_membership<T: ProofResContext>(
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

    fn has_relationship(&self, func: &GroundedFunc) -> Option<Arc<GroundedFunc>> {
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

    fn get_relationships(
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

    /// Adds a new relationship for the entity.
    /// Returns 'true' in case the relationship didn't exist previously,
    /// 'false' otherwise. If it already existed, it's value is updated.
    fn add_relationship<T: ProofResContext>(
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

    fn add_belief(&self, belief: Arc<LogSentence>, parent: &str) {
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
}

#[inline]
pub(super) fn lookahead_rules(agent: &Representation, name: &str, grounded: &GroundedRef) -> bool {
    use super::inference::rules_inference_lookahead;
    let rules: Vec<Arc<LogSentence>> = {
        let classes = agent.classes.read().unwrap();
        let class = classes.get(name).unwrap();
        let rules = &*class.rules.read().unwrap();
        rules.clone()
    };
    rules_inference_lookahead(agent, rules, grounded)
}

#[inline]
fn rollback_from_rule(agent: &Representation, rule: &Arc<LogSentence>) {
    use super::inference::rules_inference_rollback;
    rules_inference_rollback(agent, rule);
}