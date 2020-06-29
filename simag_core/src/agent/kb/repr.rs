use std::collections::HashMap;
use std::iter::FromIterator;
use std::rc::Rc;
use std::sync::Arc;

use dashmap::DashMap;

use super::entity::Entity;
use crate::agent::kb::{
    bms::ReplaceMode,
    class::*,
    inference::{
        meet_sent_requirements, ArgsProduct, GroundedResult, IExprResult, InfResults, Inference,
        QueryInput,
    },
    VarAssignment,
};
use crate::agent::lang::{
    Assert, ClassDecl, FreeClassMembership, FuncDecl, GrTerminalKind,
    GrTerminalKind::{Class as ClassTerm, Entity as EntityTerm},
    GroundedFunc, GroundedMemb, GroundedRef, LogSentence, ParseErrF, ParseTree, Parser, Predicate,
    ProofResContext, SentVarReq, TimeOps, Var,
};

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
pub(crate) struct Representation {
    pub(in crate::agent) entities: DashMap<String, Entity>,
    pub(in crate::agent) classes: DashMap<String, Class>,
    threads: rayon::ThreadPool,
}

impl Default for Representation {
    fn default() -> Self {
        Representation::new(num_cpus::get())
    }
}

impl Representation {
    pub fn new(threads: usize) -> Representation {
        #[cfg(debug_assertions)]
        {
            crate::agent::conf::tracing::Logger::get_logger();
        }

        Representation {
            entities: DashMap::new(),
            classes: DashMap::new(),
            threads: rayon::ThreadPoolBuilder::new()
                .num_threads(threads)
                .build()
                .unwrap(),
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
    pub fn tell<T: AsRef<str>>(&self, source: T) -> Result<(), Vec<ParseErrF>> {
        let pres = Parser::parse(source.as_ref(), true, &self.threads);
        if let Ok(mut sentences) = pres {
            let mut errors = Vec::new();
            for _ in 0..sentences.len() {
                match sentences.pop_front().unwrap() {
                    ParseTree::Assertion(assertions) => {
                        for assertion in assertions {
                            match assertion {
                                Assert::ClassDecl(cls_decl) => {
                                    let f = HashMap::new();
                                    let time_data = cls_decl.get_own_time_data(&f, None);
                                    for a in cls_decl {
                                        let t = time_data.clone();
                                        t.replace_value(a.get_value(), ReplaceMode::Tell);
                                        if let Some(bms) = a.bms.as_ref() {
                                            bms.overwrite_data(&t);
                                            if a.is_time_interval() {
                                                a.update_value(None);
                                            }
                                        };
                                        let x: Option<&IExprResult> = None;
                                        self.up_membership(&Arc::new(a), x)
                                    }
                                }
                                Assert::FuncDecl(func_decl) => {
                                    let a = Arc::new(func_decl.into_grounded());
                                    let x: Option<&IExprResult> = None;
                                    self.up_relation(&a, x)
                                }
                                _ => return Err(vec![ParseErrF::WrongDef]),
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
        let queries = Parser::parse(source, false, &self.threads);
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
        let inf = match Inference::try_new(self, source, depth, ignore_current, &self.threads) {
            Ok(inf) => inf,
            Err(()) => return Err(QueryErr::QueryErr),
        };
        inf.infer_facts();
        Ok(inf.get_results())
    }

    pub(in crate::agent) fn up_membership<T: ProofResContext>(
        &self,
        assert: &Arc<GroundedMemb>,
        context: Option<&T>,
    ) {
        let parent_exists = self.classes.contains_key(assert.get_parent());
        if !parent_exists {
            let class = Class::new(assert.get_parent().to_string(), ClassKind::Membership);
            self.classes.insert(class.name.clone(), class);
        }
        let decl;
        let is_new: bool;
        match assert.get_name() {
            ClassTerm(class_name) => {
                let class_exists = self.classes.contains_key(class_name);
                if class_exists {
                    let class = self.classes.get(class_name).unwrap();
                    is_new = class.add_or_update_class_membership(self, assert, context);
                    decl = ClassMember::Class(assert.clone());
                } else {
                    let class = Class::new(class_name.to_owned(), ClassKind::Membership);
                    self.classes.insert(class.name.clone(), class);
                    let class = self.classes.get(class_name).unwrap();
                    is_new = class.add_or_update_class_membership(self, assert, context);
                    decl = ClassMember::Class(assert.clone());
                }
            }
            EntityTerm(subject) => {
                let entity_exists = self.entities.contains_key(subject);
                if entity_exists {
                    let entity = self.entities.get(subject).unwrap();
                    is_new = entity.add_or_updated_class_membership(self, assert, context);
                    decl = ClassMember::Entity(assert.clone());
                } else {
                    let entity = Entity::new(subject.to_string());
                    self.entities.insert(entity.name.clone(), entity);
                    let entity = self.entities.get(subject).unwrap();
                    is_new = entity.add_or_updated_class_membership(self, assert, context);
                    decl = ClassMember::Entity(assert.clone());
                }
            }
        }
        if is_new {
            let parent = self.classes.get(assert.get_parent()).unwrap();
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
            let is_new1;
            match a.get_name() {
                ClassTerm(class_name) => {
                    if let Some(class) = self.classes.get(class_name) {
                        is_new1 = class.add_relationship(self, assert, context);
                    } else {
                        let class = Class::new(class_name.to_owned(), ClassKind::Membership);
                        self.classes.insert(class.name.clone(), class);
                        let class = self.classes.get(class_name).unwrap();
                        is_new1 = class.add_relationship(self, assert, context);
                    }
                }
                EntityTerm(subject) => {
                    if let Some(entity) = self.entities.get(subject) {
                        is_new1 = entity.add_relationship(self, assert, context);
                    } else {
                        let entity = Entity::new(subject.to_owned());
                        self.entities.insert(entity.name.clone(), entity);
                        let entity = self.entities.get(subject).unwrap();
                        is_new1 = entity.add_relationship(self, assert, context);
                    }
                }
            }
            let new_check = is_new.clone();
            *new_check.borrow_mut() = is_new1;
        };
        let relation_exists = self.classes.contains_key(assert.get_name());
        if !relation_exists {
            let relationship = Class::new(assert.get_name().to_string(), ClassKind::Relationship);
            self.classes.insert(relationship.name.clone(), relationship);
        }
        for arg in assert.get_args() {
            process_arg(arg);
        }
        if *is_new.borrow() {
            let parent = self.classes.get(assert.get_name()).unwrap();
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
                if let Some(entity) = repr.entities.get(subject) {
                    entity.add_belief(belief.clone(), subject);
                } else {
                    let entity = Entity::new(subject.to_string());
                    entity.add_belief(belief.clone(), name);
                    repr.entities.insert(subject.to_string(), entity);
                }
            } else if let Some(class) = repr.classes.get(subject) {
                class.add_belief(belief.clone(), name);
            } else {
                let class = Class::new(subject.to_string(), ClassKind::Membership);
                class.add_belief(belief.clone(), name);
                repr.classes.insert(subject.to_string(), class);
            }
        };

        for p in belief.get_all_predicates() {
            match *p {
                Assert::ClassDecl(ref cls_decl) => {
                    if let Some(class) = self.classes.get(cls_decl.get_name()) {
                        class.add_belief(belief.clone(), cls_decl.get_name());
                    } else {
                        let class =
                            Class::new(cls_decl.get_name().to_string(), ClassKind::Membership);
                        class.add_belief(belief.clone(), cls_decl.get_name());
                        self.classes.insert(class.name.clone(), class);
                    }
                    for arg in cls_decl.get_args() {
                        if !arg.is_var() {
                            match arg.get_name().into() {
                                ClassTerm(class_name) => {
                                    update(class_name, cls_decl.get_name(), false, &belief, self)
                                }
                                EntityTerm(subject_name) => {
                                    update(subject_name, cls_decl.get_name(), true, &belief, self)
                                }
                            }
                        }
                    }
                }
                Assert::FuncDecl(ref fn_decl) => {
                    if !fn_decl.is_relational() {
                        continue;
                    }
                    if let Some(class) = self.classes.get(fn_decl.get_name()) {
                        class.add_belief(belief.clone(), fn_decl.get_name());
                    } else {
                        let class =
                            Class::new(fn_decl.get_name().to_string(), ClassKind::Relationship);
                        class.add_belief(belief.clone(), fn_decl.get_name());
                        self.classes.insert(class.name.clone(), class);
                    }
                    for arg in fn_decl.get_args() {
                        if !arg.is_var() {
                            match arg.get_name().into() {
                                ClassTerm(class_name) => {
                                    update(class_name, fn_decl.get_name(), false, &belief, self)
                                }
                                EntityTerm(subject_name) => {
                                    update(subject_name, fn_decl.get_name(), true, &belief, self)
                                }
                            }
                        }
                    }
                }
                Assert::SpecialFunc(_) => {}
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

        let sent_req: SentVarReq = belief.get_lhs_predicates().into();
        for var_req in sent_req {
            if let Some(candidates) = meet_sent_requirements(self, &var_req) {
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
                            Assert::SpecialFunc(_) => {}
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
            if let Some(class) = self.classes.get(name) {
                class.add_rule(rule.clone());
            } else {
                let nc = match *p {
                    Assert::ClassDecl(_) => Class::new(name.to_string(), ClassKind::Membership),
                    Assert::FuncDecl(_) => Class::new(name.to_string(), ClassKind::Relationship),
                    Assert::SpecialFunc(_) => unreachable!(),
                };
                nc.add_rule(rule.clone());
                self.classes.insert(name.to_string(), nc);
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
        for cls in classes {
            if let Some(klass) = self.classes.get(*cls) {
                let mut v = vec![];
                for e in &**klass.members.read() {
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
        for func in funcs {
            if let Some(func_ref) = self.classes.get(&*func.get_name()) {
                let mut m = HashMap::new();
                for e in &**func_ref.value().members.read() {
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
        }
        dict
    }

    pub(in crate::agent) fn get_obj_from_class<G, S>(
        &self,
        class: &str,
        subject: G,
    ) -> Option<Arc<GroundedMemb>>
    where
        G: std::ops::Deref<Target = GrTerminalKind<S>>,
        S: AsRef<str>,
    {
        match *subject {
            EntityTerm(ref subject_name) => {
                if let Some(entity) = self.entities.get(subject_name.as_ref()) {
                    match entity.belongs_to_class(class, false) {
                        Some(r) => Some(r),
                        None => None,
                    }
                } else {
                    None
                }
            }
            ClassTerm(ref class_name) => {
                if let Some(klass) = self.classes.get(class_name.as_ref()) {
                    match klass.belongs_to_class(class, false) {
                        Some(r) => Some(r),
                        None => None,
                    }
                } else {
                    None
                }
            }
        }
    }

    /// Takes a grounded predicate from a query and returns the membership truth value.
    /// It checks the truth value based on the time intervals of the predicate.
    pub(in crate::agent) fn class_membership_query(&self, pred: &GroundedMemb) -> Option<bool> {
        match pred.get_name() {
            EntityTerm(subject) => {
                if let Some(entity) = self.entities.get(subject) {
                    if let Some(current) = entity.belongs_to_class(pred.get_parent(), true) {
                        return current.compare(pred);
                    }
                }
            }
            ClassTerm(class_name) => {
                if let Some(class) = self.classes.get(class_name) {
                    if let Some(current) = class.belongs_to_class(pred.get_parent(), true) {
                        return current.compare(pred);
                    }
                }
            }
        }
        None
    }

    pub(in crate::agent) fn get_class_membership(
        &self,
        subject: &FreeClassMembership,
    ) -> Vec<Arc<GroundedMemb>> {
        if let EntityTerm(name) = subject.get_name().into() {
            if let Some(entity) = self.entities.get(name) {
                entity.get_class_membership(subject)
            } else {
                vec![]
            }
        } else if let Some(class) = self.classes.get(subject.get_name()) {
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
        if let EntityTerm(subject_name) = subject.into() {
            if let Some(entity) = self.entities.get(subject_name) {
                if let Some(current) = entity.has_relationship(pred) {
                    return current.compare(pred);
                }
            }
        } else if let Some(class) = self.classes.get(subject) {
            if let Some(current) = class.has_relationship(pred) {
                return current.compare(pred);
            }
        }
        None
    }

    pub(in crate::agent) fn get_relationship(
        &self,
        pred: &GroundedFunc,
        subject: &str,
    ) -> Option<Arc<GroundedFunc>> {
        if let EntityTerm(subject_name) = subject.into() {
            if let Some(entity) = self.entities.get(subject_name) {
                if let Some(current) = entity.has_relationship(pred) {
                    if current.compare_ignoring_times(pred) {
                        return Some(current);
                    } else {
                        return None;
                    }
                }
            }
        } else if let Some(class) = self.classes.get(subject) {
            if let Some(current) = class.has_relationship(pred) {
                if current.compare_ignoring_times(pred) {
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
        // FIXME: return iteraror avoid collection with drain
        let mut res = HashMap::new();
        for (pos, arg) in func.get_args().iter().enumerate() {
            if !arg.is_var() {
                if let EntityTerm(subject_name) = arg.get_name().into() {
                    if let Some(entity) = self.entities.get(subject_name) {
                        let mut v: HashMap<&str, Vec<Arc<GroundedFunc>>> =
                            entity.get_relationships(pos, arg);
                        for (_, mut funcs) in v.drain() {
                            // Safety: guaranteed this lives as long as self and mutable borrows don't leak
                            // return type is: (&'a str, Arc<GrFunc>) where &'str lives as long as Arc<GrFunc>
                            // &'a str must NOT outlive (nor leak from) this Repr, this would be UB,
                            let rel = unsafe { &*(funcs[0].get_name() as *const str) };
                            res.entry(rel).or_insert_with(|| vec![]).append(&mut funcs);
                        }
                    }
                } else if let Some(class) = self.classes.get(arg.get_name()) {
                    let mut v: HashMap<&str, Vec<Arc<GroundedFunc>>> =
                        class.get_relationships(pos, arg);
                    for (_, mut funcs) in v.drain() {
                        // Safety: guaranteed this lives as long as self and mutable borrows don't leak
                        // return type is: (&'a str, Arc<GrFunc>) where &'str lives as long as Arc<GrFunc>
                        // &'a str must NOT outlive (nor leak from) this Repr, this would be UB,
                        let rel = unsafe { &*(funcs[0].get_name() as *const str) };
                        res.entry(rel).or_insert_with(|| vec![]).append(&mut funcs);
                    }
                }
            }
        }
        res
    }

    pub fn set_threads(&mut self, threads: usize) {
        self.threads = rayon::ThreadPoolBuilder::new()
            .num_threads(threads)
            .build()
            .unwrap();
    }

    pub fn clear(&mut self) {
        self.entities.clear();
        self.classes.clear();
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

#[inline]
pub(super) fn lookahead_rules(agent: &Representation, name: &str, grounded: &GroundedRef) -> bool {
    use super::inference::rules_inference_lookahead;
    let rules: Vec<Arc<LogSentence>> = {
        let class = agent.classes.get(name).unwrap();
        let rules = &*class.rules.read();
        rules.clone()
    };
    rules_inference_lookahead(agent, rules, grounded)
}

#[inline]
fn rollback_from_rule(agent: &Representation, rule: &Arc<LogSentence>) {
    use super::inference::rules_inference_rollback;
    rules_inference_rollback(agent, rule);
}
