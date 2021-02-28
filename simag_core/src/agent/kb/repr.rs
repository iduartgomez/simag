use std::{
    collections::{HashMap, HashSet},
    iter::FromIterator,
    ops::Deref,
    rc::Rc,
    sync::{atomic::AtomicBool, Arc},
    time::Duration,
    time::Instant,
};

use crossbeam::channel::{Receiver, Sender};
use dashmap::DashMap;
use parking_lot::{Condvar, Mutex};

use crate::agent::kb::{
    bms::{build_declaration_bms, BmsWrapper, RecordHistory},
    class::*,
    entity::Entity,
    inference::{
        meet_sent_requirements, ArgsProduct, GroundedResult, IExprResult, InfResults, Inference,
        QueryInput,
    },
    VarAssignment,
};
use crate::agent::lang::{
    Assert, BuiltIns, ClassDecl, FreeClassMembership, FuncDecl, GrTerminalKind,
    GrTerminalKind::{Class as ClassTerm, Entity as EntityTerm},
    GroundedFunc, GroundedMemb, GroundedRef, LocFn, LogSentence, MoveFn, ParseErrF, ParseTree,
    Parser, Point, Predicate, ProofResContext, SentVarReq, Var,
};

#[cfg(feature = "persistence")]
use super::{entity::EntityMetadata, storage::ReprStorageManager};
#[cfg(feature = "persistence")]
use std::sync::atomic::Ordering;

// TODO: find better solution for self-referential borrows escaping self method scopes
// this should remove the use of unsafe in this module which seems a little bit unnecessary?
// Avoid copying/cloning if possible.

/// The idea behind this is to split events in two types run in the main and background thread:
/// - shared events (main thread), only require a read lock into the inner data and
///   can be shared between threads.
/// - unique events (service thread), require a write lock since they may mutate the inner data.
///
/// There are two kind of unique events, those triggered by schedeluded maintenance tasks and those
/// that are a byproduct of an evaluation (shared event types).
/// When the repr is free to perform writes because there are no reads, it will place a local repr
/// lock and process any scheduled unique events.
///
/// Since the locking is done only once when available this is a cheap operation and allows
/// updating the repr state in an orderly fashion, following always the sequence: read -> write -> read.
/// Likewise maintenace tasks are only performed when there are no more shared events in queue.
pub(in crate::agent) struct InnerData<T>(Arc<DashMap<String, T>>);

impl<T> Clone for InnerData<T> {
    fn clone(&self) -> Self {
        InnerData(self.0.clone())
    }
}

impl<T> Deref for InnerData<T> {
    type Target = DashMap<String, T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A container for internal agent's representations.
///
/// An agent can have any number of such representations at any moment,
/// all of which are contained in this object.
///
/// The class includes methods to encode and decode the representations
/// to/from data streams or idioms.
///
/// # Attributes
/// - entities -> Unique members (entities) of their own set/class.
///   Entities are denoted with a $ symbol followed by an alphanumeric literal.
/// - classes -> Sets of objects (entities or subclasses) that share a common property.
///   This includes 'classes of relationships' and other 'functions'.
pub(crate) struct Representation {
    pub(super) id: uuid::Uuid,
    config: ReprConfig,
    pub(in crate::agent) entities: InnerData<Entity>,
    pub(in crate::agent) classes: InnerData<Class>,
    svc_queue: Sender<BackgroundTask>,
    threads: rayon::ThreadPool,
    readers: Arc<(Mutex<usize>, Condvar)>,
}

impl Representation {
    /// Time allocated for the service thread to execute a batch of tasks before starving
    /// readers.
    const BG_TASK_TIME_SLICE: Duration = Duration::from_nanos(100);
    /// Frequency with which the background secondary tasks are to be performed.
    const BG_SEC_TASK_FREQ: Duration = Duration::from_secs(1);

    #[cfg(feature = "persistence")]
    pub fn new(threads: usize) -> std::io::Result<Representation> {
        let (rep, bg_task_rcv) = Self::internal_new(threads);
        let shared_data = ReprSharedData {
            entities: rep.entities.clone(),
            classes: rep.classes.clone(),
            config: rep.config.clone(),
            readers: rep.readers.clone(),
            bg_task_rcv,
            storage_layer: ReprStorageManager::new(rep.id.clone(), None, None)?,
        };
        rep.threads.spawn(move || bg_thread(shared_data));
        Ok(rep)
    }

    #[cfg(not(feature = "persistence"))]
    pub fn new(threads: usize) -> Representation {
        let (rep, bg_task_rcv) = Self::internal_new(threads);
        #[allow(unused_mut, unused_variables)]
        let mut shared_data = ReprSharedData {
            entities: rep.entities.clone(),
            classes: rep.classes.clone(),
            config: rep.config.clone(),
            readers: rep.readers.clone(),
            bg_task_rcv,
        };
        rep.threads.spawn(move || bg_thread(shared_data));
        rep
    }

    fn internal_new(threads: usize) -> (Self, Receiver<BackgroundTask>) {
        #[cfg(any(test, debug_assertions))]
        {
            crate::agent::config::tracing::Logger::get_logger();
        }

        let (svc_th_msg_queue, bg_task_rcv) = crossbeam::channel::unbounded();
        let entities = InnerData(Arc::new(DashMap::new()));
        let classes = InnerData(Arc::new(DashMap::new()));
        let config = ReprConfig::default();
        let readers = Arc::new((Mutex::new(0), Condvar::new()));

        // reserve at least one thread for the background thread
        (
            Representation {
                id: uuid::Uuid::new_v4(),
                config,
                entities,
                classes,
                threads: rayon::ThreadPoolBuilder::new()
                    .num_threads(threads + 1)
                    .build()
                    .unwrap(),
                svc_queue: svc_th_msg_queue,
                readers: readers,
            },
            bg_task_rcv,
        )
    }

    #[cfg(feature = "persistence")]
    pub fn enable_persistence(&mut self) {
        self.config.persist.swap(true, Ordering::SeqCst);
    }

    #[cfg(feature = "persistence")]
    pub fn disable_persistence(&mut self) {
        self.config.persist.swap(false, Ordering::SeqCst);
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
    ///
    /// Will include the individual '$Lucy' in the professor category.
    ///
    /// `>>> r.tell("((let x) professor[x,u=1] := person[x,u=1])")`
    ///
    /// All the individuals which are professors will be added to the
    /// person category, and the formula will be stored in the professor
    /// class for future use.
    ///
    /// For more examples check the LogSentence type docs.
    pub fn tell<T: AsRef<str>>(&self, source: T) -> Result<(), Vec<ParseErrF>> {
        let pres = Parser::parse(source.as_ref(), true, &self.threads);
        if let Ok(mut sentences) = pres {
            let mut errors = Vec::new();
            let num_sentences = sentences.len();
            self.add_readers(num_sentences);
            for _ in 0..num_sentences {
                match sentences.pop_front().unwrap() {
                    ParseTree::Assertion(assertions) => {
                        for assertion in assertions {
                            match assertion {
                                Assert::ClassDecl(cls_decl) => build_declaration_bms(cls_decl)
                                    .map_err(|err| vec![err])?
                                    .for_each(|decl| {
                                        self.up_membership(&Arc::new(decl), None::<&IExprResult>)
                                    }),
                                Assert::FuncDecl(func_decl) => {
                                    let a = Arc::new(func_decl.into());
                                    self.up_relation(&a, None::<&IExprResult>)
                                }
                                Assert::SpecialFunc(BuiltIns::Location(loc_fn)) => self
                                    .upsert_objects_with_loc(loc_fn.objects_to_update().flat_map(
                                        |(obj, loc)| match Arc::try_unwrap(obj) {
                                            Ok(t) => Some((t, loc)),
                                            _ => None,
                                        },
                                    )),
                                _ => return Err(vec![ParseErrF::WrongDef]),
                            }
                        }
                    }
                    ParseTree::IExpr(iexpr) => self.add_belief(&Arc::new(iexpr)),
                    ParseTree::Expr(rule) => self.add_rule(&Arc::new(rule)),
                    ParseTree::ParseErr(err) => errors.push(err),
                }
            }
            self.rm_readers(num_sentences);
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
            let num_readers = queries.len();
            self.add_readers(num_readers);
            let pres = QueryInput::ManyQueries(queries);
            let answ = self.ask_internal(pres, usize::max_value(), false);
            self.rm_readers(num_readers);
            answ
        } else {
            Err(QueryErr::ParseErr(queries.unwrap_err()))
        }
    }

    pub(in crate::agent) fn ask_internal(
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

    fn add_readers(&self, readers: usize) {
        let mut num_readers = self.readers.0.lock();
        *num_readers += readers;
    }

    fn rm_readers(&self, readers: usize) {
        let mut lock = self.readers.0.lock();
        *lock -= readers;
        if *lock == 0 {
            self.readers.1.notify_one();
        }
    }

    pub(in crate::agent) fn up_membership<T: ProofResContext>(
        &self,
        assert: &Arc<GroundedMemb>,
        context: Option<&T>,
    ) {
        let entities = &*self.entities;
        let classes = &*self.classes;

        let parent_exists = classes.contains_key(assert.get_parent());
        if !parent_exists {
            let class = Class::new(
                assert.get_parent().to_string(),
                ClassKind::Membership,
                self.svc_queue.clone(),
            );
            classes.insert(class.name.clone(), class);
        }
        let decl;
        let is_new: bool;
        match assert.get_name() {
            ClassTerm(class_name) => {
                let class_exists = classes.contains_key(class_name);
                if class_exists {
                    let class = classes.get(class_name).unwrap();
                    is_new = class.add_or_update_class_membership(self, assert, context);
                    decl = ClassMember::Class(assert.clone());
                } else {
                    let class = Class::new(
                        class_name.to_owned(),
                        ClassKind::Membership,
                        self.svc_queue.clone(),
                    );
                    classes.insert(class.name.clone(), class);
                    let class = classes.get(class_name).unwrap();
                    is_new = class.add_or_update_class_membership(self, assert, context);
                    decl = ClassMember::Class(assert.clone());
                }
            }
            EntityTerm(subject) => {
                let entity_exists = entities.contains_key(subject);
                if entity_exists {
                    let entity = entities.get(subject).unwrap();
                    is_new = entity.add_or_updated_class_membership(self, assert, context);
                    decl = ClassMember::Entity(assert.clone());
                } else {
                    let entity = Entity::new(subject.to_string(), self.svc_queue.clone());
                    entities.insert(entity.name.clone(), entity);
                    let entity = entities.get(subject).unwrap();
                    is_new = entity.add_or_updated_class_membership(self, assert, context);
                    decl = ClassMember::Entity(assert.clone());
                }
            }
        }
        if is_new {
            let parent = classes.get(assert.get_parent()).unwrap();
            parent.add_member(decl);
        }
    }

    fn upsert_objects_with_loc(&self, objs: impl Iterator<Item = (GrTerminalKind<String>, Point)>) {
        let classes = &*self.classes;
        let entities = &*self.entities;

        for (term, loc) in objs {
            match term {
                ClassTerm(class_name) => {
                    if let Some(class) = classes.get(&class_name) {
                        (*class).with_location(loc, None);
                    } else {
                        let class =
                            Class::new(class_name, ClassKind::Membership, self.svc_queue.clone());
                        class.with_location(loc, None);
                        classes.insert(class.name.clone(), class);
                    }
                }
                EntityTerm(subject) => {
                    if let Some(entity) = entities.get(&subject) {
                        (*entity).with_location(loc, None);
                    } else {
                        let entity = Entity::new(subject, self.svc_queue.clone());
                        entity.with_location(loc, None);
                        entities.insert(entity.name.clone(), entity);
                    }
                }
            }
        }
    }

    pub(in crate::agent) fn up_relation<T: ProofResContext>(
        &self,
        assert: &Arc<GroundedFunc>,
        context: Option<&T>,
    ) {
        let classes = &*self.classes;
        let entities = &*self.entities;

        // it doesn't matter this is overwritten, as if it exists, it exists for all
        let is_new = Rc::new(::std::cell::RefCell::new(true));
        let process_arg = |a: &GroundedMemb| {
            let is_new1;
            match a.get_name() {
                ClassTerm(class_name) => {
                    if let Some(class) = classes.get(class_name) {
                        is_new1 = class.add_relationship(self, assert, context);
                    } else {
                        let class = Class::new(
                            class_name.to_owned(),
                            ClassKind::Membership,
                            self.svc_queue.clone(),
                        );
                        classes.insert(class.name.clone(), class);
                        let class = classes.get(class_name).unwrap();
                        is_new1 = class.add_relationship(self, assert, context);
                    }
                }
                EntityTerm(subject) => {
                    if let Some(entity) = entities.get(subject) {
                        is_new1 = entity.add_relationship(self, assert, context);
                    } else {
                        let entity = Entity::new(subject.to_owned(), self.svc_queue.clone());
                        entities.insert(entity.name.clone(), entity);
                        let entity = entities.get(subject).unwrap();
                        is_new1 = entity.add_relationship(self, assert, context);
                    }
                }
            }
            let new_check = is_new.clone();
            *new_check.borrow_mut() = is_new1;
        };
        let relation_exists = classes.contains_key(assert.get_name());
        if !relation_exists {
            let relationship = Class::new(
                assert.get_name().to_string(),
                ClassKind::Relationship,
                self.svc_queue.clone(),
            );
            classes.insert(relationship.name.clone(), relationship);
        }
        for arg in assert.get_args() {
            process_arg(arg);
        }
        if *is_new.borrow() {
            let parent = classes.get(assert.get_name()).unwrap();
            parent.add_relation_to_class(assert.clone());
        }
    }

    fn add_belief(&self, belief: &Arc<LogSentence>) {
        let entities = &*self.entities;
        let classes = &*self.classes;

        let update = |subject: &str, name: &str, is_entity: bool, belief: &Arc<LogSentence>| {
            if is_entity {
                if let Some(entity) = entities.get(subject) {
                    entity.add_belief(belief.clone(), subject);
                } else {
                    let entity = Entity::new(subject.to_string(), self.svc_queue.clone());
                    entity.add_belief(belief.clone(), name);
                    entities.insert(subject.to_string(), entity);
                }
            } else if let Some(class) = classes.get(subject) {
                class.add_belief(belief.clone(), name);
            } else {
                let class = Class::new(
                    subject.to_string(),
                    ClassKind::Membership,
                    self.svc_queue.clone(),
                );
                class.add_belief(belief.clone(), name);
                classes.insert(subject.to_string(), class);
            }
        };

        for p in belief.get_all_predicates() {
            match *p {
                Assert::ClassDecl(ref cls_decl) => {
                    if let Some(class) = classes.get(cls_decl.get_name()) {
                        class.add_belief(belief.clone(), cls_decl.get_name());
                    } else {
                        let class = Class::new(
                            cls_decl.get_name().to_string(),
                            ClassKind::Membership,
                            self.svc_queue.clone(),
                        );
                        class.add_belief(belief.clone(), cls_decl.get_name());
                        classes.insert(class.name.clone(), class);
                    }
                    for arg in cls_decl.get_args() {
                        if !arg.is_var() {
                            match arg.get_name().into() {
                                ClassTerm(class_name) => {
                                    update(class_name, cls_decl.get_name(), false, &belief)
                                }
                                EntityTerm(subject_name) => {
                                    update(subject_name, cls_decl.get_name(), true, &belief)
                                }
                            }
                        }
                    }
                }
                Assert::FuncDecl(ref fn_decl) => {
                    if !fn_decl.is_relational() {
                        continue;
                    }
                    if let Some(class) = classes.get(fn_decl.get_name()) {
                        class.add_belief(belief.clone(), fn_decl.get_name());
                    } else {
                        let class = Class::new(
                            fn_decl.get_name().to_string(),
                            ClassKind::Relationship,
                            self.svc_queue.clone(),
                        );
                        class.add_belief(belief.clone(), fn_decl.get_name());
                        classes.insert(class.name.clone(), class);
                    }
                    for arg in fn_decl.get_args() {
                        if !arg.is_var() {
                            match arg.get_name().into() {
                                ClassTerm(class_name) => {
                                    update(class_name, fn_decl.get_name(), false, &belief)
                                }
                                EntityTerm(subject_name) => {
                                    update(subject_name, fn_decl.get_name(), true, &belief)
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
                        Predicate::FreeMembershipToClass(ref free) => {
                            if let Some(ls) = candidates.get(free.get_var_ref()) {
                                for entity in ls {
                                    let grfact = Arc::new(GroundedMemb::from_free(
                                        free,
                                        entity.name.as_ref(),
                                    ));
                                    self.ask_internal(QueryInput::AskClassMember(grfact), 0, true)
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
                            self.ask_internal(
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
        let mut assigned: HashSet<&str> =
            HashSet::with_capacity(sent_req.size_hint().1.unwrap_or(0));
        for var_req in sent_req {
            if let Some(candidates) = meet_sent_requirements(self, &var_req) {
                for (var, assign) in candidates.iter() {
                    let it = belief.get_rhs_predicates();
                    for pred in it.iter().filter(|x| x.contains(&**var)) {
                        match pred {
                            Assert::ClassDecl(ref cls_decl) => {
                                iter_cls_candidates(cls_decl, &candidates)
                            }
                            Assert::FuncDecl(ref func_decl) => {
                                iter_func_candidates(func_decl, &candidates)
                            }
                            Assert::SpecialFunc(BuiltIns::Move(move_fn)) => self
                                .iter_move_candidates(
                                    belief,
                                    move_fn,
                                    assign,
                                    &candidates,
                                    &mut assigned,
                                ),
                            Assert::SpecialFunc(_) => {}
                        }
                    }
                }
            }
        }
    }

    fn iter_move_candidates<'a, 'b: 'a>(
        &self,
        belief: &Arc<LogSentence>,
        move_fn: &MoveFn,
        assign: &[Arc<VarAssignment<'b>>],
        candidates: &HashMap<&Var, Vec<Arc<VarAssignment>>>,
        assigned: &'a mut HashSet<&'b str>,
    ) {
        let entities = &*self.entities;
        let classes = &*self.classes;

        assign.iter().for_each(|a| {
            let name = *a.name;
            if assigned.get(name) == None {
                // add this sent as move_fn to the potential candidates
                match a.name {
                    GrTerminalKind::Entity(_) => {
                        if let Some(ent) = entities.get(name) {
                            ent.add_move_belief(belief);
                        }
                    }
                    GrTerminalKind::Class(_) => {
                        if let Some(cls) = classes.get(name) {
                            cls.add_move_belief(belief);
                        }
                    }
                }
                assigned.insert(name);
            }
        });
        if let Some(mapped) = ArgsProduct::product(candidates.clone()) {
            let mut pos: Option<Point> = None;
            for args in mapped {
                let args = HashMap::from_iter(args.iter().map(|(v, a)| (*v, &**a)));
                for arg in args
                    .values()
                    .map(|v| GrTerminalKind::from((*v.name).to_owned()))
                {
                    if let Some(pos) = &pos {
                        // ask if the obj is in that location
                        self.ask_internal(
                            QueryInput::AskLocation(LocFn::from((arg, pos.clone()))),
                            0,
                            true,
                        )
                        .unwrap();
                    } else {
                        // fetch the location the first time
                        let (_, loc_assign) = belief.get_assignments(self, Some(&args));
                        let locs = move_fn.get_location(&loc_assign).unwrap();
                        let loc = locs.get_last_value().1.unwrap();
                        pos = Some(loc.clone());
                        self.ask_internal(
                            QueryInput::AskLocation(LocFn::from((arg, loc))),
                            0,
                            true,
                        )
                        .unwrap();
                    }
                }
            }
        }
    }

    fn add_rule(&self, rule: &Arc<LogSentence>) {
        let classes = &*self.classes;

        let preds = rule.get_all_predicates();
        for p in preds {
            let name = p.get_name();
            if let Some(class) = classes.get(name) {
                class.add_rule(rule.clone());
            } else {
                let nc = match *p {
                    Assert::ClassDecl(_) => Class::new(
                        name.to_string(),
                        ClassKind::Membership,
                        self.svc_queue.clone(),
                    ),
                    Assert::FuncDecl(_) => Class::new(
                        name.to_string(),
                        ClassKind::Relationship,
                        self.svc_queue.clone(),
                    ),
                    Assert::SpecialFunc(_) => unreachable!(),
                };
                nc.add_rule(rule.clone());
                classes.insert(name.to_string(), nc);
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
        let sclasses = &*self.classes;

        let mut dict = HashMap::new();
        for cls in classes {
            if let Some(klass) = sclasses.get(*cls) {
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
        let sclasses = &*self.classes;

        let mut dict = HashMap::new();
        for func in funcs {
            if let Some(func_ref) = sclasses.get(&*func.get_name()) {
                let mut m = HashMap::new();
                for e in &**func_ref.value().members.read() {
                    if let ClassMember::Func(ref f) = *e {
                        if f.get_value().is_some() {
                            for name in f.get_args_names() {
                                let name = unsafe { std::mem::transmute::<&str, &'a str>(name) };
                                let e: &mut Vec<_> = m.entry(name).or_insert_with(Vec::new);
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
        let entities = &*self.entities;
        let classes = &*self.classes;

        match *subject {
            EntityTerm(ref subject_name) => {
                // let entities = &*entities;
                if let Some(entity) = entities.get(subject_name.as_ref()) {
                    match entity.belongs_to_class(class, false) {
                        Some(r) => Some(r),
                        None => None,
                    }
                } else {
                    None
                }
            }
            ClassTerm(ref class_name) => {
                if let Some(klass) = classes.get(class_name.as_ref()) {
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
        let entities = &*self.entities;
        let classes = &*self.classes;

        match pred.get_name() {
            EntityTerm(subject) => {
                if let Some(entity) = entities.get(subject) {
                    if let Some(current) = entity.belongs_to_class(pred.get_parent(), true) {
                        return current.compare(pred);
                    }
                }
            }
            ClassTerm(class_name) => {
                if let Some(class) = classes.get(class_name) {
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
        let entities = &*self.entities;
        let classes = &*self.classes;

        if let EntityTerm(name) = subject.get_name().into() {
            if let Some(entity) = entities.get(name) {
                entity.get_class_membership(subject)
            } else {
                vec![]
            }
        } else if let Some(class) = classes.get(subject.get_name()) {
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
        let entities = &*self.entities;
        let classes = &*self.classes;

        if let EntityTerm(subject_name) = subject.into() {
            if let Some(entity) = entities.get(subject_name) {
                if let Some(current) = entity.has_relationship(pred) {
                    return current.compare(pred);
                }
            }
        } else if let Some(class) = classes.get(subject) {
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
        let entities = &*self.entities;
        let classes = &*self.classes;

        if let EntityTerm(subject_name) = subject.into() {
            if let Some(entity) = entities.get(subject_name) {
                if let Some(current) = entity.has_relationship(pred) {
                    if current.compare_ignoring_times(pred) {
                        return Some(current);
                    } else {
                        return None;
                    }
                }
            }
        } else if let Some(class) = classes.get(subject) {
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
        let classes = &*self.classes;
        let entities = &*self.entities;

        let mut res = HashMap::new();
        for (pos, arg) in func.get_args().iter().enumerate() {
            if !arg.is_var() {
                if let EntityTerm(subject_name) = arg.get_name().into() {
                    if let Some(entity) = entities.get(subject_name) {
                        let mut v: HashMap<&str, Vec<Arc<GroundedFunc>>> =
                            entity.get_relationships(pos, arg);
                        for (_, mut funcs) in v.drain() {
                            // Safety: guaranteed this lives as long as self and mutable borrows don't leak
                            // return type is: (&'a str, Arc<GrFunc>) where &'str lives as long as Arc<GrFunc>
                            // &'a str must NOT outlive (nor leak from) this Repr, this would be UB,
                            let rel = unsafe { &*(funcs[0].get_name() as *const str) };
                            res.entry(rel).or_insert_with(Vec::new).append(&mut funcs);
                        }
                    }
                } else if let Some(class) = classes.get(arg.get_name()) {
                    let mut v: HashMap<&str, Vec<Arc<GroundedFunc>>> =
                        class.get_relationships(pos, arg);
                    for (_, mut funcs) in v.drain() {
                        // Safety: guaranteed this lives as long as self and mutable borrows don't leak
                        // return type is: (&'a str, Arc<GrFunc>) where &'str lives as long as Arc<GrFunc>
                        // &'a str must NOT outlive (nor leak from) this Repr, this would be UB,
                        let rel = unsafe { &*(funcs[0].get_name() as *const str) };
                        res.entry(rel).or_insert_with(Vec::new).append(&mut funcs);
                    }
                }
            }
        }
        res
    }

    pub fn set_threads(&mut self, threads: usize) {
        let mut new_tp = rayon::ThreadPoolBuilder::new()
            .num_threads(threads)
            .build()
            .unwrap();
        std::mem::swap(&mut new_tp, &mut self.threads);
        std::mem::drop(new_tp);
    }

    pub fn clear(&mut self) {
        self.entities.clear();
        self.classes.clear();
    }

    #[allow(clippy::type_complexity)]
    pub(super) fn find_objs_by_loc<'a, T: AsRef<str> + 'a>(
        &self,
        objects: impl Iterator<Item = (&'a Arc<GrTerminalKind<T>>, &'a Point)>,
    ) -> Vec<(&'a Point, Arc<GrTerminalKind<T>>, Option<bool>)> {
        let classes = &*self.classes;
        let entities = &*self.entities;

        let mut answer = vec![];
        for (term, loc) in objects {
            match &**term {
                ClassTerm(class_name) => {
                    if let Some(class) = classes.get(class_name.as_ref()) {
                        let rec = class.location.get_record_at_location(loc, None);
                        if !rec.is_empty() {
                            answer.push((loc, term.clone(), Some(true)))
                        } else {
                            answer.push((loc, term.clone(), Some(false)))
                        }
                    } else {
                        answer.push((loc, term.clone(), None))
                    }
                }
                EntityTerm(subject) => {
                    if let Some(entity) = entities.get(subject.as_ref()) {
                        let rec = entity.location.get_record_at_location(loc, None);
                        if !rec.is_empty() {
                            answer.push((loc, term.clone(), Some(true)))
                        } else {
                            answer.push((loc, term.clone(), Some(false)))
                        }
                    } else {
                        answer.push((loc, term.clone(), None))
                    }
                }
            }
        }
        answer
    }

    pub(super) fn find_all_objs_in_loc<'a>(&self, loc: &'a Point) -> Vec<GrTerminalKind<String>> {
        let classes = &*self.classes;
        let entities = &*self.entities;

        let mut answer = vec![];
        for class in &*classes {
            if class
                .location
                .get_record_at_location(loc, None)
                .into_iter()
                .next()
                .flatten()
                .is_some()
            {
                answer.push(GrTerminalKind::Class(class.key().to_owned()));
            }
        }
        for entity in &*entities {
            if entity
                .location
                .get_record_at_location(loc, None)
                .into_iter()
                .next()
                .flatten()
                .is_some()
            {
                answer.push(GrTerminalKind::Entity(entity.key().to_owned()));
            }
        }
        answer
    }
}

#[cfg(not(feature = "persistence"))]
impl Default for Representation {
    fn default() -> Self {
        Representation::new(num_cpus::get())
    }
}

#[cfg(feature = "persistence")]
impl Default for Representation {
    fn default() -> Self {
        Representation::new(num_cpus::get()).unwrap()
    }
}

impl Drop for Representation {
    fn drop(&mut self) {
        loop {
            match self.svc_queue.try_send(BackgroundTask::Shutdown) {
                Err(crossbeam::channel::TrySendError::Disconnected(_)) => break,
                Ok(()) | Err(_) => {
                    // keep retrying
                }
            }
        }
    }
}

#[derive(Clone)]
struct ReprConfig {
    #[cfg(feature = "persistence")]
    persist: Arc<AtomicBool>,
}

impl Default for ReprConfig {
    fn default() -> Self {
        ReprConfig {
            #[cfg(feature = "persistence")]
            persist: Arc::new(AtomicBool::new(false)),
        }
    }
}

struct ReprSharedData {
    entities: InnerData<Entity>,
    classes: InnerData<Class>,
    config: ReprConfig,
    readers: Arc<(Mutex<usize>, Condvar)>,
    bg_task_rcv: Receiver<BackgroundTask>,
    #[cfg(feature = "persistence")]
    storage_layer: ReprStorageManager,
}

impl ReprSharedData {
    // TODO: this should be optimized and is a first naive implementation by:
    // 1. checking if state has indeed changed since last persistence first
    // 2. perform incremental persistence over different time chunks to not starve the main thread
    #[cfg(feature = "persistence")]
    fn persist(&mut self) -> bincode::Result<()> {
        if self.config.persist.load(Ordering::SeqCst) {
            let entities: Result<Vec<_>, _> = self
                .entities
                .iter()
                .map(|entity| entity.persist())
                .collect();
            for mut ent in entities? {
                let mut metadata = EntityMetadata::new(&ent);
                ent.classes.drain(..).fold(Ok(()), |res, bin| {
                    self.add_to_storage(res, bin, &mut metadata)
                })?;
                ent.relations.drain(..).flatten().fold(Ok(()), |res, bin| {
                    self.add_to_storage(res, bin, &mut metadata)
                })?;
                ent.beliefs.drain(..).flatten().fold(Ok(()), |res, bin| {
                    self.add_to_storage(res, bin, &mut metadata)
                })?;
                ent.move_beliefs.drain(..).fold(Ok(()), |res, bin| {
                    self.add_to_storage(res, bin, &mut metadata)
                })?;
                self.storage_layer.upsert_metada(metadata);
            }
        }
        Ok(())
    }

    #[cfg(feature = "persistence")]
    fn add_to_storage<T>(
        &mut self,
        res: std::io::Result<()>,
        bin: T,
        metadata: &mut EntityMetadata,
    ) -> std::io::Result<()>
    where
        T: super::storage::ToBinaryObject,
    {
        match res {
            Err(err) => Err(err),
            Ok(()) => {
                let addr = self.storage_layer.insert_rec(bin)?;
                metadata.stored_data.insert(addr);
                Ok(())
            }
        }
    }
}

fn bg_thread(mut shared_data: ReprSharedData) {
    let (num_writers_lock, cvar) = &*shared_data.readers.clone();
    let mut time_slice = Instant::now() + Representation::BG_TASK_TIME_SLICE;
    let mut last_bg_exec = Instant::now();
    let mut potentially_disconnected = false;
    loop {
        let mut num_writers = num_writers_lock.lock();
        if *num_writers > 0 {
            if cvar
                .wait_for(&mut num_writers, Duration::from_secs(10))
                .timed_out()
            {
                // timed out... parent thread may have been killed leaving the number of reads in
                // a corrupted state. We can double check this by checking if the other end of the channel
                // has been disconnected below
                potentially_disconnected = true;
            }
        }
        let msg = shared_data.bg_task_rcv.try_recv();
        match msg {
            Ok(msg) => match background_service(msg) {
                Ok(()) => {}
                Err(()) => {
                    std::mem::drop(num_writers);
                    break;
                }
            },
            Err(crossbeam::channel::TryRecvError::Empty) => {
                if potentially_disconnected {
                    // wasn't really disconnected, just starved because concurrent reads, try again
                    potentially_disconnected = false;
                    continue;
                }
                if last_bg_exec.elapsed() > Representation::BG_SEC_TASK_FREQ {
                    #[cfg(feature = "persistence")]
                    shared_data.persist().expect("failed to perform persist op");
                    last_bg_exec = Instant::now();
                }
            }
            Err(crossbeam::channel::TryRecvError::Disconnected) if potentially_disconnected => {
                // attempt to persist the current state a last time
                #[cfg(feature = "persistence")]
                shared_data.persist().expect("failed to perform persist op");
                break;
            }
            Err(crossbeam::channel::TryRecvError::Disconnected) => {
                break;
            }
        }
        if Instant::now() > time_slice {
            std::mem::drop(num_writers);
            std::thread::sleep(Duration::from_nanos(100));
            time_slice = Instant::now() + Representation::BG_TASK_TIME_SLICE;
        }
    }
}

/// Performs critical functions in the background service.
#[inline(always)]
fn background_service(msg: BackgroundTask) -> Result<(), ()> {
    use BackgroundTask::*;

    match msg {
        CompactBmsLog(rec) => {
            let rec = rec;
            if !rec.is_null() {
                let rec = unsafe { &*rec as &BmsWrapper<_> };
                rec.compact_record_log();
            }
        }
        Shutdown => return Err(()),
    }

    Ok(())
}

pub(super) enum BackgroundTask {
    /// Compact a log that was marked for sweep at this address.
    /// Safety: Those are pinned and guaranteed to have stable addresses,
    /// so are largely safe to dereference.
    /// It shouldn't be freed either cause the parent object won't be dropped until
    /// all the messages before receiving a shutdown signal have been processed.
    CompactBmsLog(*const BmsWrapper<RecordHistory>),
    Shutdown,
}

unsafe impl Send for BackgroundTask {}

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

    pub fn get_located_objects(&self) -> HashMap<Point, Vec<ObjName<'a>>> {
        self.0.get_located_objects()
    }
}

pub(super) fn lookahead_rules(agent: &Representation, name: &str, grounded: &GroundedRef) -> bool {
    use super::inference::rules_inference_lookahead;
    let rules: Vec<Arc<LogSentence>> = {
        let classes = &*agent.classes;
        let class = classes.get(name).unwrap();
        let rules = &*class.rules.read();
        rules.clone()
    };
    rules_inference_lookahead(agent, rules, grounded)
}

fn rollback_from_rule(agent: &Representation, rule: &Arc<LogSentence>) {
    use super::inference::rules_inference_rollback;
    rules_inference_rollback(agent, rule);
}
