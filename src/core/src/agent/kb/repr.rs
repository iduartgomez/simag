use std::collections::{HashMap, VecDeque};
use std::iter::FromIterator;
use std::sync::RwLock;
use std::rc::Rc;

use float_cmp::ApproxEqUlps;

use agent;
use lang;
use lang::{GroundedClsMemb, GroundedFunc, LogSentence, ParseErrF, ParseTree};
use super::*;

/// This type is a container for internal agent's representations.
/// An agent can have any number of such representations at any moment,
/// all of which are contained in this object.
///
/// The class includes methods to encode and decode the representations
/// to/from data streams or idioms.
///
/// Attributes:
///     entities -> Unique members (entities) of their own set/class.
///     | Entities are denoted with a $ symbol followed by an id.
///     classes -> Sets of objects (entities or subclasses) that share a common property.
///     | This includes 'classes of relationships' and other 'functions'.
pub struct Representation {
    pub entities: RwLock<HashMap<Rc<String>, Box<Entity>>>,
    pub classes: RwLock<HashMap<Rc<String>, Box<Class>>>,
}

impl Representation {
    pub fn new() -> Representation {
        Representation {
            entities: RwLock::new(HashMap::new()),
            classes: RwLock::new(HashMap::new()),
        }
    }

    /// Parses a sentence (or several of them) into an usable formula
    /// and stores it into the internal representation along with the
    /// corresponding classes. In case the sentence is a predicate,
    /// the objects get declared as members of their classes.
    ///
    /// Accepts first-order logic sentences sentences, both atomic
    /// sentences ('Lucy is a professor') and complex sentences compossed
    /// of different atoms and operators ('If someone is a professor,
    /// then it's a person'). Examples:
    ///
    /// `>>> r.tell("(professor[$Lucy,u=1])")`
    /// will include the individual '$Lucy' in the professor category)
    /// `>>> r.tell("((let x) professor[x,u=1] |> person[x,u=1])")`
    /// all the individuals which are professors will be added to the
    /// person category, and the formula will be stored in the professor
    /// class for future use.
    ///
    /// For more examples check the LogSentence type docs.
    pub fn tell(&self, source: String) -> Result<(), Vec<ParseErrF>> {
        let pres = lang::logic_parser(source, true);
        if pres.is_ok() {
            let mut pres: VecDeque<ParseTree> = pres.unwrap();
            let mut errors = Vec::new();
            for _ in 0..pres.len() {
                match pres.pop_front().unwrap() {
                    ParseTree::Assertion(assertions) => {
                        for assertion in assertions {
                            if assertion.is_class() {
                                let cls_decl = assertion.unwrap_cls();
                                let f = HashMap::new();
                                let time_data = cls_decl.get_own_time_data(&f, None);
                                for a in cls_decl {
                                    let t = time_data.clone();
                                    t.replace_last_val(a.get_value());
                                    a.override_time_data(&t);
                                    self.up_membership(Rc::new(a), None)
                                }
                            } else {
                                let a = Rc::new(assertion.unwrap_fn().into_grounded());
                                self.up_relation(a, None)
                            }
                        }
                    }
                    ParseTree::IExpr(iexpr) => self.add_belief(Rc::new(iexpr)),
                    ParseTree::Expr(rule) => self.add_rule(Rc::new(rule)),
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
    pub fn ask(&self, source: String) -> Answer {
        let pres = lang::logic_parser(source, false);
        if pres.is_ok() {
            let pres = QueryInput::ManyQueries(pres.unwrap());
            let mut inf = match Inference::new(self, pres, false) {
                Ok(inf) => inf,
                Err(()) => return Answer::QueryErr,
            };
            {
                let mut inf_r = unsafe { &mut *(&mut inf as *mut Box<Inference>) };
                inf_r.infer_facts();
            }
            inf.get_results()
        } else {
            Answer::ParseErr(pres.unwrap_err())
        }
    }

    pub fn ask_processed(&self, source: QueryInput, ignore_current: bool) -> Answer {
        let mut inf = match Inference::new(self, source, ignore_current) {
            Ok(inf) => inf,
            Err(()) => return Answer::QueryErr,
        };
        {
            let mut inf_r = unsafe { &mut *(&mut inf as *mut Box<Inference>) };
            inf_r.infer_facts();
        }
        inf.get_results()
    }

    pub fn up_membership(&self,
                         assert: Rc<lang::GroundedClsMemb>,
                         context: Option<&agent::ProofResult>) {
        let parent_exists = self.classes.read().unwrap().contains_key(&assert.get_parent());
        if !parent_exists {
            let class = Box::new(Class::new(assert.get_parent(), ClassKind::Membership));
            self.classes.write().unwrap().insert(class.name.clone(), class);
        }
        let decl;
        let is_new: bool;
        if (&assert.get_name()).starts_with('$') {
            let entity_exists = self.entities.read().unwrap().contains_key(&assert.get_name());
            if entity_exists {
                let lock = self.entities.read().unwrap();
                let entity = lock.get(&assert.get_name()).unwrap();
                is_new = entity.add_class_membership(self, assert.clone(), context);
                decl = ClassMember::Entity(assert.clone());
            } else {
                let entity = Box::new(Entity::new(assert.get_name()));
                is_new = entity.add_class_membership(self, assert.clone(), context);
                self.entities.write().unwrap().insert(entity.name.clone(), entity);
                decl = ClassMember::Entity(assert.clone());
            }
        } else {
            let class_exists = self.classes.read().unwrap().contains_key(&assert.get_name());
            if class_exists {
                let lock = self.classes.read().unwrap();
                let class = lock.get(&assert.get_name()).unwrap();
                is_new = class.add_class_membership(self, assert.clone(), context);
                decl = ClassMember::Class(assert.clone());
            } else {
                let class = Box::new(Class::new(assert.get_name(), ClassKind::Membership));
                is_new = class.add_class_membership(self, assert.clone(), context);
                self.classes.write().unwrap().insert(class.name.clone(), class);
                decl = ClassMember::Class(assert.clone());
            }
        }
        if is_new {
            let lock = self.classes.read().unwrap();
            let parent = lock.get(&assert.get_parent()).unwrap();
            parent.add_member(decl);
        }
    }

    pub fn up_relation(&self,
                       assert: Rc<lang::GroundedFunc>,
                       context: Option<&agent::ProofResult>) {
        // it doesn't matter this is overwritten, as if it exists, it exists for all
        let is_new = Rc::new(::std::cell::RefCell::new(true));
        let process_arg = |a: &GroundedClsMemb| {
            let subject = a.get_name();
            let is_new1;
            if (&subject).starts_with('$') {
                let entity_exists = self.entities.read().unwrap().contains_key(&subject);
                if entity_exists {
                    let lock = self.entities.read().unwrap();
                    let entity = lock.get(&subject).unwrap();
                    is_new1 = entity.add_relationship(self, assert.clone(), context);
                } else {
                    let entity = Box::new(Entity::new(subject));
                    is_new1 = entity.add_relationship(self, assert.clone(), context);
                    self.entities.write().unwrap().insert(entity.name.clone(), entity);
                }
            } else {
                let class_exists = self.classes.read().unwrap().contains_key(&subject);
                if class_exists {
                    let lock = self.classes.read().unwrap();
                    let class = lock.get(&subject).unwrap();
                    is_new1 = class.add_relationship(self, assert.clone(), context);
                } else {
                    let class = Box::new(Class::new(subject, ClassKind::Membership));
                    is_new1 = class.add_relationship(self, assert.clone(), context);
                    self.classes.write().unwrap().insert(class.name.clone(), class);
                }
            }
            let new_check = is_new.clone();
            *new_check.borrow_mut() = is_new1;
        };
        let relation_exists = self.classes.read().unwrap().contains_key(&assert.name);
        if !relation_exists {
            let relationship = Box::new(Class::new(assert.name.clone(), ClassKind::Relationship));
            self.classes.write().unwrap().insert(relationship.name.clone(), relationship);
        }
        process_arg(&assert.args[0]);
        process_arg(&assert.args[1]);
        if assert.third.is_some() {
            process_arg(assert.third.as_ref().unwrap())
        }
        if *is_new.borrow() {
            let lock = self.classes.read().unwrap();
            let parent = lock.get(&assert.name).unwrap();
            parent.add_grounded_relationship(assert.clone());
        }
    }

    fn add_belief(&self, belief: Rc<LogSentence>) {
        fn update(subject: Rc<String>,
                  name: Rc<String>,
                  is_entity: bool,
                  belief: Rc<LogSentence>,
                  repr: &Representation) {
            if is_entity {
                let entity_exists = repr.entities.read().unwrap().contains_key(&subject);
                if entity_exists {
                    let lock = repr.entities.read().unwrap();
                    let entity = lock.get(&subject).unwrap();
                    entity.add_belief(belief.clone(), subject);
                } else {
                    let entity = Box::new(Entity::new(subject.clone()));
                    entity.add_belief(belief.clone(), name);
                    repr.entities
                        .write()
                        .unwrap()
                        .insert(subject.clone(), entity);
                }
            } else {
                let class_exists = repr.classes.read().unwrap().contains_key(&subject);
                if class_exists {
                    let lock = repr.classes.read().unwrap();
                    let class = lock.get(&subject).unwrap();
                    class.add_belief(belief.clone(), name);
                } else {
                    let class = Box::new(Class::new(subject.clone(), ClassKind::Membership));
                    class.add_belief(belief.clone(), name);
                    repr.classes
                        .write()
                        .unwrap()
                        .insert(subject.clone(), class);
                }
            }
        };

        for p in belief.get_all_predicates() {
            match *p {
                lang::Assert::ClassDecl(ref cls_decl) => {
                    let class_exists = self.classes
                        .read()
                        .unwrap()
                        .contains_key(&cls_decl.get_name());
                    if class_exists {
                        self.classes
                            .read()
                            .unwrap()
                            .get(&cls_decl.get_name())
                            .unwrap()
                            .add_belief(belief.clone(), cls_decl.get_name())
                    } else {
                        let class = Box::new(Class::new(cls_decl.get_name(),
                                                        ClassKind::Membership));
                        class.add_belief(belief.clone(), cls_decl.get_name());
                        self.classes.write().unwrap().insert(class.name.clone(), class);
                    }
                    for arg in cls_decl.get_args() {
                        if !arg.is_var() {
                            let subject = arg.get_name();
                            if subject.starts_with('$') {
                                update(subject, cls_decl.get_name(), true, belief.clone(), self)
                            } else {
                                update(subject, cls_decl.get_name(), false, belief.clone(), self)
                            }
                        }
                    }
                }
                lang::Assert::FuncDecl(ref fn_decl) => {
                    if !fn_decl.is_relational() {
                        continue;
                    }
                    let class_exists = self.classes
                        .read()
                        .unwrap()
                        .contains_key(&fn_decl.get_name());
                    if class_exists {
                        self.classes
                            .read()
                            .unwrap()
                            .get(&fn_decl.get_name())
                            .unwrap()
                            .add_belief(belief.clone(), fn_decl.get_name())
                    } else {
                        let class = Box::new(Class::new(fn_decl.get_name(),
                                                        ClassKind::Relationship));
                        class.add_belief(belief.clone(), fn_decl.get_name());
                        self.classes.write().unwrap().insert(class.name.clone(), class);
                    }
                    for arg in fn_decl.get_args() {
                        if !arg.is_var() {
                            let subject = arg.get_name();
                            if subject.starts_with('$') {
                                update(subject, fn_decl.get_name(), true, belief.clone(), self)
                            } else {
                                update(subject, fn_decl.get_name(), false, belief.clone(), self)
                            }
                        }
                    }
                }
            }
        }

        let iter_cls_candidates = |cls_decl: &lang::ClassDecl,
                                   candidates: &HashMap<Rc<lang::Var>, Vec<Rc<VarAssignment>>>| {
            let f = HashMap::new();
            for a in cls_decl.get_args() {
                match *a {
                    lang::Predicate::FreeClsMemb(ref free) => {
                        if let Some(ls) = candidates.get(&free.get_var()) {
                            for entity in ls {
                                let grfact = Rc::new(GroundedClsMemb::from_free(free,
                                                                                entity.name
                                                                                    .clone(),
                                                                                &f));
                                self.ask_processed(QueryInput::AskClassMember(grfact), true);
                            }
                        }
                    }
                    _ => continue,
                }
            }
        };
        let iter_func_candidates = |func_decl: &lang::FuncDecl,
                                    candidates: &HashMap<Rc<lang::Var>, Vec<Rc<VarAssignment>>>| {
            let mapped = ArgsProduct::product(candidates.clone());
            if let Some(mapped) = mapped {
                let f = HashMap::new();
                for args in mapped {
                    let args = HashMap::from_iter(args.iter()
                        .map(|&(ref v, ref a)| (v.clone(), &**a)));
                    if let Ok(grfunc) = GroundedFunc::from_free(func_decl, &args, &f) {
                        self.ask_processed(QueryInput::AskRelationalFunc(Rc::new(grfunc)), true);
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
                            lang::Assert::ClassDecl(ref cls_decl) => {
                                iter_cls_candidates(cls_decl, &candidates)
                            }
                            lang::Assert::FuncDecl(ref func_decl) => {
                                iter_func_candidates(func_decl, &candidates)
                            }
                        }
                    }
                }
            }
        }
    }

    fn add_rule(&self, rule: Rc<LogSentence>) {
        let mut n = Vec::new();
        let preds = rule.get_all_predicates();
        for p in preds {
            let name = p.get_name();
            n.push(name.clone());
            let class_exists = self.classes.read().unwrap().contains_key(&name);
            if class_exists {
                let lock = self.classes.read().unwrap();
                let class = lock.get(&name).unwrap();
                class.add_rule(rule.clone());
            } else {
                let nc = match *p {
                    lang::Assert::ClassDecl(_) => {
                        Box::new(Class::new(name.clone(), ClassKind::Membership))
                    }
                    lang::Assert::FuncDecl(_) => {
                        Box::new(Class::new(name.clone(), ClassKind::Relationship))
                    }
                };
                nc.add_rule(rule.clone());
                self.classes.write().unwrap().insert(name, nc);
            }
        }
        let obj_dic = self.by_class(&n);
        for _e in obj_dic {
            panic!("not implemented: rule.call(self, e)")
        }
    }

    /// Takes a vector of class names and returns a hash map with those classes as keys
    /// and the memberships to those classes.
    pub fn by_class(&self,
                    classes: &[Rc<String>])
                    -> HashMap<Rc<String>, Vec<Rc<GroundedClsMemb>>> {
        let mut dict = HashMap::new();
        let lock = self.classes.read().unwrap();
        for cls in classes {
            let cls_ref = lock.get(cls);
            if cls_ref.is_none() {
                continue;
            }
            let mut v = vec![];
            for e in &**cls_ref.unwrap().members.read().unwrap() {
                match *e {
                    ClassMember::Class(ref m) |
                    ClassMember::Entity(ref m) => v.push(m.clone()),
                    _ => {}
                }
            }
            dict.insert(cls.clone(), v);
        }
        dict
    }

    /// Takes a vector of relation declarations and returns a hash map with those relation
    /// names as keys and a hash map of the objects which have one relation of that kind
    /// as value (with a list of the grounded functions for each object).
    #[allow(type_complexity)]
    pub fn by_relationship(&self,
                           funcs: &[&lang::FuncDecl])
                           -> HashMap<Rc<String>, HashMap<Rc<String>, Vec<Rc<GroundedFunc>>>> {
        let mut dict = HashMap::new();
        let lock = self.classes.read().unwrap();
        for func in funcs {
            let func_ref = lock.get(&func.get_name());
            if func_ref.is_none() {
                continue;
            }
            let mut m = HashMap::new();
            for e in &**func_ref.unwrap().members.read().unwrap() {
                if let ClassMember::Func(ref f) = *e {
                    for name in f.get_args_names() {
                        let e: &mut Vec<_> = m.entry(name).or_insert(Vec::new());
                        e.push(f.clone())
                    }
                }
            }
            dict.insert(func.get_name(), m);
        }
        dict
    }

    pub fn get_entity_from_class(&self,
                                 class: Rc<String>,
                                 subject: Rc<String>)
                                 -> Option<Rc<GroundedClsMemb>> {
        if subject.starts_with('$') {
            let entity_exists = self.entities.read().unwrap().contains_key(&subject);
            if entity_exists {
                let lock = self.entities.read().unwrap();
                match lock.get(&subject).unwrap().belongs_to_class(class) {
                    Some(r) => Some(r.clone()),
                    None => None,
                }
            } else {
                None
            }
        } else {
            let class_exists = self.classes.read().unwrap().contains_key(&subject);
            if class_exists {
                let lock = self.classes.read().unwrap();
                match lock.get(&subject).unwrap().belongs_to_class(class) {
                    Some(r) => Some(r.clone()),
                    None => None,
                }
            } else {
                None
            }
        }
    }

    pub fn class_membership(&self, pred: &GroundedClsMemb) -> Option<bool> {
        let subject = pred.get_name();
        if subject.starts_with('$') {
            if let Some(entity) = self.entities.read().unwrap().get(&subject) {
                if let Some(current) = entity.belongs_to_class(pred.get_parent()) {
                    if *current == *pred {
                        return Some(true);
                    } else {
                        return Some(false);
                    }
                }
            }
        } else if let Some(class) = self.classes.read().unwrap().get(&subject) {
            if let Some(current) = class.belongs_to_class(pred.get_parent()) {
                if *current == *pred {
                    return Some(true);
                } else {
                    return Some(false);
                }
            }
        }
        None
    }

    pub fn get_class_membership(&self, subject: &lang::FreeClsOwner) -> Vec<Rc<GroundedClsMemb>> {
        let name = &subject.term;
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

    pub fn has_relationship(&self, pred: &GroundedFunc, subject: Rc<String>) -> Option<bool> {
        if subject.starts_with('$') {
            if let Some(entity) = self.entities.read().unwrap().get(&subject) {
                if let Some(current) = entity.has_relationship(pred) {
                    if *current == *pred {
                        return Some(true);
                    } else {
                        return Some(false);
                    }
                }
            }
        } else if let Some(class) = self.classes.read().unwrap().get(&subject) {
            if let Some(current) = class.has_relationship(pred) {
                if *current == *pred {
                    return Some(true);
                } else {
                    return Some(false);
                }
            }
        }
        None
    }

    pub fn get_relationship(&self,
                            pred: &GroundedFunc,
                            subject: Rc<String>)
                            -> Option<Rc<GroundedFunc>> {
        if subject.starts_with('$') {
            if let Some(entity) = self.entities.read().unwrap().get(&subject) {
                if let Some(current) = entity.has_relationship(pred) {
                    if *current == *pred {
                        return Some(current);
                    } else {
                        return None;
                    }
                }
            }
        } else if let Some(class) = self.classes.read().unwrap().get(&subject) {
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

    pub fn get_relationships(&self,
                             func: &lang::FuncDecl)
                             -> HashMap<Rc<String>, Vec<Rc<GroundedFunc>>> {
        let mut res = HashMap::new();
        for (pos, arg) in func.get_args().enumerate() {
            if !arg.is_var() {
                let name = arg.get_name();
                if name.starts_with('$') {
                    if let Some(entity) = self.entities.read().unwrap().get(&name) {
                        let mut v = entity.get_relationships(pos, arg);
                        for (rel, mut funcs) in v.drain() {
                            res.entry(rel).or_insert(vec![]).append(&mut funcs);
                        }
                    }
                } else if let Some(class) = self.classes.read().unwrap().get(&name) {
                    let mut v = class.get_relationships(pos, arg);
                    for (rel, mut funcs) in v.drain() {
                        res.entry(rel).or_insert(vec![]).append(&mut funcs);
                    }
                }
            }
        }
        res
    }
}

/// An entity is the unique member of it's own class.
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
///
///     * name: unique name to identify the entity.
///     * classes: categories to which the object belongs. Includes the degree of membership
///       (ie. ("cold", 0.9)).
///     * attr: implicit attributes of the object, unique to itself.
///     * beliefs: hese are the cognitions attributed to the object by the agent owning this
///       representation.
///     * relations: Functions between objects and/or classes.
///
#[derive(Debug)]
pub struct Entity {
    pub name: Rc<String>,
    classes: RwLock<HashMap<Rc<String>, Rc<GroundedClsMemb>>>,
    relations: RwLock<HashMap<Rc<String>, Vec<Rc<GroundedFunc>>>>,
    beliefs: RwLock<HashMap<Rc<String>, Vec<Rc<LogSentence>>>>,
}

impl Entity {
    fn new(name: Rc<String>) -> Entity {
        Entity {
            name: name,
            classes: RwLock::new(HashMap::new()),
            relations: RwLock::new(HashMap::new()),
            beliefs: RwLock::new(HashMap::new()),
        }
    }

    fn belongs_to_class(&self, class_name: Rc<String>) -> Option<Rc<GroundedClsMemb>> {
        let lock = self.classes.read().unwrap();
        match lock.get(&class_name) {
            Some(r) => Some(r.clone()),
            None => None,
        }
    }

    fn get_class_membership(&self, compare: &lang::FreeClsOwner) -> Vec<Rc<GroundedClsMemb>> {
        let lock = self.classes.read().unwrap();
        lock.values().filter(|x| compare.filter_grounded(&**x)).cloned().collect::<Vec<_>>()
    }

    fn add_class_membership(&self,
                            agent: &Representation,
                            grounded: Rc<GroundedClsMemb>,
                            context: Option<&agent::ProofResult>)
                            -> bool {
        let mut lock = self.classes.write().unwrap();
        let name = grounded.get_parent();
        let stmt_exists = lock.contains_key(&name);
        if stmt_exists {
            let current = lock.get(&name).unwrap();
            GroundedClsMemb::update(current.clone(), agent, grounded, context);
            false
        } else {
            lock.insert(name, grounded);
            true
        }
    }

    fn has_relationship(&self, func: &GroundedFunc) -> Option<Rc<GroundedFunc>> {
        let lock = self.relations.read().unwrap();
        if let Some(relation_type) = lock.get(&func.get_name()) {
            for rel in relation_type {
                if rel.comparable(func) {
                    return Some(rel.clone());
                }
            }
        }
        None
    }

    fn get_relationships(&self,
                         pos: usize,
                         compare: &lang::Predicate)
                         -> HashMap<Rc<String>, Vec<Rc<GroundedFunc>>> {
        let mut res = HashMap::new();
        let self_name = &**self.name;
        let (op, val) = compare.get_uval();
        let lock = self.relations.read().unwrap();
        for (rel_name, functions) in lock.iter() {
            for f in functions {
                if f.name_in_pos(self_name, &pos) {
                    match op {
                        None => res.entry(rel_name.clone()).or_insert(vec![]).push(f.clone()),
                        Some(lang::CompOperator::Equal) => {
                            if f.get_value().approx_eq_ulps(val.as_ref().unwrap(), 2) {
                                res.entry(rel_name.clone()).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(lang::CompOperator::More) => {
                            if *val.as_ref().unwrap() < f.get_value() {
                                res.entry(rel_name.clone()).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(lang::CompOperator::Less) => {
                            if *val.as_ref().unwrap() > f.get_value() {
                                res.entry(rel_name.clone()).or_insert(vec![]).push(f.clone())
                            }
                        }
                    }
                }
            }
        }
        res
    }

    /// Adds a new relationship for the entity.
    /// Returns 'true' in case the relationship didn't exist previously,
    /// 'false' otherwise. If it already existed, it's value is updated.
    fn add_relationship(&self,
                        agent: &Representation,
                        func: Rc<GroundedFunc>,
                        context: Option<&agent::ProofResult>)
                        -> bool {
        let mut lock = self.relations.write().unwrap();
        let name = func.get_name();
        let stmt_exists = lock.contains_key(&name);
        if stmt_exists {
            let funcs_type = lock.get_mut(&name).unwrap();
            let mut found_rel = false;
            for f in funcs_type.iter_mut() {
                if f.comparable(&*func) {
                    GroundedFunc::update(f.clone(), agent, func.clone(), context);
                    found_rel = true;
                    break;
                }
            }
            if !found_rel {
                funcs_type.push(func.clone());
                true
            } else {
                false
            }
        } else {
            lock.insert(name, vec![func.clone()]);
            true
        }
    }

    fn add_belief(&self, belief: Rc<LogSentence>, parent: Rc<String>) {
        let sent_exists = self.beliefs.read().unwrap().contains_key(&parent);
        if sent_exists {
            if let Some(ls) = self.beliefs.write().unwrap().get_mut(&parent) {
                ls.push(belief)
            }
        } else {
            self.beliefs.write().unwrap().insert(parent, vec![belief]);
        }
    }
}

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
pub struct Class {
    pub name: Rc<String>,
    classes: RwLock<HashMap<Rc<String>, Rc<GroundedClsMemb>>>,
    relations: RwLock<HashMap<Rc<String>, Vec<Rc<GroundedFunc>>>>,
    pub beliefs: RwLock<HashMap<Rc<String>, Vec<Rc<LogSentence>>>>,
    rules: RwLock<Vec<Rc<LogSentence>>>,
    kind: ClassKind,
    members: RwLock<Vec<ClassMember>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassKind {
    Relationship,
    Membership,
}

#[derive(Debug, Clone)]
enum ClassMember {
    Entity(Rc<GroundedClsMemb>),
    Class(Rc<GroundedClsMemb>),
    Func(Rc<GroundedFunc>),
}

impl ClassMember {
    fn unwrap_memb(&self) -> Rc<GroundedClsMemb> {
        match *self {
            ClassMember::Entity(ref obj) |
            ClassMember::Class(ref obj) => obj.clone(),
            _ => panic!(),
        }
    }

    fn unwrap_fn(&self) -> Rc<GroundedFunc> {
        match *self {
            ClassMember::Func(ref f) => f.clone(),
            _ => panic!(),
        }
    }
}

impl Class {
    fn new(name: Rc<String>, kind: ClassKind) -> Class {
        Class {
            name: name,
            classes: RwLock::new(HashMap::new()),
            relations: RwLock::new(HashMap::new()),
            beliefs: RwLock::new(HashMap::new()),
            rules: RwLock::new(Vec::new()),
            kind: kind,
            members: RwLock::new(Vec::new()),
        }
    }

    fn belongs_to_class(&self, class_name: Rc<String>) -> Option<Rc<GroundedClsMemb>> {
        let lock = self.classes.read().unwrap();
        match lock.get(&class_name) {
            Some(r) => Some(r.clone()),
            None => None,
        }
    }

    fn get_class_membership(&self, compare: &lang::FreeClsOwner) -> Vec<Rc<GroundedClsMemb>> {
        let lock = self.classes.read().unwrap();
        lock.values().filter(|x| compare.filter_grounded(&**x)).cloned().collect::<Vec<_>>()
    }

    /// Set a superclass of this class
    fn add_class_membership(&self,
                            agent: &Representation,
                            grounded: Rc<GroundedClsMemb>,
                            context: Option<&agent::ProofResult>)
                            -> bool {
        let mut lock = self.classes.write().unwrap();
        let name = grounded.get_parent();
        let stmt_exists = lock.contains_key(&name);
        if stmt_exists {
            let current = lock.get_mut(&name).unwrap();
            GroundedClsMemb::update(current.clone(), agent, grounded, context);
            false
        } else {
            lock.insert(name, grounded);
            true
        }
    }

    /// Add members of this class, being them other classes or entities.
    fn add_member(&self, member: ClassMember) {
        self.members.write().unwrap().push(member);
    }

    pub fn get_members(&self, comp: &lang::FreeClsMemb) -> Vec<Rc<GroundedClsMemb>> {
        let lock = self.members.read().unwrap();
        lock.iter()
            .map(|x| x.unwrap_memb())
            .filter(|m| comp.equal_to_grounded(m))
            .collect::<Vec<_>>()
    }

    fn has_relationship(&self, func: &GroundedFunc) -> Option<Rc<GroundedFunc>> {
        let lock = self.relations.read().unwrap();
        if let Some(relation_type) = lock.get(&func.get_name()) {
            for rel in relation_type {
                if rel.comparable(func) {
                    return Some(rel.clone());
                }
            }
        }
        None
    }

    fn get_relationships(&self,
                         pos: usize,
                         compare: &lang::Predicate)
                         -> HashMap<Rc<String>, Vec<Rc<GroundedFunc>>> {
        let mut res = HashMap::new();
        let self_name = &**self.name;
        let (op, val) = compare.get_uval();
        let lock = self.relations.read().unwrap();
        for (rel_name, functions) in lock.iter() {
            for f in functions {
                if f.name_in_pos(self_name, &pos) {
                    match op {
                        None => res.entry(rel_name.clone()).or_insert(vec![]).push(f.clone()),
                        Some(lang::CompOperator::Equal) => {
                            if f.get_value().approx_eq_ulps(val.as_ref().unwrap(), 2) {
                                res.entry(rel_name.clone()).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(lang::CompOperator::More) => {
                            if *val.as_ref().unwrap() < f.get_value() {
                                res.entry(rel_name.clone()).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(lang::CompOperator::Less) => {
                            if *val.as_ref().unwrap() > f.get_value() {
                                res.entry(rel_name.clone()).or_insert(vec![]).push(f.clone())
                            }
                        }
                    }
                }
            }
        }
        res
    }

    pub fn get_funcs(&self, func: &lang::FuncDecl) -> Vec<Rc<GroundedFunc>> {
        let mut res = vec![];
        let lock = self.members.read().unwrap();
        for curr_func in lock.iter() {
            let curr_func = curr_func.unwrap_fn();
            let mut process = true;
            for (i, arg) in func.get_args().enumerate() {
                if !arg.is_var() && (arg.get_name() != curr_func.get_args_names()[i]) {
                    process = false;
                    break;
                }
                if i == 0 {
                    match func.get_uval() {
                        (lang::CompOperator::Equal, val) => {
                            if !val.approx_eq_ulps(&curr_func.get_value(), 2) {
                                process = false;
                            }
                        }
                        (lang::CompOperator::More, val) => {
                            if val > curr_func.get_value() {
                                process = false;
                            }
                        }
                        (lang::CompOperator::Less, val) => {
                            if val < curr_func.get_value() {
                                process = false;
                            }
                        }
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
    fn add_relationship(&self,
                        agent: &Representation,
                        func: Rc<GroundedFunc>,
                        context: Option<&agent::ProofResult>)
                        -> bool {
        let mut lock = self.relations.write().unwrap();
        let name = func.get_name();
        let stmt_exists = lock.contains_key(&name);
        if stmt_exists {
            let funcs_type = lock.get_mut(&name).unwrap();
            let mut found_rel = false;
            for f in funcs_type.iter_mut() {
                if f.comparable(&*func) {
                    GroundedFunc::update(f.clone(), agent, func.clone(), context);
                    found_rel = true;
                    break;
                }
            }
            if !found_rel {
                funcs_type.push(func.clone());
                true
            } else {
                false
            }
        } else {
            lock.insert(name, vec![func.clone()]);
            true
        }
    }

    /// Add a grounded relationship of this kind of relationship
    fn add_grounded_relationship(&self, func: Rc<GroundedFunc>) {
        self.members.write().unwrap().push(ClassMember::Func(func));
    }

    fn add_belief(&self, belief: Rc<LogSentence>, parent: Rc<String>) {
        let sent_exists = self.beliefs.read().unwrap().contains_key(&parent);
        if sent_exists {
            if let Some(ls) = self.beliefs.write().unwrap().get_mut(&parent) {
                ls.push(belief)
            }
        } else {
            self.beliefs.write().unwrap().insert(parent, vec![belief]);
        }
    }

    fn add_rule(&self, rule: Rc<LogSentence>) {
        self.rules.write().unwrap().push(rule);
    }
}
