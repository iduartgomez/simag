//! Main knowledge-base logic module, in this module reside the different
//! types that transform and store the data for the individual agents and
//! serve as representations of the different objects and the relationships
//! between them.
//!
//! ## Main
//! **Representation**: Main type, stores all the representations and
//! relationships for a given agent in a concrete time.
//!
//! **Entity**: Represents a singular entity, which is the unique
//! member of it's own set.
//!
//! **Classes**: The sets in which the agent can classify objects.
//! Also stores the types of relations an object can have (class of relations).
//!
//! ## Support types, methods and functions
//! **Inference**: Encapsulates the whole inference process, where the inference
//! is operated to solving the query (including query parsing, data fetching
//! and unification).

use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::FromIterator;
use std::hash::{Hash, Hasher};
use std::cell::RefCell;
use std::sync::{RwLock, Mutex, Arc};
// use std::thread;
use std::rc::Rc;

use chrono::{UTC, DateTime};

use lang;
use lang::{ParseTree, ParseErrF, GroundedClsMemb, GroundedFunc, LogSentence};

type Date = DateTime<UTC>;

pub struct Representation<'a> {
    entities: RwLock<HashMap<&'a str, Box<Entity<'a>>>>,
    classes: RwLock<HashMap<&'a str, Box<Class<'a>>>>,
    log_sentences: Mutex<Vec<Box<LogSentence>>>,
}

/// This type is a container for internal agent's representations.
/// An agent can have any number of such representations at any moment,
/// all of which are contained in this object.
///
/// The class includes methods to encode and decode the representations
/// to/from data streams or idioms.
///
/// Attributes:
///     entities -> Unique members (entities) of their own set/class.
///     | Entities are denoted with a $ symbol followed by a name.
///     classes -> Sets of objects that share a common property.
impl<'a> Representation<'a> {
    pub fn new() -> Representation<'a> {
        Representation {
            entities: RwLock::new(HashMap::new()),
            classes: RwLock::new(HashMap::new()),
            log_sentences: Mutex::new(Vec::new()),
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
    pub fn tell(&'a self, source: String) -> Result<(), Vec<ParseErrF>> {
        let pres = lang::logic_parser(source, true);
        if pres.is_ok() {
            let mut pres: VecDeque<ParseTree> = pres.unwrap();
            let mut errors = Vec::new();
            for _ in 0..pres.len() {
                match pres.pop_front().unwrap() {
                    ParseTree::Assertion(assertions) => {
                        for assertion in assertions {
                            if assertion.is_class() {
                                for a in assertion.unwrap_cls().into_iter() {
                                    let a = Rc::new(a);
                                    self.up_membership(a)
                                }
                            } else {
                                let a = Rc::new(assertion.unwrap_fn().into_grounded());
                                self.up_relation(a)
                            }
                        }
                    }
                    ParseTree::IExpr(iexpr) => {
                        let r = self.add_logsentence(iexpr);
                        self.add_belief(r)
                    }
                    ParseTree::Rule(rule) => {
                        let r = self.add_logsentence(rule);
                        self.add_rule(r)
                    }
                    ParseTree::ParseErr(err) => errors.push(err),
                }
            }
            if errors.len() > 0 {
                Err(errors)
            } else {
                Ok(())
            }
        } else {
            Err(vec![pres.unwrap_err()])
        }
    }

    fn add_logsentence(&self, sent: LogSentence) -> &LogSentence {
        let mut lock = self.log_sentences.lock().unwrap();
        let ls: &mut Vec<Box<LogSentence>> =
            unsafe { &mut *(&mut *lock as *mut Vec<Box<LogSentence>>) };
        ls.push(Box::new(sent));
        ls.last().unwrap()
    }

    /// Asks the KB if some fact is true and returns the answer to the query.
    pub fn ask(&'a self, source: String, single_answer: bool) -> Answer {
        let pres = lang::logic_parser(source, false);
        if pres.is_ok() {
            let pres = QueryInput::ManyQueries(pres.unwrap());
            let mut inf = Inference::new(&self, pres, single_answer, false);
            {
                let mut inf_r = unsafe { &mut *(&mut inf as *mut Inference) as &mut Inference };
                inf_r.infer_facts();
            }
            inf.get_results()
        } else {
            Answer::ParseErr(pres.unwrap_err())
        }
    }

    fn ask_processed(&'a self,
                     source: QueryInput,
                     single_answer: bool,
                     ignore_current: bool)
                     -> Answer {
        let mut inf = Inference::new(&self, source, single_answer, ignore_current);
        {
            let mut inf_r = unsafe { &mut *(&mut inf as *mut Inference) as &mut Inference };
            inf_r.infer_facts();
        }
        inf.get_results()
    }

    pub fn up_membership(&self, assert: Rc<lang::GroundedClsMemb>) {
        let parent_exists = self.classes.read().unwrap().contains_key(assert.get_parent());
        if !parent_exists {
            let class = Box::new(Class::new(String::from(assert.get_parent()),
                                            ClassKind::Membership));
            let name = unsafe { &*(&class.name as *const String) as &str };
            self.classes.write().unwrap().insert(name, class);
        }
        match (&assert.get_name()).starts_with("$") {
            true => {
                let entity_exists = self.entities.read().unwrap().contains_key(assert.get_name());
                if entity_exists {
                    let lock = self.entities.read().unwrap();
                    let entity = lock.get(assert.get_name()).unwrap();
                    entity.add_class_membership(assert);
                } else {
                    let entity = Box::new(Entity::new(String::from(assert.get_name())));
                    entity.add_class_membership(assert);
                    let name = unsafe { &*(&entity.name as *const String) as &str };
                    self.entities.write().unwrap().insert(name, entity);
                }
            }
            false => {
                let class_exists = self.classes.read().unwrap().contains_key(assert.get_name());
                if class_exists {
                    let lock = self.classes.read().unwrap();
                    let class = lock.get(assert.get_name()).unwrap();
                    class.add_class_membership(assert);
                } else {
                    let class = Box::new(Class::new(String::from(assert.get_name()),
                                                    ClassKind::Membership));
                    class.add_class_membership(assert);
                    let name = unsafe { &*(&class.name as *const String) as &str };
                    self.classes.write().unwrap().insert(name, class);
                }
            }
        }
    }

    pub fn up_relation(&self, assert: Rc<lang::GroundedFunc>) {
        let process_arg = |a: &GroundedClsMemb| {
            let subject = a.get_name();
            match (&subject).starts_with("$") {
                true => {
                    let entity_exists = self.entities.read().unwrap().contains_key(subject);
                    if entity_exists {
                        let lock = self.entities.read().unwrap();
                        let entity = lock.get(subject).unwrap();
                        entity.add_relationship(assert.clone());
                    } else {
                        let entity = Box::new(Entity::new(String::from(subject)));
                        entity.add_relationship(assert.clone());
                        let name = unsafe { &*(&entity.name as *const String) as &str };
                        self.entities.write().unwrap().insert(name, entity);
                    }
                }
                false => {
                    let class_exists = self.classes.read().unwrap().contains_key(subject);
                    if class_exists {
                        let lock = self.classes.read().unwrap();
                        let class = lock.get(&subject).unwrap();
                        class.add_relationship(assert.clone());
                    } else {
                        let class = Box::new(Class::new(String::from(subject),
                                                        ClassKind::Membership));
                        class.add_relationship(assert.clone());
                        let name = unsafe { &*(&class.name as *const String) as &str };
                        self.classes.write().unwrap().insert(name, class);
                    }
                }
            }
        };
        let relation_exists = self.classes.read().unwrap().contains_key(assert.name.as_str());
        if !relation_exists {
            let relationship = Box::new(Class::new(assert.name.clone(), ClassKind::Relationship));
            let name = unsafe { &*(&relationship.name as *const String) as &str };
            self.classes.write().unwrap().insert(name, relationship);
        }
        process_arg(&assert.args[0]);
        process_arg(&assert.args[1]);
        if assert.third.is_some() {
            process_arg(assert.third.as_ref().unwrap())
        }
    }

    fn add_belief(&'a self, belief: &'a LogSentence) {
        let update = move |subject: &str, name: &String, is_entity: bool| {
            let name = unsafe { &*(&*name as *const String) };
            if is_entity {
                let entity_exists = self.entities.read().unwrap().contains_key(subject);
                if entity_exists {
                    let lock = self.entities.read().unwrap();
                    let entity = lock.get(subject).unwrap();
                    entity.add_belief(belief, name);
                } else {
                    let entity = Box::new(Entity::new(String::from(subject)));
                    entity.add_belief(belief, name);
                    let name = unsafe { &*(&entity.name as *const String) as &str };
                    self.entities
                        .write()
                        .unwrap()
                        .insert(name, entity);
                }
            } else {
                let class_exists = self.classes.read().unwrap().contains_key(subject);
                if class_exists {
                    let lock = self.classes.read().unwrap();
                    let class = lock.get(subject).unwrap();
                    class.add_belief(belief, name);
                } else {
                    let class = Box::new(Class::new(String::from(subject), ClassKind::Membership));
                    class.add_belief(belief, name);
                    let name = unsafe { &*(&class.name as *const String) as &str };
                    self.classes
                        .write()
                        .unwrap()
                        .insert(name, class);
                }
            }
        };

        for p in belief.get_all_predicates() {
            match p {
                &lang::Assert::ClassDecl(ref cls_decl) => {
                    let class_exists =
                        self.classes.read().unwrap().contains_key(cls_decl.get_name_as_str());
                    if class_exists {
                        self.classes
                            .read()
                            .unwrap()
                            .get(cls_decl.get_name_as_str())
                            .unwrap()
                            .add_belief(&belief, cls_decl.get_name_as_string_ref())
                    } else {
                        let class = Box::new(Class::new(String::from(cls_decl.get_name_as_str()),
                                                        ClassKind::Membership));
                        class.add_belief(&belief, cls_decl.get_name_as_str());
                        let name = unsafe { &*(&class.name as *const String) as &str };
                        self.classes.write().unwrap().insert(name, class);
                    }
                    for arg in cls_decl.get_args() {
                        if !arg.is_var() {
                            let subject = arg.get_name();
                            match subject.starts_with("$") {
                                true => update(subject, cls_decl.get_name_as_string_ref(), true),
                                false => update(subject, cls_decl.get_name_as_string_ref(), false),
                            }
                        }
                    }
                }
                &lang::Assert::FuncDecl(ref fn_decl) => {
                    let class_exists =
                        self.classes.read().unwrap().contains_key(fn_decl.get_name());
                    if class_exists {
                        self.classes
                            .read()
                            .unwrap()
                            .get(fn_decl.get_name())
                            .unwrap()
                            .add_belief(&belief, fn_decl.get_name_as_string_ref())
                    } else {
                        let class = Box::new(Class::new(String::from(fn_decl.get_name()),
                                                        ClassKind::Relationship));
                        class.add_belief(&belief, fn_decl.get_name());
                        let name = unsafe { &*(&class.name as *const String) as &str };
                        self.classes.write().unwrap().insert(name, class);
                    }
                    for arg in fn_decl.get_args() {
                        if !arg.is_var() {
                            let subject = arg.get_name();
                            match subject.starts_with("$") {
                                true => update(subject, fn_decl.get_name_as_string_ref(), true),
                                false => update(subject, fn_decl.get_name_as_string_ref(), false),
                            }
                        }
                    }
                }
            }
        }

        for query in belief.get_rhs_predicates() {
            match query {
                &lang::Assert::ClassDecl(ref cls_decl) => {
                    self.ask_processed(QueryInput::AskClassMember(cls_decl.clone()), true, true);
                }
                &lang::Assert::FuncDecl(ref func_decl) => {
                    self.ask_processed(QueryInput::AskRelationalFunc(func_decl.clone()),
                                       true,
                                       true);
                }
            }
        }
    }

    fn add_rule(&'a self, rule: &'a LogSentence) {
        let mut n = HashSet::new();
        let preds = rule.get_all_predicates();
        for p in preds {
            let name: &str = unsafe { &*(p.get_name_as_string_ref() as *const String) };
            n.insert(name);
            let class_exists = self.classes.read().unwrap().contains_key(name);
            if class_exists {
                let lock = self.classes.read().unwrap();
                let class = lock.get(name).unwrap();
                class.add_rule(&rule);
            } else {
                let nc = match p {
                    &lang::Assert::ClassDecl(_) => {
                        Box::new(Class::new(String::from(name), ClassKind::Membership))
                    }
                    &lang::Assert::FuncDecl(_) => {
                        Box::new(Class::new(String::from(name), ClassKind::Relationship))
                    }
                };
                nc.add_rule(&rule);
                let name = unsafe { &*(&nc.name as *const String) as &str };
                self.classes.write().unwrap().insert(name, nc);
            }
        }
        let mut obj_dic = self.entities_by_class(&n);
        let mut cls_dic = self.classes_by_class(&n);
        for (k, v) in cls_dic.drain() {
            obj_dic.insert(k, v);
        }
        for _e in obj_dic.keys() {
            panic!("not implemented: rule.call(self, e)")
        }
    }

    fn entities_by_class(&'a self,
                         classes: &HashSet<&'a str>)
                         -> HashMap<&'a str, HashSet<&'a &str>> {
        let mut dict = HashMap::new();
        let lock = self.entities.read().unwrap();
        for (name, e) in lock.iter() {
            // lifetime bound to 'a
            let e = unsafe { &*(&**e as *const Entity) };
            let s = e.belongs_to_classes_set(&classes);
            let s = unsafe { &*(&s as *const HashSet<&str>) };
            let t = e.has_relationships_set(&classes);
            let t = unsafe { &*(&t as *const HashSet<&str>) };
            let u = t.union(&s).collect::<HashSet<&&str>>();
            if u.len() > 0 {
                dict.insert(&**name, u);
            }
        }
        dict
    }

    fn classes_by_class(&'a self,
                        classes: &HashSet<&'a str>)
                        -> HashMap<&'a str, HashSet<&'a &str>> {
        let mut dict = HashMap::new();
        let lock = self.classes.read().unwrap();
        for (name, e) in lock.iter() {
            // lifetime bound to 'a
            let e = unsafe { &*(&**e as *const Class) };
            let s = e.belongs_to_classes_set(&classes);
            let s = unsafe { &*(&s as *const HashSet<&str>) };
            let t = e.has_relationships_set(&classes);
            let t = unsafe { &*(&t as *const HashSet<&str>) };
            let u = t.union(&s).collect::<HashSet<&&str>>();
            if u.len() > 0 {
                dict.insert(&**name, u);
            }
        }
        dict
    }

    pub fn get_entity_from_class(&self, class: &str, subject: &str) -> Option<&GroundedClsMemb> {
        match subject.starts_with("$") {
            true => {
                let entity_exists = self.entities.read().unwrap().contains_key(subject);
                if entity_exists {
                    let lock = self.entities.read().unwrap();
                    match lock.get(subject).unwrap().belongs_to_class(class) {
                        Some(r) => {
                            Some(unsafe { &*(r as *const GroundedClsMemb) as &GroundedClsMemb })
                        }
                        None => None,
                    }
                } else {
                    None
                }
            }
            false => {
                let class_exists = self.classes.read().unwrap().contains_key(subject);
                if class_exists {
                    let lock = self.classes.read().unwrap();
                    match lock.get(subject).unwrap().belongs_to_class(class) {
                        Some(r) => {
                            Some(unsafe { &*(r as *const GroundedClsMemb) as &GroundedClsMemb })
                        }
                        None => None,
                    }
                } else {
                    None
                }
            }
        }
    }

    fn class_membership(&self, pred: &GroundedClsMemb) -> Option<bool> {
        let subject = pred.get_name();
        match subject.starts_with("$") {
            true => {
                if let Some(entity) = self.entities.read().unwrap().get(subject) {
                    if let Some(current) = entity.belongs_to_class(pred.get_parent()) {
                        if current == pred {
                            return Some(true);
                        } else {
                            return Some(false);
                        }
                    }
                }
            }
            false => {
                if let Some(class) = self.classes.read().unwrap().get(subject) {
                    if let Some(current) = class.belongs_to_class(pred.get_parent()) {
                        if current == pred {
                            return Some(true);
                        } else {
                            return Some(false);
                        }
                    }
                }
            }
        }
        None
    }

    pub fn has_relationship(&self, pred: &GroundedFunc, subject: &str) -> Option<bool> {
        match subject.starts_with("$") {
            true => {
                if let Some(entity) = self.entities.read().unwrap().get(subject) {
                    if let Some(current) = entity.has_relationship(pred) {
                        if current == pred {
                            return Some(true);
                        } else {
                            return Some(false);
                        }
                    }
                }
            }
            false => {
                if let Some(class) = self.classes.read().unwrap().get(subject) {
                    if let Some(current) = class.has_relationship(pred) {
                        if current == pred {
                            return Some(true);
                        } else {
                            return Some(false);
                        }
                    }
                }
            }
        }
        None
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
/// For example, an object can belong to the set 'cold' with a degree of
/// 0.9 (in natural language then it would be 'very cold') or 0.1
/// (then it would be 'a bit cold', the subjective adjectives are defined
/// in the class itself).
#[derive(Debug)]
pub struct Entity<'a> {
    pub name: String,
    classes: RwLock<HashMap<&'a str, Rc<GroundedClsMemb>>>,
    relations: RwLock<HashMap<&'a str, Vec<Rc<GroundedFunc>>>>,
    beliefs: RwLock<HashMap<&'a str, Vec<&'a LogSentence>>>,
}

impl<'a> Entity<'a> {
    fn new(name: String) -> Entity<'a> {
        Entity {
            name: name,
            classes: RwLock::new(HashMap::new()),
            relations: RwLock::new(HashMap::new()),
            beliefs: RwLock::new(HashMap::new()),
        }
    }

    /// Returns the intersection between the provided list of classes and the
    /// set of classes the entity belongs to.
    fn belongs_to_classes<'b>(&'a self,
                              class_list: &Vec<&'b lang::ClassDecl>)
                              -> Option<HashMap<&'b str, &GroundedClsMemb>> {
        let mut matches = HashMap::new();
        let lock = self.classes.read().unwrap();
        let d = unsafe { &*(&*lock as *const HashMap<&str, Rc<GroundedClsMemb>>) };
        for decl in class_list {
            let key = decl.get_name_as_str();
            if let Some(cls) = d.get(key) {
                matches.insert(key, &**cls);
            }
        }
        if matches.len() > 0 {
            Some(matches)
        } else {
            None
        }
    }

    /// Returns the intersection between the provided set of class names and the
    /// set of classes the entity belongs to.
    fn belongs_to_classes_set<'b>(&'a self, class_set: &HashSet<&'b str>) -> HashSet<&'b str> {
        let mut matches = HashSet::new();
        for key in class_set {
            if let Some(_) = self.classes.read().unwrap().get(*key) {
                matches.insert(*key);
            }
        }
        matches
    }

    fn belongs_to_class(&self, class_name: &str) -> Option<&GroundedClsMemb> {
        let lock = self.classes.read().unwrap();
        match lock.get(class_name) {
            Some(ref r) => Some(unsafe { &*(&***r as *const GroundedClsMemb) as &GroundedClsMemb }),
            None => None,
        }
    }

    fn add_class_membership(&self, grounded: Rc<GroundedClsMemb>) {
        let mut lock = self.classes.write().unwrap();
        let mut d = unsafe { &mut *(&mut *lock as *mut HashMap<&str, Rc<GroundedClsMemb>>) };
        let name: &str = unsafe { &**(grounded.get_parent_as_string_ref() as *const String) };
        let stmt_exists = d.contains_key(name);
        match stmt_exists {
            true => {
                let current = d.get(&name).unwrap();
                current.update(grounded)
            }
            false => {
                d.insert(name, grounded);
            }
        }
    }

    /// Returns the intersection between the provided list of relational functions and the
    /// set of relational functions the entity has.
    fn has_relationships<'b>(&'a self,
                             func_list: &Vec<&'b lang::FuncDecl>,
                             var: Option<Rc<lang::Var>>)
                             -> Option<HashMap<&'b str, Vec<&GroundedFunc>>> {
        let mut matches: HashMap<&str, Vec<&GroundedFunc>> = HashMap::new();
        let lock = self.relations.read().unwrap();
        let d = unsafe { &*(&*lock as *const HashMap<&str, Vec<Rc<GroundedFunc>>>) };
        for decl in func_list {
            if let Some(relation_type) = d.get(decl.get_name()) {
                for rel in relation_type {
                    if rel.comparable_entity(decl, &self.name, var.clone()) {
                        let name = rel.get_name();
                        if matches.contains_key(name) {
                            let v = matches.get_mut(name).unwrap();
                            v.push(rel)
                        } else {
                            matches.insert(decl.get_name(), vec![rel]);
                        }
                    }
                }
            }
        }
        if matches.len() > 0 {
            Some(matches)
        } else {
            None
        }
    }

    fn has_relationships_set<'b>(&'a self, relations_set: &HashSet<&'b str>) -> HashSet<&'b str> {
        let mut matches: HashSet<&str> = HashSet::new();
        for key in relations_set {
            if let Some(_) = self.relations.read().unwrap().get(*key) {
                matches.insert(key);
            }
        }
        matches
    }

    fn has_relationship(&self, func: &GroundedFunc) -> Option<&GroundedFunc> {
        let lock = self.relations.read().unwrap();
        let d = unsafe { &*(&*lock as *const HashMap<&str, Vec<Rc<GroundedFunc>>>) };
        if let Some(relation_type) = d.get(func.get_name()) {
            for rel in relation_type {
                if rel.comparable(func) {
                    return Some(rel);
                }
            }
        }
        None
    }

    fn add_relationship(&self, func: Rc<GroundedFunc>) {
        let mut lock = self.relations.write().unwrap();
        let mut d = unsafe { &mut *(&mut *lock as *mut HashMap<&str, Vec<Rc<GroundedFunc>>>) };
        let name = func.get_name();
        let stmt_exists = d.contains_key(name);
        match stmt_exists {
            true => {
                let funcs_type = d.get_mut(name).unwrap();
                let mut found_rel = false;
                for f in funcs_type.iter_mut() {
                    if f.comparable(&*func) {
                        f.update(func.clone());
                        found_rel = true;
                        break;
                    }
                }
                if !found_rel {
                    funcs_type.push(func.clone())
                }
            }
            false => {
                let name: &str = unsafe { &**(func.get_parent_as_string_ref() as *const String) };
                d.insert(name, vec![func.clone()]);
            }
        }
    }

    fn add_belief(&self, belief: &'a LogSentence, parent: &'a str) {
        let sent_exists = self.beliefs.read().unwrap().contains_key(parent);
        match sent_exists {
            true => {
                if let Some(ls) = self.beliefs.write().unwrap().get_mut(parent) {
                    ls.push(belief)
                }
            }
            false => {
                self.beliefs.write().unwrap().insert(parent, vec![belief]);
            }
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
pub struct Class<'a> {
    pub name: String,
    classes: RwLock<HashMap<&'a str, Rc<GroundedClsMemb>>>,
    relations: RwLock<HashMap<&'a str, Vec<Rc<GroundedFunc>>>>,
    beliefs: RwLock<HashMap<&'a str, Vec<&'a LogSentence>>>,
    rules: RwLock<Vec<&'a LogSentence>>,
    _kind: ClassKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassKind {
    Relationship,
    Membership,
}

impl<'a> Class<'a> {
    fn new(name: String, kind: ClassKind) -> Class<'a> {
        Class {
            name: name,
            classes: RwLock::new(HashMap::new()),
            relations: RwLock::new(HashMap::new()),
            beliefs: RwLock::new(HashMap::new()),
            rules: RwLock::new(Vec::new()),
            _kind: kind,
        }
    }

    fn belongs_to_class(&self, class_name: &str) -> Option<&GroundedClsMemb> {
        let lock = self.classes.read().unwrap();
        match lock.get(class_name) {
            Some(ref r) => Some(unsafe { &*(&***r as *const GroundedClsMemb) as &GroundedClsMemb }),
            None => None,
        }
    }

    /// Returns the intersection between the provided set of class names and the
    /// set of superclasses of `self`.
    fn belongs_to_classes_set<'b>(&'a self, class_set: &HashSet<&'b str>) -> HashSet<&'b str> {
        let mut matches = HashSet::new();
        for key in class_set {
            if let Some(_) = self.classes.read().unwrap().get(*key) {
                matches.insert(*key);
            }
        }
        matches
    }

    /// Returns the intersection between the provided list of classes and the
    /// set of classes the entity belongs to.
    fn belongs_to_classes<'b>(&'a self,
                              class_list: &Vec<&'b lang::ClassDecl>)
                              -> Option<HashMap<&'b str, &GroundedClsMemb>> {
        let mut matches = HashMap::new();
        let lock = self.classes.read().unwrap();
        let d = unsafe { &*(&*lock as *const HashMap<&str, Rc<GroundedClsMemb>>) };
        for decl in class_list {
            let key = decl.get_name_as_str();
            if let Some(cls) = d.get(key) {
                matches.insert(key, &**cls);
            }
        }
        if matches.len() > 0 {
            Some(matches)
        } else {
            None
        }
    }

    fn add_class_membership(&self, grounded: Rc<GroundedClsMemb>) {
        let mut lock = self.classes.write().unwrap();
        let mut d = unsafe { &mut *(&mut *lock as *mut HashMap<&str, Rc<GroundedClsMemb>>) };
        let name: &str = unsafe { &**(grounded.get_parent_as_string_ref() as *const String) };
        let stmt_exists = d.contains_key(name);
        match stmt_exists {
            true => {
                let current = d.get_mut(&name).unwrap();
                current.update(grounded)
            }
            false => {
                d.insert(name, grounded);
            }
        }
    }

    /// Returns the intersection between the provided list of relational functions and the
    /// set of relational functions the entity has.
    fn has_relationships<'b>(&'a self,
                             func_list: &Vec<&'b lang::FuncDecl>,
                             var: Option<Rc<lang::Var>>)
                             -> Option<HashMap<&'b str, Vec<&GroundedFunc>>> {
        let mut matches: HashMap<&str, Vec<&GroundedFunc>> = HashMap::new();
        let lock = self.relations.read().unwrap();
        let d = unsafe { &*(&*lock as *const HashMap<&str, Vec<Rc<GroundedFunc>>>) };
        for decl in func_list {
            if let Some(relation_type) = d.get(decl.get_name()) {
                for rel in relation_type {
                    if rel.comparable_entity(decl, &self.name, var.clone()) {
                        let name = rel.get_name();
                        if matches.contains_key(name) {
                            let v = matches.get_mut(name).unwrap();
                            v.push(rel)
                        } else {
                            matches.insert(decl.get_name(), vec![rel]);
                        }
                    }
                }
            }
        }
        if matches.len() > 0 {
            Some(matches)
        } else {
            None
        }
    }

    fn has_relationships_set<'b>(&'a self, relations_set: &HashSet<&'b str>) -> HashSet<&'b str> {
        let mut matches: HashSet<&str> = HashSet::new();
        for key in relations_set {
            if let Some(_) = self.relations.read().unwrap().get(*key) {
                matches.insert(key);
            }
        }
        matches
    }

    fn has_relationship(&self, func: &GroundedFunc) -> Option<&GroundedFunc> {
        let lock = self.relations.read().unwrap();
        let d = unsafe { &*(&*lock as *const HashMap<&str, Vec<Rc<GroundedFunc>>>) };
        if let Some(relation_type) = d.get(func.get_name()) {
            for rel in relation_type {
                if rel.comparable(func) {
                    return Some(rel);
                }
            }
        }
        None
    }

    fn add_relationship(&self, func: Rc<GroundedFunc>) {
        let mut lock = self.relations.write().unwrap();
        let mut d = unsafe { &mut *(&mut *lock as *mut HashMap<&str, Vec<Rc<GroundedFunc>>>) };
        let name = func.get_name();
        let stmt_exists = d.contains_key(name);
        match stmt_exists {
            true => {
                let funcs_type = d.get_mut(name).unwrap();
                let mut found_rel = false;
                for f in funcs_type.iter_mut() {
                    if f.comparable(&*func) {
                        f.update(func.clone());
                        found_rel = true;
                        break;
                    }
                }
                if !found_rel {
                    funcs_type.push(func.clone())
                }
            }
            false => {
                let name: &str = unsafe { &**(func.get_parent_as_string_ref() as *const String) };
                d.insert(name, vec![func.clone()]);
            }
        }
    }

    fn add_belief(&self, belief: &'a LogSentence, parent: &'a str) {
        let sent_exists = self.beliefs.read().unwrap().contains_key(parent);
        match sent_exists {
            true => {
                if let Some(ls) = self.beliefs.write().unwrap().get_mut(parent) {
                    ls.push(belief)
                }
            }
            false => {
                self.beliefs.write().unwrap().insert(parent, vec![belief]);
            }
        }
    }

    fn add_rule(&self, rule: &'a LogSentence) {
        self.rules.write().unwrap().push(rule);
    }
}

struct Inference<'a> {
    query: QueryProcessed<'a>,
    kb: &'a Representation<'a>,
    ignore_current: bool,
    single: bool,
    nodes: RwLock<HashMap<&'a str, Vec<Arc<ProofNode<'a>>>>>,
    queue: RwLock<HashMap<Arc<ProofNode<'a>>, HashSet<Arc<ProofArgs<'a>>>>>,
    results: RwLock<HashMap<&'a str, HashMap<&'a str, Option<(bool, Option<Date>)>>>>,
    repeat: Mutex<Vec<(&'a LogSentence, Arc<ProofArgs<'a>>)>>,
    assignments: Mutex<Vec<VarAssignment<'a>>>,
}

#[derive(Debug, PartialEq)]
pub enum Answer<'a> {
    Single(Option<bool>),
    Multiple(HashMap<&'a str, HashMap<&'a str, Option<(bool, Option<Date>)>>>),
    QueryErr,
    ParseErr(ParseErrF),
}

impl<'a> Inference<'a> {
    fn new(agent: &'a Representation<'a>,
           query_input: QueryInput,
           single: bool,
           ignore_current: bool)
           -> Inference<'a> {
        let query = QueryProcessed::new().get_query(query_input);
        Inference {
            query: query.unwrap(),
            kb: agent,
            ignore_current: ignore_current,
            single: single,
            nodes: RwLock::new(HashMap::new()),
            queue: RwLock::new(HashMap::new()),
            results: RwLock::new(HashMap::new()),
            repeat: Mutex::new(vec![]),
            assignments: Mutex::new(vec![]),
        }
    }

    fn get_results(self) -> Answer<'a> {
        if self.single {
            let results = self.results.into_inner().unwrap();
            if results.len() == 0 {
                return Answer::Single(None);
            }
            for r0 in results.values() {
                for r1 in r0.values() {
                    if let &Some((false, _)) = r1 {
                        return Answer::Single(Some(false));
                    }
                    if let &None = r1 {
                        return Answer::Single(None);
                    }
                }
            }
            Answer::Single(Some(true))
        } else {
            Answer::Multiple(self.results.into_inner().unwrap())
        }
    }

    /// Inference function from first-order logic sentences.
    ///
    /// Gets a query from an 'ask' call, encapsulates the query subtitutions,
    /// processes it (including caching of partial results or tracking
    /// var substitution) and returns the answer to the query. If new
    /// knowledge is produced then it's passed to an other procedure for
    /// addition to the KB.
    fn infer_facts(&'a mut self) {
        fn query_cls<'a>(inf: &'a Inference<'a>, query: &'a str, actv_query: ActiveQuery<'a>) {
            let mut pass = InfTrial::new(inf, actv_query);
            pass.get_rules(vec![query]);
            {
                let mut lock = inf.nodes.write().unwrap();
                for nodes in lock.values_mut() {
                    nodes.sort_by(|a, b| a.proof.created.cmp(&b.proof.created));
                }
            }
            let ref mut pass_ref = unsafe { &*(&pass as *const InfTrial) as &InfTrial };
            // run the query, if there is no result and there is an update,
            // then loop again, else stop
            loop {
                pass_ref.unify(query, VecDeque::new(), HashSet::new());
                if !pass.updated.borrow().contains(&true) || !*pass.feedback.borrow() {
                    break;
                } else {
                    pass.updated = RefCell::new(vec![]);
                }
            }
        }

        for (obj, preds) in self.query.cls_queries_grounded.iter() {
            for pred in preds {
                let query: &str = pred.get_parent();
                let mut result = None;
                if !self.ignore_current {
                    result = self.kb.class_membership(pred);
                }
                if result.is_some() {
                    let mut lock = self.results.write().unwrap();
                    let mut answ = lock.entry(query).or_insert(HashMap::new());
                    answ.insert(*obj, Some((result.unwrap(), None)));
                } else {
                    {
                        let mut lock = self.results.write().unwrap();
                        let mut answ = lock.entry(query).or_insert(HashMap::new());
                        answ.insert(*obj, None);
                    }
                    // if no result was found from the kb directly
                    // make an inference from a grounded fact
                    let actv_query = ActiveQuery::Class(obj, query, pred);
                    query_cls(self, query, actv_query);
                }
            }
        }

        for pred in self.query.func_queries_grounded.iter() {
            let query: &str = pred.name.as_str();
            let mut result = None;
            for arg in pred.args.iter() {
                let obj = arg.get_name();
                if !self.ignore_current {
                    result = self.kb.has_relationship(pred, obj);
                }
                if result.is_some() {
                    let mut lock = self.results.write().unwrap();
                    let mut answ = lock.entry(query).or_insert(HashMap::new());
                    answ.insert(obj, Some((result.unwrap(), None)));
                } else {
                    {
                        let mut lock = self.results.write().unwrap();
                        let mut answ = lock.entry(query).or_insert(HashMap::new());
                        answ.insert(obj, None);
                    }
                    let actv_query = ActiveQuery::Func(obj, query, pred);
                    query_cls(self, query, actv_query);
                }
            }
        }

        // cls_queries_free: HashMap<&'a Rc<lang::Var>, Vec<&'a lang::FreeClsMemb>>,
        // cls_queries_grounded: HashMap<&'a str, Vec<&'a lang::GroundedClsMemb>>,
        // func_memb_query: Vec<&'a lang::GroundedClsMemb>,
        // func_queries_free: HashMap<&'a Rc<lang::Var>, Vec<&'a lang::FuncDecl>>,
    }
}

struct InfTrial<'a> {
    kb: &'a Representation<'a>,
    actv: ActiveQuery<'a>,
    updated: RefCell<Vec<bool>>,
    feedback: RefCell<bool>,
    nodes: &'a RwLock<HashMap<&'a str, Vec<Arc<ProofNode<'a>>>>>,
    queue: &'a RwLock<HashMap<Arc<ProofNode<'a>>, HashSet<Arc<ProofArgs<'a>>>>>,
    results: &'a RwLock<HashMap<&'a str, HashMap<&'a str, Option<(bool, Option<Date>)>>>>,
    repeat: &'a Mutex<Vec<(&'a LogSentence, Arc<ProofArgs<'a>>)>>,
    assignments: &'a Mutex<Vec<VarAssignment<'a>>>,
    valid: RefCell<Option<(Arc<ProofNode<'a>>, Arc<ProofArgs<'a>>)>>,
}

enum ActiveQuery<'a> {
    // `(obj_name, pred_name, fn/cls decl)`
    Class(&'a str, &'a str, &'a GroundedClsMemb),
    Func(&'a str, &'a str, &'a GroundedFunc),
}

impl<'a> ActiveQuery<'a> {
    #[inline]
    fn get_func(&self) -> &'a GroundedFunc {
        match self {
            &ActiveQuery::Func(_, _, ref gf) => gf,
            _ => panic!(),
        }
    }

    #[inline]
    fn get_cls(&self) -> &'a GroundedClsMemb {
        match self {
            &ActiveQuery::Class(_, _, ref gt) => gt,
            _ => panic!(),
        }
    }
}

type ProofArgs<'a> = Vec<(Rc<lang::Var>, &'a VarAssignment<'a>)>;

#[derive(Debug)]
pub struct ProofResult<'a> {
    pub result: Option<bool>,
    args: Arc<ProofArgs<'a>>,
    node: Arc<ProofNode<'a>>,
    pub grounded: Vec<(lang::Grounded, Date)>,
}

impl<'a> ProofResult<'a> {
    fn new(args: Arc<ProofArgs<'a>>, node: Arc<ProofNode<'a>>) -> ProofResult<'a> {
        ProofResult {
            result: None,
            args: args,
            node: node,
            grounded: vec![],
        }
    }
}

impl<'a> InfTrial<'a> {
    fn new(inf: &'a Inference<'a>, actv_query: ActiveQuery<'a>) -> InfTrial<'a> {
        InfTrial {
            kb: inf.kb,
            actv: actv_query,
            updated: RefCell::new(vec![]),
            feedback: RefCell::new(true),
            nodes: &inf.nodes,
            queue: &inf.queue,
            results: &inf.results,
            repeat: &inf.repeat,
            assignments: &inf.assignments,
            valid: RefCell::new(None),
        }
    }

    fn unify(&'a self, parent: &'a str, mut chk: VecDeque<&'a str>, mut done: HashSet<&'a str>) {
        *self.valid.borrow_mut() = None;
        // for each node in the subtitution tree unifify variables
        // and try every possible substitution until (if) a solution is found
        // the proofs are tried in order of addition to the KB
        if let Some(nodes) = self.nodes.read().unwrap().get(parent) {
            // the node for each rule is stored in an efficient sorted list
            // by rule creation datetime, from newest to oldest
            // as the newest rules take precedence
            for node in nodes.iter() {
                // recursively try unifying all possible argument with the
                // operating logic sentence:
                // get all the entities/classes from the kb that meet the proof requeriments
                let aents = self.entities_meet_sent_req(node.proof.var_req.as_ref().unwrap());
                let acls = self.classes_meet_sent_req(node.proof.var_req.as_ref().unwrap());
                let mut assignments;
                if aents.is_some() && acls.is_some() {
                    assignments = aents.unwrap();
                    let mut acls = acls.unwrap();
                    for (k, v) in acls.drain() {
                        assignments.insert(k, v);
                    }
                } else if acls.is_some() {
                    assignments = acls.unwrap();
                } else if aents.is_some() {
                    assignments = aents.unwrap();
                } else {
                    for e in node.antecedents.clone() {
                        if !done.contains(&e) && !chk.contains(&e) {
                            chk.push_back(e);
                        }
                    }
                    continue;
                }
                let mut n_a: HashMap<Rc<lang::Var>, Vec<&VarAssignment>> = HashMap::new();
                for (var, assigned) in assignments.drain() {
                    let mut n_av: Vec<&VarAssignment> = vec![];
                    for a in assigned {
                        let mut lock = self.assignments.lock().unwrap();
                        lock.push(a);
                        let a_ref = unsafe {
                            // extend lifetime of VarAssignment ref
                            &*(lock.last().unwrap() as *const VarAssignment) as &VarAssignment
                        };
                        n_av.push(a_ref);
                    }
                    n_a.insert(var, n_av);
                }
                let n_a_ref = unsafe {
                    // extend lifetime of VarAssignment ref
                    &*(&n_a as *const HashMap<Rc<lang::Var>, Vec<&VarAssignment>>)
                            as &HashMap<Rc<lang::Var>, Vec<&VarAssignment>>
                };
                // lazily iterate over all possible combinations of the substitutions
                let mapped = ArgsProduct::product(n_a_ref);
                for args in mapped {
                    let args_done = {
                        if let Some(queued) = self.queue.read().unwrap().get(node) {
                            queued.contains(&args)
                        } else {
                            false
                        }
                    };
                    if !args_done {
                        let mut n_args: HashMap<Rc<lang::Var>, &VarAssignment> =
                            HashMap::with_capacity(args.len());
                        let mut pargs = vec![];
                        for &(ref k, v) in args.iter() {
                            n_args.insert(k.clone(), v);
                            pargs.push(v.name)
                        }
                        let mut context = ProofResult::new(args.clone(), node.clone());
                        node.proof.solve(self.kb, Some(n_args), &mut context);
                        if context.result.is_some() {
                            self.updated.borrow_mut().push(true);
                            let mut lock = self.queue.write().unwrap();
                            lock.entry(node.clone()).or_insert(HashSet::new()).insert(args);
                            self.add_result(context);
                            break;
                        }
                    }
                }
                for e in node.antecedents.clone() {
                    if !done.contains(&e) && !chk.contains(&e) {
                        chk.push_back(e);
                    }
                }
                if let Some((ref node, ref assigned)) = *self.valid.borrow() {
                    // the result may be replaced in the KB by other proof which is less current,
                    // to ensure that the valid result stays in the KB after all proofs are done,
                    // repeat the valid one with proper arguments
                    self.repeat.lock().unwrap().push((node.proof, assigned.clone()))
                }
            }
        }
        println!("chk: {:?}; done: {:?}", chk, done);
        println!("node_keys: {:?}", self.nodes.read().unwrap().keys().collect::<Vec<_>>());
        if !*self.feedback.borrow() {
            return;
        } else if chk.len() > 0 {
            done.insert(parent);
            self.get_rules(Vec::from_iter(chk.iter().map(|x| *x)));
            let p = chk.pop_front().unwrap();
            self.unify(p, chk, done)
        }
    }

    fn add_result(&self, mut context: ProofResult<'a>) {
        // add category/function to the object dictionary
        // and to results dict if is the result for the query
        let (query_obj, query_pred, is_func) = match self.actv {
            ActiveQuery::Class(ref obj, ref query_pred, _) => (obj, query_pred, false),
            ActiveQuery::Func(ref obj, ref query_pred, _) => (obj, query_pred, true),
        };
        if let Some(false) = context.result {
            self.results
                .write()
                .unwrap()
                .entry(query_obj)
                .or_insert(HashMap::new())
                .insert(query_pred, Some((false, None)));
            return;
        }
        for subst in context.grounded.drain(..) {
            match subst {
                (lang::Grounded::Function(gf), date) => {
                    let gf: &GroundedFunc = &*gf;
                    if is_func {
                        let query_func = self.actv.get_func();
                        if query_func.comparable(&gf) {
                            let val;
                            if query_func == gf {
                                val = true;
                            } else {
                                val = false;
                            }
                            let mut d = self.results.write().unwrap();
                            let mut d = d.entry(query_pred).or_insert(HashMap::new());
                            if d.contains_key(query_obj) {
                                let cond_ok;
                                if let Some(&Some((_, Some(ref cdate)))) = d.get(query_obj) {
                                    if &date >= cdate {
                                        cond_ok = true;
                                    } else {
                                        cond_ok = false;
                                    }
                                } else {
                                    cond_ok = true;
                                }
                                if cond_ok {
                                    d.insert(query_obj, Some((val, Some(date))));
                                    *(self.valid.borrow_mut()) = Some((context.node.clone(),
                                                                       context.args.clone()));
                                }
                            } else {
                                d.insert(query_obj, Some((val, Some(date))));
                                *(self.valid.borrow_mut()) = Some((context.node.clone(),
                                                                   context.args.clone()));
                            }
                            *self.feedback.borrow_mut() = false;
                        }
                    }
                }
                (lang::Grounded::Terminal(gt), date) => {
                    if !is_func {
                        let query_cls = self.actv.get_cls();
                        if query_cls.comparable(&gt) {
                            let gt: &GroundedClsMemb = &*gt;
                            let val;
                            if query_cls == gt {
                                val = true;
                            } else {
                                val = false;
                            }
                            let mut d = self.results.write().unwrap();
                            let mut d = d.entry(query_pred).or_insert(HashMap::new());
                            if d.contains_key(query_obj) {
                                let cond_ok;
                                if let Some(&Some((_, Some(ref cdate)))) = d.get(query_obj) {
                                    if &date >= cdate {
                                        cond_ok = true;
                                    } else {
                                        cond_ok = false;
                                    }
                                } else {
                                    cond_ok = true;
                                }
                                if cond_ok {
                                    d.insert(query_obj, Some((val, Some(date))));
                                    *(self.valid.borrow_mut()) = Some((context.node.clone(),
                                                                       context.args.clone()));
                                }
                            } else {
                                d.insert(query_obj, Some((val, Some(date))));
                                *(self.valid.borrow_mut()) = Some((context.node.clone(),
                                                                   context.args.clone()));
                            }
                            *self.feedback.borrow_mut() = false;
                        }
                    }
                }
            }
        }
    }

    fn entities_meet_sent_req(&'a self,
                              req: &'a HashMap<Rc<lang::Var>, Vec<Rc<lang::Assert>>>)
                              -> Option<HashMap<Rc<lang::Var>, Vec<VarAssignment>>> {

        let mut results: HashMap<Rc<lang::Var>, Vec<VarAssignment>> = HashMap::new();
        for (var, asserts) in req.iter() {
            let mut class_list = Vec::new();
            let mut relations_list = Vec::new();
            for a in asserts {
                match **a {
                    lang::Assert::FuncDecl(ref f) => relations_list.push(f),
                    lang::Assert::ClassDecl(ref c) => class_list.push(c),
                }
            }
            let lock = self.kb.entities.read().unwrap();
            let d = unsafe { &*(&*lock as *const HashMap<&str, Box<Entity>>) };
            for (id, entity) in d.iter() {
                let mut gr_memb = HashMap::new();
                let mut gr_relations = HashMap::new();
                if class_list.len() > 0 {
                    if let Some(classes) = entity.belongs_to_classes(&class_list) {
                        if classes.len() == class_list.len() {
                            gr_memb = classes;
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                if relations_list.len() > 0 {
                    if let Some(relations) =
                           entity.has_relationships(&relations_list, Some(var.clone())) {
                        if relations.len() == relations_list.len() {
                            gr_relations = relations;
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                if results.contains_key(var) {
                    let v = results.get_mut(var).unwrap();
                    v.push(VarAssignment {
                        name: id,
                        classes: gr_memb,
                        funcs: gr_relations,
                    })
                } else {
                    results.insert(var.clone(),
                                   vec![VarAssignment {
                                            name: id,
                                            classes: gr_memb,
                                            funcs: gr_relations,
                                        }]);
                }
            }
            if !results.contains_key(var) {
                return None;
            }
        }
        Some(results)
    }

    fn classes_meet_sent_req(&'a self,
                             req: &'a HashMap<Rc<lang::Var>, Vec<Rc<lang::Assert>>>)
                             -> Option<HashMap<Rc<lang::Var>, Vec<VarAssignment>>> {

        let mut results: HashMap<Rc<lang::Var>, Vec<VarAssignment>> = HashMap::new();
        for (var, asserts) in req.iter() {
            let mut class_list = Vec::new();
            let mut relations_list = Vec::new();
            for a in asserts {
                match **a {
                    lang::Assert::FuncDecl(ref f) => relations_list.push(f),
                    lang::Assert::ClassDecl(ref c) => class_list.push(c),
                }
            }
            let lock = self.kb.classes.read().unwrap();
            let d = unsafe { &*(&*lock as *const HashMap<&str, Box<Class>>) };
            for (id, cls) in d.iter() {
                let mut gr_memb = HashMap::new();
                let mut gr_relations = HashMap::new();
                if class_list.len() > 0 {
                    if let Some(classes) = cls.belongs_to_classes(&class_list) {
                        if classes.len() == class_list.len() {
                            gr_memb = classes;
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                if relations_list.len() > 0 {
                    if let Some(relations) =
                           cls.has_relationships(&relations_list, Some(var.clone())) {
                        if relations.len() == relations_list.len() {
                            gr_relations = relations;
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                if results.contains_key(var) {
                    let v = results.get_mut(var).unwrap();
                    v.push(VarAssignment {
                        name: id,
                        classes: gr_memb,
                        funcs: gr_relations,
                    })
                } else {
                    results.insert(var.clone(),
                                   vec![VarAssignment {
                                            name: id,
                                            classes: gr_memb,
                                            funcs: gr_relations,
                                        }]);
                }
            }
            if !results.contains_key(var) {
                return None;
            }
        }
        Some(results)
    }

    fn get_rules(&self, cls_ls: Vec<&'a str>) {
        let mut rules = HashSet::new();
        for vrules in self.nodes.read().unwrap().values() {
            for r in vrules {
                rules.insert(r.proof);
            }
        }
        let rules = unsafe { &*(&rules as *const HashSet<&LogSentence>) };
        for cls in cls_ls {
            if let Some(stored) = self.kb.classes.read().unwrap().get(cls) {
                let lock = stored.beliefs.read().unwrap();
                let a: HashSet<&LogSentence> =
                    HashSet::from_iter(lock.get(cls).unwrap().iter().map(|x| *x));
                for sent in a.difference(&rules) {
                    let mut antecedents = vec![];
                    for p in sent.get_lhs_predicates() {
                        antecedents.push(p.get_name())
                    }
                    let node = Arc::new(ProofNode::new(sent, antecedents.clone()));
                    for pred in sent.get_rhs_predicates() {
                        let name = pred.get_name();
                        let mut lock = self.nodes.write().unwrap();
                        let mut ls = lock.entry(name).or_insert(vec![]);
                        if let None = ls.iter().map(|x| x.id).find(|x| *x == sent.get_id()) {
                            ls.push(node.clone());
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
struct ArgsProduct<'a> {
    indexes: RwLock<HashMap<Rc<lang::Var>, usize>>,
    input: &'a HashMap<Rc<lang::Var>, Vec<&'a VarAssignment<'a>>>,
    done: RwLock<HashSet<Arc<ProofArgs<'a>>>>,
}

type ArgsProductInput<'a> = HashMap<Rc<lang::Var>, Vec<&'a VarAssignment<'a>>>;

impl<'a> ArgsProduct<'a> {
    fn product(input: &'a ArgsProductInput<'a>) -> ArgsProduct<'a> {
        let mut indexes = HashMap::new();
        for (k, _) in input.iter() {
            indexes.insert(k.clone(), 0_usize);
        }
        ArgsProduct {
            indexes: RwLock::new(indexes),
            input: input,
            done: RwLock::new(HashSet::new()),
        }
    }
}

impl<'a> ::std::iter::Iterator for ArgsProduct<'a> {
    type Item = Arc<ProofArgs<'a>>;

    fn next(&mut self) -> Option<Arc<ProofArgs<'a>>> {
        loop {
            let mut row_0 = vec![];
            for (k1, v1) in self.input.iter() {
                let idx_1 = *(self.indexes.read().unwrap().get(k1).unwrap());
                let e = (k1.clone(), &*v1[idx_1]);
                row_0.push(e);
            }
            let res = Arc::new(row_0);
            let exists = self.done.read().unwrap().contains(&res);
            if !exists {
                self.done.write().unwrap().insert(res.clone());
                return Some(res);
            }
            if self.completed_iter() {
                return None;
            }
        }
    }
}

impl<'a> ArgsProduct<'a> {
    fn completed_iter(&self) -> bool {
        let mut max = 0;
        {
            let mut lock = self.indexes.write().unwrap();
            for (k, v) in lock.iter_mut() {
                let l = self.input.get(k).unwrap().len();
                if *v < (l - 1) {
                    *v += 1;
                    break;
                } else {
                    max += 1;
                }
            }
        }
        if max == self.indexes.read().unwrap().len() {
            true
        } else {
            false
        }
    }
}

#[derive(Debug)]
struct ProofNode<'a> {
    proof: &'a LogSentence,
    antecedents: Vec<&'a str>,
    id: &'a [u8],
}

impl<'a> ProofNode<'a> {
    fn new(proof: &'a LogSentence, antecedents: Vec<&'a str>) -> ProofNode<'a> {
        ProofNode {
            proof: proof,
            antecedents: antecedents,
            id: proof.get_id(),
        }
    }
}

impl<'a> ::std::cmp::PartialEq for ProofNode<'a> {
    fn eq(&self, other: &ProofNode) -> bool {
        self.id == other.id
    }
}

impl<'a> ::std::cmp::Eq for ProofNode<'a> {}

impl<'a> Hash for ProofNode<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct VarAssignment<'a> {
    pub name: &'a str,
    classes: HashMap<&'a str, &'a GroundedClsMemb>,
    funcs: HashMap<&'a str, Vec<&'a GroundedFunc>>,
}

impl<'a> VarAssignment<'a> {
    #[inline]
    pub fn get_class(&self, name: &str) -> Option<&&GroundedClsMemb> {
        self.classes.get(name)
    }

    #[inline]
    pub fn get_relationship(&self, func: &GroundedFunc) -> Option<&GroundedFunc> {
        if let Some(funcs) = self.funcs.get(func.get_name()) {
            for owned_f in funcs {
                if owned_f.comparable(func) {
                    return Some(owned_f);
                }
            }
        }
        None
    }
}

impl<'a> ::std::cmp::PartialEq for VarAssignment<'a> {
    fn eq(&self, other: &VarAssignment) -> bool {
        self.name == other.name
    }
}

impl<'a> ::std::cmp::Eq for VarAssignment<'a> {}

impl<'a> Hash for VarAssignment<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Debug)]
enum QueryInput {
    AskRelationalFunc(lang::FuncDecl),
    AskClassMember(lang::ClassDecl),
    ManyQueries(VecDeque<ParseTree>),
}

#[derive(Debug)]
struct QueryProcessed<'a> {
    cls_queries_free: HashMap<Rc<lang::Var>, Vec<&'a lang::FreeClsMemb>>,
    cls_queries_grounded: HashMap<&'a str, Vec<&'a lang::GroundedClsMemb>>,
    cls_memb_query: Vec<&'a lang::GroundedClsMemb>,
    func_queries_free: HashMap<Rc<lang::Var>, Vec<Rc<lang::FuncDecl>>>,
    func_queries_grounded: Vec<lang::GroundedFunc>,
    func_memb_query: Vec<&'a lang::GroundedClsMemb>,
    vars: Vec<Rc<lang::Var>>,
    cls: Vec<Rc<lang::ClassDecl>>,
    func: Vec<Rc<lang::FuncDecl>>,
}

impl<'a> QueryProcessed<'a> {
    fn new() -> QueryProcessed<'a> {
        QueryProcessed {
            cls_queries_free: HashMap::new(),
            cls_queries_grounded: HashMap::new(),
            cls_memb_query: vec![],
            func_queries_free: HashMap::new(),
            func_queries_grounded: vec![],
            func_memb_query: vec![],
            vars: vec![],
            cls: vec![],
            func: vec![],
        }
    }

    fn get_query(mut self, prequery: QueryInput) -> Result<QueryProcessed<'a>, ()> {
        fn assert_memb(query: &mut QueryProcessed, cdecl: Rc<lang::ClassDecl>) -> Result<(), ()> {
            let cdecl = unsafe { &*(&cdecl as *const Rc<lang::ClassDecl>) as &Rc<lang::ClassDecl> };
            match cdecl.get_parent() {
                &lang::Terminal::GroundedTerm(_) => {
                    for a in cdecl.get_args() {
                        match a {
                            &lang::Predicate::FreeClsMemb(ref t) => {
                                query.push_to_clsquery_free(t.get_var(), t);
                            }
                            &lang::Predicate::GroundedClsMemb(ref t) => {
                                query.push_to_clsquery_grounded(t.get_name(), t);
                            }
                        }
                    }
                }
                &lang::Terminal::FreeTerm(_) => {
                    for a in cdecl.get_args() {
                        match a {
                            &lang::Predicate::FreeClsMemb(_) => {
                                return Err(());
                            }
                            &lang::Predicate::GroundedClsMemb(ref t) => {
                                query.ask_class_memb(t);
                            }
                        }
                    }
                }
                _ => return Err(()),
            }
            Ok(())
        }

        fn assert_rel(query: &mut QueryProcessed, fdecl: Rc<lang::FuncDecl>) -> Result<(), ()> {
            let fdecl = unsafe { &*(&fdecl as *const Rc<lang::FuncDecl>) as &Rc<lang::FuncDecl> };
            match fdecl.get_parent() {
                &lang::Terminal::GroundedTerm(_) => {
                    match fdecl.is_grounded() {
                        true => {
                            query.push_to_fnquery_grounded(fdecl.as_ref().clone().into_grounded());
                        }
                        false => {
                            for a in fdecl.get_args() {
                                if let &lang::Predicate::FreeClsMemb(ref t) = a {
                                    query.push_to_fnquery_free(t.get_var(), fdecl.clone());
                                }
                            }
                        }
                    }
                }
                &lang::Terminal::FreeTerm(_) => {
                    for a in fdecl.get_args() {
                        match a {
                            &lang::Predicate::FreeClsMemb(_) => {
                                return Err(());
                            }
                            &lang::Predicate::GroundedClsMemb(ref t) => {
                                query.ask_relationships(t);
                            }
                        }
                    }
                }
                _ => return Err(()),
            }
            Ok(())
        }

        match prequery {
            QueryInput::AskClassMember(cdecl) => {
                self.cls.push(Rc::new(cdecl));
                let cdecl = self.cls.last().unwrap().clone();
                assert_memb(&mut self, cdecl)?;
            }
            QueryInput::AskRelationalFunc(fdecl) => {
                self.func.push(Rc::new(fdecl));
                let fdecl = self.func.last().unwrap().clone();
                assert_rel(&mut self, fdecl)?;
            }
            QueryInput::ManyQueries(trees) => {
                for parsetree in trees {
                    match parsetree {
                        lang::ParseTree::Assertion(assertions) => {
                            for a in assertions {
                                if let Err(()) = match a {
                                    lang::Assert::ClassDecl(cdecl) => {
                                        self.cls.push(Rc::new(cdecl));
                                        let cdecl = self.cls.last().unwrap().clone();
                                        assert_memb(&mut self, cdecl)
                                    }
                                    lang::Assert::FuncDecl(fdecl) => {
                                        self.func.push(Rc::new(fdecl));
                                        let fdecl = self.func.last().unwrap().clone();
                                        assert_rel(&mut self, fdecl)
                                    }
                                } {
                                    return Err(());
                                }
                            }
                        }
                        lang::ParseTree::IExpr(expr) => {
                            let (mut vars, preds) = expr.extract_all_predicates();
                            self.vars.append(&mut vars);
                            for a in preds {
                                if let Err(()) = match *a {
                                    lang::Assert::ClassDecl(ref cdecl) => {
                                        self.cls.push(Rc::new(cdecl.clone()));
                                        let cdecl = self.cls.last().unwrap().clone();
                                        assert_memb(&mut self, cdecl)
                                    }
                                    lang::Assert::FuncDecl(ref fdecl) => {
                                        self.func.push(Rc::new(fdecl.clone()));
                                        let fdecl = self.func.last().unwrap().clone();
                                        assert_rel(&mut self, fdecl)
                                    }
                                } {
                                    return Err(());
                                }
                            }
                        }
                        _ => return Err(()),
                    }
                }
            }
        }
        Ok(self)
    }

    #[inline]
    fn push_to_clsquery_grounded(&mut self, term: &'a str, cls: &'a lang::GroundedClsMemb) {
        self.cls_queries_grounded.entry(term).or_insert(vec![]).push(cls);
    }

    #[inline]
    fn push_to_clsquery_free(&mut self, term: Rc<lang::Var>, cls: &'a lang::FreeClsMemb) {
        self.cls_queries_free.entry(term).or_insert(vec![]).push(cls);
    }

    #[inline]
    fn push_to_fnquery_grounded(&mut self, func: lang::GroundedFunc) {
        self.func_queries_grounded.push(func)
    }

    #[inline]
    fn push_to_fnquery_free(&mut self, term: Rc<lang::Var>, func: Rc<lang::FuncDecl>) {
        self.func_queries_free.entry(term).or_insert(vec![]).push(func);
    }

    #[inline]
    fn ask_class_memb(&mut self, term: &'a lang::GroundedClsMemb) {
        self.cls_memb_query.push(term);
    }

    #[inline]
    fn ask_relationships(&mut self, term: &'a lang::GroundedClsMemb) {
        self.func_memb_query.push(term);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ask_pred() {
        let test_01 = String::from("
            ( professor[$Lucy,u=1] )
        ");
        let q01_01 = "(professor[$Lucy,u=1] && person[$Lucy,u=1])".to_string();
        let q01_02 = "(professor[$Lucy,u=1])".to_string();
        let rep = Representation::new();
        rep.tell(test_01).unwrap();
        assert_eq!(rep.ask(q01_01, true), Answer::Single(None));
        assert_eq!(rep.ask(q01_02, true), Answer::Single(Some(true)));

        let test_02 = String::from("
            ( professor[$Lucy,u=1] )
            ( dean[$John,u=1] )
            ( ( let x ) ( dean[x,u=1] |> professor[x,u=1] ) )
            ( ( let x ) ( professor[x,u=1] |> person[x,u=1] ) )
        ");
        let q02_01 = "(professor[$Lucy,u>0] && person[$Lucy,u<1])".to_string();
        let q02_02 = "(person[$John,u=1])".to_string();
        let rep = Representation::new();
        rep.tell(test_02).unwrap();
        assert_eq!(rep.ask(q02_01, true), Answer::Single(Some(false)));
        assert_eq!(rep.ask(q02_02, true), Answer::Single(Some(true)));

        let test_03 = String::from("
            ( fn::owns[$M1,u=1;$Nono] )
            ( missile[$M1,u=1] )
            ( american[$West,u=1] )
            ( fn::enemy[$Nono,u=1;$America] )
            (( let x, y, z )
             (( american[x,u=1] && weapon[y,u=1] && fn::sells[y,u=1;x;z] && hostile[z,u=1]  )
                 |> criminal[x,u=1] ))
            (( let x )
             (( fn::owns[x,u=1;$Nono] && missile[x,u=1] ) |> fn::sells[x,u=1;$West;$Nono] ))
            (( let x ) ( missile[x,u=1] |> weapon[x,u=1] ) )
            (( let x ) ( fn::enemy[x,u=1;$America] |> hostile[x,u=1] ) )
        ");
        let q03_01 = "(criminal[$West,u=1])".to_string();
        let rep = Representation::new();
        rep.tell(test_03).unwrap();
        assert_eq!(rep.ask(q03_01, true), Answer::Single(Some(true)));

        let _test_04 = String::from("
            (( let x, y, t1:time, t2:time=\"*now\" )
             (( dog[x,u=1] && meat[y,u=1] && fn::eat(t1=time)[y,u=1;x] && fn::time_calc(t1<t2) )
              |> fat(time=t2)[x,u=1] ))
            ( dog[$Pancho,u=1] )
            ( meat[$M1,u=1] )
            ( fn::eat(time=\"2015.07.05.10.25\")[$M1,u=1;$Pancho] )
        ");
        let _q04_01 = "(fat(t='*now')[$Pancho,u=1])".to_string();

        let _test_05 = String::from("
            (( let x, y, t1:time, t2:time=\"2016.01.01\" )
             (( (dog[x,u=1] && meat[y,u=1] && fn::eat(t1=time)[y,u=1;x]) && fn::time_calc(t1<t2) )
              |> fat(time=t2)[x,u=1] ))
            ( dog[$Pancho,u=1] )
            ( meat[$M1,u=1] )
            ( fn::eat(time=\"2015.07.05.10.25\")[$M1,u=1;$Pancho] )
        ");
        let _q05_01 = "(fat(t='*now')[$Pancho,u=1])".to_string();

        let _test_06 = String::from("
            # query for all 'professor'
        	( \
                                     professor[$Lucy,u=1] )
        	( dean[$John,u=1] )
        	\
                                     ((let x) (dean[x,u=1] |> professor[x,u=1]))
        ");
        let _q06_01 = "((let x) (professor[x,u=1]))".to_string();

        let _test_07 = String::from("
            # query for all classes '$Lucy' is member of
        	\
                                     (professor[$Lucy,u=1])
        	((let x) (professor[x,u=1] \
                                     |> person[x,u=1]))
        	(ugly[$Lucy,u=0.2])
        ");
        let _q07_01 = "((let x) (x[$Lucy,u>0.5]))".to_string();
    }

    #[test]
    fn ask_func() {
        let test_01 = String::from("
            ( professor[$Lucy,u=1] )
            ( dean[$John,u=1] )
            ( fn::criticize[$John,u=1;$Lucy] )
        ");
        let q01_01 = "(fn::criticize[$John,u=1;$Lucy])".to_string();
        let rep = Representation::new();
        rep.tell(test_01).unwrap();
        assert_eq!(rep.ask(q01_01, true), Answer::Single(Some(true)));

        let test_02 = String::from("
            ( professor[$Lucy,u=1] )
            ( dean[$John,u=1] )
            ( fn::criticize[$John,u=1;$Lucy] )
            ( (let x) ( dean[x,u=1] |> professor[x,u=1] ) )
            ( (let x) ( professor[x,u=1] |> person[x,u=1] ) )
            ( (let x, y)
              (( person[x,u=1] && person[y,u=1] && dean[y,u=1] && fn::criticize[y,u=1;x] )
                 |> fn::friend[x,u=0;y] ))
        ");
        let q02_01 = "(fn::friend[$Lucy,u=0;$John])".to_string();
        let rep = Representation::new();
        rep.tell(test_02).unwrap();
        assert_eq!(rep.ask(q02_01, true), Answer::Single(Some(true)));

        let test_03 = String::from("
            ( fn::owns[$M1,u=1;$Nono] )
            ( missile[$M1,u=1] )
            ( american[$West,u=1] )
            ( fn::enemy[$Nono,u=1;$America] )
            (( let x,y,z )
                (( american[x,u=1] && weapon[y,u=1] && fn::sells[y,u>0.5;x;z] && hostile[z,u=1] )
                 |> criminal[x,u=1] ))
            (( let x ) ( ( fn::owns[x,u=1;$Nono] && missile[x,u=1] )
                        |> fn::sells[x,u=1;$West;$Nono] ) )
            (( let x ) ( missile[x,u=1] |> weapon[x,u=1] ) )
            (( let x ) ( fn::enemy[x,u=1;$America] |> hostile[x,u=1] ) )
        ");
        let q03_01 = "(fn::sells[$M1,u=1;$West;$Nono])".to_string();
        let rep = Representation::new();
        rep.tell(test_03).unwrap();
        assert_eq!(rep.ask(q03_01, true), Answer::Single(Some(true)));

        let test_04 = String::from("
            ( animal[cow,u=1] )
            ( female[cow,u=1] )
            ( (animal[cow,u=1] && female[cow,u=1]) |> fn::produce[milk,u=1;cow] )
        ");
        let q04_01 = "(fn::produce[milk,u=1;cow])".to_string();
        let rep = Representation::new();
        rep.tell(test_04).unwrap();
        assert_eq!(rep.ask(q04_01, true), Answer::Single(Some(true)));

        let _test_05 = String::from("
            (( let x, y, t1: time=\"2015.01.01\", t2: time=\"2015.02.01\" )
             ( ( dog[x,u=1] && meat[y,u=1] && fat(time=t2)[x,u=1] && fn::time_calc(t1<t2) )
               |> fn::eat(time=t1)[y,u=1;x]
             )
            )
            ( dog[$Pancho,u=1] )
            ( meat[$M1,u=1] )
            ( fat[$Pancho,u=1] )
        ");
        let _q05_01 = "(fn::eat[$M1,u=1;$Pancho])".to_string();

        let _test_06 = String::from("
        	(( let x, y, t1: time=\"2015.01.01\", t2: time )
             ( ( dog[x,u=1] && meat[y,u=1] && fat(time=t2)[x,u=1] && fn::time_calc(t1<t2) )
               |> fn::eat(time=t1)[y,u=1;x]
             )
            )
            ( dog[$Pancho,u=1] )
            ( meat[$M1,u=1] )
            ( fat(time=\"2015.12.01\")[$Pancho,u=1] )
        ");
        let _q06_01 = "(fn::eat[$M1,u=1;$Pancho])".to_string();

        let _test_07 = String::from("
            # retrieve all objs which fit to a criteria
            (cow[$Lucy,u=1])
            (goat[$Vicky,u=1])
            ((let x) ((cow[x,u=1] || goat[x,u=1]) |> (female[x,u=1] && animal[x,u=1])))
            ((let x) ((female[x,u>0] && animal[x,u>0]) |> fn::produce[milk,u=1;x]))
        ");
        let _q07_01 = "((let x) (fn::produce[milk,u>0;x]))".to_string();

        let _test_08 = String::from("
            # retrieve all relations between objects
            (fn::loves[$Vicky,u=1;$Lucy])
        ");
        let _q08_01 = "((let x) (fn::x[$Vicky,u>0;$Lucy]))".to_string();
    }
}
