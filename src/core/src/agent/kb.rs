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
//! Also stores the types of relations an object can have.
//!
//! ## Support types, methods and functions
//! **Inference**: Encapsulates the whole inference process, from making
//! a temporal substitution representation where the inference is operated to
//! solving the query (including query parsing, data fetching and unification).

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{RwLock, Mutex};

use lang;
use lang::{ParseTree, ParseErrF, GroundedTerm, GroundedFunc, LogSentence};

pub struct Representation<'a> {
    entities: RwLock<HashMap<&'a str, Box<Entity<'a>>>>,
    classes: RwLock<HashMap<&'a str, Box<Class<'a>>>>,
    _log_sentences: Mutex<Vec<Box<LogSentence>>>,
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
            _log_sentences: Mutex::new(Vec::new()),
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
                                    self.up_membership(a)
                                }
                            } else {
                                self.up_relation(assertion.unwrap_fn().into_grounded())
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
        let mut lock = self._log_sentences.lock().unwrap();
        let ls: &mut Vec<Box<LogSentence>> =
            unsafe { &mut *(&mut *lock as *mut Vec<Box<LogSentence>>) };
        ls.push(Box::new(sent));
        ls.get(ls.len() - 1).unwrap()
    }

    /// Asks the KB if some fact is true and returns the answer to the query.
    pub fn ask(&'a self, source: String, single_answer: bool) -> Answer {
        let pres = lang::logic_parser(source, false);
        if pres.is_ok() {
            let pres = QueryInput::ManyQueries(pres.unwrap());
            Inference::new(&self, pres, single_answer)
        } else {
            Answer::ParseErr(pres.unwrap_err())
        }
    }

    pub fn up_membership(&self, assert: lang::GroundedTerm) {
        let parent_exists = self.classes.read().unwrap().contains_key(assert.get_parent());
        if !parent_exists {
            let class = Box::new(Class::new(String::from(assert.get_parent()), ClassKind::Membership));
            let name = unsafe {&*(&class.name as *const String) as &str };
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
                    let name = unsafe {&*(&entity.name as *const String) as &str };
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
                    let class = Box::new(Class::new(String::from(assert.get_name()), ClassKind::Membership));
                    class.add_class_membership(assert);
                    let name = unsafe {&*(&class.name as *const String) as &str };
                    self.classes.write().unwrap().insert(name, class);
                }
            }
        }
    }

    pub fn up_relation(&self, assert: lang::GroundedFunc) {
        let process_arg = |a: &GroundedTerm| {
            let subject = a.get_name();
            match (&subject).starts_with("$") {
                true => {
                    let entity_exists = self.entities.read().unwrap().contains_key(subject);
                    if entity_exists {
                        let lock = self.entities.read().unwrap();
                        let entity = lock.get(subject).unwrap();
                        entity.add_relationship(&assert);
                    } else {
                        let entity = Box::new(Entity::new(String::from(subject)));
                        entity.add_relationship(&assert);
                        let name = unsafe {&*(&entity.name as *const String) as &str };
                        self.entities.write().unwrap().insert(name, entity);
                    }
                }
                false => {
                    let class_exists = self.classes.read().unwrap().contains_key(subject);
                    if class_exists {
                        let lock = self.classes.read().unwrap();
                        let class = lock.get(&subject).unwrap();
                        class.add_relationship(&assert);
                    } else {
                        let class = Box::new(Class::new(String::from(subject), ClassKind::Membership));
                        class.add_relationship(&assert);
                        let name = unsafe {&*(&class.name as *const String) as &str };
                        self.classes.write().unwrap().insert(name, class);
                    }
                }
            }
        };
        let relation_exists = self.classes.read().unwrap().contains_key(assert.name.as_str());
        if !relation_exists {
            let relationship = Box::new(Class::new(assert.name.clone(), ClassKind::Relationship));
            let name = unsafe {&*(&relationship.name as *const String) as &str };
            self.classes.write().unwrap().insert(name, relationship);
        }
        process_arg(&assert.args[0]);
        process_arg(&assert.args[1]);
        if assert.third.is_some() {
            process_arg(assert.third.as_ref().unwrap())
        }
    }

    fn add_belief(&self, belief: &'a LogSentence) {
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
                    let name = unsafe {&*(&entity.name as *const String) as &str };
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
                    let name = unsafe {&*(&class.name as *const String) as &str };
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
                    let class_exists = self.classes.read().unwrap().contains_key(cls_decl.get_name_as_str());
                    if class_exists {
                        self.classes
                            .read()
                            .unwrap()
                            .get(cls_decl.get_name_as_str())
                            .unwrap()
                            .add_belief(&belief, cls_decl.get_name_as_string_ref())
                    } else {
                        let class = Box::new(Class::new(String::from(cls_decl.get_name_as_str()), ClassKind::Membership));
                        class.add_belief(&belief, cls_decl.get_name_as_str());
                        let name = unsafe {&*(&class.name as *const String) as &str };
                        self.classes.write().unwrap().insert(name, class);
                    }
                    for arg in cls_decl.get_args() {
                        if arg.is_not_var() {
                            let subject = arg.get_name();
                            match subject.starts_with("$") {
                                true => update(subject, cls_decl.get_name_as_string_ref(), true),
                                false => update(subject, cls_decl.get_name_as_string_ref(), false),
                            }
                        }
                    }
                }
                &lang::Assert::FuncDecl(ref fn_decl) => {
                    let class_exists = self.classes.read().unwrap().contains_key(fn_decl.get_name());
                    if class_exists {
                        self.classes
                            .read()
                            .unwrap()
                            .get(fn_decl.get_name())
                            .unwrap()
                            .add_belief(&belief, fn_decl.get_name_as_string_ref())
                    } else {
                        let class = Box::new(Class::new(String::from(fn_decl.get_name()), ClassKind::Relationship));
                        class.add_belief(&belief, fn_decl.get_name());
                        let name = unsafe {&*(&class.name as *const String) as &str };
                        self.classes.write().unwrap().insert(name, class);
                    }
                    for arg in fn_decl.get_args() {
                        if arg.is_not_var() {
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
        panic!("has to run a query for every lhs pred in the sentence")
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
                let name = unsafe {&*(&nc.name as *const String) as &str };
                self.classes.write().unwrap().insert(name, nc);
            }
        }
        let mut obj_dic = self.entities_by_class(&n);
        let mut cls_dic = self.classes_by_class(&n);
        for (k, v) in cls_dic.drain() {
            obj_dic.insert(k, v);
        }
        for _ in obj_dic.keys() {
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

    pub fn get_entity_from_class(&self, class: &str, subject: &str) -> Option<&GroundedTerm> {
        match subject.starts_with("$") {
            true => {
                let entity_exists = self.entities.read().unwrap().contains_key(subject);
                if entity_exists {
                    let lock = self.entities.read().unwrap();
                    match lock.get(subject).unwrap().belongs_to_class(class) {
                        Some(r) => Some(unsafe { &*(r as *const GroundedTerm) as &GroundedTerm }),
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
                        Some(r) => Some(unsafe { &*(r as *const GroundedTerm) as &GroundedTerm }),
                        None => None,
                    }
                } else {
                    None
                }
            }
        }
    }

    fn class_membership(&self, pred: &GroundedTerm) -> Option<bool> {
        let subject = pred.get_name();
        match subject.starts_with("$") {
            true => {
                if let Some(entity) = self.entities.read().unwrap().get(subject) {
                    if let Some(current) = entity.belongs_to_class(pred.get_name()) {
                        if current == pred {
                            return Some(true)
                        } else {
                            return Some(false)
                        }
                    }
                }
            }
            false => {
                if let Some(class) = self.classes.read().unwrap().get(subject) {
                    if let Some(current) = class.belongs_to_class(pred.get_name()) {
                        if current == pred {
                            return Some(true)
                        } else {
                            return Some(false)
                        }
                    }
                }
            }
        }
        None
    }

    fn has_relationship(&self, pred: &GroundedFunc, subject: &str) -> Option<bool> {
        match subject.starts_with("$") {
            true => {
                if let Some(entity) = self.entities.read().unwrap().get(subject) {
                    if let Some(current) = entity.has_relationship(pred) {
                        if current == pred {
                            return Some(true)
                        } else {
                            return Some(false)
                        }
                    }
                }
            }
            false => {
                if let Some(class) = self.classes.read().unwrap().get(subject) {
                    if let Some(current) = class.has_relationship(pred) {
                        if current == pred {
                            return Some(true)
                        } else {
                            return Some(false)
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
pub struct Entity<'a> {
    pub name: String,
    classes: RwLock<HashMap<&'a str, Box<GroundedTerm>>>,
    relations: RwLock<HashMap<&'a str, Vec<Box<GroundedFunc>>>>,
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
                              -> Option<HashMap<&'b str, &GroundedTerm>> {
        let mut matches = HashMap::new();
        let lock = self.classes.read().unwrap();
        let d = unsafe { &*(&*lock as *const HashMap<&str, Box<GroundedTerm>>) };
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

    fn belongs_to_class(&self, class_name: &str) -> Option<&GroundedTerm> {
        let lock = self.classes.read().unwrap();
        match lock.get(class_name) {
            Some(ref r) => Some(unsafe { &*(&***r as *const GroundedTerm) as &GroundedTerm }),
            None => None,
        }
    }

    fn add_class_membership(&self, grounded: GroundedTerm) {
        let mut lock = self.classes.write().unwrap();
        let mut d = unsafe { &mut *(&mut *lock as *mut HashMap<&str, Box<GroundedTerm>>) };
        let grounded = Box::new(grounded);
        let name: &str = unsafe { &*(grounded.get_parent_as_string_ref() as *const String) };
        let stmt_exists = d.contains_key(name);
        match stmt_exists {
            true => {
                let current = d.get_mut(&name).unwrap();
                current.update(*grounded)
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
                             var: Option<*const lang::Var>)
                             -> Option<HashMap<&'b str, Vec<&GroundedFunc>>> {
        let mut matches: HashMap<&str, Vec<&GroundedFunc>> = HashMap::new();
        let lock = self.relations.read().unwrap();
        let d = unsafe { &*(&*lock as *const HashMap<&str, Vec<Box<GroundedFunc>>>) };
        for decl in func_list {
            if let Some(relation_type) = d.get(decl.get_name()) {
                for rel in relation_type {
                    if rel.comparable_entity(decl, &self.name, var) {
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
        let d = unsafe { &*(&*lock as *const HashMap<&str, Vec<Box<GroundedFunc>>>) };
        if let Some(relation_type) = d.get(func.get_name()) {
            for rel in relation_type {
                if rel.comparable(func) {
                    return Some(rel)
                }
            }
        }
        None
    }

    fn add_relationship(&self, func: &GroundedFunc) {
        let mut lock = self.relations.write().unwrap();
        let mut d = unsafe { &mut *(&mut *lock as *mut HashMap<&str, Vec<Box<GroundedFunc>>>) };
        let name = func.get_name();
        let stmt_exists = d.contains_key(name);
        match stmt_exists {
            true => {
                let funcs_type = d.get_mut(name).unwrap();
                let mut found_rel = false;
                for f in funcs_type.iter_mut() {
                    if f.comparable(func) {
                        f.update(func);
                        found_rel = true;
                        break;
                    }
                }
                if !found_rel {
                    funcs_type.push(Box::new((*func).clone()))
                }
            }
            false => {
                let grounded = Box::new((*func).clone());
                let name: &str =
                    unsafe { &*(grounded.get_parent_as_string_ref() as *const String) };
                d.insert(name, vec![grounded]);
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
pub struct Class<'a> {
    pub name: String,
    classes: RwLock<HashMap<&'a str, Box<GroundedTerm>>>,
    relations: RwLock<HashMap<&'a str, Vec<Box<GroundedFunc>>>>,
    beliefs: RwLock<HashMap<&'a str, Vec<&'a LogSentence>>>,
    rules: RwLock<Vec<&'a LogSentence>>,
    kind: ClassKind,
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
            kind: kind,
        }
    }

    fn belongs_to_class(&self, class_name: &str) -> Option<&GroundedTerm> {
        let lock = self.classes.read().unwrap();
        match lock.get(class_name) {
            Some(ref r) => Some(unsafe { &*(&***r as *const GroundedTerm) as &GroundedTerm }),
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

    fn add_class_membership(&self, grounded: GroundedTerm) {
        let mut lock = self.classes.write().unwrap();
        let mut d = unsafe { &mut *(&mut *lock as *mut HashMap<&str, Box<GroundedTerm>>) };
        let grounded = Box::new(grounded);
        let name: &str = unsafe { &*(grounded.get_parent_as_string_ref() as *const String) };
        let stmt_exists = d.contains_key(name);
        match stmt_exists {
            true => {
                let current = d.get_mut(&name).unwrap();
                current.update(*grounded)
            }
            false => {
                d.insert(name, grounded);
            }
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
        let d = unsafe { &*(&*lock as *const HashMap<&str, Vec<Box<GroundedFunc>>>) };
        if let Some(relation_type) = d.get(func.get_name()) {
            for rel in relation_type {
                if rel.comparable(func) {
                    return Some(rel)
                }
            }
        }
        None
    }

    fn add_relationship(&self, func: &GroundedFunc) {
        let mut lock = self.relations.write().unwrap();
        let mut d = unsafe { &mut *(&mut *lock as *mut HashMap<&str, Vec<Box<GroundedFunc>>>) };
        let name = func.get_name();
        let stmt_exists = d.contains_key(name);
        match stmt_exists {
            true => {
                let funcs_type = d.get_mut(name).unwrap();
                let mut found_rel = false;
                for f in funcs_type.iter_mut() {
                    if f.comparable(func) {
                        f.update(func);
                        found_rel = true;
                        break;
                    }
                }
                if !found_rel {
                    funcs_type.push(Box::new((*func).clone()))
                }
            }
            false => {
                let grounded = Box::new((*func).clone());
                let name: &str =
                    unsafe { &*(grounded.get_parent_as_string_ref() as *const String) };
                d.insert(name, vec![grounded]);
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
    results: HashMap<&'a str, HashMap<&'a str, Option<bool>>>,
    query: QueryProcessed<'a>,
    kb: &'a Representation<'a>,
    obj_dic: HashMap<&'a str, HashSet<&'a &'a str>>,
}

enum ActiveQuery<'a> {
    None,
    Class(&'a str, &'a str, &'a GroundedTerm),
    Func(&'a str, &'a str, &'a GroundedFunc),
}

pub enum Answer<'a> {
    Single(Option<bool>),
    Multiple(HashMap<&'a str, HashMap<&'a str, Option<bool>>>),
    QueryErr,
    ParseErr(ParseErrF),
}

pub struct VarAssignment<'a> {
    pub name: &'a str,
    classes: HashMap<&'a str, &'a GroundedTerm>,
    funcs: HashMap<&'a str, Vec<&'a GroundedFunc>>,
}

impl<'a> VarAssignment<'a> {
    #[inline]
    pub fn get_class(&self, name: &str) -> &GroundedTerm {
        self.classes.get(name).unwrap()
    }

    #[inline]
    pub fn get_relationship(&self, func: &GroundedFunc) -> Option<&GroundedFunc> {
        for owned_f in self.funcs.get(func.get_name()).unwrap() {
            if owned_f.comparable(func) {
                return Some(owned_f);
            }
        }
        None
    }
}

struct InferencePass<'a> {
    query: HashMap<&'a str, bool>,
    actv: ActiveQuery<'a>,
}

impl<'a> Inference<'a> {
    fn new(agent: &'a Representation<'a>, query_input: QueryInput, single: bool) -> Answer {
        let query = QueryProcessed::new().get_query(query_input);
        let inf = Inference {
            results: HashMap::new(),
            query: query.unwrap(),
            kb: agent,
            obj_dic: HashMap::new(),
        };
        if single {
            for r0 in inf.get_results().values() {
                for r1 in r0.values() {
                    if let &Some(false) = r1 {
                        return Answer::Single(Some(false))
                    } else {
                        return Answer::Single(None);
                    }
                }
            }
            Answer::Single(Some(true))
        } else {
            Answer::Multiple(inf.get_results())
        }
    }

    fn get_results(self) -> HashMap<&'a str, HashMap<&'a str, Option<bool>>> {
        let Inference {
            results,
            query: _,
            kb: _,
            obj_dic: _
        } = self;
        results
    }

    /// Inference function from first-order logic sentences.
    ///
    /// Gets a query from an 'ask' call, encapsulates the query subtitutions,
    /// processes it (including caching of partial results or tracking
    /// var substitution) and returns the answer to the query. If new
    /// knowledge is produced then it's passed to an other procedure for
    /// addition to the KB.
    fn infer_facts(&'a mut self, ignore_current: bool) {
        let query_cls = |query: &str, actv_query: ActiveQuery| {
            // create a lookup table for memoizing results of previous passes
            let pass = InferencePass {
                query: HashMap::new(),
                actv: actv_query,
            };
            
            /*
            if not hasattr(self, 'queue'):
                self.queue = dict()
                for query in self.nodes.values():
                    for node in query: self.queue[node] = set()
            else:
                for node in self.queue: self.queue[node] = set()
            # Run the query, if there is no result and there is
            # an update, then rerun it again, else stop
            k, result, self._updated = True, None, list()
            while not result and k is True:
                chk, done = deque(), list()
                result = self.unify(q, chk, done)
                k = True if True in self._updated else False
                self._updated = list()
            */
        };

        /*
        # Get relevant rules to infer the query
        self.rules, self.done = OrderedSet(), [None]
        while hasattr(self, 'ctgs'):
            try: self.get_rules()
            except Inference.NoSolutionError: pass
        # Get the caterogies for each individual/class
        self.obj_dic = self.kb.objs_by_ctg(self.chk_ctgs, 'individuals')
        klass_dic = self.kb.objs_by_ctg(self.chk_ctgs, 'classes')
        self.obj_dic.update(klass_dic)
        */
        let mut entities = self.kb.entities_by_class(&self.query.clslist);
        for (k, v) in entities.drain() {
            self.obj_dic.insert(k, v);
        }
        let mut classes = self.kb.classes_by_class(&self.query.clslist);
        for (k, v) in classes.drain() {
            self.obj_dic.insert(k, v);
        }
        for (obj, preds) in self.query.cls_queries_grounded.iter() {
            for pred in preds {
                let query: &str = pred.get_parent();
                let mut result = None;
                if !ignore_current {
                    result = self.kb.class_membership(pred);
                }
                if result.is_some() {
                    let mut answ = self.results.entry(query).or_insert(HashMap::new());
                    answ.insert(*obj, result);
                } else {
                    // if no result was found from the kb directly
                    // make an inference from a grounded fact
                    let actv_query = ActiveQuery::Class(obj, query, pred);
                    query_cls(query, actv_query);
                }
            }
        }

        for pred in self.query.func_queries_grounded.iter() {
            let query: &str = pred.name.as_str();
            let mut result = None;
            for arg in pred.args.iter() {
                let obj = arg.get_name();
                if !ignore_current {
                    result = self.kb.has_relationship(pred, obj);
                }
                if result.is_some() {
                    let mut answ = self.results.entry(query).or_insert(HashMap::new());
                    answ.insert(obj, result);
                } else {
                    let actv_query = ActiveQuery::Func(obj, query, pred);
                    query_cls(query, actv_query);
                }
            }
        }
    }

    fn entities_meet_sent_req(&self,
                        req: &HashMap<*const lang::Var, Vec<*const lang::Assert>>)
                           -> HashMap<*const lang::Var, Vec<VarAssignment>> {

        let mut results: HashMap<*const lang::Var, Vec<VarAssignment>> = HashMap::new();
        for (var, asserts) in req.iter() {
            let mut class_list = Vec::new();
            let mut relations_list = Vec::new();
            for a in asserts {
                let a = unsafe { &**a };
                match a {
                    &lang::Assert::FuncDecl(ref f) => relations_list.push(f),
                    &lang::Assert::ClassDecl(ref c) => class_list.push(c),
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
                    if let Some(relations) = entity.has_relationships(&relations_list, Some(*var)) {
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
                    results.insert(*var,
                                   vec![VarAssignment {
                                            name: id,
                                            classes: gr_memb,
                                            funcs: gr_relations,
                                        }]);
                }
            }
        }
        results
    }

    fn get_rules(&mut self) {
        /*
        def mk_node(pos):
            # makes inference nodes for the evaluation
            for const in sent.get_preds(pos, unique=True):
                node = self.InferNode(classes, preds, const, sent)
                if node.const in self.nodes:
                    self.nodes[node.const].add(node)
                else:
                    self.nodes[node.const] = SortedListWithKey(
                        key=lambda x: x.rule.created)
                    self.nodes[node.const].add(node)

        if len(self.ctgs) > 0:
            cls = self.ctgs.pop()
        else:
            cls = None
        if cls is not None:
            self.done.append(cls)
            try:
                chk_rules = OrderedSet(self.kb.classes[cls].cog)
                chk_rules = chk_rules - self.rules
            except:
                raise Inference.NoSolutionError(cls)
            for sent in chk_rules:
                preds = sent.get_preds(particles=True)
                classes = []
                for p in preds:
                    if issubclass(p.pred.__class__, LogPredicate):
                        classes.append(p.pred.parent)
                    elif issubclass(p.pred.__class__, LogFunction):
                        classes.append(p.pred.func)
                mk_node('r')
                filtered = [e for e in classes
                            if e not in self.done and e not in self.ctgs]
                self.ctgs.extend(filtered)
                if cls in classes:
                    preds = sent.get_preds(branch='r', particles=True)
                    classes = []
                    for p in preds:
                        if issubclass(p.pred.__class__,LogPredicate):
                            classes.append(p.pred.parent)
                        elif issubclass(p.pred.__class__, LogFunction):
                            classes.append(p.pred.func)
                    mk_node('l')
                    filtered = [e for e in classes if e not in self.done
                                and e not in self.ctgs]
                    self.ctgs.extend(filtered)
            self.rules = self.rules | chk_rules
            self.get_rules()
        else:
            self.done.pop(0)
            self.chk_ctgs = set(self.done)
            del self.done
            del self.rules
            del self.ctgs

            class InferNode(object):
                def __init__(self, classes, ants, const, rule):
                    self.rule = rule
                    self.const = const
                    self.ants = tuple(classes)
                    if hasattr(rule, 'var_order'):
                        self.subs = OrderedDict((v, set()) for v in rule.var_order)
                    else:
                        self.subs = {}
                    disjunct = {}
                    for p in ants:
                        ant = p.pred
                        if p.parent.cond == '||':
                            prev = disjunct.setdefault(p.parent, [])
                            prev.append(p)
                        else:
                            if issubclass(ant.__class__, LogFunction):
                                args = ant.get_args()
                                for v in args:
                                    if v in self.subs:
                                        self.subs[v].add(ant.func)
                            elif issubclass(ant.__class__, LogPredicate):
                                if ant.term in self.subs:
                                    self.subs[ant.term].add(ant.parent)
                    # flatten nested disjunctions,
                    # TODO: this probably should be cached when the sentence
                    # is constructed
                    rm = []
                    for parent, childs in disjunct.items():
                        for child in childs:
                            if child in disjunct:
                                disjunct[parent].extend(disjunct[child])
                                rm.append(parent)
                    for k, v in disjunct.items():
                        if k not in rm:
                            names = tuple([
                                p.pred.parent
                                if issubclass(p.pred.__class__, LogPredicate)
                                else p.pred.func for p in v])
                            for p in v:
                                ant = p.pred
                                if issubclass(ant.__class__, LogFunction):
                                    args = ant.get_args()
                                    for v in args:
                                        if v in self.subs:
                                            self.subs[v].add(names)
                                elif issubclass(ant.__class__, LogPredicate):
                                    if ant.term in self.subs:
                                        self.subs[ant.term].add(names)
        */
    }
}

enum QueryInput {
    AskRelationalFunc(lang::FuncDecl),
    AskClassMember(lang::ClassDecl),
    ManyQueries(VecDeque<ParseTree>),
}

struct QueryProcessed<'a> {
    _askcls: Vec<Box<lang::ClassDecl>>,
    _askfunc: Vec<Box<lang::FuncDecl>>,
    cls_queries_free: HashMap<&'a lang::Var, Vec<&'a lang::FreeTerm>>,
    cls_queries_grounded: HashMap<&'a str, Vec<&'a lang::GroundedTerm>>,
    cls_memb_query: Vec<&'a lang::GroundedTerm>,
    func_memb_query: Vec<&'a lang::GroundedTerm>,
    func_queries_free: HashMap<&'a lang::Var, Vec<&'a lang::FuncDecl>>,
    func_queries_grounded: Vec<lang::GroundedFunc>,
    clslist: HashSet<&'a str>,
}

impl<'a> QueryProcessed<'a> {
    fn new() -> QueryProcessed<'a> {
        QueryProcessed {
            _askcls: vec![],
            _askfunc: vec![],
            cls_queries_free: HashMap::new(),
            cls_queries_grounded: HashMap::new(),
            cls_memb_query: vec![],
            func_queries_free: HashMap::new(),
            func_queries_grounded: vec![],
            func_memb_query: vec![],
            clslist: HashSet::new(),
        }
    }

    fn get_query(mut self, prequery: QueryInput) -> Result<QueryProcessed<'a>, ()> {
        fn assert_memb(query: &mut QueryProcessed, cdecl: lang::ClassDecl) -> Result<(), ()> {
            let cdecl = unsafe { &*(query.append_askcls(cdecl) as *const lang::ClassDecl) };
            match cdecl.get_parent() {
                &lang::Terminal::GroundedTerm(ref parent) => {
                    query.push_to_clsls(parent);
                    for a in cdecl.get_args() {
                        match a {
                            &lang::Predicate::FreeTerm(ref t) => {
                                query.push_to_clsquery_free(t.get_var_ref(), t);
                            }
                            &lang::Predicate::GroundedTerm(ref t) => {
                                query.push_to_clsquery_grounded(t.get_name(), t);
                            }
                        }
                    }
                }
                &lang::Terminal::FreeTerm(_) => {
                    for a in cdecl.get_args() {
                        match a {
                            &lang::Predicate::FreeTerm(_) => {
                                return Err(());
                            }
                            &lang::Predicate::GroundedTerm(ref t) => {
                                query.ask_class_memb(t);
                            }
                        }
                    }
                }
                _ => return Err(()),
            }
            Ok(())
        }

        fn assert_rel(query: &mut QueryProcessed, fdecl: lang::FuncDecl) -> Result<(), ()> {
            let fdecl = unsafe { &*(query.append_askfn(fdecl) as *const lang::FuncDecl) };
            match fdecl.get_parent() {
                &lang::Terminal::GroundedTerm(ref parent) => {
                    query.push_to_clsls(parent);
                    match fdecl.is_grounded() {
                        true => {
                            query.push_to_fnquery_grounded(fdecl.clone().into_grounded());
                        }
                        false => {
                            for a in fdecl.get_args() {
                                if let &lang::Predicate::FreeTerm(ref t) = a {
                                    query.push_to_fnquery_free(t.get_var_ref(), fdecl);
                                }
                            }
                        }
                    }
                }
                &lang::Terminal::FreeTerm(_) => {
                    for a in fdecl.get_args() {
                        match a {
                            &lang::Predicate::FreeTerm(_) => {
                                return Err(());
                            }
                            &lang::Predicate::GroundedTerm(ref t) => {
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
                assert_memb(&mut self, cdecl)?;
            }
            QueryInput::AskRelationalFunc(fdecl) => {
                assert_rel(&mut self, fdecl)?;
            }
            QueryInput::ManyQueries(trees) => {
                for parsetree in trees {
                    match parsetree {
                        lang::ParseTree::Assertion(assertions) => {
                            for a in assertions {
                                if let Err(()) = match a {
                                    lang::Assert::ClassDecl(cdecl) => assert_memb(&mut self, cdecl),
                                    lang::Assert::FuncDecl(fdecl) => assert_rel(&mut self, fdecl),
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
    fn append_askcls(&mut self, cls: lang::ClassDecl) -> &lang::ClassDecl {
        self._askcls.push(Box::new(cls));
        &**self._askcls.get(self._askcls.len() - 1).unwrap()
    }

    #[inline]
    fn append_askfn(&mut self, func: lang::FuncDecl) -> &lang::FuncDecl {
        self._askfunc.push(Box::new(func));
        &**self._askfunc.get(self._askfunc.len() - 1).unwrap()
    }

    #[inline]
    fn push_to_clsquery_grounded(&mut self, term: &'a str, cls: &'a lang::GroundedTerm) {
        self.cls_queries_grounded.entry(term).or_insert(vec![]).push(cls);
    }

    #[inline]
    fn push_to_clsquery_free(&mut self, term: &'a lang::Var, cls: &'a lang::FreeTerm) {
        self.cls_queries_free.entry(term).or_insert(vec![]).push(cls);
    }

    #[inline]
    fn push_to_fnquery_grounded(&mut self, func: lang::GroundedFunc) {
        self.func_queries_grounded.push(func)
    }

    #[inline]
    fn push_to_fnquery_free(&mut self, term: &'a lang::Var, func: &'a lang::FuncDecl) {
        self.func_queries_free.entry(term).or_insert(vec![]).push(func);
    }

    #[inline]
    fn push_to_clsls(&mut self, term: &'a str) {
        self.clslist.insert(term);
    }

    #[inline]
    fn ask_class_memb(&mut self, term: &'a lang::GroundedTerm) {
        self.cls_memb_query.push(term);
    }

    #[inline]
    fn ask_relationships(&mut self, term: &'a lang::GroundedTerm) {
        self.func_memb_query.push(term);
    }
}
