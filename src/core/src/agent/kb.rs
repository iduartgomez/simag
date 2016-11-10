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
    entities: RwLock<HashMap<String, Entity<'a>>>,
    classes: RwLock<HashMap<String, Class<'a>>>,
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
                    ParseTree::Rule(rule) => self.add_rule(rule),
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
    pub fn ask(&self, source: String, single_answer: bool) -> Answer {
        let pres = lang::logic_parser(source, false);
        if pres.is_ok() {
            let pres = pres.unwrap();
            Inference::new(pres, single_answer)
        } else {
            Answer::ParseErr(pres.unwrap_err())
        }
    }

    pub fn up_membership(&self, assert: lang::GroundedTerm) {
        let subject = String::from(assert.get_name());
        let parent = String::from(assert.get_parent());
        let parent_exists = self.classes.read().unwrap().contains_key(&parent);
        if !parent_exists {
            let class = Class::new(parent.clone(), ClassKind::Membership);
            self.classes.write().unwrap().insert(parent, class);
        }
        match (&subject).starts_with("$") {
            true => {
                let entity_exists = self.entities.read().unwrap().contains_key(&subject);
                if entity_exists {
                    let lock = self.entities.read().unwrap();
                    let entity = lock.get(&subject).unwrap();
                    entity.add_class_membership(assert);
                } else {
                    let entity = Entity::new(subject.clone());
                    entity.add_class_membership(assert);
                    self.entities.write().unwrap().insert(subject, entity);
                }
            }
            false => {
                let class_exists = self.classes.read().unwrap().contains_key(&subject);
                if class_exists {
                    let lock = self.classes.read().unwrap();
                    let class = lock.get(&subject).unwrap();
                    class.add_class_membership(assert);
                } else {
                    let class = Class::new(subject.clone(), ClassKind::Membership);
                    class.add_class_membership(assert);
                    self.classes.write().unwrap().insert(subject, class);
                }
            }
        }
    }

    pub fn up_relation(&self, assert: lang::GroundedFunc) {
        let process_arg = |a: &GroundedTerm| {
            let subject = String::from(a.get_name());
            match (&subject).starts_with("$") {
                true => {
                    let entity_exists = self.entities.read().unwrap().contains_key(&subject);
                    if entity_exists {
                        let lock = self.entities.read().unwrap();
                        let entity = lock.get(&subject).unwrap();
                        entity.add_relationship(&assert);
                    } else {
                        let entity = Entity::new(subject.clone());
                        entity.add_relationship(&assert);
                        self.entities.write().unwrap().insert(subject, entity);
                    }
                }
                false => {
                    let class_exists = self.classes.read().unwrap().contains_key(&subject);
                    if class_exists {
                        let lock = self.classes.read().unwrap();
                        let class = lock.get(&subject).unwrap();
                        class.add_relationship(&assert);
                    } else {
                        let class = Class::new(subject.clone(), ClassKind::Membership);
                        class.add_relationship(&assert);
                        self.classes.write().unwrap().insert(subject, class);
                    }
                }
            }
        };
        let relation_exists = self.classes.read().unwrap().contains_key(&assert.name);
        if !relation_exists {
            let relationship = Class::new(assert.name.clone(), ClassKind::Relationship);
            self.classes.write().unwrap().insert(assert.name.clone(), relationship);
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
                    let entity: Entity<'a> = Entity::new(String::from(subject));
                    entity.add_belief(belief, name);
                    self.entities
                        .write()
                        .unwrap()
                        .insert(String::from(subject), entity);
                }
            } else {
                let class_exists = self.classes.read().unwrap().contains_key(subject);
                if class_exists {
                    let lock = self.classes.read().unwrap();
                    let class = lock.get(subject).unwrap();
                    class.add_belief(belief, name);
                } else {
                    let class: Class<'a> = Class::new(String::from(subject), ClassKind::Membership);
                    class.add_belief(belief, name);
                    self.classes
                        .write()
                        .unwrap()
                        .insert(String::from(subject), class);
                }
            }
        };
        for p in belief.get_all_predicates() {
            match p {
                &lang::Assert::ClassDecl(ref cls_decl) => {
                    let parent = cls_decl.get_name_as_string_ref();
                    let class_exists = self.classes.read().unwrap().contains_key(parent);
                    if class_exists {
                        self.classes
                            .read()
                            .unwrap()
                            .get(parent)
                            .unwrap()
                            .add_belief(&belief, parent)
                    } else {
                        let class = Class::new(parent.clone(), ClassKind::Membership);
                        class.add_belief(&belief, parent);
                        self.classes.write().unwrap().insert(String::from(parent.as_str()), class);
                    }
                    for arg in cls_decl.get_args() {
                        if arg.is_not_var() {
                            let subject = arg.get_name();
                            match subject.starts_with("$") {
                                true => update(subject, parent, true),
                                false => update(subject, parent, false),
                            }
                        }
                    }
                }
                &lang::Assert::FuncDecl(ref fn_decl) => {
                    let parent = fn_decl.get_name_as_string_ref();
                    let class_exists = self.classes.read().unwrap().contains_key(parent);
                    if class_exists {
                        self.classes
                            .read()
                            .unwrap()
                            .get(parent)
                            .unwrap()
                            .add_belief(&belief, parent)
                    } else {
                        let class = Class::new(parent.clone(), ClassKind::Relationship);
                        class.add_belief(&belief, parent);
                        self.classes.write().unwrap().insert(parent.clone(), class);
                    }
                    for arg in fn_decl.get_args() {
                        if arg.is_not_var() {
                            let subject = arg.get_name();
                            match subject.starts_with("$") {
                                true => update(subject, parent, true),
                                false => update(subject, parent, false),
                            }
                        }
                    }
                }
            }
        }
        panic!("has to run a query for every lhs pred in the sentence")
    }

    fn add_rule(&'a self, rule: LogSentence) {
        // TODO: must add the rules to entities/classes in the rel_func args too
        let rule = Box::new(rule);
        let mut n = HashSet::new();
        {
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
                            Class::new(String::from(name), ClassKind::Membership)
                        }
                        &lang::Assert::FuncDecl(_) => {
                            Class::new(String::from(name), ClassKind::Relationship)
                        }
                    };
                    nc.add_rule(&rule);
                    self.classes.write().unwrap().insert(String::from(name), nc);
                }
            }
        }
        self._log_sentences.lock().unwrap().push(rule);
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
        for e in lock.values() {
            // lifetime bound to 'a
            let e = unsafe { &*(e as *const Entity) };
            let s = e.belongs_to_classes_set(&classes);
            let s = unsafe { &*(&s as *const HashSet<&str>) };
            let t = e.has_relationships_set(&classes);
            let t = unsafe { &*(&t as *const HashSet<&str>) };
            let u = t.union(&s).collect::<HashSet<&&str>>();
            if u.len() > 0 {
                dict.insert(e.name.as_str(), u);
            }
        }
        dict
    }

    fn classes_by_class(&'a self,
                        classes: &HashSet<&'a str>)
                        -> HashMap<&'a str, HashSet<&'a &str>> {
        let mut dict = HashMap::new();
        let lock = self.classes.read().unwrap();
        for e in lock.values() {
            // lifetime bound to 'a
            let e = unsafe { &*(e as *const Class) };
            let s = e.belongs_to_classes_set(&classes);
            let s = unsafe { &*(&s as *const HashSet<&str>) };
            let t = e.has_relationships_set(&classes);
            let t = unsafe { &*(&t as *const HashSet<&str>) };
            let u = t.union(&s).collect::<HashSet<&&str>>();
            if u.len() > 0 {
                dict.insert(e.name.as_str(), u);
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

    pub fn test_predicates(&'a self,
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
            let lock = self.entities.read().unwrap();
            let d = unsafe { &*(&*lock as *const HashMap<String, Entity>) };
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
            let key = decl.get_name();
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

    fn add_rule(&self, _rule: &LogSentence) {
        unimplemented!()
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

    fn add_rule(&self, _rule: &LogSentence) {
        unimplemented!()
    }
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

pub enum Answer {
    Single(Option<bool>),
    Multiple(Vec<Option<bool>>),
    QueryErr,
    ParseErr(ParseErrF),
}

struct Inference {
    results: Vec<Option<bool>>,
}

impl Inference {
    fn new(_query: VecDeque<ParseTree>, single: bool) -> Answer {
        let inf = Inference { results: Vec::new() };
        if single {
            for r in inf.get_results() {
                if r.is_some() {
                    match r {
                        Some(false) => return Answer::Single(Some(false)),
                        _ => {}
                    }
                } else {
                    return Answer::Single(None);
                }
            }
            Answer::Single(Some(true))
        } else {
            Answer::Multiple(inf.get_results())
        }
    }

    fn get_results(self) -> Vec<Option<bool>> {
        let Inference { results } = self;
        results
    }
}
