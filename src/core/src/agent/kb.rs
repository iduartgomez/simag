//! Main knowledge-base logic module, in this module reside the different
//! types that transform and store the data for the individual agents and
//! serve as representations of the different objects and the relationships
//! between them.
//!
//! Main
//! ----
//! `Representation`: Main type, stores all the representations and
//! relationships for a given agent in a concrete time.
//!
//! `Entity`: Represents a singular entity, which is the unique
//! member of it's own set.
//!
//! `Classes`: The sets in which the agent can classify objects.
//! Also stores the types of relations an object can have.
//!
//! Support types, methods and functions
//! -------------------------------------------
//! `Inference`: Encapsulates the whole inference process, from making
//! a temporal substitution representation where the inference is operated to
//! solving the query (including query parsing, data fetching and unification).

use std::collections::{HashMap, VecDeque};
use std::sync::RwLock;

use lang;
use lang::{ParseTree, ParseErrF, GroundedTerm, GroundedFunc};

pub struct Representation {
    entities: RwLock<HashMap<String, Entity>>,
    classes: RwLock<HashMap<String, Class>>,
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
    /// '''>>> r.tell("(professor[$Lucy,u=1])")'''
    /// will include the individual '$Lucy' in the professor category)
    /// '''>>> r.tell("((let x) professor[x,u=1] |> person[x,u=1])")'''
    /// all the individuals which are professors will be added to the
    /// person category, and the formula will be stored in the professor
    /// class for future use.
    ///
    /// For more examples check the LogSentence type docs.
    pub fn tell(&self, source: String) -> Result<(), Vec<ParseErrF>> {
        let pres = lang::logic_parser(source);
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
                    ParseTree::IExpr(iexpr) => self.add_belief(iexpr),
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

    /// Asks the KB if some fact is true and returns the answer to the query.
    pub fn ask(&self, source: String, single_answer: bool) -> Answer {
        let pres = lang::logic_parser(source);
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
            let class = Class::new(&parent, ClassKind::Membership);
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
                    let entity = Entity::new(&subject);
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
                    let class = Class::new(&subject, ClassKind::Membership);
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
                        let entity = Entity::new(&subject);
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
                        let class = Class::new(&subject, ClassKind::Membership);
                        class.add_relationship(&assert);
                        self.classes.write().unwrap().insert(subject, class);
                    }
                }
            }
        };
        let relation_exists = self.classes.read().unwrap().contains_key(&assert.name);
        if !relation_exists {
            let relationship = Class::new(&assert.name, ClassKind::Relationship);
            self.classes.write().unwrap().insert(String::from(assert.name.as_ref()), relationship);
        }
        process_arg(&assert.args[0]);
        process_arg(&assert.args[1]);
        if assert.third.is_some() {
            process_arg(assert.third.as_ref().unwrap())
        }
    }

    fn add_belief(&self, belief: lang::LogSentence) {
        let update = |subject: &str, is_entity: bool| {
            if is_entity {
                let entity_exists = self.entities.read().unwrap().contains_key(subject);
                if entity_exists {
                    let lock = self.entities.read().unwrap();
                    let entity = lock.get(subject).unwrap();
                    entity.add_belief(&belief);
                } else {
                    let entity = Entity::new(&subject);
                    entity.add_belief(&belief);
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
                    class.add_belief(&belief);
                } else {
                    let class = Class::new(&subject, ClassKind::Membership);
                    class.add_belief(&belief);
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
                    let parent = cls_decl.get_name();
                    let class_exists = self.classes.read().unwrap().contains_key(parent);
                    if class_exists {
                        self.classes.read().unwrap().get(parent).unwrap().add_belief(&belief)
                    } else {
                        let class = Class::new(parent, ClassKind::Membership);
                        class.add_belief(&belief);
                        self.classes.write().unwrap().insert(String::from(parent), class);
                    }
                    for arg in cls_decl.get_args() {
                        if arg.is_not_var() {
                            let subject = arg.get_name();
                            match subject.starts_with("$") {
                                true => update(subject, true),
                                false => update(subject, false),
                            }
                        }
                    }
                }
                &lang::Assert::FuncDecl(ref fn_decl) => {
                    let parent = fn_decl.get_name();
                    let class_exists = self.classes.read().unwrap().contains_key(parent);
                    if class_exists {
                        self.classes.read().unwrap().get(parent).unwrap().add_belief(&belief)
                    } else {
                        let class = Class::new(parent, ClassKind::Relationship);
                        class.add_belief(&belief);
                        self.classes.write().unwrap().insert(String::from(parent), class);
                    }
                    for arg in fn_decl.get_args() {
                        if arg.is_not_var() {
                            let subject = arg.get_name();
                            match subject.starts_with("$") {
                                true => update(subject, true),
                                false => update(subject, false),
                            }
                        }
                    }
                }
            }
        }
        panic!("has to run a query for every rhs pred in the sentence")
    }

    fn add_rule(&self, _rule: lang::LogSentence) {
        unimplemented!()
    }

    pub fn get_entity_from_class(&self,
                                 class: &str,
                                 subject: &str)
                                 -> Option<&GroundedTerm> {

        match subject.starts_with("$") {
            true => {
                let entity_exists = self.entities.read().unwrap().contains_key(subject);
                if entity_exists {
                    let lock = self.entities.read().unwrap();
                    match lock.get(subject).unwrap().belongs_to_class(class) {
                        Some(r) => Some(unsafe { &*(r as *const GroundedTerm) as &GroundedTerm }),
                        None => None
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
                        None => None
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn test_predicates(&self,
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
            for (id, entity) in self.entities.read().unwrap().iter() {
                let mut gr_memb: HashMap<String, *const GroundedTerm> = HashMap::new();
                let mut gr_relations: HashMap<String, Vec<*const GroundedFunc>> = HashMap::new();
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
                        name: id.clone(),
                        classes: gr_memb,
                        funcs: gr_relations,
                    })
                } else {
                    results.insert(*var,
                                   vec![VarAssignment {
                                            name: id.clone(),
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
pub struct Entity {
    name: String,
    classes: RwLock<HashMap<String, GroundedTerm>>,
    relations: RwLock<HashMap<String, Vec<GroundedFunc>>>,
}

impl<'a> Entity {
    fn new(name: &str) -> Entity {
        Entity {
            name: String::from(name),
            classes: RwLock::new(HashMap::new()),
            relations: RwLock::new(HashMap::new()),
        }
    }

    /// Returns the intersection between the provided list of classes and the
    /// set of classes the entity belongs to.
    fn belongs_to_classes(&'a self,
                          class_list: &Vec<&'a lang::ClassDecl>)
                          -> Option<HashMap<String, *const GroundedTerm>> {
        let mut matches = HashMap::new();
        for decl in class_list {
            let key = decl.get_name();
            if let Some(cls) = self.classes.read().unwrap().get(key) {
                matches.insert(String::from(key), cls as *const GroundedTerm);
            }
        }
        if matches.len() > 0 {
            Some(matches)
        } else {
            None
        }
    }

    fn belongs_to_class(&self, class_name: &str) -> Option<&GroundedTerm> {
        let lock = self.classes.read().unwrap();
        match lock.get(class_name) {
            Some(ref r) => Some(unsafe { &*(*r as *const GroundedTerm) as &GroundedTerm }),
            None => None,
        }
    }

    fn add_class_membership(&self, _grounded: GroundedTerm) {
        unimplemented!()
    }

    /// Returns the intersection between the provided list of relational functions and the
    /// set of relational functions the entity has.
    fn has_relationships(&self,
                         func_list: &Vec<&lang::FuncDecl>,
                         var: Option<*const lang::Var>)
                         -> Option<HashMap<String, Vec<*const GroundedFunc>>> {
        let mut matches: HashMap<String, Vec<*const GroundedFunc>> = HashMap::new();
        for decl in func_list {
            let lock = self.relations.read().unwrap();
            if let Some(relation_type) = lock.get(decl.get_name()) {
                for rel in relation_type {
                    if rel.comparable_entity(decl, &self.name, var) {
                        let name = rel.get_name();
                        if matches.contains_key(name) {
                            let v = matches.get_mut(name).unwrap();
                            v.push(rel as *const GroundedFunc)
                        } else {
                            matches.insert(String::from(name), vec![rel as *const GroundedFunc]);
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

    fn get_relationship(&self, _func: GroundedFunc) -> Option<&GroundedFunc> {
        unimplemented!()
    }

    fn add_relationship(&self, _func: &GroundedFunc) {
        unimplemented!()
    }

    fn add_belief(&self, _belief: &lang::LogSentence) {
        unimplemented!()
    }
}

/// A class is a set of entities that share some properties.
/// It can be a subset of others supersets, and viceversa.
///
/// Membership is not binary, but fuzzy, being the extreme cases (0, 1)
/// the classic binary membership. Likewise, membership to a class can be
/// temporal. For more info check '''Entity''' type documentation.
///
/// All the attributes of a class are inherited by their members
/// (to a fuzzy degree).
pub struct Class {
    name: String,
    classes: RwLock<HashMap<String, GroundedTerm>>,
    relations: RwLock<HashMap<String, Vec<GroundedFunc>>>,
    kind: ClassKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassKind {
    Relationship,
    Membership,
}

impl Class {
    fn new(name: &str, kind: ClassKind) -> Class {
        Class {
            name: String::from(name),
            classes: RwLock::new(HashMap::new()),
            relations: RwLock::new(HashMap::new()),
            kind: kind,
        }
    }

    fn belongs_to_class(&self, class_name: &str) -> Option<&GroundedTerm> {
        let lock = self.classes.read().unwrap();
        match lock.get(class_name) {
            Some(ref r) => Some(unsafe { &*(*r as *const GroundedTerm) as &GroundedTerm }),
            None => None,
        }
    }

    fn add_class_membership(&self, _grounded: GroundedTerm) {
        unimplemented!()
    }

    fn add_relationship(&self, _grounded: &GroundedFunc) {
        unimplemented!()
    }

    fn add_belief(&self, _belief: &lang::LogSentence) {
        unimplemented!()
    }
}

pub struct VarAssignment {
    pub name: String,
    classes: HashMap<String, *const GroundedTerm>,
    funcs: HashMap<String, Vec<*const GroundedFunc>>,
}

impl<'a> VarAssignment {
    #[inline]
    pub fn get_class(&self, name: &str) -> &GroundedTerm {
        unsafe { &**(self.classes.get(name).unwrap()) as &GroundedTerm }
    }

    #[inline]
    pub fn get_relationship(&self, func: &GroundedFunc) -> Option<&GroundedFunc> {
        for owned_f in self.funcs.get(func.get_name()).unwrap() {
            let owned_f = unsafe { &**owned_f as &GroundedFunc };
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
