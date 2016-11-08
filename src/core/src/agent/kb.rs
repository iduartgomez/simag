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

use lang;
use lang::{ParseTree, ParseErrF, GroundedTerm, GroundedFunc};

pub struct Representation {
    entities: HashMap<String, Entity>,
    classes: HashMap<usize, usize>,
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
            entities: HashMap::new(),
            classes: HashMap::new(),
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
                                self.up_membership(assertion.unwrap_cls())
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

    pub fn up_membership(&self, assert: lang::ClassDecl) {}

    pub fn up_relation(&self, assert: lang::GroundedFunc) {}

    fn add_belief(&self, belief: lang::LogSentence) {}

    fn add_rule(&self, rule: lang::LogSentence) {}

    pub fn get_entity_from_class(&self, class_name: &str) -> Option<&GroundedTerm> {
        unimplemented!()
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
            for (id, entity) in self.entities.iter() {
                let mut gr_memb: HashMap<&str, &GroundedTerm> = HashMap::new();
                let mut gr_relations: HashMap<&str, Vec<&GroundedFunc>> = HashMap::new();
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
pub struct Entity {
    name: String,
    classes: HashMap<String, GroundedTerm>,
    relations: HashMap<String, Vec<GroundedFunc>>,
}

impl<'a> Entity {
    /// Returns the intersection between the provided list of classes and the
    /// set of classes the entity belongs to.
    fn belongs_to_classes(&'a self,
                          class_list: &Vec<&'a lang::ClassDecl>)
                          -> Option<HashMap<&str, &GroundedTerm>> {
        let mut matches = HashMap::new();
        for decl in class_list {
            let key = decl.get_name();
            if let Some(cls) = self.classes.get(key) {
                matches.insert(key, cls);
            }
        }
        if matches.len() > 0 {
            Some(matches)
        } else {
            None
        }
    }

    fn belongs_to_class(&self, class_name: &str) -> Option<&GroundedTerm> {
        unimplemented!()
    }

    fn add_class_membership(&self, grounded: lang::ClassDecl) {
        unimplemented!()
    }

    /// Returns the intersection between the provided list of relational functions and the
    /// set of relational functions the entity has.
    fn has_relationships(&self,
                         func_list: &Vec<&lang::FuncDecl>,
                         var: Option<*const lang::Var>)
                         -> Option<HashMap<&str, Vec<&GroundedFunc>>> {
        let mut matches: HashMap<&str, Vec<&GroundedFunc>> = HashMap::new();
        for decl in func_list {
            if let Some(relation_type) = self.relations.get(decl.get_name()) {
                for rel in relation_type {
                    if rel.comparable_entity(decl, &self.name, var) {
                        let name = rel.get_name();
                        if matches.contains_key(name) {
                            let v = matches.get_mut(name).unwrap();
                            v.push(rel)
                        } else {
                            matches.insert(name, vec![rel]);
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

    fn get_relationship(&self, func: lang::FuncDecl) -> Option<&GroundedTerm> {
        unimplemented!()
    }

    fn add_relationship(&self, fun: lang::FuncDecl) {
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
pub struct Class;

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
    fn new(query: VecDeque<ParseTree>, single: bool) -> Answer {
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
