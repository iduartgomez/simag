use FLOAT_EQ_ULPS;

use super::*;
use lang::*;

use float_cmp::ApproxEqUlps;

use std::collections::{HashMap, VecDeque};
use std::iter::FromIterator;
use std::rc::Rc;
use std::sync::{Arc, RwLock};

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
#[derive(Default)]
pub struct Representation {
    pub entities: RwLock<HashMap<String, Entity>>,
    pub classes: RwLock<HashMap<String, Class>>,
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
        let pres = logic_parser(source, true);
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
                                    a.overwrite_time_data(&t);
                                    let x: Option<&super::IExprResult> = None;
                                    self.up_membership(Arc::new(a), x)
                                }
                            } else {
                                let a = Arc::new(assertion.unwrap_fn().into_grounded());
                                let x: Option<&super::IExprResult> = None;
                                self.up_relation(a, x)
                            }
                        }
                    }
                    ParseTree::IExpr(iexpr) => self.add_belief(Arc::new(iexpr)),
                    ParseTree::Expr(rule) => self.add_rule(Arc::new(rule)),
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
        let pres = logic_parser(source, false);
        if pres.is_ok() {
            let pres = QueryInput::ManyQueries(pres.unwrap());
            let mut inf = match Inference::new(self, pres, usize::max_value(), false) {
                Ok(inf) => inf,
                Err(()) => return Answer::QueryErr,
            };
            {
                let inf_r = unsafe { &mut *(&mut inf as *mut Box<Inference>) };
                inf_r.infer_facts();
            }
            inf.get_results()
        } else {
            Answer::ParseErr(pres.unwrap_err())
        }
    }

    pub fn ask_processed(&self, source: QueryInput, depth: usize, ignore_current: bool) -> Answer {
        let mut inf = match Inference::new(self, source, depth, ignore_current) {
            Ok(inf) => inf,
            Err(()) => return Answer::QueryErr,
        };
        {
            let inf_r = unsafe { &mut *(&mut inf as *mut Box<Inference>) };
            inf_r.infer_facts();
        }
        inf.get_results()
    }

    pub fn up_membership<T: ProofResContext>(&self,
                                             assert: Arc<GroundedMemb>,
                                             context: Option<&T>) {
        let parent_exists = self.classes.read().unwrap().contains_key(assert.get_parent());
        if !parent_exists {
            let class = Class::new(assert.get_parent().to_string(), ClassKind::Membership);
            self.classes.write().unwrap().insert(class.name.clone(), class);
        }
        let decl;
        let is_new: bool;
        if (assert.get_name()).starts_with('$') {
            let entity_exists = self.entities.read().unwrap().contains_key(assert.get_name());
            if entity_exists {
                let lock = self.entities.read().unwrap();
                let entity = lock.get(assert.get_name()).unwrap();
                is_new = entity.add_class_membership(self, assert.clone(), context);
                decl = ClassMember::Entity(assert.clone());
            } else {
                let entity = Entity::new(assert.get_name().to_string());
                is_new = entity.add_class_membership(self, assert.clone(), context);
                self.entities.write().unwrap().insert(entity.name.clone(), entity);
                decl = ClassMember::Entity(assert.clone());
            }
        } else {
            let class_exists = self.classes.read().unwrap().contains_key(assert.get_name());
            if class_exists {
                let lock = self.classes.read().unwrap();
                let class = lock.get(assert.get_name()).unwrap();
                is_new = class.add_class_membership(self, assert.clone(), context);
                decl = ClassMember::Class(assert.clone());
            } else {
                let class = Class::new(assert.get_name().to_string(), ClassKind::Membership);
                is_new = class.add_class_membership(self, assert.clone(), context);
                self.classes.write().unwrap().insert(class.name.clone(), class);
                decl = ClassMember::Class(assert.clone());
            }
        }
        if is_new {
            let lock = self.classes.read().unwrap();
            let parent = lock.get(assert.get_parent()).unwrap();
            parent.add_member(decl);
        }
    }

    pub fn up_relation<T: ProofResContext>(&self, assert: Arc<GroundedFunc>, context: Option<&T>) {
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
                    is_new1 = entity.add_relationship(self, assert.clone(), context);
                } else {
                    let entity = Entity::new(subject.to_string());
                    is_new1 = entity.add_relationship(self, assert.clone(), context);
                    self.entities.write().unwrap().insert(entity.name.clone(), entity);
                }
            } else {
                let class_exists = self.classes.read().unwrap().contains_key(subject);
                if class_exists {
                    let lock = self.classes.read().unwrap();
                    let class = lock.get(subject).unwrap();
                    is_new1 = class.add_relationship(self, assert.clone(), context);
                } else {
                    let class = Class::new(subject.to_string(), ClassKind::Membership);
                    is_new1 = class.add_relationship(self, assert.clone(), context);
                    self.classes.write().unwrap().insert(class.name.clone(), class);
                }
            }
            let new_check = is_new.clone();
            *new_check.borrow_mut() = is_new1;
        };
        let relation_exists = self.classes.read().unwrap().contains_key(&assert.name);
        if !relation_exists {
            let relationship = Class::new(assert.name.clone(), ClassKind::Relationship);
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

    fn add_belief(&self, belief: Arc<LogSentence>) {
        fn update(subject: &str,
                  name: &str,
                  is_entity: bool,
                  belief: Arc<LogSentence>,
                  repr: &Representation) {
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
                    let class_exists = self.classes
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
                        let class = Class::new(cls_decl.get_name().to_string(),
                                               ClassKind::Membership);
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
                Assert::FuncDecl(ref fn_decl) => {
                    if !fn_decl.is_relational() {
                        continue;
                    }
                    let class_exists = self.classes
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
                        let class = Class::new(fn_decl.get_name().to_string(),
                                               ClassKind::Relationship);
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

        let iter_cls_candidates =
            |cls_decl: &ClassDecl, candidates: &HashMap<&Var, Vec<Arc<VarAssignment>>>| for a in
                cls_decl.get_args() {
                match *a {
                    Predicate::FreeClsMemb(ref free) => {
                        if let Some(ls) = candidates.get(free.get_var_ref()) {
                            for entity in ls {
                                let grfact = Arc::new(GroundedMemb::from_free(free, entity.name));
                                self.ask_processed(QueryInput::AskClassMember(grfact), 0, true);
                            }
                        }
                    }
                    _ => continue,
                }
            };
        let iter_func_candidates = |func_decl: &FuncDecl,
                                    candidates: &HashMap<&Var, Vec<Arc<VarAssignment>>>| {
            let mapped = ArgsProduct::product(candidates.clone());
            if let Some(mapped) = mapped {
                let f = HashMap::new();
                for args in mapped {
                    let args = HashMap::from_iter(args.iter()
                        .map(|&(v, ref a)| (v, &**a)));
                    if let Ok(grfunc) = GroundedFunc::from_free(func_decl, &args, &f) {
                        self.ask_processed(QueryInput::AskRelationalFunc(Arc::new(grfunc)),
                                           0,
                                           true);
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

    fn add_rule(&self, rule: Arc<LogSentence>) {
        let mut cls_names: Vec<&str> = Vec::new();
        let mut func_names: Vec<&FuncDecl> = Vec::new();
        let preds = rule.get_all_predicates();
        for p in preds {
            let name = p.get_name();
            if p.is_class() {
                cls_names.push(name);
            } else {
                func_names.push(p.unwrap_fn_as_ref());
            }
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
        super::rule_inf::rules_inference(self, &cls_names, &func_names);
    }

    /// Takes a vector of class names and returns a hash map with those classes as keys
    /// and the memberships to those classes.
    pub fn by_class<'a, 'b>(&'a self,
                            classes: &'b [&str])
                            -> HashMap<&'b str, Vec<Arc<GroundedMemb>>> {
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
                    ClassMember::Class(ref m) |
                    ClassMember::Entity(ref m) => {
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
    #[allow(type_complexity)]
    pub fn by_relationship<'a, 'b>
        (&'a self,
         funcs: &'b [&'b FuncDecl])
         -> HashMap<&'b str, HashMap<&'a str, Vec<Arc<GroundedFunc>>>> {
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
                            let name = unsafe { ::std::mem::transmute::<&str, &'a str>(name) };
                            let e: &mut Vec<_> = m.entry(name).or_insert(Vec::new());
                            e.push(f.clone());
                        }
                    }
                }
            }
            dict.insert(func.get_name(), m);
        }
        dict
    }

    pub fn get_entity_from_class(&self, class: &str, subject: &str) -> Option<Arc<GroundedMemb>> {
        if subject.starts_with('$') {
            let entity_exists = self.entities.read().unwrap().contains_key(subject);
            if entity_exists {
                let lock = self.entities.read().unwrap();
                match lock.get(subject).unwrap().belongs_to_class(class) {
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
                match lock.get(subject).unwrap().belongs_to_class(class) {
                    Some(r) => Some(r.clone()),
                    None => None,
                }
            } else {
                None
            }
        }
    }

    pub fn class_membership(&self, pred: &GroundedMemb) -> Option<bool> {
        let subject = pred.get_name();
        if subject.starts_with('$') {
            if let Some(entity) = self.entities.read().unwrap().get(subject) {
                if let Some(current) = entity.belongs_to_class(pred.get_parent()) {
                    if *current == *pred {
                        return Some(true);
                    } else {
                        return Some(false);
                    }
                }
            }
        } else if let Some(class) = self.classes.read().unwrap().get(subject) {
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

    pub fn get_class_membership(&self, subject: &FreeClsOwner) -> Vec<Arc<GroundedMemb>> {
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

    pub fn has_relationship(&self, pred: &GroundedFunc, subject: &str) -> Option<bool> {
        if subject.starts_with('$') {
            if let Some(entity) = self.entities.read().unwrap().get(subject) {
                if let Some(current) = entity.has_relationship(pred) {
                    if *current == *pred {
                        return Some(true);
                    } else {
                        return Some(false);
                    }
                }
            }
        } else if let Some(class) = self.classes.read().unwrap().get(subject) {
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
                            subject: &str)
                            -> Option<Arc<GroundedFunc>> {
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

    pub fn get_relationships(&self, func: &FuncDecl) -> HashMap<&str, Vec<Arc<GroundedFunc>>> {
        let mut res = HashMap::new();
        for (pos, arg) in func.get_args().enumerate() {
            if !arg.is_var() {
                let name = arg.get_name();
                if name.starts_with('$') {
                    if let Some(entity) = self.entities.read().unwrap().get(name) {
                        let mut v: HashMap<&str, Vec<Arc<GroundedFunc>>> =
                            entity.get_relationships(pos, arg);
                        for (_, mut funcs) in v.drain() {
                            let t = unsafe { &*(&*funcs[0] as *const GroundedFunc) };
                            let rel = t.get_name();
                            res.entry(rel).or_insert(vec![]).append(&mut funcs);
                        }
                    }
                } else if let Some(class) = self.classes.read().unwrap().get(name) {
                    let mut v: HashMap<&str, Vec<Arc<GroundedFunc>>> =
                        class.get_relationships(pos, arg);
                    for (_, mut funcs) in v.drain() {
                        let t = unsafe { &*(&*funcs[0] as *const GroundedFunc) };
                        let rel = t.get_name();
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
    pub name: String,
    classes: RwLock<HashMap<String, Arc<GroundedMemb>>>,
    relations: RwLock<HashMap<String, Vec<Arc<GroundedFunc>>>>,
    beliefs: RwLock<HashMap<String, Vec<Arc<LogSentence>>>>,
}

impl Entity {
    fn new(name: String) -> Entity {
        Entity {
            name: name,
            classes: RwLock::new(HashMap::new()),
            relations: RwLock::new(HashMap::new()),
            beliefs: RwLock::new(HashMap::new()),
        }
    }

    fn belongs_to_class(&self, class_name: &str) -> Option<Arc<GroundedMemb>> {
        let lock = self.classes.read().unwrap();
        match lock.get(class_name) {
            Some(r) if r.get_value().is_some() => Some(r.clone()),
            Some(_) | None => None,
        }
    }

    fn get_class_membership(&self, compare: &FreeClsOwner) -> Vec<Arc<GroundedMemb>> {
        let lock = self.classes.read().unwrap();
        lock.values().filter(|x| compare.filter_grounded(&**x)).cloned().collect::<Vec<_>>()
    }

    fn add_class_membership<T: ProofResContext>(&self,
                                                agent: &Representation,
                                                grounded: Arc<GroundedMemb>,
                                                context: Option<&T>)
                                                -> bool {
        let name = grounded.get_parent();
        let stmt_exists = {
            let lock = self.classes.read().unwrap();
            lock.contains_key(name)
        };
        if stmt_exists {
            let lock = self.classes.read().unwrap();
            let current = lock.get(name).unwrap();
            if let Some(context) = context {
                current.update(agent, &*grounded, Some(context.get_id()));
                current.bms
                    .as_ref()
                    .unwrap()
                    .update_producers(Grounded::Class(Arc::downgrade(&current.clone())), context);
            } else {
                current.update(agent, &*grounded, None);
            }
            false
        } else {
            let mut lock = self.classes.write().unwrap();
            if let Some(context) = context {
                let bms = grounded.bms.as_ref().unwrap();
                bms.last_was_produced(Some(context.get_id()));
                bms.update_producers(Grounded::Class(Arc::downgrade(&grounded.clone())), context);
            }
            lock.insert(name.to_string(), grounded.clone());
            true
        }
    }

    fn has_relationship(&self, func: &GroundedFunc) -> Option<Arc<GroundedFunc>> {
        let lock = self.relations.read().unwrap();
        if let Some(relation_type) = lock.get(func.get_name()) {
            for rel in relation_type {
                if rel.comparable(func) && rel.get_value().is_some() {
                    return Some(rel.clone());
                }
            }
        }
        None
    }

    fn get_relationships(&self,
                         pos: usize,
                         compare: &Predicate)
                         -> HashMap<&str, Vec<Arc<GroundedFunc>>> {
        let mut res = HashMap::new();
        let (op, val) = compare.get_uval();
        let lock = self.relations.read().unwrap();
        for functions in lock.values() {
            for f in functions {
                if f.name_in_pos(&*self.name, &pos) {
                    let t = unsafe { &*(&**f as *const GroundedFunc) };
                    let rel_name = t.get_name();
                    match op {
                        None => res.entry(rel_name).or_insert(vec![]).push(f.clone()),
                        Some(CompOperator::Equal) => {
                            if f.get_value()
                                .unwrap()
                                .approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS) {
                                res.entry(rel_name).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(CompOperator::More) => {
                            if *val.as_ref().unwrap() < f.get_value().unwrap() {
                                res.entry(rel_name).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(CompOperator::Less) => {
                            if *val.as_ref().unwrap() > f.get_value().unwrap() {
                                res.entry(rel_name).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(CompOperator::LessEqual) => {
                            if *val.as_ref().unwrap() > f.get_value().unwrap() ||
                               f.get_value()
                                .unwrap()
                                .approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS) {
                                res.entry(rel_name).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(CompOperator::MoreEqual) => {
                            if *val.as_ref().unwrap() < f.get_value().unwrap() ||
                               f.get_value()
                                .unwrap()
                                .approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS) {
                                res.entry(rel_name).or_insert(vec![]).push(f.clone())
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
    fn add_relationship<T: ProofResContext>(&self,
                                            agent: &Representation,
                                            func: Arc<GroundedFunc>,
                                            context: Option<&T>)
                                            -> bool {
        let name = func.get_name();
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
                            f.update(agent, &*func, Some(context.get_id()));
                            f.bms.update_producers(Grounded::Function(Arc::downgrade(&f.clone())),
                                                   context);
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
                    func.bms.last_was_produced(Some(context.get_id()));
                    func.bms.update_producers(Grounded::Function(Arc::downgrade(&func.clone())),
                                              context);
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
                func.bms.last_was_produced(Some(context.get_id()));
                func.bms
                    .update_producers(Grounded::Function(Arc::downgrade(&func.clone())), context);
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
            self.beliefs.write().unwrap().insert(parent.to_string(), vec![belief]);
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
    pub name: String,
    classes: RwLock<HashMap<String, Arc<GroundedMemb>>>,
    relations: RwLock<HashMap<String, Vec<Arc<GroundedFunc>>>>,
    pub beliefs: RwLock<HashMap<String, Vec<Arc<LogSentence>>>>,
    rules: RwLock<Vec<Arc<LogSentence>>>,
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
    Entity(Arc<GroundedMemb>),
    Class(Arc<GroundedMemb>),
    Func(Arc<GroundedFunc>),
}

impl ClassMember {
    fn unwrap_memb(&self) -> Arc<GroundedMemb> {
        match *self {
            ClassMember::Entity(ref obj) |
            ClassMember::Class(ref obj) => obj.clone(),
            _ => panic!(),
        }
    }

    fn unwrap_fn(&self) -> Arc<GroundedFunc> {
        match *self {
            ClassMember::Func(ref f) => f.clone(),
            _ => panic!(),
        }
    }
}

impl Class {
    fn new(name: String, kind: ClassKind) -> Class {
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

    fn belongs_to_class(&self, class_name: &str) -> Option<Arc<GroundedMemb>> {
        let lock = self.classes.read().unwrap();
        match lock.get(class_name) {
            Some(r) if r.get_value().is_some() => Some(r.clone()),
            Some(_) | None => None,
        }
    }

    fn get_class_membership(&self, compare: &FreeClsOwner) -> Vec<Arc<GroundedMemb>> {
        let lock = self.classes.read().unwrap();
        lock.values().filter(|x| compare.filter_grounded(&**x)).cloned().collect::<Vec<_>>()
    }

    /// Set a superclass of this class
    fn add_class_membership<T: ProofResContext>(&self,
                                                agent: &Representation,
                                                grounded: Arc<GroundedMemb>,
                                                context: Option<&T>)
                                                -> bool {
        let name = grounded.get_parent();
        let stmt_exists = {
            let lock = self.classes.read().unwrap();
            lock.contains_key(name)
        };
        if stmt_exists {
            let lock = self.classes.read().unwrap();
            let current = lock.get(name).unwrap();
            if let Some(context) = context {
                current.update(agent, &*grounded, Some(context.get_id()));
                current.bms
                    .as_ref()
                    .unwrap()
                    .update_producers(Grounded::Class(Arc::downgrade(&current.clone())), context);
            } else {
                current.update(agent, &*grounded, None);
            }
            false
        } else {
            let mut lock = self.classes.write().unwrap();
            if let Some(context) = context {
                let bms = grounded.bms.as_ref().unwrap();
                bms.last_was_produced(Some(context.get_id()));
                bms.update_producers(Grounded::Class(Arc::downgrade(&grounded.clone())), context);
            }
            lock.insert(name.to_string(), grounded.clone());
            true
        }
    }

    /// Add members of this class, being them other classes or entities.
    fn add_member(&self, member: ClassMember) {
        self.members.write().unwrap().push(member);
    }

    pub fn get_members(&self, comp: &FreeClsMemb) -> Vec<Arc<GroundedMemb>> {
        let lock = self.members.read().unwrap();
        lock.iter()
            .map(|x| x.unwrap_memb())
            .filter(|m| comp.grounded_eq(m))
            .collect::<Vec<_>>()
    }

    fn has_relationship(&self, func: &GroundedFunc) -> Option<Arc<GroundedFunc>> {
        let lock = self.relations.read().unwrap();
        if let Some(relation_type) = lock.get(func.get_name()) {
            for rel in relation_type {
                if rel.comparable(func) && rel.get_value().is_some() {
                    return Some(rel.clone());
                }
            }
        }
        None
    }

    fn get_relationships(&self,
                         pos: usize,
                         compare: &Predicate)
                         -> HashMap<&str, Vec<Arc<GroundedFunc>>> {
        let mut res = HashMap::new();
        let (op, val) = compare.get_uval();
        let lock = self.relations.read().unwrap();
        for functions in lock.values() {
            for f in functions {
                if f.name_in_pos(&*self.name, &pos) {
                    let t = unsafe { &*(&**f as *const GroundedFunc) };
                    let rel_name = t.get_name();
                    match op {
                        None => res.entry(rel_name).or_insert(vec![]).push(f.clone()),
                        Some(CompOperator::Equal) => {
                            if f.get_value()
                                .unwrap()
                                .approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS) {
                                res.entry(rel_name).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(CompOperator::More) => {
                            if *val.as_ref().unwrap() < f.get_value().unwrap() {
                                res.entry(rel_name).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(CompOperator::Less) => {
                            if *val.as_ref().unwrap() > f.get_value().unwrap() {
                                res.entry(rel_name).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(CompOperator::LessEqual) => {
                            if *val.as_ref().unwrap() > f.get_value().unwrap() ||
                               f.get_value()
                                .unwrap()
                                .approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS) {
                                res.entry(rel_name).or_insert(vec![]).push(f.clone())
                            }
                        }
                        Some(CompOperator::MoreEqual) => {
                            if *val.as_ref().unwrap() < f.get_value().unwrap() ||
                               f.get_value()
                                .unwrap()
                                .approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS) {
                                res.entry(rel_name).or_insert(vec![]).push(f.clone())
                            }
                        }
                    }
                }
            }
        }
        res
    }

    pub fn get_funcs(&self, func: &FuncDecl) -> Vec<Arc<GroundedFunc>> {
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
                        (CompOperator::Equal, val) => {
                            if !val.approx_eq_ulps(&curr_func.get_value().unwrap(), FLOAT_EQ_ULPS) {
                                process = false;
                            }
                        }
                        (CompOperator::More, val) => {
                            if val > curr_func.get_value().unwrap() {
                                process = false;
                            }
                        }
                        (CompOperator::Less, val) => {
                            if val < curr_func.get_value().unwrap() {
                                process = false;
                            }
                        }
                        (CompOperator::LessEqual, val) => {
                            if val < curr_func.get_value().unwrap() ||
                               !val.approx_eq_ulps(&curr_func.get_value().unwrap(), FLOAT_EQ_ULPS) {
                                process = false;
                            }
                        }
                        (CompOperator::MoreEqual, val) => {
                            if val > curr_func.get_value().unwrap() ||
                               !val.approx_eq_ulps(&curr_func.get_value().unwrap(), FLOAT_EQ_ULPS) {
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
    fn add_relationship<T: ProofResContext>(&self,
                                            agent: &Representation,
                                            func: Arc<GroundedFunc>,
                                            context: Option<&T>)
                                            -> bool {
        let name = func.get_name();
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
                            f.update(agent, &*func, Some(context.get_id()));
                            f.bms.update_producers(Grounded::Function(Arc::downgrade(&f.clone())),
                                                   context);
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
                    func.bms.last_was_produced(Some(context.get_id()));
                    func.bms.update_producers(Grounded::Function(Arc::downgrade(&func.clone())),
                                              context);
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
                func.bms.last_was_produced(Some(context.get_id()));
                func.bms
                    .update_producers(Grounded::Function(Arc::downgrade(&func.clone())), context);
            }
            lock.insert(name.to_string(), vec![func.clone()]);
            true
        }
    }

    /// Add a grounded relationship of this kind of relationship
    fn add_grounded_relationship(&self, func: Arc<GroundedFunc>) {
        self.members.write().unwrap().push(ClassMember::Func(func));
    }

    fn add_belief(&self, belief: Arc<LogSentence>, parent: &str) {
        let sent_exists = self.beliefs.read().unwrap().contains_key(parent);
        if sent_exists {
            if let Some(ls) = self.beliefs.write().unwrap().get_mut(parent) {
                ls.push(belief)
            }
        } else {
            self.beliefs.write().unwrap().insert(parent.to_string(), vec![belief]);
        }
    }

    fn add_rule(&self, rule: Arc<LogSentence>) {
        self.rules.write().unwrap().push(rule);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn repr_eval_fol() {
        // indicative conditional
        // (true |> true)
        let rep = Representation::new();
        let fol = String::from("
            ( drugDealer[$West,u=1] |> ( scum[$West,u=1] && good[$West,u=0] ) )
            ( drugDealer[$West,u=1] )
        ");
        let q = "( scum[$West,u=1] && good[$West,u=0] )".to_string();
        rep.tell(fol);

        // (false |> none)
        let fol = String::from("
            ( drugDealer[$West,u=1] |> ( scum[$West,u=1] && good[$West,u=0] ) )
            ( drugDealer[$West,u=0] )
        ");
        let ask = "( scum[$West,u=1] && good[$West,u=0] )".to_string();

        // material implication statements
        // none
        let fol = String::from("
            ( drugDealer[$West,u=1] => ( scum[$West,u=1] && good[$West,u=0] ) )
        ");
        let ask = "".to_string();

        // true (true => true)
        let fol = String::from("
            ( drugDealer[$West,u=1] => ( scum[$West,u=1] && good[$West,u=0] ) )
            ( drugDealer[$West,u=1] && scum[$West,u=1] && good[$West,u=0] )
        ");
        let ask = "".to_string();

        // true (false => true)
        let fol = String::from("
            ( drugDealer[$West,u=1] => ( scum[$West,u=1] && good[$West,u=0] ) )
            ( drugDealer[$West,u=0] && scum[$West,u=1] && good[$West,u=0] )
        ");
        let ask = "".to_string();

        // false (true => false)
        let fol = String::from("
            ( drugDealer[$West,u=1] => ( scum[$West,u=1] && good[$West,u=0] ) )
            ( drugDealer[$West,u=1] && scum[$West,u=0] && good[$West,u=1] )
        ");
        let ask = "".to_string();

        // true (false => false)
        let fol = String::from("
            ( drugDealer[$West,u=1] => ( scum[$West,u=1] && good[$West,u=0] ) )
            ( drugDealer[$West,u=0] && scum[$West,u=0] && good[$West,u=1] )
        ");
        let ask = "".to_string();

        // equivalence statements
        // none  (none <=> true)
        let fol = String::from("
            ( drugDealer[$West,u=1] <=> ( scum[$West,u=1] && good[$West,u=0] ) )
            ( scum[$West,u=1] )
        ");
        let ask = "".to_string();

        // is false (false <=> true )
        let fol = String::from("
            ( drugDealer[$West,u=1] <=> ( scum[$West,u=1] && good[$West,u=0] ) )
            ( scum[$West,u=1] )
            ( good[$West,u=0] )
            ( drugDealer[$West,u=0] )
        ");
        let ask = "".to_string();

        // is false (true <=> false )
        let fol = String::from("
            ( drugDealer[$West,u=1] <=> ( scum[$West,u=1] && good[$West,u=0] ) )
            ( scum[$West,u=0] )
            ( good[$West,u=1] )
            ( drugDealer[$West,u=1] )
        ");
        let ask = "".to_string();

        // is true ( true <=> true )
        let fol = String::from("
            ( drugDealer[$West,u=1] <=> ( scum[$West,u=1] && good[$West,u=0] ) )
            ( scum[$West,u=1] )
            ( good[$West,u=0] )
            ( drugDealer[$West,u=1] )
        ");
        let ask = "".to_string();

        // is true ( false <=> false )
        let fol = String::from("
            ( drugDealer[$West,u=1] <=> ( scum[$West,u=1] && good[$West,u=0] ) )
            ( scum[$West,u=0] )
            ( good[$West,u=1] )
            ( drugDealer[$West,u=0] )
        ");
        let ask = "".to_string();
    }
}
