//! Stores a serie of logical atoms (be them predicates or
//! connectives), that form a well-formed logic formula. These are rulesets
//! for reasoning, cataloging objects into sets/classes, and the relationships
//! between these objects.
//!
//! LogSentence types are akin to minimal working compiled programs formed
//! by compounded expressions which will evaluate with the current knowledge
//! when called and perform any subtitution in the knowledge base if pertinent.
use std::str;
use std::collections::HashMap;
use std::fmt;

use chrono::{UTC, DateTime};

use lang::parser::*;
use lang::common::*;
use agent;

/// Type to store a first-order logic complex sentence.
///
/// This sentence is the result of parsing a sentence and compile
/// it in an usable form for the agent to classify and reason about
/// objects and relations, cannot be instantiated directly.
///
/// It's callable when instantiated, accepts as arguments:
/// 1) the working knowledge-base
/// 2) n types which will subsitute the variables in the sentence
///    or a list of string.
#[derive(Debug)]
pub struct LogSentence {
    particles: Vec<Box<Particle>>,
    produced: Vec<*const LogSentence>,
    created: usize,
    vars: Option<Vec<Box<Var>>>,
    skolem: Option<Vec<Box<Skolem>>>,
    root: *const Particle,
    pub var_req: Option<HashMap<*const Var, Vec<*const Assert>>>,
    _id: Vec<u8>,
}

impl<'a> LogSentence {
    pub fn new(ast: &Next, context: &mut Context) -> Result<LogSentence, ParseErrF> {
        let mut sent = LogSentence {
            particles: Vec::new(),
            produced: Vec::new(),
            created: 0,
            skolem: None,
            vars: None,
            root: ::std::ptr::null(),
            var_req: None,
            _id: vec![],
        };
        let r = walk_ast(ast, &mut sent, context);
        if r.is_err() {
            Err(r.unwrap_err())
        } else {
            link_sent_childs(&mut sent);
            // add var requeriments
            let req = sent.get_var_requeriments();
            if req.len() > 0 {
                sent.var_req = Some(req);
            }
            // classify the kind of sentence and check that are correct
            if sent.vars.is_none() {
                if !context.iexpr() {
                    context.stype = SentType::Rule;
                } else {
                    return Err(ParseErrF::RuleInclICond(format!("{}", sent)));
                }
            } else if context.iexpr() {
                if let Err(err) = correct_iexpr(&sent) {
                    return Err(err);
                }
            }
            sent.generate_unique_id();
            Ok(sent)
        }
    }

    pub fn solve(&self,
                 agent: &agent::Representation,
                 assignments: Option<HashMap<*const Var, &agent::VarAssignment>>,
                 context: &mut agent::ProofResult) {
        let root = unsafe { &*(self.root) };
        if let Some(res) = root.solve(agent, &assignments) {
            if res {
                if root.is_icond() {
                    root.substitute(agent, &assignments, &true)
                }
                context.result = Some(true);
            } else {
                if root.is_icond() {
                    root.substitute(agent, &assignments, &false)
                }
                context.result = Some(false);
            }
        } else {
            context.result = None;
        }
    }

    pub fn get_all_predicates(&self) -> Vec<&Assert> {
        let mut v = Vec::new();
        for p in &self.particles {
            match **p {
                Particle::Atom(ref p) => v.push(&p.pred),
                _ => {}
            }
        }
        v
    }

    pub fn get_rhs_predicates(&self) -> Vec<&Assert> {
        unimplemented!()
    }

    pub fn get_lhs_predicates(&self) -> Vec<&Assert> {
        unimplemented!()
    }

    /// Returns the requeriments a variable must meet to fit the criteria in a sentence.
    fn get_var_requeriments(&self) -> HashMap<*const Var, Vec<*const Assert>> {
        let mut requeriments = HashMap::new();
        if self.vars.is_none() {
            return requeriments;
        }
        let mut predicates = Vec::new();
        for p in &self.particles {
            if p.is_atom() {
                predicates.push(&**p)
            }
        }
        for var in self.vars.as_ref().unwrap() {
            let mut var_req = Vec::new();
            for p in &predicates {
                let pred = p.get_atom();
                if pred.contains(var) {
                    var_req.push(&(pred.pred) as *const Assert)
                }
            }
            requeriments.insert(&**var as *const Var, var_req);
        }
        requeriments
    }

    fn add_var(&mut self, var: Box<Var>) {
        if self.vars.is_none() {
            self.vars = Some(Vec::new());
        }
        self.vars.as_mut().unwrap().push(var)
    }

    fn add_skolem(&mut self, skolem: Box<Skolem>) {
        if self.skolem.is_none() {
            self.vars = Some(Vec::new());
        }
        self.skolem.as_mut().unwrap().push(skolem)
    }

    fn add_particle(&mut self, p: Box<Particle>) {
        self.particles.push(p)
    }

    fn generate_unique_id(&mut self) {
        for a in self.particles.iter() {
            match &**a {
                &Particle::Conjunction(_) => self._id.push(0),
                &Particle::Disjunction(_) => self._id.push(1),
                &Particle::Equivalence(_) => self._id.push(2),
                &Particle::Implication(_) => self._id.push(3),
                &Particle::IndConditional(_) => self._id.push(4),
                &Particle::Atom(ref p) => {
                    let mut id_1 = p.get_id();
                    self._id.append(&mut id_1)
                }
            }
        }
    }

    pub fn get_id(&self) -> &[u8] {
        &self._id
    }
}

impl ::std::cmp::PartialEq for LogSentence {
    fn eq(&self, other: &LogSentence) -> bool {
        self._id == other._id
    }
}

impl ::std::cmp::Eq for LogSentence {}

impl ::std::hash::Hash for LogSentence {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        self._id.hash(state);
    }
}

impl fmt::Display for LogSentence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let root = unsafe { &*(self.root) };
        let prelim: String = format!("Sentence({})", root);
        let mut breaks = Vec::new();
        let mut depth = 0_usize;
        use std::iter;
        let tab_times = |depth: usize| -> String { iter::repeat("    ").take(depth).collect() };
        let mut in_atom = false;
        for (i, c) in prelim.chars().enumerate() {
            if c == '(' {
                if (i >= 10) && (&prelim[i - 9..i] == "Predicate") {
                    in_atom = true;
                    continue;
                }
                depth += 1;
                let s = format!("\n{}", tab_times(depth));
                breaks.push((i + 1, s));
            } else if c == ')' {
                if in_atom {
                    in_atom = false;
                    continue;
                }
                depth -= 1;
                let s = format!("\n{}", tab_times(depth));
                breaks.push((i, s));
            } else if c == 'n' {
                if &prelim[i..i + 3] == "n1:" {
                    let s = format!("\n{}", tab_times(depth));
                    breaks.push((i, s))
                }
            }
        }
        let mut slices = Vec::new();
        let mut prev: usize = 0;
        for (pos, b) in breaks.drain(..) {
            slices.push(String::from(&prelim[prev..pos]));
            slices.push(b);
            prev = pos;
        }
        slices.push(String::from(&prelim[prev..]));
        let mut collected = String::new();
        for s in slices {
            collected.push_str(&s)
        }
        write!(f, "{}", collected)
    }
}

pub enum SentType {
    IExpr,
    Expr,
    Rule,
}

#[derive(Debug, Clone)]
struct LogicIndCond {
    parent: *const Particle,
    next: Vec<*const Particle>,
}

impl LogicIndCond {
    fn new() -> LogicIndCond {
        LogicIndCond {
            parent: ::std::ptr::null::<Particle>(),
            next: Vec::with_capacity(2),
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>)
             -> Option<bool> {
        let n0 = unsafe { &*(self.next[0]) };
        if let Some(res) = n0.solve(agent, assignments) {
            if res {
                Some(true)
            } else {
                Some(false)
            }
        } else {
            None
        }
    }

    #[inline]
    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>,
                  rhs: &bool) {
        let n1 = unsafe { &*(self.next[1]) };
        n1.substitute(agent, assignments, rhs)
    }

    fn get_parent(&self) -> Option<&Particle> {
        if self.parent.is_null() {
            None
        } else {
            unsafe { Some(&*(self.parent)) }
        }
    }

    fn get_next(&self, pos: usize) -> Option<&Particle> {
        unsafe {
            if pos == 0 && self.next.len() >= 1 {
                Some(&*(self.next[0]))
            } else if pos == 1 && self.next.len() == 2 {
                Some(&*(self.next[1]))
            } else {
                None
            }
        }
    }
}

impl fmt::Display for LogicIndCond {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n0 = match self.get_next(0) {
            Some(n0) => String::from(format!("{}", n0)),
            None => String::from("none"),
        };
        let n1 = match self.get_next(1) {
            Some(n1) => String::from(format!("{}", n1)),
            None => String::from("none"),
        };
        write!(f, "Conditional(n0: {}, n1: {})", n0, n1)
    }
}

#[derive(Debug, Clone)]
struct LogicEquivalence {
    parent: *const Particle,
    next: Vec<*const Particle>,
}

impl LogicEquivalence {
    fn new() -> LogicEquivalence {
        LogicEquivalence {
            parent: ::std::ptr::null::<Particle>(),
            next: Vec::with_capacity(2),
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>)
             -> Option<bool> {
        let n0 = unsafe { &*(self.next[0]) };
        let n1 = unsafe { &*(self.next[1]) };
        let n0_res;
        if let Some(res) = n0.solve(agent, assignments) {
            if res {
                n0_res = true;
            } else {
                n0_res = false;
            }
        } else {
            return None;
        }
        let n1_res;
        if let Some(res) = n1.solve(agent, assignments) {
            if res {
                n1_res = true;
            } else {
                n1_res = false;
            }
        } else {
            return None;
        }
        if n0_res == n1_res {
            Some(true)
        } else {
            Some(false)
        }
    }

    fn get_parent(&self) -> Option<&Particle> {
        if self.parent.is_null() {
            None
        } else {
            unsafe { Some(&*(self.parent)) }
        }
    }

    fn get_next(&self, pos: usize) -> Option<&Particle> {
        unsafe {
            if pos == 0 && self.next.len() >= 1 {
                Some(&*(self.next[0]))
            } else if pos == 1 && self.next.len() == 2 {
                Some(&*(self.next[1]))
            } else {
                None
            }
        }
    }
}

impl fmt::Display for LogicEquivalence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n0 = match self.get_next(0) {
            Some(n0) => String::from(format!("{}", n0)),
            None => String::from("none"),
        };
        let n1 = match self.get_next(1) {
            Some(n1) => String::from(format!("{}", n1)),
            None => String::from("none"),
        };
        write!(f, "Equivalence(n0: {}, n1: {})", n0, n1)
    }
}

#[derive(Debug, Clone)]
struct LogicImplication {
    parent: *const Particle,
    next: Vec<*const Particle>,
}

impl LogicImplication {
    fn new() -> LogicImplication {
        LogicImplication {
            parent: ::std::ptr::null::<Particle>(),
            next: Vec::with_capacity(2),
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>)
             -> Option<bool> {
        let n0 = unsafe { &*(self.next[0]) };
        let n1 = unsafe { &*(self.next[1]) };
        let n0_res;
        if let Some(res) = n0.solve(agent, assignments) {
            if res {
                n0_res = true;
            } else {
                n0_res = false;
            }
        } else {
            return None;
        }
        let n1_res;
        if let Some(res) = n1.solve(agent, assignments) {
            if res {
                n1_res = true;
            } else {
                n1_res = false;
            }
        } else {
            return None;
        }
        if n0_res && !n1_res {
            Some(false)
        } else {
            Some(true)
        }
    }

    fn get_parent(&self) -> Option<&Particle> {
        if self.parent.is_null() {
            None
        } else {
            unsafe { Some(&*(self.parent)) }
        }
    }

    fn get_next(&self, pos: usize) -> Option<&Particle> {
        unsafe {
            if pos == 0 && self.next.len() >= 1 {
                Some(&*(self.next[0]))
            } else if pos == 1 && self.next.len() == 2 {
                Some(&*(self.next[1]))
            } else {
                None
            }
        }
    }
}

impl fmt::Display for LogicImplication {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n0 = match self.get_next(0) {
            Some(n0) => String::from(format!("{}", n0)),
            None => String::from("none"),
        };
        let n1 = match self.get_next(1) {
            Some(n1) => String::from(format!("{}", n1)),
            None => String::from("none"),
        };
        write!(f, "Implication(n0: {}, n1: {})", n0, n1)
    }
}

#[derive(Debug, Clone)]
struct LogicConjunction {
    parent: *const Particle,
    next: Vec<*const Particle>,
}

impl LogicConjunction {
    fn new() -> LogicConjunction {
        LogicConjunction {
            parent: ::std::ptr::null::<Particle>(),
            next: Vec::with_capacity(2),
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>)
             -> Option<bool> {
        let n0 = unsafe { &*(self.next[0]) };
        let n1 = unsafe { &*(self.next[1]) };
        if let Some(res) = n0.solve(agent, assignments) {
            if !res {
                return Some(false);
            }
        } else {
            return None;
        }
        if let Some(res) = n1.solve(agent, assignments) {
            if !res {
                return Some(false);
            }
        } else {
            return None;
        }
        Some(true)
    }

    #[inline]
    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>,
                  rhs: &bool) {
        let n1 = unsafe { &*(self.next[1]) };
        n1.substitute(agent, assignments, rhs);
        let n0 = unsafe { &*(self.next[0]) };
        n0.substitute(agent, assignments, rhs);
    }

    fn get_parent(&self) -> Option<&Particle> {
        if self.parent.is_null() {
            None
        } else {
            unsafe { Some(&*(self.parent)) }
        }
    }

    fn get_next(&self, pos: usize) -> Option<&Particle> {
        unsafe {
            if pos == 0 && self.next.len() >= 1 {
                Some(&*(self.next[0]))
            } else if pos == 1 && self.next.len() == 2 {
                Some(&*(self.next[1]))
            } else {
                None
            }
        }
    }
}

impl fmt::Display for LogicConjunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n0 = match self.get_next(0) {
            Some(n0) => String::from(format!("{}", n0)),
            None => String::from("none"),
        };
        let n1 = match self.get_next(1) {
            Some(n1) => String::from(format!("{}", n1)),
            None => String::from("none"),
        };
        write!(f, "Conjunction(n0: {}, n1: {})", n0, n1)
    }
}

#[derive(Debug, Clone)]
struct LogicDisjunction {
    parent: *const Particle,
    next: Vec<*const Particle>,
}

impl LogicDisjunction {
    fn new() -> LogicDisjunction {
        LogicDisjunction {
            parent: ::std::ptr::null::<Particle>(),
            next: Vec::with_capacity(2),
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>)
             -> Option<bool> {
        let n0 = unsafe { &*(self.next[0]) };
        let n1 = unsafe { &*(self.next[1]) };
        let n0_res;
        if let Some(res) = n0.solve(agent, assignments) {
            if res {
                n0_res = true;
            } else {
                n0_res = false;
            }
        } else {
            return None;
        }
        let n1_res;
        if let Some(res) = n1.solve(agent, assignments) {
            if res {
                n1_res = true;
            } else {
                n1_res = false;
            }
        } else {
            return None;
        }
        if n0_res != n1_res {
            Some(true)
        } else {
            Some(false)
        }
    }

    #[inline]
    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>,
                  rhs: &bool) {
        if *rhs {
            let n1 = unsafe { &*(self.next[1]) };
            n1.substitute(agent, assignments, rhs)
        } else {
            let n0 = unsafe { &*(self.next[0]) };
            n0.substitute(agent, assignments, rhs)
        }
    }

    fn get_parent(&self) -> Option<&Particle> {
        if self.parent.is_null() {
            None
        } else {
            unsafe { Some(&*(self.parent)) }
        }
    }

    fn get_next(&self, pos: usize) -> Option<&Particle> {
        unsafe {
            if pos == 0 && self.next.len() >= 1 {
                Some(&*(self.next[0]))
            } else if pos == 1 && self.next.len() == 2 {
                Some(&*(self.next[1]))
            } else {
                None
            }
        }
    }
}

impl fmt::Display for LogicDisjunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n0 = match self.get_next(0) {
            Some(n0) => String::from(format!("{}", n0)),
            None => String::from("none"),
        };
        let n1 = match self.get_next(1) {
            Some(n1) => String::from(format!("{}", n1)),
            None => String::from("none"),
        };
        write!(f, "Disjunction(n0: {}, n1: {})", n0, n1)
    }
}

#[derive(Debug, Clone)]
struct LogicAtom {
    parent: *const Particle,
    pred: Assert,
}

impl LogicAtom {
    fn new(term: Assert) -> LogicAtom {
        LogicAtom {
            parent: ::std::ptr::null::<Particle>(),
            pred: term,
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>)
             -> Option<bool> {
        if let Some(res) = self.pred.equal_to_grounded(agent, assignments) {
            if res {
                Some(true)
            } else {
                Some(false)
            }
        } else {
            None
        }
    }

    #[inline]
    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>) {
        self.pred.substitute(agent, assignments)
    }

    #[inline]
    fn contains(&self, pred: &Var) -> bool {
        self.pred.contains(pred)
    }

    fn get_name(&self) -> &str {
        self.pred.get_name()
    }

    fn get_parent(&self) -> Option<&Particle> {
        if self.parent.is_null() {
            None
        } else {
            unsafe { Some(&*(self.parent)) }
        }
    }

    #[inline]
    fn get_id(&self) -> Vec<u8> {
        self.pred.get_id()
    }
}

impl fmt::Display for LogicAtom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Predicate({})", self.get_name())
    }
}

#[derive(Debug)]
enum Particle {
    Atom(LogicAtom),
    Conjunction(LogicConjunction),
    Disjunction(LogicDisjunction),
    Implication(LogicImplication),
    Equivalence(LogicEquivalence),
    IndConditional(LogicIndCond),
}

impl Particle {
    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>)
             -> Option<bool> {
        match *self {
            Particle::Conjunction(ref p) => p.solve(agent, assignments),
            Particle::Disjunction(ref p) => p.solve(agent, assignments),
            Particle::Implication(ref p) => p.solve(agent, assignments),
            Particle::Equivalence(ref p) => p.solve(agent, assignments),
            Particle::IndConditional(ref p) => p.solve(agent, assignments),
            Particle::Atom(ref p) => p.solve(agent, assignments),
        }
    }

    #[inline]
    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>,
                  rhs: &bool) {
        match *self {
            Particle::IndConditional(ref p) => p.substitute(agent, assignments, rhs),
            Particle::Disjunction(ref p) => p.substitute(agent, assignments, rhs),
            Particle::Conjunction(ref p) => p.substitute(agent, assignments, rhs),
            Particle::Atom(ref p) => p.substitute(agent, assignments),
            _ => panic!("simag: wrong operator on the rhs of the expression"),
        }
    }

    #[inline]
    fn get_disjunction(&mut self) -> Result<&mut LogicDisjunction, ParseErrF> {
        match *self {
            Particle::Disjunction(ref mut p) => Ok(p),
            _ => Err(ParseErrF::IConnectAfterOr),
        }
    }

    #[inline]
    fn get_conjunction(&mut self) -> Result<&mut LogicConjunction, ParseErrF> {
        match *self {
            Particle::Conjunction(ref mut p) => Ok(p),
            _ => Err(ParseErrF::IConnectAfterOr),
        }
    }

    #[inline]
    fn add_parent(&mut self, ptr: *const Particle) {
        match *self {
            Particle::Conjunction(ref mut p) => {
                p.parent = ptr;
            }
            Particle::Disjunction(ref mut p) => {
                p.parent = ptr;
            }
            Particle::Implication(ref mut p) => {
                p.parent = ptr;
            }
            Particle::Equivalence(ref mut p) => {
                p.parent = ptr;
            }
            Particle::IndConditional(ref mut p) => {
                p.parent = ptr;
            }
            Particle::Atom(ref mut p) => {
                p.parent = ptr;
            }
        }
    }

    #[inline]
    fn get_parent(&self) -> Option<&Particle> {
        match *self {
            Particle::Conjunction(ref p) => p.get_parent(),
            Particle::Disjunction(ref p) => p.get_parent(),
            Particle::Implication(ref p) => p.get_parent(),
            Particle::Equivalence(ref p) => p.get_parent(),
            Particle::IndConditional(ref p) => p.get_parent(),
            Particle::Atom(ref p) => p.get_parent(),
        }
    }

    #[inline]
    fn get_next(&self, pos: usize) -> Option<&Particle> {
        match *self {
            Particle::Conjunction(ref p) => p.get_next(pos),
            Particle::Disjunction(ref p) => p.get_next(pos),
            Particle::Implication(ref p) => p.get_next(pos),
            Particle::Equivalence(ref p) => p.get_next(pos),
            Particle::IndConditional(ref p) => p.get_next(pos),
            Particle::Atom(_) => None,
        }
    }

    #[inline]
    fn get_atom(&self) -> &LogicAtom {
        match *self {
            Particle::Atom(ref p) => p,
            _ => panic!("simag: expected a predicate, found an operator"),
        }
    }

    #[inline]
    fn is_atom(&self) -> bool {
        match *self {
            Particle::Atom(_) => true,
            _ => false,
        }
    }

    #[inline]
    fn is_icond(&self) -> bool {
        match *self {
            Particle::IndConditional(_) => true,
            _ => false,
        }
    }

    #[inline]
    fn is_conj(&self) -> bool {
        match *self {
            Particle::Conjunction(_) => true,
            _ => false,
        }
    }

    #[inline]
    fn is_disj(&self) -> bool {
        match *self {
            Particle::Disjunction(_) => true,
            _ => false,
        }
    }

    #[inline]
    fn is_equiv(&self) -> bool {
        match *self {
            Particle::Equivalence(_) => true,
            _ => false,
        }
    }

    #[inline]
    fn is_impl(&self) -> bool {
        match *self {
            Particle::Implication(_) => true,
            _ => false,
        }
    }

    fn add_rhs(&mut self, next: *mut Particle) {
        match *self {
            Particle::Conjunction(ref mut p) => {
                if p.next.len() == 2 {
                    p.next.pop();
                }
                p.next.push(next)
            }
            Particle::Disjunction(ref mut p) => {
                if p.next.len() == 2 {
                    p.next.pop();
                }
                p.next.push(next)
            }
            Particle::Implication(ref mut p) => {
                if p.next.len() == 2 {
                    p.next.pop();
                }
                p.next.push(next)
            }
            Particle::Equivalence(ref mut p) => {
                if p.next.len() == 2 {
                    p.next.pop();
                }
                p.next.push(next)
            }
            Particle::IndConditional(ref mut p) => {
                if p.next.len() == 2 {
                    p.next.pop();
                }
                p.next.push(next)
            }
            _ => panic!("simag: expected an operator, found a predicate instead"),
        };
    }

    fn add_lhs(&mut self, next: *mut Particle) {
        match *self {
            Particle::Conjunction(ref mut p) => {
                if p.next.len() == 1 {
                    p.next.insert(0, next)
                } else if p.next.len() == 0 {
                    p.next.push(next)
                } else {
                    p.next.remove(0);
                    p.next.insert(0, next)
                }
            }
            Particle::Disjunction(ref mut p) => {
                if p.next.len() == 1 {
                    p.next.insert(0, next)
                } else if p.next.len() == 0 {
                    p.next.push(next)
                } else {
                    p.next.remove(0);
                    p.next.insert(0, next)
                }
            }
            Particle::Implication(ref mut p) => {
                if p.next.len() == 1 {
                    p.next.insert(0, next)
                } else if p.next.len() == 0 {
                    p.next.push(next)
                } else {
                    p.next.remove(0);
                    p.next.insert(0, next)
                }
            }
            Particle::Equivalence(ref mut p) => {
                if p.next.len() == 1 {
                    p.next.insert(0, next)
                } else if p.next.len() == 0 {
                    p.next.push(next)
                } else {
                    p.next.remove(0);
                    p.next.insert(0, next)
                }
            }
            Particle::IndConditional(ref mut p) => {
                if p.next.len() == 1 {
                    p.next.insert(0, next)
                } else if p.next.len() == 0 {
                    p.next.push(next)
                } else {
                    p.next.remove(0);
                    p.next.insert(0, next)
                }
            }
            _ => panic!("simag: expected an operator, found a predicate instead"),
        };
    }
}

impl fmt::Display for Particle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Particle::Atom(ref p) => write!(f, "{}", p),
            Particle::Conjunction(ref p) => write!(f, "{}", p),
            Particle::Disjunction(ref p) => write!(f, "{}", p),
            Particle::Equivalence(ref p) => write!(f, "{}", p),
            Particle::Implication(ref p) => write!(f, "{}", p),
            Particle::IndConditional(ref p) => write!(f, "{}", p),
        }
    }
}

// infrastructure to construct compiled logsentences:

pub struct Context {
    pub stype: SentType,
    pub vars: Vec<*const Var>,
    pub skols: Vec<*const Skolem>,
    aliasing_vars: HashMap<*const Var, (usize, *const Var)>,
    aliasing_skols: HashMap<*const Skolem, (usize, *const Skolem)>,
    from_chain: bool,
    in_rhs: bool,
    pub in_assertion: bool,
}

impl Context {
    pub fn new() -> Context {
        Context {
            vars: Vec::new(),
            skols: Vec::new(),
            stype: SentType::Expr,
            in_rhs: true,
            from_chain: false,
            aliasing_vars: HashMap::new(),
            aliasing_skols: HashMap::new(),
            in_assertion: false,
        }
    }

    fn iexpr(&self) -> bool {
        match self.stype {
            SentType::Expr => false,
            SentType::IExpr => true,
            SentType::Rule => false,
        }
    }
}

fn walk_ast(ast: &Next,
            sent: &mut LogSentence,
            context: &mut Context)
            -> Result<*mut Particle, ParseErrF> {
    match *ast {
        Next::Assert(ref decl) => {
            let mut particle = Box::new(match decl {
                &AssertBorrowed::ClassDecl(ref decl) => {
                    let cls = match ClassDecl::from(decl, context) {
                        Err(err) => return Err(err),
                        Ok(cls) => cls,
                    };
                    let atom = LogicAtom::new(Assert::ClassDecl(cls));
                    Particle::Atom(atom)
                }
                &AssertBorrowed::FuncDecl(ref decl) => {
                    let func = match FuncDecl::from(decl, context) {
                        Err(err) => return Err(err),
                        Ok(func) => func,
                    };
                    let atom = LogicAtom::new(Assert::FuncDecl(func));
                    Particle::Atom(atom)
                }
            });
            let res = &mut *particle as *mut Particle;
            sent.add_particle(particle);
            context.from_chain = false;
            Ok(res)
        }
        Next::ASTNode(ref ast) => {
            let mut v_cnt = 0;
            let mut s_cnt = 0;
            let mut swap_vars: Vec<(usize, *const Var, *const Var)> = Vec::new();
            let mut swap_skolem: Vec<(usize, *const Skolem, *const Skolem)> = Vec::new();

            fn drop_local_vars(context: &mut Context, v_cnt: usize) {
                let l = context.vars.len() - v_cnt;
                let local_vars = context.vars.drain(l..).collect::<Vec<*const Var>>();
                for v in local_vars {
                    if context.aliasing_vars.contains_key(&v) {
                        let (idx, aliased) = context.aliasing_vars.remove(&v).unwrap();
                        context.vars.insert(idx, aliased);
                    }
                }
            }

            fn drop_local_skolems(context: &mut Context, s_cnt: usize) {
                let l = context.skols.len() - s_cnt;
                let local_skolem = context.skols.drain(l..).collect::<Vec<*const Skolem>>();
                for v in local_skolem {
                    if context.aliasing_skols.contains_key(&v) {
                        let (idx, aliased) = context.aliasing_skols.remove(&v).unwrap();
                        context.skols.insert(idx, aliased);
                    }
                }
            }

            // make vars and add to sent, also add them to local scope context
            if ast.vars.is_some() {
                for v in ast.vars.as_ref().unwrap() {
                    match *v {
                        // if there is a var in context with the current name, alias it
                        VarDeclBorrowed::Var(ref v) => {
                            let var = match Var::from(v, context) {
                                Err(err) => return Err(err),
                                Ok(val) => Box::new(val),
                            };
                            for (i, v) in context.vars.iter().enumerate() {
                                let v_r: &Var = unsafe { &**v };
                                if v_r.name == var.name {
                                    swap_vars.push((i, *v, &*var as *const Var));
                                }
                            }
                            context.vars.push(&*var);
                            v_cnt += 1;
                            sent.add_var(var);
                        }
                        VarDeclBorrowed::Skolem(ref s) => {
                            let skolem = match Skolem::from(s, context) {
                                Err(err) => return Err(err),
                                Ok(val) => Box::new(val),
                            };
                            for (i, v) in context.skols.iter().enumerate() {
                                let v_r: &Skolem = unsafe { &**v };
                                if v_r.name == skolem.name {
                                    swap_skolem.push((i, *v, &*skolem as *const Skolem));
                                }
                            }
                            context.skols.push(&*skolem);
                            s_cnt += 1;
                            sent.add_skolem(skolem);
                        }
                    }
                }
                for &(i, aliased, var) in &swap_vars {
                    context.vars.remove(i);
                    context.aliasing_vars.insert(var, (i, aliased));
                }
                for &(i, aliased, var) in &swap_skolem {
                    context.skols.remove(i);
                    context.aliasing_skols.insert(var, (i, aliased));
                }
            }
            if ast.logic_op.is_some() {
                let mut op = Box::new(match ast.logic_op.as_ref().unwrap() {
                    &LogicOperator::ICond => {
                        context.stype = SentType::IExpr;
                        Particle::IndConditional(LogicIndCond::new())
                    }
                    &LogicOperator::And => Particle::Conjunction(LogicConjunction::new()),
                    &LogicOperator::Or => Particle::Disjunction(LogicDisjunction::new()),
                    &LogicOperator::Implication => Particle::Implication(LogicImplication::new()),
                    &LogicOperator::Biconditional => Particle::Equivalence(LogicEquivalence::new()),
                });
                let ptr = &mut *op as *mut Particle;
                let next = match walk_ast(&ast.next, sent, context) {
                    Ok(opt) => opt,
                    Err(err) => return Err(err),
                };
                drop_local_vars(context, v_cnt);
                drop_local_skolems(context, s_cnt);
                if context.in_rhs {
                    op.add_rhs(next);
                } else {
                    op.add_lhs(next);
                }
                sent.add_particle(op);
                Ok(ptr)
            } else {
                let res = walk_ast(&ast.next, sent, context);
                drop_local_vars(context, v_cnt);
                drop_local_skolems(context, s_cnt);
                res
            }
        }
        Next::Chain(ref nodes) => {
            if nodes.len() == 2 {
                let in_side = context.in_rhs;
                // walk lhs
                context.in_rhs = false;
                let lhs_ptr = match walk_ast(&nodes[0], sent, context) {
                    Ok(ptr) => ptr,
                    Err(err) => return Err(err),

                };
                let mut lhs_r: &mut Particle = unsafe { &mut *lhs_ptr };
                let lhs_is_atom = lhs_r.is_atom();
                // walk rhs
                context.in_rhs = true;
                let rhs_ptr = match walk_ast(&nodes[1], sent, context) {
                    Ok(ptr) => ptr,
                    Err(err) => return Err(err),
                };
                let mut rhs_r: &mut Particle = unsafe { &mut *rhs_ptr };
                let rhs_is_atom = rhs_r.is_atom();
                // lhs is connective and rhs isn't
                let return_rhs;
                if !lhs_is_atom && rhs_is_atom {
                    return_rhs = false;
                    lhs_r.add_rhs(rhs_ptr);
                } else if lhs_is_atom && !rhs_is_atom {
                    return_rhs = true;
                    rhs_r.add_lhs(lhs_ptr);
                } else {
                    if context.from_chain {
                        // rhs comes from a chain, parent is lhs op
                        return_rhs = false;
                        lhs_r.add_rhs(rhs_ptr);
                    } else {
                        // lhs comes from a chain, parent is rhs op
                        return_rhs = true;
                        rhs_r.add_lhs(lhs_ptr);
                    }
                }
                context.in_rhs = in_side;
                context.from_chain = true;
                if return_rhs {
                    Ok(rhs_ptr)
                } else {
                    Ok(lhs_ptr)
                }
            } else {
                let len = nodes.len() - 1;
                let first: *mut Particle = walk_ast(&nodes[0], sent, context)?;
                let operator;
                let r = unsafe { &*first };
                match r {
                    &Particle::Conjunction(_) => {
                        operator = LogicOperator::And;
                    }
                    &Particle::Disjunction(_) => {
                        operator = LogicOperator::Or;
                    }
                    _ => return Err(ParseErrF::IConnectInChain),
                }
                let mut prev = first;
                if operator.is_and() {
                    for i in 1..len {
                        let ptr = walk_ast(&nodes[i], sent, context)?;
                        let a = unsafe { &mut *ptr };
                        let is_conj = a.get_conjunction();
                        if is_conj.is_err() {
                            return Err(is_conj.unwrap_err());
                        } else {
                            let r = unsafe { &mut *prev };
                            r.add_rhs(ptr);
                            prev = ptr;
                        }
                    }
                } else {
                    for i in 1..len {
                        let ptr = walk_ast(&nodes[i], sent, context)?;
                        let a = unsafe { &mut *ptr };
                        let is_disj = a.get_disjunction();
                        if is_disj.is_err() {
                            return Err(is_disj.unwrap_err());
                        } else {
                            let r = unsafe { &mut *prev };
                            r.add_rhs(ptr);
                            prev = ptr;
                        }
                    }
                }
                let last = walk_ast(&nodes[len], sent, context).unwrap();
                let r = unsafe { &mut *prev };
                r.add_rhs(last);
                context.from_chain = true;
                Ok(first)
            }
        }
        Next::None => Err(ParseErrF::WrongDef),
    }
}

fn link_sent_childs(sent: &mut LogSentence) {
    // add entry point of the sentence and parent to childs
    let mut addresses: Vec<*const Particle> = Vec::new();
    for p in &sent.particles {
        if let Some(a) = p.get_next(0) {
            addresses.push(&*a as *const Particle)
        }
        if let Some(a) = p.get_next(1) {
            addresses.push(&*a as *const Particle)
        }
    }
    let mut childs: HashMap<usize, (Option<usize>, Option<usize>)> = HashMap::new();
    for a in &sent.particles {
        if !addresses.contains(&(&**a as *const Particle)) {
            sent.root = &**a as *const Particle;
        }
        let a_int = &**a as *const Particle as usize;
        let c0 = match a.get_next(0) {
            Some(a) => Some(&*a as *const Particle as usize),
            None => None,
        };
        let c1 = match a.get_next(1) {
            Some(a) => Some(&*a as *const Particle as usize),
            None => None,
        };
        childs.insert(a_int, (c0, c1));
    }
    unsafe {
        for (p, (n0, n1)) in childs {
            if let Some(a) = n0 {
                let a = &mut *(a as *mut Particle);
                a.add_parent(p as *const Particle);
            }
            if let Some(a) = n1 {
                let a = &mut *(a as *mut Particle);
                a.add_parent(p as *const Particle);
            }
        }
    }
}

fn correct_iexpr(sent: &LogSentence) -> Result<(), ParseErrF> {
    fn has_icond_child(p: &Particle) -> Result<(), ParseErrF> {
        if let Some(n1_0) = p.get_next(0) {
            match n1_0 {
                &Particle::IndConditional(_) => return Err(ParseErrF::IExprICondLHS),
                _ => {}
            }
            if let Err(err) = has_icond_child(n1_0) {
                return Err(err);
            }
        }
        if let Some(n1_1) = p.get_next(1) {
            match n1_1 {
                &Particle::IndConditional(_) => return Err(ParseErrF::IExprICondLHS),
                _ => {}
            }
            if let Err(err) = has_icond_child(n1_1) {
                return Err(err);
            }
        }
        Ok(())
    }

    fn wrong_operator(p: &Particle) -> Result<(), ParseErrF> {
        if let Some(n1_0) = p.get_next(0) {
            // test that the lhs does not include any indicative conditional
            if let Err(err) = has_icond_child(n1_0) {
                return Err(err);
            }
        }
        // test that the rh-most-s does include only icond or 'OR' connectives
        let mut is_wrong = Ok(());
        if let Some(n1_1) = p.get_next(1) {
            match n1_1 {
                &Particle::IndConditional(_) => {}
                &Particle::Disjunction(_) => {}
                &Particle::Conjunction(_) => {}
                &Particle::Atom(_) => {}
                _ => return Err(ParseErrF::IExprWrongOp),
            }
            is_wrong = wrong_operator(n1_1);
        }
        is_wrong
    }

    let first: &Particle = unsafe { &*(sent.root) };
    if let Some(n1_0) = first.get_next(0) {
        match n1_0 {
            &Particle::IndConditional(_) => return Err(ParseErrF::IExprNotIcond),
            _ => {}
        }
    }
    for p in &sent.particles {
        match **p {
            Particle::Atom(ref atom) => {
                if !atom.pred.parent_is_grounded() {
                    return Err(ParseErrF::WrongPredicate)
                }
            }
            _ => {}
        }
    }
    wrong_operator(first)
}

#[cfg(test)]
mod test {
    use super::*;
    use lang::parser::*;

    #[test]
    fn icond_exprs() {
        let source = String::from("
            # Err:
            ((let x y z)
             ( ( cde[x,u=1] |> fn::fgh[y,u>0.5;x;z] ) |> hij[y,u=1] )
            )

            # Err
            ((let x y z)
             ( abc[x,u=1]  |> (( cde[x,u=1] |> fn::fgh[y,u>0.5;x;z] ) && hij[y,u=1] ))
            )

            # Ok:
            ((let x y z)
             ( abc[x,u=1]  |> (
                 ( cde[x,u=1] && fn::fgh[y,u>0.5;x;z] ) |> hij[y,u=1]
             )))

            # Ok:
            (( let x y z )
             (( american[x,u=1] && weapon[y,u=1] && fn::sells[y,u>0.5;x;z] ) |> criminal[x,u=1]))
        ");

        let tree = Parser::parse(source);
        assert!(tree.is_ok());
        let mut tree = tree.unwrap();

        let sent = match tree.pop_front() {
            Some(ParseTree::ParseErr(ParseErrF::IExprWrongOp)) => {}
            _ => panic!(),
        };

        let sent = match tree.pop_front() {
            Some(ParseTree::ParseErr(ParseErrF::IExprWrongOp)) => {}
            _ => panic!(),
        };

        let sent = match tree.pop_front() {
            Some(ParseTree::IExpr(sent)) => sent,
            _ => panic!(),
        };

        unsafe {
            let sent = match tree.pop_front().unwrap() {
                ParseTree::IExpr(sent) => sent,
                _ => panic!(),
            };
            let root = &*(sent.root.unwrap());
            match root {
                &Particle::IndConditional(ref p) => {
                    match &*(p.next[0]) {
                        &Particle::Conjunction(ref op) => {
                            match &*(op.next[0]) {
                                &Particle::Atom(ref atm) => {
                                    assert_eq!(atm.get_name(), "american");
                                    match atm.get_parent().unwrap() {
                                        &Particle::Conjunction(ref op) => {
                                            match &*(op.next[0]) {
                                                &Particle::Atom(ref atm) => {
                                                    assert_eq!(atm.get_name(), "american")
                                                }
                                                _ => panic!(),
                                            };
                                        }
                                        _ => panic!(),
                                    }
                                }
                                _ => panic!(),
                            };
                            match &*(op.next[1]) {
                                &Particle::Conjunction(ref op) => {
                                    match &*(op.next[0]) {
                                        &Particle::Atom(ref atm) => {
                                            assert_eq!(atm.get_name(), "weapon")
                                        }
                                        _ => panic!(),
                                    };
                                    match &*(op.next[1]) {
                                        &Particle::Atom(ref atm) => {
                                            assert_eq!(atm.get_name(), "sells");
                                            match atm.get_parent().unwrap() {
                                                &Particle::Conjunction(ref op) => {
                                                    match &*(op.next[0]) {
                                                        &Particle::Atom(ref atm) => {
                                                            assert_eq!(atm.get_name(), "weapon")
                                                        }
                                                        _ => panic!(),
                                                    };
                                                }
                                                _ => panic!(),
                                            }
                                        }
                                        _ => panic!(),
                                    };
                                }
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                    match &*(p.next[1]) {
                        &Particle::Atom(ref atm) => assert_eq!(atm.get_name(), "criminal"),
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            };
        }
    }
}
