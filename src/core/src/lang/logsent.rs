//! Stores a serie of logical atoms (be them predicates or
//! connectives), that form a well-formed logic formula. These are rulesets
//! for reasoning, cataloging objects into sets/classes, and the relationships
//! between these objects.
//!
//! `LogSentence` types are akin to minimal working compiled programs formed
//! by compounded expressions which will evaluate with the current knowledge
//! when called and perform any subtitution in the knowledge base if pertinent.

pub use self::errors::LogSentErr;
use super::{Date, ParseErrF};

use agent;
use agent::BmsWrapper;

use chrono::UTC;
use lang::common::*;
use lang::parser::*;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

pub type SentID = usize;

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
    particles: Vec<Rc<Particle>>,
    vars: Option<Vec<Arc<Var>>>,
    skolem: Option<Vec<Arc<Skolem>>>,
    root: Option<Rc<Particle>>,
    predicates: (Vec<Rc<Assert>>, Vec<Rc<Assert>>),
    has_time_vars: usize,
    pub created: Date,
    id: Option<SentID>,
}

unsafe impl ::std::marker::Sync for LogSentence {}
unsafe impl ::std::marker::Send for LogSentence {}

impl<'a> LogSentence {
    pub fn new(ast: &Next, context: &mut ParseContext) -> Result<LogSentence, LogSentErr> {
        let mut sent = LogSentence {
            particles: Vec::new(),
            skolem: None,
            vars: None,
            root: None,
            predicates: (vec![], vec![]),
            has_time_vars: 0,
            created: UTC::now(),
            id: None,
        };
        let first = PIntermediate::new(None, None);
        let root = match walk_ast(ast, &mut sent, context, first) {
            Err(err) => return Err(err),
            Ok(root) => {
                let PIntermediate { rhs: root, .. } = root;
                root.unwrap()
            }
        };
        sent.particles.push(root.clone());
        sent.root = Some(root);
        // classify the kind of sentence and check that are correct
        if context.iexpr() {
            let mut lhs: Vec<Rc<Particle>> = vec![];
            correct_iexpr(&sent, &mut lhs)?;
            let lhs: HashSet<_> = lhs.iter().map(|x| &**x as *const Particle).collect();
            let rhs: HashSet<_> = sent.particles
                .iter()
                .filter(|x| x.is_atom())
                .map(|x| &**x as *const Particle)
                .collect();
            let rhs_v: Vec<_> = rhs.difference(&lhs)
                .map(|p| unsafe { &**p })
                .filter(|p| p.is_atom())
                .map(|p| p.clone_pred())
                .collect();
            let lhs_v: Vec<_> = lhs.iter()
                .map(|p| unsafe { &**p })
                .filter(|p| p.is_atom())
                .map(|p| p.clone_pred())
                .collect();
            sent.predicates = (lhs_v, rhs_v);
            sent.iexpr_op_arg_validation()?;
            if sent.vars.is_some() {
                let mut is_normal = false;
                let sent_r = &mut sent;
                for var in sent_r.vars.as_ref().unwrap() {
                    match var.kind {
                        VarKind::Time | VarKind::TimeDecl => {
                            sent_r.has_time_vars += 1;
                            continue;
                        }
                        VarKind::Normal => is_normal = true,
                    }
                }
                // check out that all variables are time or spatial variables
                if !is_normal {
                    context.stype = SentType::Rule;
                }
            }
        } else {
            context.stype = SentType::Rule;
        }
        sent.generate_uid();
        Ok(sent)
    }

    fn iexpr_op_arg_validation(&self) -> Result<(), LogSentErr> {
        // check validity of optional arguments for predicates in the LHS:
        for decl in &self.predicates.0 {
            match **decl {
                Assert::FuncDecl(ref func) => {
                    if let Some(ref opargs) = func.op_args {
                        for arg in opargs {
                            match *arg {
                                OpArg::TimeDecl(_) |
                                OpArg::TimeVar |
                                OpArg::TimeVarAssign(_) => {
                                    return Err(LogSentErr::InvalidOpArg);
                                }
                                _ => {}
                            }
                        }
                    }
                }
                Assert::ClassDecl(ref cls) => {
                    if let Some(ref opargs) = cls.op_args {
                        for arg in opargs {
                            match *arg {
                                OpArg::TimeDecl(_) |
                                OpArg::TimeVar |
                                OpArg::TimeVarAssign(_) => {
                                    return Err(LogSentErr::InvalidOpArg);
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
        // check validity of optional arguments for predicates in the RHS:
        for decl in &self.predicates.1 {
            match **decl {
                Assert::FuncDecl(ref func) => {
                    if let Some(ref opargs) = func.op_args {
                        for arg in opargs {
                            match *arg {
                                OpArg::TimeDecl(_) |
                                OpArg::TimeVar |
                                OpArg::TimeVarFrom(_) => {
                                    return Err(LogSentErr::InvalidOpArg);
                                }
                                _ => {}
                            }
                        }
                    }
                }
                Assert::ClassDecl(ref cls) => {
                    if let Some(ref opargs) = cls.op_args {
                        for arg in opargs {
                            match *arg {
                                OpArg::TimeDecl(_) |
                                OpArg::TimeVar |
                                OpArg::TimeVarFrom(_) => {
                                    return Err(LogSentErr::InvalidOpArg);
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn solve(&self,
                 agent: &agent::Representation,
                 assignments: Option<HashMap<&Var, &agent::VarAssignment>>,
                 context: &mut agent::ProofResult) {
        let root = &self.root;
        let time_assign = self.get_time_assignments(agent, &assignments);
        if self.has_time_vars != time_assign.len() {
            context.result = None;
            return;
        }
        if let Some(res) = root.as_ref()
            .unwrap()
            .solve(agent, &assignments, &time_assign, context) {
            if res {
                if root.as_ref().unwrap().is_icond() {
                    root.as_ref()
                        .unwrap()
                        .substitute(agent, &assignments, &time_assign, context, &false)
                }
                context.result = Some(true);
            } else {
                if root.as_ref().unwrap().is_icond() {
                    root.as_ref()
                        .unwrap()
                        .substitute(agent, &assignments, &time_assign, context, &true)
                }
                context.result = Some(false);
            }
        } else {
            context.result = None;
        }
    }

    fn get_time_assignments(&self,
                            agent: &agent::Representation,
                            var_assign: &Option<HashMap<&Var, &agent::VarAssignment>>)
                            -> HashMap<&Var, Arc<BmsWrapper>> {
        let mut time_assign = HashMap::new();
        'outer: for var in self.vars.as_ref().unwrap() {
            match var.kind {
                VarKind::Time => {
                    for pred in &self.predicates.0 {
                        if pred.get_time_decl(&*var) {
                            let dates = pred.get_dates(agent, var_assign);
                            if dates.is_none() {
                                continue 'outer;
                            }
                            time_assign.insert(&**var, dates.unwrap());
                            continue 'outer;
                        }
                    }
                }
                VarKind::TimeDecl => {
                    let dates = Arc::new(var.get_dates());
                    time_assign.insert(&**var, dates);
                }
                _ => {}
            }
        }
        time_assign
    }

    pub fn extract_all_predicates(self) -> (Vec<Arc<Var>>, Vec<Rc<Assert>>) {
        let LogSentence { vars, particles, .. } = self;
        let vars = if let Some(vars) = vars { vars } else { vec![] };
        let mut preds = vec![];
        let mut checked: HashSet<*const Particle> = HashSet::new();
        for p in particles {
            if !checked.contains(&(&*p as *const Particle)) && p.is_atom() {
                preds.push(p.clone_pred());
                checked.insert(&*p as *const Particle);
            }
        }
        (vars, preds)
    }

    pub fn get_all_predicates(&self) -> Vec<&Assert> {
        let mut v = self.get_all_lhs_predicates();
        let mut v_rhs = self.get_rhs_predicates();
        v.append(&mut v_rhs);
        v
    }

    pub fn get_rhs_predicates(&self) -> Vec<&Assert> {
        let mut v = vec![];
        for p in &self.predicates.1 {
            let p = &**p as &Assert;
            v.push(p);
        }
        v
    }

    pub fn get_all_lhs_predicates(&self) -> Vec<&Assert> {
        let mut v = vec![];
        for p in &self.predicates.0 {
            let p = &**p as &Assert;
            v.push(p);
        }
        v
    }

    pub fn get_lhs_predicates(&self) -> LhsPreds {
        LhsPreds::new(&*self.root.as_ref().unwrap().get_next(0).unwrap(), self)
    }

    fn add_var(&mut self, var: Arc<Var>) {
        if self.vars.is_none() {
            self.vars = Some(Vec::new());
        }
        self.vars.as_mut().unwrap().push(var.clone())
    }

    pub fn get_id(&self) -> usize {
        *self.id.as_ref().unwrap()
    }

    fn add_skolem(&mut self, skolem: Arc<Skolem>) {
        if self.skolem.is_none() {
            self.vars = Some(Vec::new());
        }
        self.skolem.as_mut().unwrap().push(skolem.clone())
    }

    fn add_particle(&mut self, p: Rc<Particle>) {
        self.particles.push(p)
    }

    fn generate_uid(&mut self) {
        use std::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;
        let mut id = vec![];
        for a in &self.particles {
            match **a {
                Particle::Conjunction(_) => id.push(0),
                Particle::Disjunction(_) => id.push(1),
                Particle::Equivalence(_) => id.push(2),
                Particle::Implication(_) => id.push(3),
                Particle::IndConditional(_) => id.push(4),
                Particle::Atom(ref p) => {
                    let mut id_1 = p.generate_uid();
                    id.append(&mut id_1)
                }
            }
        }
        let mut s = DefaultHasher::new();
        id.hash(&mut s);
        self.id = Some(s.finish() as usize);
    }
}

pub struct LhsPreds<'a> {
    preds: Vec<Vec<&'a Assert>>,
    index: Vec<(usize, bool)>,
    curr: usize,
    sent: &'a LogSentence,
    done: HashSet<Vec<usize>>,
}

impl<'a> LhsPreds<'a> {
    fn new(lhs_root: &'a Particle, sent: &'a LogSentence) -> LhsPreds<'a> {
        // visit each node and find the OR statements
        let mut f = vec![];
        let mut l = vec![];
        LhsPreds::dig(lhs_root, &mut f, &mut l);
        if !l.is_empty() {
            f.push(l);
        }
        let idx = vec![(0, false); f.len()];
        LhsPreds {
            preds: f,
            index: idx,
            curr: 0,
            sent: sent,
            done: HashSet::new(),
        }
    }

    fn dig(prev: &'a Particle, f: &mut Vec<Vec<&'a Assert>>, curr: &mut Vec<&'a Assert>) {
        // break up all the assertments into groups of one or more members
        // depending on whether they are childs of an OR node or not
        if let Some(lhs) = prev.get_next(0) {
            if prev.is_disjunction() {
                LhsPreds::dig(lhs, f, curr);
                LhsPreds::dig(&*prev.get_next(1).unwrap(), f, curr);
            } else {
                let mut nlhs = vec![];
                let mut nrhs = vec![];
                LhsPreds::dig(lhs, f, &mut nlhs);
                LhsPreds::dig(&*prev.get_next(1).unwrap(), f, &mut nrhs);
                if !nrhs.is_empty() {
                    f.push(nrhs);
                }
                if !nlhs.is_empty() {
                    f.push(nlhs);
                }
            }
        } else {
            curr.push(prev.pred_ref())
        }
    }

    /// Iterates the permutations of the sentence variable requeriments.
    /// This just takes into consideration the LHS variables.
    pub fn into_sent_req(self) -> SentVarReq<'a> {
        SentVarReq { iter: self }
    }
}

impl<'a> ::std::iter::Iterator for LhsPreds<'a> {
    type Item = Vec<&'a Assert>;
    fn next(&mut self) -> Option<Vec<&'a Assert>> {
        loop {
            let mut max = 0;
            for v in &self.index {
                if v.1 {
                    max += 1;
                }
            }
            if max == self.index.len() {
                return None;
            }

            let mut comb = vec![];
            let mut row = vec![];
            for (i, v) in self.preds.iter().enumerate() {
                let idx = self.index[i].0;
                comb.push(idx);
                row.push(&*v[idx]);
            }
            if self.index[self.curr].0 < self.preds[self.curr].len() {
                let mut max = true;
                for (pos, &mut (ref mut i, _)) in self.index.iter_mut().enumerate() {
                    if (pos != self.curr) && (*i < self.preds[pos].len() - 1) {
                        *i += 1;
                        max = false;
                        break;
                    }
                }
                if max && self.index[self.curr].0 < self.preds[self.curr].len() - 1 {
                    self.index[self.curr].0 += 1;
                } else if max && self.curr != self.index.len() - 1 {
                    for e in &mut self.index {
                        e.0 = 0;
                    }
                    self.index[self.curr].1 = true;
                    self.curr += 1;
                } else {
                    self.index[self.curr].1 = true;
                }
            }
            if !self.done.contains(&comb) {
                self.done.insert(comb);
                return Some(row);
            }
        }
    }
}

pub struct SentVarReq<'a> {
    iter: LhsPreds<'a>,
}

impl<'a> ::std::iter::Iterator for SentVarReq<'a> {
    type Item = HashMap<&'a Var, Vec<&'a Assert>>;
    /// Iterates the permutations of the sentence variable requeriments.
    /// This just takes into consideration the LHS variables.
    fn next(&mut self) -> Option<HashMap<&'a Var, Vec<&'a Assert>>> {
        if let Some(picks) = self.iter.next() {
            let mut requeriments = HashMap::new();
            if self.iter.sent.vars.is_none() {
                return None;
            }
            for var in self.iter.sent.vars.as_ref().unwrap() {
                let mut var_req = Vec::new();
                for a in &picks {
                    if a.contains(var) {
                        var_req.push(*a)
                    }
                }
                requeriments.insert(&**var, var_req);
            }
            Some(requeriments)
        } else {
            None
        }
    }
}

impl ::std::cmp::PartialEq for LogSentence {
    fn eq(&self, other: &LogSentence) -> bool {
        self.id == other.id
    }
}

impl ::std::cmp::Eq for LogSentence {}

impl ::std::hash::Hash for LogSentence {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl fmt::Display for LogSentence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let prelim: String = format!("Sentence({})", self.root.as_ref().unwrap());
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
            } else if c == 'n' && &prelim[i..i + 3] == "n1:" {
                let s = format!("\n{}", tab_times(depth));
                breaks.push((i, s));
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
    next_rhs: Rc<Particle>,
    next_lhs: Rc<Particle>,
}

impl LogicIndCond {
    fn new(lhs: Rc<Particle>, rhs: Rc<Particle>) -> LogicIndCond {
        LogicIndCond {
            next_rhs: rhs,
            next_lhs: lhs,
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
             time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
             context: &mut agent::ProofResult)
             -> Option<bool> {
        if let Some(res) = self.next_lhs.solve(agent, assignments, time_assign, context) {
            if res { Some(true) } else { Some(false) }
        } else {
            None
        }
    }

    #[inline]
    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
                  time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
                  context: &mut agent::ProofResult,
                  rhs: &bool) {
        if self.next_rhs.is_disjunction() || !rhs {
            self.next_rhs.substitute(agent, assignments, time_assign, context, rhs);
        }
    }

    fn get_next(&self, pos: usize) -> &Particle {
        if pos == 0 {
            &*self.next_lhs
        } else {
            &*self.next_rhs
        }
    }
}

impl fmt::Display for LogicIndCond {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n0 = String::from(format!("{}", self.next_lhs));
        let n1 = String::from(format!("{}", self.next_rhs));
        write!(f, "Conditional(n0: {}, n1: {})", n0, n1)
    }
}

#[derive(Debug, Clone)]
struct LogicEquivalence {
    next_rhs: Rc<Particle>,
    next_lhs: Rc<Particle>,
}

impl LogicEquivalence {
    fn new(lhs: Rc<Particle>, rhs: Rc<Particle>) -> LogicEquivalence {
        LogicEquivalence {
            next_rhs: rhs,
            next_lhs: lhs,
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
             time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
             context: &mut agent::ProofResult)
             -> Option<bool> {
        let n0_res;
        if let Some(res) = self.next_lhs.solve(agent, assignments, time_assign, context) {
            if res {
                n0_res = true;
            } else {
                n0_res = false;
            }
        } else {
            return None;
        }
        let n1_res;
        if let Some(res) = self.next_rhs.solve(agent, assignments, time_assign, context) {
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

    fn get_next(&self, pos: usize) -> &Particle {
        if pos == 0 {
            &*self.next_lhs
        } else {
            &*self.next_rhs
        }
    }
}

impl fmt::Display for LogicEquivalence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n0 = String::from(format!("{}", self.next_lhs));
        let n1 = String::from(format!("{}", self.next_rhs));
        write!(f, "Equivalence(n0: {}, n1: {})", n0, n1)
    }
}

#[derive(Debug, Clone)]
struct LogicImplication {
    next_rhs: Rc<Particle>,
    next_lhs: Rc<Particle>,
}

impl LogicImplication {
    fn new(lhs: Rc<Particle>, rhs: Rc<Particle>) -> LogicImplication {
        LogicImplication {
            next_rhs: rhs,
            next_lhs: lhs,
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
             time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
             context: &mut agent::ProofResult)
             -> Option<bool> {
        let n0_res;
        if let Some(res) = self.next_lhs.solve(agent, assignments, time_assign, context) {
            if res {
                n0_res = true;
            } else {
                n0_res = false;
            }
        } else {
            return None;
        }
        let n1_res;
        if let Some(res) = self.next_rhs.solve(agent, assignments, time_assign, context) {
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

    fn get_next(&self, pos: usize) -> &Particle {
        if pos == 0 {
            &*self.next_lhs
        } else {
            &*self.next_rhs
        }
    }
}

impl fmt::Display for LogicImplication {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n0 = String::from(format!("{}", self.next_lhs));
        let n1 = String::from(format!("{}", self.next_rhs));
        write!(f, "Implication(n0: {}, n1: {})", n0, n1)
    }
}

#[derive(Debug, Clone)]
struct LogicConjunction {
    next_rhs: Rc<Particle>,
    next_lhs: Rc<Particle>,
}

impl LogicConjunction {
    fn new(lhs: Rc<Particle>, rhs: Rc<Particle>) -> LogicConjunction {
        LogicConjunction {
            next_rhs: rhs,
            next_lhs: lhs,
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
             time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
             context: &mut agent::ProofResult)
             -> Option<bool> {
        if let Some(res) = self.next_lhs.solve(agent, assignments, time_assign, context) {
            if !res {
                return Some(false);
            }
        } else {
            return None;
        }
        if let Some(res) = self.next_rhs.solve(agent, assignments, time_assign, context) {
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
                  assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
                  time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
                  context: &mut agent::ProofResult,
                  rhs: &bool) {
        self.next_rhs.substitute(agent, assignments, time_assign, context, rhs);
        self.next_lhs.substitute(agent, assignments, time_assign, context, rhs);
    }

    fn get_next(&self, pos: usize) -> &Particle {
        if pos == 0 {
            &*self.next_lhs
        } else {
            &*self.next_rhs
        }
    }
}

impl fmt::Display for LogicConjunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n0 = String::from(format!("{}", self.next_lhs));
        let n1 = String::from(format!("{}", self.next_rhs));
        write!(f, "Conjunction(n0: {}, n1: {})", n0, n1)
    }
}

#[derive(Debug, Clone)]
struct LogicDisjunction {
    next_rhs: Rc<Particle>,
    next_lhs: Rc<Particle>,
}

impl LogicDisjunction {
    fn new(lhs: Rc<Particle>, rhs: Rc<Particle>) -> LogicDisjunction {
        LogicDisjunction {
            next_rhs: rhs,
            next_lhs: lhs,
        }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
             time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
             context: &mut agent::ProofResult)
             -> Option<bool> {
        let n0_res;
        if let Some(res) = self.next_lhs.solve(agent, assignments, time_assign, context) {
            if res {
                n0_res = Some(true);
            } else {
                n0_res = Some(false);
            }
        } else {
            n0_res = None;
        }
        let n1_res;
        if let Some(res) = self.next_rhs.solve(agent, assignments, time_assign, context) {
            if res {
                n1_res = Some(true);
            } else {
                n1_res = Some(false);
            }
        } else {
            n1_res = None;
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
                  assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
                  time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
                  context: &mut agent::ProofResult,
                  rhs: &bool) {
        if *rhs {
            self.next_rhs.substitute(agent, assignments, time_assign, context, rhs)
        } else {
            self.next_lhs.substitute(agent, assignments, time_assign, context, rhs)
        }
    }

    fn get_next(&self, pos: usize) -> &Particle {
        if pos == 0 {
            &*self.next_lhs
        } else {
            &*self.next_rhs
        }
    }
}

impl fmt::Display for LogicDisjunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n0 = String::from(format!("{}", self.next_lhs));
        let n1 = String::from(format!("{}", self.next_rhs));
        write!(f, "Disjunction(n0: {}, n1: {})", n0, n1)
    }
}

#[derive(Debug, Clone)]
struct LogicAtom {
    pred: Rc<Assert>,
}

impl LogicAtom {
    fn new(term: Assert) -> LogicAtom {
        LogicAtom { pred: Rc::new(term) }
    }

    #[inline]
    fn solve(&self,
             agent: &agent::Representation,
             assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
             time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
             context: &mut agent::ProofResult)
             -> Option<bool> {
        if let Some(res) = self.pred.grounded_eq(agent, assignments, time_assign, context) {
            if res { Some(true) } else { Some(false) }
        } else {
            None
        }
    }

    #[inline]
    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
                  time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
                  context: &mut agent::ProofResult) {
        self.pred.substitute(agent, assignments, time_assign, context)
    }

    fn get_name(&self) -> &str {
        self.pred.get_name()
    }

    #[inline]
    fn generate_uid(&self) -> Vec<u8> {
        self.pred.generate_uid()
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
             assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
             time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
             context: &mut agent::ProofResult)
             -> Option<bool> {
        match *self {
            Particle::Conjunction(ref p) => p.solve(agent, assignments, time_assign, context),
            Particle::Disjunction(ref p) => p.solve(agent, assignments, time_assign, context),
            Particle::Implication(ref p) => p.solve(agent, assignments, time_assign, context),
            Particle::Equivalence(ref p) => p.solve(agent, assignments, time_assign, context),
            Particle::IndConditional(ref p) => p.solve(agent, assignments, time_assign, context),
            Particle::Atom(ref p) => p.solve(agent, assignments, time_assign, context),
        }
    }

    #[inline]
    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
                  time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
                  context: &mut agent::ProofResult,
                  rhs: &bool) {
        match *self {
            Particle::IndConditional(ref p) => {
                p.substitute(agent, assignments, time_assign, context, rhs)
            }
            Particle::Disjunction(ref p) => {
                p.substitute(agent, assignments, time_assign, context, rhs)
            }
            Particle::Conjunction(ref p) => {
                p.substitute(agent, assignments, time_assign, context, rhs)
            }
            Particle::Atom(ref p) => p.substitute(agent, assignments, time_assign, context),
            _ => panic!("simag: wrong operator on the rhs of the expression"),
        }
    }

    #[inline]
    fn get_next(&self, pos: usize) -> Option<&Particle> {
        match *self {
            Particle::Conjunction(ref p) => Some(p.get_next(pos)),
            Particle::Disjunction(ref p) => Some(p.get_next(pos)),
            Particle::Implication(ref p) => Some(p.get_next(pos)),
            Particle::Equivalence(ref p) => Some(p.get_next(pos)),
            Particle::IndConditional(ref p) => Some(p.get_next(pos)),
            Particle::Atom(_) => None,
        }
    }

    fn get_next_copy(&self, pos: usize) -> Option<Rc<Particle>> {
        match *self {
            Particle::Conjunction(ref p) => {
                if pos == 0 {
                    Some(p.next_lhs.clone())
                } else {
                    Some(p.next_rhs.clone())
                }
            } 
            Particle::Disjunction(ref p) => {
                if pos == 0 {
                    Some(p.next_lhs.clone())
                } else {
                    Some(p.next_rhs.clone())
                }
            }
            Particle::Implication(ref p) => {
                if pos == 0 {
                    Some(p.next_lhs.clone())
                } else {
                    Some(p.next_rhs.clone())
                }
            }
            Particle::Equivalence(ref p) => {
                if pos == 0 {
                    Some(p.next_lhs.clone())
                } else {
                    Some(p.next_rhs.clone())
                }
            }
            Particle::IndConditional(ref p) => {
                if pos == 0 {
                    Some(p.next_lhs.clone())
                } else {
                    Some(p.next_rhs.clone())
                }
            }
            Particle::Atom(_) => None,
        }
    }

    #[inline]
    fn pred_ref(&self) -> &Assert {
        match *self {
            Particle::Atom(ref p) => &*p.pred,
            _ => panic!(),
        }
    }

    #[inline]
    fn clone_pred(&self) -> Rc<Assert> {
        match *self {
            Particle::Atom(ref p) => p.pred.clone(),
            _ => panic!(),
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
    fn is_disjunction(&self) -> bool {
        match *self {
            Particle::Disjunction(_) => true,
            _ => false,
        }
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

pub struct ParseContext {
    pub stype: SentType,
    pub vars: Vec<Arc<Var>>,
    pub skols: Vec<Arc<Skolem>>,
    shadowing_vars: HashMap<Arc<Var>, (usize, Arc<Var>)>,
    shadowing_skols: HashMap<Arc<Skolem>, (usize, Arc<Skolem>)>,
    in_rhs: bool,
    pub in_assertion: bool,
    pub is_tell: bool,
}

impl Default for ParseContext {
    fn default() -> ParseContext {
        ParseContext::new()
    }
}

impl ParseContext {
    pub fn new() -> ParseContext {
        ParseContext {
            vars: Vec::new(),
            skols: Vec::new(),
            stype: SentType::Expr,
            in_rhs: true,
            shadowing_vars: HashMap::new(),
            shadowing_skols: HashMap::new(),
            in_assertion: false,
            is_tell: false,
        }
    }

    fn iexpr(&self) -> bool {
        match self.stype {
            SentType::IExpr => true,
            SentType::Rule | SentType::Expr => false,
        }
    }
}

#[derive(Debug)]
struct PIntermediate {
    cond: Option<LogicOperator>,
    lhs: Option<Rc<Particle>>,
    rhs: Option<Rc<Particle>>,
    pred: Option<Assert>,
}

impl PIntermediate {
    fn new(op: Option<LogicOperator>, pred: Option<Assert>) -> PIntermediate {
        PIntermediate {
            cond: op,
            lhs: None,
            rhs: None,
            pred: pred,
        }
    }

    fn add_rhs(&mut self, p: Rc<Particle>) {
        self.rhs = Some(p);
    }

    fn add_lhs(&mut self, p: Rc<Particle>) {
        self.lhs = Some(p);
    }

    fn into_final(self, context: &mut ParseContext) -> Particle {
        if self.pred.is_some() {
            Particle::Atom(LogicAtom::new(self.pred.unwrap()))
        } else {
            let PIntermediate { cond, rhs, lhs, .. } = self;
            match cond.unwrap() {
                LogicOperator::ICond => {
                    context.stype = SentType::IExpr;
                    Particle::IndConditional(LogicIndCond::new(lhs.unwrap(), rhs.unwrap()))
                }
                LogicOperator::And => {
                    Particle::Conjunction(LogicConjunction::new(lhs.unwrap(), rhs.unwrap()))
                }
                LogicOperator::Or => {
                    Particle::Disjunction(LogicDisjunction::new(lhs.unwrap(), rhs.unwrap()))
                }
                LogicOperator::Implication => {
                    Particle::Implication(LogicImplication::new(lhs.unwrap(), rhs.unwrap()))
                }
                LogicOperator::Biconditional => {
                    Particle::Equivalence(LogicEquivalence::new(lhs.unwrap(), rhs.unwrap()))
                }
            }
        }
    }
}

fn walk_ast(ast: &Next,
            sent: &mut LogSentence,
            context: &mut ParseContext,
            mut parent: PIntermediate)
            -> Result<PIntermediate, LogSentErr> {

    fn add_particle(particle: PIntermediate,
                    parent: &mut PIntermediate,
                    context: &mut ParseContext,
                    sent: &mut LogSentence) {
        if context.in_rhs {
            let p = Rc::new(particle.into_final(context));
            parent.add_rhs(p.clone());
            sent.add_particle(p);
        } else {
            let p = Rc::new(particle.into_final(context));
            parent.add_lhs(p.clone());
            sent.add_particle(p);
        }
    }

    match *ast {
        Next::Assert(ref decl) => {
            let particle = match *decl {
                AssertBorrowed::ClassDecl(ref decl) => {
                    let cls = match ClassDecl::from(decl, context) {
                        Err(err) => return Err(LogSentErr::Boxed(Box::new(err))),
                        Ok(cls) => cls,
                    };
                    PIntermediate::new(None, Some(Assert::ClassDecl(cls)))
                }
                AssertBorrowed::FuncDecl(ref decl) => {
                    let func = match FuncDecl::from(decl, context) {
                        Err(err) => return Err(LogSentErr::Boxed(Box::new(err))),
                        Ok(func) => func,
                    };
                    PIntermediate::new(None, Some(Assert::FuncDecl(func)))
                }
            };
            add_particle(particle, &mut parent, context, sent);
            Ok(parent)
        }
        Next::ASTNode(ref ast) => {
            let mut v_cnt = 0;
            let mut s_cnt = 0;
            let mut swap_vars: Vec<(usize, Arc<Var>, Arc<Var>)> = Vec::new();
            let mut swap_skolem: Vec<(usize, Arc<Skolem>, Arc<Skolem>)> = Vec::new();

            fn drop_local_vars(context: &mut ParseContext, v_cnt: usize) {
                let l = context.vars.len() - v_cnt;
                let local_vars = context.vars.drain(l..).collect::<Vec<Arc<Var>>>();
                for v in local_vars {
                    if context.shadowing_vars.contains_key(&v) {
                        let (idx, shadowed) = context.shadowing_vars.remove(&v).unwrap();
                        context.vars.insert(idx, shadowed);
                    }
                }
            }

            fn drop_local_skolems(context: &mut ParseContext, s_cnt: usize) {
                let l = context.skols.len() - s_cnt;
                let local_skolem = context.skols.drain(l..).collect::<Vec<Arc<Skolem>>>();
                for v in local_skolem {
                    if context.shadowing_skols.contains_key(&v) {
                        let (idx, shadowed) = context.shadowing_skols.remove(&v).unwrap();
                        context.skols.insert(idx, shadowed);
                    }
                }
            }

            // make vars and add to sent, also add them to local scope context
            if ast.vars.is_some() {
                for v in ast.vars.as_ref().unwrap() {
                    match *v {
                        // if there is a var in context with the current name, shadow it
                        VarDeclBorrowed::Var(ref v) => {
                            let var = match Var::from(v, context) {
                                Err(err) => return Err(LogSentErr::Boxed(Box::new(err))),
                                Ok(val) => Arc::new(val),
                            };
                            for (i, v) in context.vars.iter().enumerate() {
                                if v.name == var.name {
                                    swap_vars.push((i, v.clone(), var.clone()));
                                }
                            }
                            context.vars.push(var.clone());
                            v_cnt += 1;
                            sent.add_var(var);
                        }
                        VarDeclBorrowed::Skolem(ref s) => {
                            let skolem = match Skolem::from(s, context) {
                                Err(err) => return Err(LogSentErr::Boxed(Box::new(err))),
                                Ok(val) => Arc::new(val),
                            };
                            for (i, v) in context.skols.iter().enumerate() {
                                if v.name == skolem.name {
                                    swap_skolem.push((i, v.clone(), skolem.clone()));
                                }
                            }
                            context.skols.push(skolem.clone());
                            s_cnt += 1;
                            sent.add_skolem(skolem.clone());
                        }
                    }
                }
                for &(i, ref shadowed, ref var) in &swap_vars {
                    context.vars.remove(i);
                    context.shadowing_vars.insert(var.clone(), (i, shadowed.clone()));
                }
                for &(i, ref shadowed, ref var) in &swap_skolem {
                    context.skols.remove(i);
                    context.shadowing_skols.insert(var.clone(), (i, shadowed.clone()));
                }
            }
            if ast.logic_op.is_some() {
                let op = PIntermediate::new(ast.logic_op, None);
                let next = walk_ast(&ast.next, sent, context, op)?;
                drop_local_vars(context, v_cnt);
                drop_local_skolems(context, s_cnt);
                add_particle(next, &mut parent, context, sent);
                Ok(parent)
            } else {
                let res = walk_ast(&ast.next, sent, context, parent)?;
                drop_local_vars(context, v_cnt);
                drop_local_skolems(context, s_cnt);
                Ok(res)
            }
        }
        Next::Chain(ref nodes) => {
            let in_side = context.in_rhs;
            if nodes.len() == 2 {
                if let Next::ASTNode(ref node) = nodes[0] {
                    let ASTNode { ref vars, logic_op: ref op, ref next } = **node;
                    if op.is_some() && next.would_assert() {
                        context.in_rhs = false;
                        let new_op = PIntermediate::new(*op, None);
                        let new_op = walk_ast(next, sent, context, new_op)?;
                        context.in_rhs = true;
                        let new_op = walk_ast(&nodes[1], sent, context, new_op)?;
                        context.in_rhs = in_side;
                        add_particle(new_op, &mut parent, context, sent);
                        return Ok(parent);
                    }
                }
                if let Next::ASTNode(ref node) = nodes[1] {
                    let ASTNode { ref vars, logic_op: ref op, ref next } = **node;
                    if op.is_some() && next.would_assert() {
                        context.in_rhs = true;
                        let new_op = PIntermediate::new(*op, None);
                        let new_op = walk_ast(next, sent, context, new_op)?;
                        context.in_rhs = false;
                        let new_op = walk_ast(&nodes[1], sent, context, new_op)?;
                        context.in_rhs = in_side;
                        add_particle(new_op, &mut parent, context, sent);
                        return Ok(parent);
                    }
                }
                context.in_rhs = false;
                let parent = walk_ast(&nodes[0], sent, context, parent)?;
                context.in_rhs = true;
                let parent = walk_ast(&nodes[1], sent, context, parent)?;
                context.in_rhs = in_side;
                Ok(parent)
            } else {
                let op = nodes[0].get_op();
                if !op.is_and() && !op.is_or() {
                    return Err(LogSentErr::IConnectInChain);
                }
                let mut prev_op = PIntermediate::new(Some(op), None);
                context.in_rhs = true;
                prev_op = walk_ast(nodes.last().unwrap(), sent, context, prev_op)?;
                context.in_rhs = false;
                let mut new_op;
                for i in 1..nodes.len() {
                    let i = nodes.len() - (1 + i);
                    new_op = PIntermediate::new(Some(op), None);
                    match *(&nodes[i]) {
                        Next::ASTNode(ref assert) => {
                            if let Some(ref cop) = assert.logic_op {
                                if cop != &op {
                                    return Err(LogSentErr::IConnectInChain);
                                }
                            }
                            prev_op = walk_ast(&assert.next, sent, context, prev_op)?;
                            let c = Rc::new(prev_op.into_final(context));
                            sent.add_particle(c.clone());
                            if i != 0 {
                                new_op.add_rhs(c.clone());
                            } else {
                                context.in_rhs = in_side;
                                if context.in_rhs {
                                    parent.add_rhs(c.clone());
                                } else {
                                    parent.add_lhs(c.clone());
                                }
                                break;
                            }
                            prev_op = new_op;
                        }
                        _ => return Err(LogSentErr::WrongDef),
                    }
                }
                Ok(parent)
            }
        }
        Next::None => Err(LogSentErr::WrongDef),
    }
}

fn correct_iexpr(sent: &LogSentence, lhs: &mut Vec<Rc<Particle>>) -> Result<(), LogSentErr> {

    fn has_icond_child(p: &Particle) -> Result<(), LogSentErr> {
        if let Some(n1_0) = p.get_next(0) {
            if let Particle::IndConditional(_) = *n1_0 {
                return Err(LogSentErr::IExprICondLHS);
            }
            has_icond_child(&*n1_0)?;
        }
        if let Some(n1_1) = p.get_next(1) {
            if let Particle::IndConditional(_) = *n1_1 {
                return Err(LogSentErr::IExprICondLHS);
            }
            has_icond_child(&*n1_1)?;
        }
        Ok(())
    }

    fn wrong_operator(p: &Particle) -> Result<(), LogSentErr> {
        if let Some(n1_0) = p.get_next(0) {
            // test that the lhs does not include any indicative conditional
            if n1_0.is_icond() {
                return Err(LogSentErr::IExprICondLHS);
            }
            has_icond_child(&*n1_0)?;
        }
        // test that the rh-most-s does include only icond or 'OR' connectives
        let mut is_wrong = Ok(());
        if let Some(n1_1) = p.get_next(1) {
            match *n1_1 {
                Particle::IndConditional(_) |
                Particle::Disjunction(_) |
                Particle::Conjunction(_) |
                Particle::Atom(_) => {}
                _ => return Err(LogSentErr::IExprWrongOp),
            }
            is_wrong = wrong_operator(&*n1_1);
        }
        is_wrong
    }

    fn get_lhs_preds(p: Rc<Particle>, lhs: &mut Vec<Rc<Particle>>) {
        if let Some(n1_0) = p.get_next_copy(0) {
            if let Particle::Atom(_) = *n1_0 {
                lhs.push(n1_0.clone());
            }
            get_lhs_preds(n1_0, lhs);
            let n1_1 = p.get_next_copy(1).unwrap();
            if let Particle::Atom(_) = *n1_1 {
                lhs.push(n1_1.clone());
            }
            get_lhs_preds(n1_1, lhs)
        } else {
            lhs.push(p);
        }
    }

    let first: &Particle = &*sent.root.as_ref().unwrap();
    match *first {
        Particle::IndConditional(_) => {}
        _ => return Err(LogSentErr::IExprNotIcond),
    }
    if let Some(n1_0) = first.get_next_copy(0) {
        if let Particle::IndConditional(_) = *n1_0 {
            return Err(LogSentErr::IExprICondLHS);
        }
        get_lhs_preds(n1_0, lhs)
    }
    for p in &sent.particles {
        if let Particle::Atom(ref atom) = **p {
            if !atom.pred.parent_is_grounded() && !atom.pred.parent_is_kw() {
                return Err(LogSentErr::WrongPredicate);
            }
        }
    }
    wrong_operator(first)
}

mod errors {
    use super::*;

    #[derive(Debug, PartialEq)]
    pub enum LogSentErr {
        Boxed(Box<ParseErrF>),
        RuleInclICond(usize),
        IExprWrongOp,
        IExprICondLHS,
        IExprNotIcond,
        IConnectInChain,
        IConnectAfterAnd,
        IConnectAfterOr,
        InvalidOpArg,
        WrongDef,
        WrongPredicate,
    }

    impl Into<ParseErrF> for LogSentErr {
        fn into(self) -> ParseErrF {
            ParseErrF::LogSentErr(self)
        }
    }
}

pub trait LogSentResolution {
    fn grounded_eq(&self,
                   agent: &agent::Representation,
                   assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
                   time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
                   context: &mut agent::ProofResult)
                   -> Option<bool>;

    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<&Var, &agent::VarAssignment>>,
                  time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
                  context: &mut agent::ProofResult);
}

#[cfg(test)]
mod test {
    use super::Particle;
    use lang::parser::*;

    #[test]
    fn parser_icond_exprs() {
        let source = String::from("
            # Err:
            ((let x y z)
             ( ( cde[x,u=1] |> fn::fgh[y,u>0.5;x;z] ) |> hij[y,u=1] )
            )

            # Err:
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
             (( cde[x,u=1] && hij[y,u=1] && fn::fgh[y,u>0.5;x;z] ) |> abc[x,u=1]))
        ");
        let tree = Parser::parse(source, true);
        assert!(tree.is_ok());
        let mut tree = tree.unwrap();

        assert!(tree.pop_front().unwrap().is_err());
        assert!(tree.pop_front().unwrap().is_err());
        assert_eq!(tree.pop_front().unwrap().is_err(), false);

        let sent = match tree.pop_front().unwrap() {
            ParseTree::IExpr(sent) => sent,
            _ => panic!(),
        };
        let root = &**(sent.root.as_ref().unwrap());
        match root {
            &Particle::IndConditional(ref p) => {
                match &*p.next_lhs {
                    &Particle::Conjunction(ref op) => {
                        match &*op.next_lhs {
                            &Particle::Atom(ref atm) => {
                                assert_eq!(atm.get_name(), "cde");
                            }
                            _ => panic!(),
                        };
                        match &*op.next_rhs {
                            &Particle::Conjunction(ref op) => {
                                match &*op.next_lhs {
                                    &Particle::Atom(ref atm) => assert_eq!(atm.get_name(), "hij"),
                                    _ => panic!(),
                                };
                                match &*op.next_rhs {
                                    &Particle::Atom(ref atm) => {
                                        assert_eq!(atm.get_name(), "fgh");
                                    }
                                    _ => panic!(),
                                };
                            }
                            _ => panic!(),
                        }
                    }
                    _ => panic!(),
                }
                match &*p.next_rhs {
                    &Particle::Atom(ref atm) => assert_eq!(atm.get_name(), "abc"),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }
}
