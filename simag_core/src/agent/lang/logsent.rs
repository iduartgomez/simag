//! Stores a serie of logical atoms (be them predicates or
//! connectives), that form a well-formed logic formula. These are rulesets
//! for reasoning, cataloging objects into sets/classes, and the relationships
//! between these objects.
//!
//! `LogSentence` types are akin to minimal working compiled programs formed
//! by compounded expressions which will evaluate with the current knowledge
//! when called and perform any subtitution in the knowledge base if pertinent.

use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt;
use std::iter::FromIterator;
use std::sync::Arc;

use chrono::Utc;

use self::ast_walker::PIntermediate;
use super::{
    cls_decl::ClassDecl,
    common::{Assert, Grounded, OpArg},
    fn_decl::FuncDecl,
    gr_func::GroundedFunc,
    gr_memb::GroundedMemb,
    parser::{ASTNode, AssertBorrowed, LogicOperator, Scope, VarDeclBorrowed},
    ParseErrF, Skolem, Time, Var, VarKind,
};
use crate::agent::{kb::bms::BmsWrapper, kb::repr::Representation, kb::VarAssignment};
pub(in crate::agent) type SentID = u64;
pub use self::errors::LogSentErr;

/// Type to store a first-order logic complex sentence.
///
/// This sentence is the result of parsing a sentence and compile
/// it in an usable form for the agent to classify and reason about
/// objects and relations, cannot be instantiated directly.
#[derive(Debug)]
pub(in crate::agent) struct LogSentence {
    pub id: SentID,
    pub created: Time,
    pub has_time_vars: usize,
    particles: Vec<Particle>,
    vars: Vec<Arc<Var>>,
    skolem: Vec<Arc<Skolem>>,
    /// position of the root at the particles vec
    root: usize,
    predicates: (Vec<usize>, Vec<usize>),
    sent_kind: SentKind,
}

impl<'a> LogSentence {
    pub fn try_new(ast: &ASTNode, context: &mut ParseContext) -> Result<LogSentence, LogSentErr> {
        ast_walker::walk_ast(ast, context, PIntermediate::new())?;

        let id = LogSentence::generate_uid(&context.particles);
        let mut particles = HashSet::new();
        let mut skolem = Vec::new();
        let mut vars = Vec::new();
        std::mem::swap(&mut particles, &mut context.particles);
        std::mem::swap(&mut skolem, &mut context.all_skols);
        std::mem::swap(&mut vars, &mut context.all_vars);

        let mut sent = LogSentence {
            root: particles.len() - 1,
            particles: Vec::from_iter(particles.into_iter()),
            skolem,
            vars,
            predicates: (vec![], vec![]),
            has_time_vars: 0,
            created: Utc::now(),
            id,
            sent_kind: context.stype,
        };
        sent.particles
            .sort_unstable_by(|f, s| f.pos().cmp(&s.pos()));
        sent.set_predicates(context)?;
        sent.set_sent_kind(context)?;

        // free any extra allocated memory as this is an inmutable structure
        sent.particles.shrink_to_fit();
        sent.vars.shrink_to_fit();
        sent.skolem.shrink_to_fit();
        sent.predicates.0.shrink_to_fit();
        sent.predicates.1.shrink_to_fit();

        Ok(sent)
    }

    /// Classify the kind of sentence and checks that is well formed.
    fn set_sent_kind(&mut self, context: &mut ParseContext) -> Result<(), LogSentErr> {
        if context.iexpr() {
            self.iexpr_op_arg_validation()?;
            if !self.vars.is_empty() || !self.skolem.is_empty() {
                for var in &self.vars {
                    match var.kind {
                        VarKind::TimeDecl | VarKind::Time => {
                            self.has_time_vars += 1;
                            continue;
                        }
                        VarKind::Normal => {}
                    }
                }
                let total_vars = self.vars.len() + self.skolem.len();
                // check out that all variables are time or spatial variables
                if self.has_time_vars == total_vars {
                    context.stype = SentKind::Rule;
                    self.sent_kind = SentKind::Rule;
                } else {
                    self.sent_kind = SentKind::IExpr;
                }
            } else {
                context.stype = SentKind::Rule;
                self.sent_kind = SentKind::Rule;
            }
        } else {
            context.stype = SentKind::Rule;
            self.sent_kind = SentKind::Rule;
        }
        Ok(())
    }

    /// Completes LHS/RHS predicates set up.
    fn set_predicates(&mut self, context: &ParseContext) -> Result<(), LogSentErr> {
        if context.iexpr() {
            let mut lhs = Vec::new();
            self.correct_iexpr(&mut lhs)?;
            let lhs: HashSet<_> = HashSet::from_iter(lhs.into_iter());
            let rhs: HashSet<_> = self
                .particles
                .iter()
                .filter(|x| x.is_atom())
                .map(|x| x.pos())
                .collect();
            let rhs_v: Vec<_> = rhs
                .difference(&lhs)
                .filter_map(|p| {
                    let v = &self.particles[*p];
                    if v.is_atom() {
                        Some(v.pos())
                    } else {
                        None
                    }
                })
                .collect();
            let lhs_v: Vec<_> = lhs
                .iter()
                .filter_map(|p| {
                    let v = &self.particles[*p];
                    if v.is_atom() {
                        Some(v.pos())
                    } else {
                        None
                    }
                })
                .collect();
            self.predicates = (lhs_v, rhs_v);
        } else {
            let preds: Vec<_> = self
                .particles
                .iter()
                .filter(|p| p.is_atom())
                .map(|p| p.pos())
                .collect();
            self.predicates = (preds, vec![]);
        }
        Ok(())
    }

    fn correct_iexpr(&self, lhs: &mut Vec<usize>) -> Result<(), LogSentErr> {
        fn has_icond_child(sent: &LogSentence, p: &Particle) -> Result<(), LogSentErr> {
            if let Some(n1_0) = p.get_next(0) {
                if let Particle::IndConditional(_, _) = sent.particles[n1_0] {
                    return Err(LogSentErr::IExprEntailLHS);
                }
                has_icond_child(sent, &sent.particles[n1_0])?;
            }
            if let Some(n1_1) = p.get_next(1) {
                if let Particle::IndConditional(_, _) = sent.particles[n1_1] {
                    return Err(LogSentErr::IExprEntailLHS);
                }
                has_icond_child(sent, &sent.particles[n1_1])?;
            }
            Ok(())
        }

        fn wrong_operator(sent: &LogSentence, p: &Particle) -> Result<(), LogSentErr> {
            if let Some(n1_0) = p.get_next(0) {
                // test that the lhs does not include any indicative conditional
                if sent.particles[n1_0].is_icond() {
                    return Err(LogSentErr::IExprEntailLHS);
                }
                has_icond_child(sent, &sent.particles[n1_0])?;
            }
            // test that the rh-most-s does include only icond or 'OR' connectives
            let mut is_wrong = Ok(());
            if let Some(n1_1) = p.get_next(1) {
                match sent.particles[n1_1] {
                    Particle::IndConditional(_, _)
                    | Particle::Disjunction(_, _)
                    | Particle::Conjunction(_, _)
                    | Particle::Atom(_, _) => {}
                    _ => return Err(LogSentErr::IExprWrongOp),
                }
                is_wrong = wrong_operator(sent, &sent.particles[n1_1]);
            }
            is_wrong
        }

        fn get_lhs_preds(sent: &LogSentence, p: &Particle, lhs: &mut Vec<usize>) {
            if let Some(n1_0) = p.get_next(0) {
                if let Particle::Atom(_, _) = sent.particles[n1_0] {
                    lhs.push(n1_0);
                }
                get_lhs_preds(sent, &sent.particles[n1_0], lhs);
                let n1_1 = p.get_next(1).unwrap();
                if let Particle::Atom(_, _) = sent.particles[n1_1] {
                    lhs.push(n1_1);
                }
                get_lhs_preds(sent, &sent.particles[n1_1], lhs)
            } else {
                lhs.push(p.pos());
            }
        }

        let first: &Particle = &self.particles[self.root];
        match *first {
            Particle::IndConditional(_, _) => {}
            _ => return Err(LogSentErr::IExprNotIcond),
        }

        if let Some(n1_0) = first.get_next(0) {
            if let Particle::IndConditional(_, _) = self.particles[n1_0] {
                return Err(LogSentErr::IExprEntailLHS);
            }
            get_lhs_preds(self, &self.particles[n1_0], lhs)
        }

        for p in &self.particles {
            if let Particle::Atom(_, ref atom) = p {
                if !atom.pred.parent_is_grounded() && !atom.pred.parent_is_kw() {
                    return Err(LogSentErr::WrongPredicate);
                }
            }
        }

        wrong_operator(self, first)
    }

    fn iexpr_op_arg_validation(&self) -> Result<(), LogSentErr> {
        // check validity of optional arguments for predicates in the LHS:
        for decl in &self.predicates.0 {
            match self.particles[*decl].pred_ref() {
                Assert::FuncDecl(ref func) => {
                    if let Some(ref opargs) = func.op_args {
                        for arg in opargs {
                            match *arg {
                                OpArg::TimeDecl(_) | OpArg::TimeVar | OpArg::TimeVarAssign(_) => {
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
                                OpArg::TimeDecl(_) | OpArg::TimeVar | OpArg::TimeVarAssign(_) => {
                                    return Err(LogSentErr::InvalidOpArg);
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        // check validity of optional arguments for predicates in the RHS:
        for decl in &self.predicates.1 {
            match self.particles[*decl].pred_ref() {
                Assert::FuncDecl(ref func) => {
                    if let Some(ref opargs) = func.op_args {
                        for arg in opargs {
                            match *arg {
                                OpArg::TimeDecl(_) | OpArg::TimeVar | OpArg::TimeVarFrom(_) => {
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
                                OpArg::TimeDecl(_) | OpArg::TimeVar | OpArg::TimeVarFrom(_) => {
                                    return Err(LogSentErr::InvalidOpArg);
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub fn solve<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        mut context: T,
    ) -> T {
        let root = &self.particles[self.root];
        let time_assign = {
            if !self.vars.is_empty() {
                self.get_time_assignments(agent, assignments)
            } else {
                HashMap::with_capacity(0)
            }
        };

        if self.has_time_vars != time_assign.len() {
            context.set_result(None);
            return context;
        }

        if self.sent_kind.is_iexpr() {
            if let Some(res) = root.solve(agent, assignments, &time_assign, &mut context) {
                if res {
                    root.substitute(agent, assignments, &time_assign, &mut context, false);
                    context.set_result(Some(true));
                } else {
                    root.substitute(agent, assignments, &time_assign, &mut context, true);
                    context.set_result(Some(false));
                }
            } else {
                context.set_result(None);
            }
        } else if let Some(res) = root.solve(agent, assignments, &time_assign, &mut context) {
            if res {
                if root.is_icond() {
                    context.substituting();
                    root.substitute(agent, assignments, &time_assign, &mut context, false)
                }
                context.set_result(Some(true));
            } else {
                if root.is_icond() {
                    context.substituting();
                    root.substitute(agent, assignments, &time_assign, &mut context, true)
                }
                context.set_result(Some(false));
            }
        } else if !context.is_inconsistent() {
            context.set_result(None);
        } else {
            context.set_result(Some(false));
        }

        context
    }

    fn get_time_assignments(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> HashMap<&Var, Arc<BmsWrapper>> {
        let mut time_assign = HashMap::new();
        'outer: for var in &self.vars {
            match var.kind {
                VarKind::Time => {
                    for pred in &self.predicates.0 {
                        let pred = self.particles[*pred].pred_ref();
                        if pred.get_time_decl(&*var) {
                            let times = pred.get_times(agent, var_assign);
                            if times.is_none() {
                                continue 'outer;
                            }
                            time_assign.insert(&**var, times.unwrap());
                            continue 'outer;
                        }
                    }
                }
                VarKind::TimeDecl => {
                    let times = Arc::new(var.get_times());
                    time_assign.insert(&**var, times);
                }
                _ => {}
            }
        }
        time_assign
    }

    pub fn extract_all_predicates(self) -> (Vec<Arc<Var>>, Vec<Assert>) {
        let LogSentence {
            vars, particles, ..
        } = self;
        let mut preds = vec![];
        let mut checked = HashSet::new();
        for p in particles {
            if !checked.contains(&p.pos()) && p.is_atom() {
                checked.insert(p.pos());
                preds.push(p.pred());
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
        // TODO: from particles[root] .. particles.len()
        let mut v = vec![];
        for p in &self.predicates.1 {
            let p = self.particles[*p].pred_ref();
            v.push(p);
        }
        v
    }

    pub fn get_all_lhs_predicates(&self) -> Vec<&Assert> {
        // TODO: from 0..particles[root]
        let mut v = vec![];
        for p in &self.predicates.0 {
            let p = self.particles[*p].pred_ref();
            v.push(p);
        }
        v
    }

    pub fn get_lhs_predicates(&self) -> LhsPreds {
        let next = self.particles[self.root].get_next(0).unwrap();
        LhsPreds::new(&self.particles[next], self)
    }

    fn generate_uid(particles: &HashSet<Particle>) -> SentID {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut id = vec![];
        for a in particles {
            match a {
                Particle::Conjunction(_, _) => id.push(0),
                Particle::Disjunction(_, _) => id.push(1),
                Particle::Equivalence(_, _) => id.push(2),
                Particle::Implication(_, _) => id.push(3),
                Particle::IndConditional(_, _) => id.push(4),
                Particle::Atom(_, ref p) => {
                    let mut id_1 = p.generate_uid();
                    id.append(&mut id_1)
                }
            }
        }
        let mut s = DefaultHasher::new();
        id.hash(&mut s);
        s.finish()
    }
}

impl fmt::Display for LogSentence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut particles = String::new();
        for p in &self.particles {
            particles.extend(format!("{}, ", p).chars());
        }
        let collected: String = format!("Sentence(id: {}, {})", self.id, particles);
        write!(f, "{}", collected)
    }
}

pub(in crate::agent) struct LhsPreds<'a> {
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
        LhsPreds::dig(lhs_root, sent, &mut f, &mut l);
        if !l.is_empty() {
            f.push(l);
        }
        let idx = vec![(0, false); f.len()];
        LhsPreds {
            preds: f,
            index: idx,
            curr: 0,
            sent,
            done: HashSet::new(),
        }
    }

    fn dig(
        prev: &'a Particle,
        sent: &'a LogSentence,
        f: &mut Vec<Vec<&'a Assert>>,
        curr: &mut Vec<&'a Assert>,
    ) {
        // break up all the assertments into groups of one or more members
        // depending on whether they are childs of an OR node or not
        if let Some(lhs) = prev.get_next(0) {
            if prev.is_disjunction() {
                LhsPreds::dig(&sent.particles[lhs], sent, f, curr);
                LhsPreds::dig(&sent.particles[prev.get_next(1).unwrap()], sent, f, curr);
            } else {
                let mut nlhs = vec![];
                let mut nrhs = vec![];
                LhsPreds::dig(&sent.particles[lhs], sent, f, &mut nlhs);
                LhsPreds::dig(
                    &sent.particles[prev.get_next(1).unwrap()],
                    sent,
                    f,
                    &mut nrhs,
                );
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

impl<'a> std::iter::Iterator for LhsPreds<'a> {
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

pub(in crate::agent) struct SentVarReq<'a> {
    iter: LhsPreds<'a>,
}

impl<'a> std::iter::Iterator for SentVarReq<'a> {
    type Item = HashMap<&'a Var, Vec<&'a Assert>>;
    /// Iterates the permutations of the sentence variable requeriments.
    /// This just takes into consideration the LHS variables.
    fn next(&mut self) -> Option<HashMap<&'a Var, Vec<&'a Assert>>> {
        if let Some(picks) = self.iter.next() {
            if self.iter.sent.vars.is_empty() {
                return None;
            }
            let mut requeriments = HashMap::new();
            for var in &self.iter.sent.vars {
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

impl std::cmp::PartialEq for LogSentence {
    fn eq(&self, other: &LogSentence) -> bool {
        self.id == other.id
    }
}

impl std::cmp::Eq for LogSentence {}

impl std::hash::Hash for LogSentence {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Debug, Clone, Copy)]
pub(in crate::agent) enum SentKind {
    IExpr,
    Expr,
    Rule,
}

impl SentKind {
    fn is_iexpr(self) -> bool {
        match self {
            SentKind::IExpr => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
struct LogicIndCond {
    next_rhs: usize,
    next_lhs: usize,
}

impl LogicIndCond {
    fn new(lhs: usize, rhs: usize) -> LogicIndCond {
        LogicIndCond {
            next_rhs: rhs,
            next_lhs: lhs,
        }
    }

    #[inline]
    fn solve<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) -> Option<bool> {
        if let Some(res) = {
            let next_p = context.sent().particles[self.next_lhs].clone();
            next_p.solve(agent, assignments, time_assign, context)
        } {
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
    fn substitute<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
        rhs: bool,
    ) {
        if rhs {
            return;
        };
        let next_p = context.sent().particles[self.next_rhs].clone();
        if next_p.is_disjunction() || !rhs {
            next_p.substitute(agent, assignments, time_assign, context, rhs);
        }
    }

    fn get_next(&self, pos: usize) -> usize {
        if pos == 0 {
            self.next_lhs
        } else {
            self.next_rhs
        }
    }
}

impl fmt::Display for LogicIndCond {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let lhs = format!("{}", self.next_lhs);
        let rhs = format!("{}", self.next_rhs);
        write!(f, "Conditional(lhs: {}, rhs: {})", lhs, rhs)
    }
}

#[derive(Debug, Clone)]
struct LogicEquivalence {
    next_rhs: usize,
    next_lhs: usize,
}

impl LogicEquivalence {
    fn new(lhs: usize, rhs: usize) -> LogicEquivalence {
        LogicEquivalence {
            next_rhs: rhs,
            next_lhs: lhs,
        }
    }

    #[inline]
    fn solve<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) -> Option<bool> {
        context.set_inconsistent(false);
        let n0_res = context.sent().particles[self.next_lhs].clone().solve(
            agent,
            assignments,
            time_assign,
            context,
        );
        let first = context.is_inconsistent();
        context.set_inconsistent(false);
        let n1_res = context.sent().particles[self.next_rhs].clone().solve(
            agent,
            assignments,
            time_assign,
            context,
        );
        let second = context.is_inconsistent();
        if let Some(val0) = n0_res {
            if let Some(val1) = n1_res {
                if val0 == val1 {
                    context.set_inconsistent(false);
                    return Some(true);
                } else {
                    context.set_inconsistent(true);
                    return Some(false);
                }
            }
            if first == second {
                context.set_inconsistent(false);
                Some(true)
            } else {
                context.set_inconsistent(true);
                Some(false)
            }
        } else {
            context.set_inconsistent(false);
            Some(true)
        }
    }

    fn get_next(&self, pos: usize) -> usize {
        if pos == 0 {
            self.next_lhs
        } else {
            self.next_rhs
        }
    }
}

impl fmt::Display for LogicEquivalence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let lhs = format!("{}", self.next_lhs);
        let rhs = format!("{}", self.next_rhs);
        write!(f, "Equivalence(lhs: {}, rhs: {})", lhs, rhs)
    }
}

#[derive(Debug, Clone)]
struct LogicImplication {
    next_rhs: usize,
    next_lhs: usize,
}

impl LogicImplication {
    fn new(lhs: usize, rhs: usize) -> LogicImplication {
        LogicImplication {
            next_rhs: rhs,
            next_lhs: lhs,
        }
    }

    #[inline]
    fn solve<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) -> Option<bool> {
        let n0_res = context.sent().particles[self.next_lhs].clone().solve(
            agent,
            assignments,
            time_assign,
            context,
        );
        context.set_inconsistent(false);
        let n1_res = context.sent().particles[self.next_rhs].clone().solve(
            agent,
            assignments,
            time_assign,
            context,
        );
        if let Some(true) = n0_res {
            if let Some(false) = n1_res {
                Some(false)
            } else if context.is_inconsistent() {
                Some(false)
            } else {
                Some(true)
            }
        } else {
            context.set_inconsistent(false);
            Some(true)
        }
    }

    fn get_next(&self, pos: usize) -> usize {
        if pos == 0 {
            self.next_lhs
        } else {
            self.next_rhs
        }
    }
}

impl fmt::Display for LogicImplication {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let lhs = format!("{}", self.next_lhs);
        let rhs = format!("{}", self.next_rhs);
        write!(f, "Implication(lhs: {}, rhs: {})", lhs, rhs)
    }
}

#[derive(Debug, Clone)]
struct LogicConjunction {
    next_rhs: usize,
    next_lhs: usize,
}

impl LogicConjunction {
    fn new(lhs: usize, rhs: usize) -> LogicConjunction {
        LogicConjunction {
            next_rhs: rhs,
            next_lhs: lhs,
        }
    }

    #[inline]
    fn solve<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) -> Option<bool> {
        let n0_res = context.sent().particles[self.next_lhs].clone().solve(
            agent,
            assignments,
            time_assign,
            context,
        );
        let rhs_res = context.sent().particles[self.next_rhs].clone().solve(
            agent,
            assignments,
            time_assign,
            context,
        );
        if n0_res.is_none() | rhs_res.is_none() {
            return None;
        }
        if let Some(false) = n0_res {
            return Some(false);
        } else if let Some(false) = rhs_res {
            return Some(false);
        }
        Some(true)
    }

    #[inline]
    fn substitute<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
        rhs: bool,
    ) {
        context.sent().particles[self.next_rhs].clone().substitute(
            agent,
            assignments,
            time_assign,
            context,
            rhs,
        );
        context.sent().particles[self.next_lhs].clone().substitute(
            agent,
            assignments,
            time_assign,
            context,
            rhs,
        );
    }

    fn get_next(&self, pos: usize) -> usize {
        if pos == 0 {
            self.next_lhs
        } else {
            self.next_rhs
        }
    }
}

impl fmt::Display for LogicConjunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let lhs = format!("{}", self.next_lhs);
        let rhs = format!("{}", self.next_rhs);
        write!(f, "Conjunction(lhs: {}, rhs: {})", lhs, rhs)
    }
}

#[derive(Debug, Clone)]
struct LogicDisjunction {
    next_rhs: usize,
    next_lhs: usize,
}

impl LogicDisjunction {
    fn new(lhs: usize, rhs: usize) -> LogicDisjunction {
        LogicDisjunction {
            next_rhs: rhs,
            next_lhs: lhs,
        }
    }

    #[inline]
    fn solve<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) -> Option<bool> {
        let n0_res = context.sent().particles[self.next_lhs].clone().solve(
            agent,
            assignments,
            time_assign,
            context,
        );
        let n1_res = context.sent().particles[self.next_rhs].clone().solve(
            agent,
            assignments,
            time_assign,
            context,
        );
        if n0_res != n1_res {
            if n0_res.is_some() && n1_res.is_some() {
                context.set_inconsistent(false);
            } else if let Some(false) = n0_res {
                return Some(false);
            } else if let Some(false) = n1_res {
                return Some(false);
            }
            context.set_inconsistent(false);
            Some(true)
        } else if let Some(false) = n0_res {
            Some(false)
        } else {
            None
        }
    }

    #[inline]
    fn substitute<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
        rhs: bool,
    ) {
        if rhs {
            context.sent().particles[self.next_rhs].clone().substitute(
                agent,
                assignments,
                time_assign,
                context,
                rhs,
            )
        } else {
            context.sent().particles[self.next_lhs].clone().substitute(
                agent,
                assignments,
                time_assign,
                context,
                rhs,
            )
        }
    }

    fn get_next(&self, pos: usize) -> usize {
        if pos == 0 {
            self.next_lhs
        } else {
            self.next_rhs
        }
    }
}

impl fmt::Display for LogicDisjunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let lhs = format!("{}", self.next_lhs);
        let rhs = format!("{}", self.next_rhs);
        write!(f, "Disjunction(lhs: {}, rhs: {})", lhs, rhs)
    }
}

#[derive(Debug, Clone)]
struct LogicAtom {
    pred: Assert,
}

impl LogicAtom {
    fn new(term: Assert) -> LogicAtom {
        LogicAtom { pred: term }
    }

    #[inline]
    fn solve<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) -> Option<bool> {
        if let Some(res) = self
            .pred
            .grounded_eq(agent, assignments, time_assign, context)
        {
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
    fn substitute<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) {
        self.pred
            .substitute(agent, assignments, time_assign, context)
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

#[derive(Debug, Clone)]
/// EnumVariant(<position at the sentence vec>, <kind>)
enum Particle {
    Atom(usize, LogicAtom),
    Conjunction(usize, LogicConjunction),
    Disjunction(usize, LogicDisjunction),
    Implication(usize, LogicImplication),
    Equivalence(usize, LogicEquivalence),
    IndConditional(usize, LogicIndCond),
}

impl Particle {
    #[inline]
    fn solve<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) -> Option<bool> {
        match *self {
            Particle::Conjunction(_, ref p) => p.solve(agent, assignments, time_assign, context),
            Particle::Disjunction(_, ref p) => p.solve(agent, assignments, time_assign, context),
            Particle::Implication(_, ref p) => p.solve(agent, assignments, time_assign, context),
            Particle::Equivalence(_, ref p) => p.solve(agent, assignments, time_assign, context),
            Particle::IndConditional(_, ref p) => p.solve(agent, assignments, time_assign, context),
            Particle::Atom(_, ref p) => p.solve(agent, assignments, time_assign, context),
        }
    }

    #[inline]
    fn substitute<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
        rhs: bool,
    ) {
        match *self {
            Particle::IndConditional(_, ref p) => {
                p.substitute(agent, assignments, time_assign, context, rhs)
            }
            Particle::Disjunction(_, ref p) => {
                p.substitute(agent, assignments, time_assign, context, rhs)
            }
            Particle::Conjunction(_, ref p) => {
                p.substitute(agent, assignments, time_assign, context, rhs)
            }
            Particle::Atom(_, ref p) => p.substitute(agent, assignments, time_assign, context),
            Particle::Implication(_, _) | Particle::Equivalence(_, _) => {}
        }
    }

    #[inline]
    fn get_next(&self, pos: usize) -> Option<usize> {
        match *self {
            Particle::Conjunction(_, ref p) => Some(p.get_next(pos)),
            Particle::Disjunction(_, ref p) => Some(p.get_next(pos)),
            Particle::Implication(_, ref p) => Some(p.get_next(pos)),
            Particle::Equivalence(_, ref p) => Some(p.get_next(pos)),
            Particle::IndConditional(_, ref p) => Some(p.get_next(pos)),
            Particle::Atom(_, _) => None,
        }
    }

    #[inline]
    fn pred_ref(&self) -> &Assert {
        match self {
            Particle::Atom(_, p) => &p.pred,
            Particle::Conjunction(_, _)
            | Particle::Disjunction(_, _)
            | Particle::Equivalence(_, _)
            | Particle::Implication(_, _)
            | Particle::IndConditional(_, _) => unreachable!(),
        }
    }

    #[inline]
    fn pred(self) -> Assert {
        match self {
            Particle::Atom(_, p) => p.pred,
            Particle::Conjunction(_, _)
            | Particle::Disjunction(_, _)
            | Particle::Equivalence(_, _)
            | Particle::Implication(_, _)
            | Particle::IndConditional(_, _) => unreachable!(),
        }
    }

    #[inline]
    fn is_atom(&self) -> bool {
        match *self {
            Particle::Atom(_, _) => true,
            _ => false,
        }
    }

    #[inline]
    fn is_icond(&self) -> bool {
        match *self {
            Particle::IndConditional(_, _) => true,
            _ => false,
        }
    }

    #[inline]
    fn is_disjunction(&self) -> bool {
        match *self {
            Particle::Disjunction(_, _) => true,
            _ => false,
        }
    }

    #[inline]
    fn pos(&self) -> usize {
        match self {
            Particle::Conjunction(p, _) => *p,
            Particle::Disjunction(p, _) => *p,
            Particle::Implication(p, _) => *p,
            Particle::Equivalence(p, _) => *p,
            Particle::IndConditional(p, _) => *p,
            Particle::Atom(p, _) => *p,
        }
    }
}

impl fmt::Display for Particle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Particle::Atom(pos, ref p) => write!(f, "p{}.{}", pos, p),
            Particle::Conjunction(pos, ref p) => write!(f, "p{}.{}", pos, p),
            Particle::Disjunction(pos, ref p) => write!(f, "p{}.{}", pos, p),
            Particle::Equivalence(pos, ref p) => write!(f, "p{}.{}", pos, p),
            Particle::Implication(pos, ref p) => write!(f, "p{}.{}", pos, p),
            Particle::IndConditional(pos, ref p) => write!(f, "p{}.{}", pos, p),
        }
    }
}

impl PartialEq for Particle {
    fn eq(&self, other: &Self) -> bool {
        self.pos() == other.pos()
    }
}

impl Eq for Particle {}

impl std::hash::Hash for Particle {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pos().hash(state);
    }
}

pub(in crate::agent) trait ProofResContext {
    fn sent(&self) -> &LogSentence;

    fn set_result(&mut self, res: Option<bool>);

    fn get_id(&self) -> SentID;

    /// Get the resolution time for this context
    fn get_production_time(&self) -> Time;

    fn push_grounded_func(&mut self, grounded: GroundedFunc, time: Time);

    fn push_grounded_cls(&mut self, grounded: GroundedMemb, time: Time);

    fn newest_grfact(&self) -> Time;

    fn set_newest_grfact(&mut self, time: Time);

    fn get_antecedents(&self) -> &[Grounded];

    fn push_antecedents(&mut self, grounded: Grounded);

    fn push_false_fn_assert(&mut self, func: Arc<GroundedFunc>);

    fn push_false_cls_assert(&mut self, cls: Arc<GroundedMemb>);

    fn compare_relation(&self, func: &GroundedFunc) -> bool;

    fn compare_cls(&self, cls: &GroundedMemb) -> bool;

    fn has_relationship(&self, func: &GroundedFunc) -> Option<bool>;

    fn has_cls_memb(&self, cls: &GroundedMemb) -> Option<bool>;

    fn substituting(&mut self);

    fn is_substituting(&self) -> bool;

    fn set_inconsistent(&mut self, val: bool);

    fn is_inconsistent(&self) -> bool;
}

pub(in crate::agent) trait LogSentResolution<T: ProofResContext> {
    fn grounded_eq(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) -> Option<bool>;

    fn substitute(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    );
}

pub(in crate::agent) struct ParseContext {
    pub stype: SentKind,
    pub in_assertion: bool,
    pub is_tell: bool,
    pub depth: usize,
    pub vars: Vec<Arc<Var>>,
    pub skols: Vec<Arc<Skolem>>,
    particles: HashSet<Particle>,
    particles_num: usize,
    shadowing_vars: HashMap<Arc<Var>, (usize, Arc<Var>)>,
    shadowing_skols: HashMap<Arc<Skolem>, (usize, Arc<Skolem>)>,
    all_vars: Vec<Arc<Var>>,
    all_skols: Vec<Arc<Skolem>>,
    in_rhs: bool,
}

impl Default for ParseContext {
    fn default() -> ParseContext {
        ParseContext::new()
    }
}

impl ParseContext {
    pub fn new() -> ParseContext {
        ParseContext {
            stype: SentKind::Expr,
            in_assertion: false,
            is_tell: false,
            depth: 0,
            vars: Vec::new(),
            skols: Vec::new(),
            particles: HashSet::new(),
            particles_num: 0,
            shadowing_vars: HashMap::new(),
            shadowing_skols: HashMap::new(),
            all_vars: Vec::new(),
            all_skols: Vec::new(),
            in_rhs: true,
        }
    }

    pub fn push_var(&mut self, decl: &VarDeclBorrowed) -> Result<(), ParseErrF> {
        match decl {
            VarDeclBorrowed::Var(ref var) => {
                let var = Arc::new(Var::from(var, self)?);
                self.vars.push(var.clone());
                self.all_vars.push(var);
                Ok(())
            }
            VarDeclBorrowed::Skolem(ref var) => {
                let var = Arc::new(Skolem::from(var, self)?);
                self.skols.push(var.clone());
                self.all_skols.push(var);
                Ok(())
            }
        }
    }

    #[allow(dead_code)]
    pub fn var_in_context(&self, decl: &VarDeclBorrowed) -> Result<bool, ParseErrF> {
        match decl {
            VarDeclBorrowed::Var(ref var) => {
                let var = &Var::from(var, self)?;
                Ok(self.vars.iter().any(|x| var.name_eq(x.as_ref())))
            }
            VarDeclBorrowed::Skolem(ref var) => {
                let var = &Skolem::from(var, self)?;
                Ok(self.skols.iter().any(|x| var.name_eq(x.as_ref())))
            }
        }
    }

    fn iexpr(&self) -> bool {
        match self.stype {
            SentKind::IExpr => true,
            SentKind::Rule | SentKind::Expr => false,
        }
    }
}

mod ast_walker {
    use super::*;
    use crate::agent::lang::BuiltIns;

    #[derive(Debug)]
    pub(super) struct PIntermediate {
        cond: Option<LogicOperator>,
        pred: Option<Assert>,
        pub lhs: Option<Particle>,
        pub rhs: Option<Particle>,
    }

    impl From<LogicOperator> for PIntermediate {
        fn from(op: LogicOperator) -> Self {
            PIntermediate {
                cond: Some(op),
                lhs: None,
                rhs: None,
                pred: None,
            }
        }
    }

    impl From<Assert> for PIntermediate {
        fn from(assert: Assert) -> Self {
            PIntermediate {
                cond: None,
                lhs: None,
                rhs: None,
                pred: Some(assert),
            }
        }
    }

    impl PIntermediate {
        pub fn new() -> Self {
            PIntermediate {
                cond: None,
                lhs: None,
                rhs: None,
                pred: None,
            }
        }

        fn add_rhs(&mut self, p: Particle) {
            self.rhs = Some(p);
        }

        fn add_lhs(&mut self, p: Particle) {
            self.lhs = Some(p);
        }

        fn into_final(self, context: &mut ParseContext) -> Particle {
            if self.pred.is_some() {
                let p = Particle::Atom(context.particles_num, LogicAtom::new(self.pred.unwrap()));
                context.particles_num += 1;
                p
            } else {
                let PIntermediate { cond, rhs, lhs, .. } = self;
                let rhs = rhs.unwrap();
                let lhs = lhs.unwrap();
                let particle = match cond.unwrap() {
                    LogicOperator::Entail => {
                        context.stype = SentKind::IExpr;
                        Particle::IndConditional(
                            context.particles_num,
                            LogicIndCond::new(lhs.pos(), rhs.pos()),
                        )
                    }
                    LogicOperator::And => Particle::Conjunction(
                        context.particles_num,
                        LogicConjunction::new(lhs.pos(), rhs.pos()),
                    ),
                    LogicOperator::Or => Particle::Disjunction(
                        context.particles_num,
                        LogicDisjunction::new(lhs.pos(), rhs.pos()),
                    ),
                    LogicOperator::Implication => Particle::Implication(
                        context.particles_num,
                        LogicImplication::new(lhs.pos(), rhs.pos()),
                    ),
                    LogicOperator::Biconditional => Particle::Equivalence(
                        context.particles_num,
                        LogicEquivalence::new(lhs.pos(), rhs.pos()),
                    ),
                };
                context.particles_num += 1;
                context.particles.insert(rhs);
                context.particles.insert(lhs);
                particle
            }
        }
    }

    pub(super) fn walk_ast(
        ast: &ASTNode,
        context: &mut ParseContext,
        mut parent: PIntermediate,
    ) -> Result<PIntermediate, LogSentErr> {
        let mut v_cnt = 0;
        let mut s_cnt = 0;
        match *ast {
            ASTNode::Assert(ref decl) => {
                let particle = match *decl {
                    AssertBorrowed::ClassDecl(ref decl) => {
                        let cls = match ClassDecl::from(decl, context) {
                            Err(err) => return Err(LogSentErr::Boxed(Box::new(err))),
                            Ok(cls) => cls,
                        };
                        PIntermediate::from(Assert::ClassDecl(cls))
                    }
                    AssertBorrowed::FuncDecl(ref decl) => {
                        // try to convert to a special function:
                        if let Ok(special_func) = BuiltIns::try_from((decl, &mut *context)) {
                            PIntermediate::from(Assert::SpecialFunc(special_func))
                        } else {
                            match FuncDecl::from(decl, context) {
                                Err(err) => return Err(LogSentErr::Boxed(Box::new(err))),
                                Ok(func) => PIntermediate::from(Assert::FuncDecl(func)),
                            }
                        }
                    }
                };
                add_particle(particle, &mut parent, context);
                Ok(parent)
            }
            ASTNode::Scope(ref ast) => {
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
                    let vars = ast.vars.as_ref().unwrap();
                    decl_scope_vars(context, vars, &mut v_cnt, &mut s_cnt)?;
                }
                if ast.logic_op.is_some() {
                    let op = PIntermediate::from(ast.logic_op.unwrap());
                    let next = walk_ast(&ast.next, context, op)?;
                    drop_local_vars(context, v_cnt);
                    drop_local_skolems(context, s_cnt);
                    add_particle(next, &mut parent, context);
                    Ok(parent)
                } else {
                    let res = walk_ast(&ast.next, context, parent)?;
                    drop_local_vars(context, v_cnt);
                    drop_local_skolems(context, s_cnt);
                    Ok(res)
                }
            }
            ASTNode::Chain(ref nodes) => {
                let in_side = context.in_rhs;
                if nodes.len() == 2 {
                    if let ASTNode::Scope(ref node) = nodes[0] {
                        let Scope {
                            logic_op: ref op,
                            ref next,
                            ..
                        } = **node;
                        if op.is_some() && next.would_assert() {
                            context.in_rhs = false;
                            let new_op = PIntermediate::from(op.unwrap());
                            let new_op = walk_ast(next, context, new_op)?;
                            context.in_rhs = true;
                            let new_op = walk_ast(&nodes[1], context, new_op)?;
                            context.in_rhs = in_side;
                            add_particle(new_op, &mut parent, context);
                            return Ok(parent);
                        }
                    }
                    if let ASTNode::Scope(ref node) = nodes[1] {
                        let Scope {
                            logic_op: ref op,
                            ref next,
                            ..
                        } = **node;
                        if op.is_some() && next.would_assert() {
                            context.in_rhs = true;
                            let new_op = PIntermediate::from(op.unwrap());
                            let new_op = walk_ast(next, context, new_op)?;
                            context.in_rhs = false;
                            let new_op = walk_ast(&nodes[1], context, new_op)?;
                            context.in_rhs = in_side;
                            add_particle(new_op, &mut parent, context);
                            return Ok(parent);
                        }
                    }
                    context.in_rhs = false;
                    let parent = walk_ast(&nodes[0], context, parent)?;
                    context.in_rhs = true;
                    let parent = walk_ast(&nodes[1], context, parent)?;
                    context.in_rhs = in_side;
                    Ok(parent)
                } else {
                    let op = nodes[0].get_op();
                    if !op.is_and() && !op.is_or() {
                        return Err(LogSentErr::IConnectInChain);
                    }
                    let mut prev_op = PIntermediate::from(op);
                    context.in_rhs = true;
                    prev_op = walk_ast(nodes.last().unwrap(), context, prev_op)?;
                    context.in_rhs = false;
                    let mut new_op;
                    for i in 1..nodes.len() {
                        let i = nodes.len() - (1 + i);
                        new_op = PIntermediate::from(op);
                        match nodes[i] {
                            ASTNode::Scope(ref assert) => {
                                if let Some(ref cop) = assert.logic_op {
                                    if cop != &op {
                                        return Err(LogSentErr::IConnectInChain);
                                    }
                                }
                                prev_op = walk_ast(&assert.next, context, prev_op)?;
                                let op = prev_op.into_final(context);
                                if i != 0 {
                                    new_op.add_rhs(op.clone());
                                } else {
                                    context.in_rhs = in_side;
                                    if context.in_rhs {
                                        parent.add_rhs(op);
                                    } else {
                                        parent.add_lhs(op);
                                    }
                                    break;
                                }
                                context.particles.insert(op);
                                prev_op = new_op;
                            }
                            _ => return Err(LogSentErr::WrongDef),
                        }
                    }
                    Ok(parent)
                }
            }
            ASTNode::None => Err(LogSentErr::WrongDef),
        }
    }

    fn decl_scope_vars<'a>(
        context: &mut ParseContext,
        vars: &[VarDeclBorrowed<'a>],
        v_cnt: &mut usize,
        s_cnt: &mut usize,
    ) -> Result<(), LogSentErr> {
        let mut swap_vars: Vec<(usize, Arc<Var>, Arc<Var>)> = Vec::new();
        let mut swap_skolem: Vec<(usize, Arc<Skolem>, Arc<Skolem>)> = Vec::new();
        for v in vars {
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
                    *v_cnt += 1;
                    context.vars.push(var.clone());
                    context.all_vars.push(var);
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
                    *s_cnt += 1;
                    context.skols.push(skolem.clone());
                    context.all_skols.push(skolem);
                }
            }
        }
        for &(i, ref shadowed, ref var) in &swap_vars {
            context.vars.remove(i);
            context
                .shadowing_vars
                .insert(var.clone(), (i, shadowed.clone()));
        }
        for &(i, ref shadowed, ref var) in &swap_skolem {
            context.skols.remove(i);
            context
                .shadowing_skols
                .insert(var.clone(), (i, shadowed.clone()));
        }
        Ok(())
    }

    fn add_particle(
        particle: PIntermediate,
        parent: &mut PIntermediate,
        context: &mut ParseContext,
    ) {
        if context.in_rhs {
            let p = particle.into_final(context);
            parent.add_rhs(p.clone());
            context.particles.insert(p);
        } else {
            let p = particle.into_final(context);
            parent.add_lhs(p.clone());
            context.particles.insert(p);
        }
    }
}

mod errors {
    use super::*;

    #[derive(Debug, PartialEq)]
    pub enum LogSentErr {
        Boxed(Box<ParseErrF>),
        RuleInclEntail(usize),
        IExprWrongOp,
        IExprEntailLHS,
        IExprNotIcond,
        IConnectInChain,
        IConnectAfterAnd,
        IConnectAfterOr,
        InvalidOpArg,
        WrongDef,
        WrongPredicate,
        Unreachable,
    }

    impl Into<ParseErrF> for LogSentErr {
        fn into(self) -> ParseErrF {
            ParseErrF::LogSentErr(self)
        }
    }
}

#[cfg(test)]
mod test {
    use super::Particle;
    use crate::agent::lang::parser::*;

    #[test]
    fn parse_wrong_funcs() {
        let source = String::from(
            "
                # Err:
                ((let x y z)
                 ( ( cde[x,u=1] := fn::fgh[y,u>0.5;x;z] ) := hij[y,u=1] )
                )

                # Err:
                ((let x y z)
                 ( abc[x,u=1]  := (( cde[x,u=1] := fn::fgh[y,u>0.5;x;z] ) && hij[y,u=1] ))
                )
            ",
        );
        let tpool = rayon::ThreadPoolBuilder::new().build().unwrap();
        let mut tree = Parser::parse(source.as_str(), true, &tpool).unwrap();
        assert!(tree.pop_front().unwrap().is_err());
        assert!(tree.pop_front().unwrap().is_err());
    }

    #[test]
    fn parser_correct_icond_exprs() {
        let source = String::from(
            "
            # Ok:
            (( let x y z )
              (( cde[x,u=1] && hij[y,u=1] && fn::fgh[y,u>0.5;x;z] ) := abc[x,u=1])
            )
            ",
        );
        let tpool = rayon::ThreadPoolBuilder::new().build().unwrap();
        let tree = Parser::parse(source.as_str(), true, &tpool);
        let sent = match tree.unwrap().pop_front().unwrap() {
            ParseTree::IExpr(sent) => sent,
            _ => panic!(),
        };
        let root = &sent.particles[sent.root];
        match root {
            Particle::IndConditional(_, ref p) => {
                match &sent.particles[p.next_lhs] {
                    Particle::Conjunction(_, ref op) => {
                        match sent.particles[op.next_lhs] {
                            Particle::Atom(_, ref atm) => {
                                assert_eq!(atm.get_name(), "cde");
                            }
                            Particle::Conjunction(_, _)
                            | Particle::Disjunction(_, _)
                            | Particle::Equivalence(_, _)
                            | Particle::Implication(_, _)
                            | Particle::IndConditional(_, _) => panic!(),
                        };
                        match &sent.particles[op.next_rhs] {
                            Particle::Conjunction(_, ref op) => {
                                match sent.particles[op.next_lhs] {
                                    Particle::Atom(_, ref atm) => assert_eq!(atm.get_name(), "hij"),
                                    _ => panic!(),
                                };
                                match sent.particles[op.next_rhs] {
                                    Particle::Atom(_, ref atm) => {
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
                match &sent.particles[p.next_rhs] {
                    Particle::Atom(_, ref atm) => assert_eq!(atm.get_name(), "abc"),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }

        let source = String::from(
            "
            # Ok:
            ((let x y z)
             ( abc[x,u=1]  := (
                 ( cde[x,u=1] && fn::fgh[y,u>0.5;x;z] ) := hij[y,u=1]
             ))
            )
            ",
        );
        let tpool = rayon::ThreadPoolBuilder::new().build().unwrap();
        let tree = Parser::parse(source.as_str(), true, &tpool);
        match tree.unwrap().pop_front().unwrap() {
            ParseTree::IExpr(_sent) => {}
            _ => panic!(),
        };
    }
}
