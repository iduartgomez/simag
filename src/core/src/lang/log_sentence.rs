#![allow(dead_code)]
#![allow(unused_variables)]

use lang::parser::{ASTNode, Assert};

/// Takes a parsed FOL sentence and creates an object with the embedded methods to resolve it.
pub fn make_logic_sent(ast: ASTNode) -> LogSentence {
    fn traverse_ast() {
        unimplemented!()
    }
    LogSentence::new()
}

/// Object to store a first-order logic complex sentence.
///
/// This sentence is the result of parsing a sentence and encode
/// it in an usable form for the agent to classify and reason about
/// objects and relations, cannot be instantiated directly.
///
/// It's callable when instantiated, accepts as arguments:
/// 1) the working knowledge-base
/// 2) n strings which will subsitute the variables in the sentence
///    or a list of string.
pub struct LogSentence {
    depth: usize,
    particles: Vec<Particle>,
    produced: Vec<Box<LogSentence>>,
    created: usize,
}

impl LogSentence {
    fn new() -> LogSentence {
        LogSentence {
            depth: 0,
            particles: Vec::new(),
            produced: Vec::new(),
            created: 0,
        }
    }
}

pub enum SolveErr {
    AssertionError(String),
}

#[derive(Debug)]
enum Condition {
    Predicate,
    Other,
}

#[derive(Debug, Clone, Copy)]
enum Solution {
    None,
}

pub enum Particle {
    Atom(LogicAtom),
    Conjunction(LogicConjunction),
    Disjunction(LogicDisjunction),
    Implication(LogicImplication),
    Equivalence(LogicEquivalence),
    Condition(LogicIndCond),
}

#[derive(Debug)]
pub struct LogicIndCond {
    depth: usize,
    cond: Condition,
    parent: *const Particle,
    results: [Solution; 2],
    next: Vec<*const Particle>,
}

impl LogicIndCond {
    fn new(cond: Condition,
           depth: usize,
           parent: *const Particle,
           args: Option<Vec<&str>>)
           -> LogicIndCond {
        LogicIndCond {
            depth: depth,
            cond: cond,
            parent: parent,
            results: [Solution::None; 2],
            next: Vec::with_capacity(2),
        }
    }

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
    }
}

#[derive(Debug)]
pub struct LogicEquivalence {
    depth: usize,
    cond: Condition,
    parent: *const Particle,
    results: [Solution; 2],
    next: Vec<*const Particle>,
}

impl LogicEquivalence {
    fn new(cond: Condition,
           depth: usize,
           parent: *const Particle,
           args: Option<Vec<&str>>)
           -> LogicEquivalence {
        LogicEquivalence {
            depth: depth,
            cond: cond,
            parent: parent,
            results: [Solution::None; 2],
            next: Vec::with_capacity(2),
        }
    }

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        let err = format!("operators of the type `{:?}` can't be on the left \
        side of sentence: `{:?}`",
                          self.cond,
                          0);
        Err(SolveErr::AssertionError(err))
    }
}

#[derive(Debug)]
pub struct LogicImplication {
    depth: usize,
    cond: Condition,
    parent: *const Particle,
    results: [Solution; 2],
    next: Vec<*const Particle>,
}

impl LogicImplication {
    fn new(cond: Condition,
           depth: usize,
           parent: *const Particle,
           args: Option<Vec<&str>>)
           -> LogicImplication {
        LogicImplication {
            depth: depth,
            cond: cond,
            parent: parent,
            results: [Solution::None; 2],
            next: Vec::with_capacity(2),
        }
    }

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        let err = format!("operators of the type `{:?}` can't be on the left \
        side of sentence: `{:?}`",
                          self.cond,
                          0);
        Err(SolveErr::AssertionError(err))
    }
}

#[derive(Debug)]
pub struct LogicConjunction {
    depth: usize,
    cond: Condition,
    parent: *const Particle,
    results: [Solution; 2],
    next: Vec<*const Particle>,
}

impl LogicConjunction {
    fn new(cond: Condition,
           depth: usize,
           parent: *const Particle,
           args: Option<Vec<&str>>)
           -> LogicConjunction {
        LogicConjunction {
            depth: depth,
            cond: cond,
            parent: parent,
            results: [Solution::None; 2],
            next: Vec::with_capacity(2),
        }
    }

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
    }
}

#[derive(Debug)]
pub struct LogicDisjunction {
    depth: usize,
    cond: Condition,
    parent: *const Particle,
    results: [Solution; 2],
    next: Vec<*const Particle>,
}

impl LogicDisjunction {
    fn new(cond: Condition,
           depth: usize,
           parent: *const Particle,
           args: Option<Vec<&str>>)
           -> LogicDisjunction {
        LogicDisjunction {
            depth: depth,
            cond: cond,
            parent: parent,
            results: [Solution::None; 2],
            next: Vec::with_capacity(2),
        }
    }

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
    }
}

#[derive(Debug)]
pub struct LogicAtom {
    depth: usize,
    cond: Condition,
    parent: *const Particle,
    pred: Assert,
}

impl LogicAtom {
    fn new(cond: Condition,
           depth: usize,
           parent: *const Particle,
           term: Assert,
           args: Option<Vec<&str>>)
           -> LogicAtom {
        LogicAtom {
            depth: depth,
            cond: cond,
            parent: parent,
            pred: term,
        }
    }

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
    }
}
