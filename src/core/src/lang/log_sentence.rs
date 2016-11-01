#![allow(dead_code)]
#![allow(unused_variables)]

use lang::parser::{ASTNode, IsTerm, Assert};

/// Takes a parsed FOL sentence and creates an object with the embedded methods to resolve it.
pub fn make_logic_sent<P: Particle>(ast: ASTNode) -> LogSentence<P> {
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
pub struct LogSentence<P: Particle> {
    depth: usize,
    particles: Vec<P>,
    produced: Vec<Box<LogSentence<P>>>,
    created: usize,
}

impl<P: Particle> LogSentence<P> {
    fn new() -> LogSentence<P> {
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

pub trait Particle {
    fn solve_proof(&self, proof: usize);
    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr>;
    // fn resolve(&mut self, proof: usize, args: Option<Vec<&str>>);
    // fn get_result(&self) -> Solution;
    // fn return_val(&mut self);
    // fn repr(&self) -> &str;
}

#[derive(Debug)]
struct LogicIndCond<'a, P: 'a + Particle> {
    depth: usize,
    cond: Condition,
    parent: &'a P,
    results: [Solution; 2],
    next: Vec<&'a P>,
}

impl<'a, P: 'a + Particle> LogicIndCond<'a, P> {
    fn new(cond: Condition,
           depth: usize,
           parent: &'a P,
           args: Option<Vec<&str>>)
           -> LogicIndCond<'a, P> {
        LogicIndCond {
            depth: depth,
            cond: cond,
            parent: parent,
            results: [Solution::None; 2],
            next: Vec::with_capacity(2),
        }
    }
}

impl<'a, P: Particle> Particle for LogicIndCond<'a, P> {
    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
    }
}

#[derive(Debug)]
struct LogicEquivalence<'a, P: 'a + Particle> {
    depth: usize,
    cond: Condition,
    parent: &'a P,
    results: [Solution; 2],
    next: Vec<&'a P>,
}

impl<'a, P: 'a + Particle> LogicEquivalence<'a, P> {
    fn new(cond: Condition,
           depth: usize,
           parent: &'a P,
           args: Option<Vec<&str>>)
           -> LogicEquivalence<'a, P> {
        LogicEquivalence {
            depth: depth,
            cond: cond,
            parent: parent,
            results: [Solution::None; 2],
            next: Vec::with_capacity(2),
        }
    }
}

impl<'a, P: Particle> Particle for LogicEquivalence<'a, P> {
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
struct LogicImplication<'a, P: 'a + Particle> {
    depth: usize,
    cond: Condition,
    parent: &'a P,
    results: [Solution; 2],
    next: Vec<&'a P>,
}

impl<'a, P: 'a + Particle> LogicImplication<'a, P> {
    fn new(cond: Condition,
           depth: usize,
           parent: &'a P,
           args: Option<Vec<&str>>)
           -> LogicImplication<'a, P> {
        LogicImplication {
            depth: depth,
            cond: cond,
            parent: parent,
            results: [Solution::None; 2],
            next: Vec::with_capacity(2),
        }
    }
}

impl<'a, P: Particle> Particle for LogicImplication<'a, P> {
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
struct LogicConjunction<'a, P: 'a + Particle> {
    depth: usize,
    cond: Condition,
    parent: &'a P,
    results: [Solution; 2],
    next: Vec<&'a P>,
}

impl<'a, P: 'a + Particle> LogicConjunction<'a, P> {
    fn new(cond: Condition,
           depth: usize,
           parent: &'a P,
           args: Option<Vec<&str>>)
           -> LogicConjunction<'a, P> {
        LogicConjunction {
            depth: depth,
            cond: cond,
            parent: parent,
            results: [Solution::None; 2],
            next: Vec::with_capacity(2),
        }
    }
}

impl<'a, P: Particle> Particle for LogicConjunction<'a, P> {
    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
    }
}

#[derive(Debug)]
struct LogicDisjunction<'a, P: 'a + Particle> {
    depth: usize,
    cond: Condition,
    parent: &'a P,
    results: [Solution; 2],
    next: Vec<&'a P>,
}

impl<'a, P: 'a + Particle> LogicDisjunction<'a, P> {
    fn new(cond: Condition,
           depth: usize,
           parent: &'a P,
           args: Option<Vec<&str>>)
           -> LogicDisjunction<'a, P> {
        LogicDisjunction {
            depth: depth,
            cond: cond,
            parent: parent,
            results: [Solution::None; 2],
            next: Vec::with_capacity(2),
        }
    }
}

impl<'a, P: Particle> Particle for LogicDisjunction<'a, P> {
    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
    }
}

#[derive(Debug)]
struct LogicAtom<'a, P: 'a + Particle, T: IsTerm> {
    depth: usize,
    cond: Condition,
    parent: &'a P,
    pred: Assert<T>,
}

impl<'a, P: 'a + Particle, T: IsTerm> LogicAtom<'a, P, T> {
    fn new(cond: Condition,
           depth: usize,
           parent: &'a P,
           term: Assert<T>,
           args: Option<Vec<&str>>)
           -> LogicAtom<'a, P, T> {
        LogicAtom {
            depth: depth,
            cond: cond,
            parent: parent,
            pred: term,
        }
    }
}

impl<'a, P: 'a + Particle, T: IsTerm> Particle for LogicAtom<'a, P, T> {
    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
    }
}
