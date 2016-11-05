//! `LogSentence
//! ------------
//!
//! Stores a serie of logical atoms (be them predicates or
//! connectives), that form a well-formed logic formula. These are rulesets
//! for reasoning, cataloging objects into sets/classes, and the relationships
//! between these objects.
//!
//! LogSentence types are akin to minimal working compiled programs formed
//! by compounded expressions which will evaluate with the current knowledge
//! when called and perform any subtitution in the knowledge base if pertinent.
use std::str;
use std::fmt;

use lang::parser::*;
use lang::common::*;

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
    produced: Vec<Box<LogSentence>>,
    created: usize,
    vars: Option<Vec<Box<Var>>>,
    skolem: Option<Vec<Box<Skolem>>>,
    root: Option<*const Particle>,
}

impl LogSentence {
    pub fn new(ast: &Next, context: &mut Context) -> Result<LogSentence, ParseErrF> {
        let mut sent = LogSentence {
            particles: Vec::new(),
            produced: Vec::new(),
            created: 0,
            skolem: None,
            vars: None,
            root: None,
        };
        let r = walk_ast(ast, &mut sent, context);
        if r.is_err() {
            Err(r.unwrap_err())
        } else {
            _link_sent_childs(&mut sent);
            // classify the kind of sentence and check that are correct
            if sent.vars.is_none() {
                if !context.iexpr() {
                    context.stype = SentType::Rule;
                } else {
                    return Err(ParseErrF::RuleIncludesICond);
                }
            } else if context.iexpr() && !sent.correct_iexpr() {
                return Err(ParseErrF::ICondLHS);
            }
            Ok(sent)
        }
    }

    pub fn has_vars(&self) -> bool {
        if self.vars.is_some() {
            true
        } else {
            false
        }
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

    fn correct_iexpr(&self) -> bool {
        let first: &Particle = unsafe { &*(self.root.unwrap()) };

        // test that the lhs does not include any indicative conditional connective
        fn has_icond_child(p: &Particle) -> bool {
            match p.get_next(0) {
                Some(n) => {
                    match n {
                        &Particle::IndConditional(_) => return true,
                        _ => {}
                    }
                    if has_icond_child(n) {
                        return true;
                    }
                }
                None => {}
            }
            match p.get_next(1) {
                Some(n) => {
                    match n {
                        &Particle::IndConditional(_) => return true,
                        _ => {}
                    }
                    if has_icond_child(n) {
                        return true;
                    }
                }
                None => {}
            }
            false
        }
        let icond_in_lhs = match first {
            &Particle::IndConditional(_) => {
                match first.get_next(0) {
                    Some(next) => {
                        match next {
                            &Particle::IndConditional(_) => true,
                            _ => has_icond_child(next),
                        }
                    }
                    None => false,
                }
            }
            _ => false,
        };
        if icond_in_lhs {
            return false;
        }

        // test that the rh-most-s does include only icond or 'OR' connectives
        fn icond_child_wrong_side(p: &Particle) -> bool {
            match p.get_next(1) {
                Some(n0) => {
                    match n0 {
                        &Particle::IndConditional(_) => {
                            match n0.get_next(0) {
                                Some(n1) => {
                                    if has_icond_child(n1) {
                                        return true;
                                    }
                                }
                                None => {}
                            }
                            icond_child_wrong_side(n0);
                        }
                        &Particle::Disjunction(_) => {
                            match n0.get_next(0) {
                                Some(n1) => {
                                    if has_icond_child(n1) {
                                        return true;
                                    }
                                }
                                None => {}
                            }
                            icond_child_wrong_side(n0);
                        }
                        _ => {
                            match n0.get_next(0) {
                                Some(n1) => {
                                    if has_icond_child(n1) {
                                        return true;
                                    }
                                }
                                None => {}
                            }
                        }
                    }
                }
                None => {}
            }
            false
        }
        if !icond_child_wrong_side(first) {
            true
        } else {
            false
        }
    }
}

impl fmt::Display for LogSentence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let root = unsafe { &*(self.root.unwrap()) };
        write!(f, "LogSentence({})", root)
    }
}

pub enum SentType {
    IExpr,
    Expr,
    Rule,
}

enum SolveErr {
    AssertionError(String),
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

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
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
        write!(f, "IndCond(n0: {}, n1: {})", n0, n1)
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

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        let err = format!("operators of the type `<=>` can't be on the left \
        side of sentence: `{:?}`",
                          0);
        Err(SolveErr::AssertionError(err))
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
        write!(f, "Equiv(n0: {}, n1: {})", n0, n1)
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

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        let err = format!("operators of the type `=>` can't be on the left \
        side of sentence: `{:?}`",
                          0);
        Err(SolveErr::AssertionError(err))
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
        write!(f, "Impl(n0: {}, n1: {})", n0, n1)
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

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
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
        write!(f, "Conj(n0: {}, n1: {})", n0, n1)
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

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
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
        write!(f, "Disj(n0: {}, n1: {})", n0, n1)
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

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
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
}

impl fmt::Display for LogicAtom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Atom({})", self.get_name())
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
    fn get_disjunction(&mut self) -> Result<&mut LogicDisjunction, ParseErrF> {
        match *self {
            Particle::Disjunction(ref mut p) => Ok(p),
            _ => Err(ParseErrF::IConnectAfterOr),
        }
    }

    fn get_conjunction(&mut self) -> Result<&mut LogicConjunction, ParseErrF> {
        match *self {
            Particle::Conjunction(ref mut p) => Ok(p),
            _ => Err(ParseErrF::IConnectAfterOr),
        }
    }

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

    fn get_next(&self, pos: usize) -> Option<&Particle> {
        match *self {
            Particle::Conjunction(ref p) => p.get_next(pos),
            Particle::Disjunction(ref p) => p.get_next(pos),
            Particle::Implication(ref p) => p.get_next(pos),
            Particle::Equivalence(ref p) => p.get_next(pos),
            Particle::IndConditional(ref p) => p.get_next(pos),
            Particle::Atom(ref p) => None,
        }
    }

    fn is_atom(&self) -> bool {
        match *self {
            Particle::Atom(_) => true,
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

pub struct Context {
    pub stype: SentType,
    pub vars: Vec<*const Var>,
    pub skols: Vec<*const Skolem>,
    from_chain: bool,
    in_rhs: bool,
}

impl Context {
    pub fn new() -> Context {
        Context {
            vars: Vec::new(),
            skols: Vec::new(),
            stype: SentType::Expr,
            in_rhs: true,
            from_chain: false,
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
            // make vars and add to sent, also add them to local scope context
            if ast.vars.is_some() {
                for v in ast.vars.as_ref().unwrap() {
                    match *v {
                        VarDeclBorrowed::Var(ref v) => {
                            let var = match Var::from(v, context) {
                                Err(err) => return Err(err),
                                Ok(val) => Box::new(val),
                            };
                            context.vars.push(&*var);
                            v_cnt += 1;
                            sent.add_var(var);
                        }
                        VarDeclBorrowed::Skolem(ref s) => {
                            let skolem = match Skolem::from(s, context) {
                                Err(err) => return Err(err),
                                Ok(val) => Box::new(val),
                            };
                            context.skols.push(&*skolem);
                            s_cnt += 1;
                            sent.add_skolem(skolem);
                        }
                    }
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
                // drop local scope vars from context
                let l = context.vars.len() - v_cnt;
                context.vars.truncate(l);
                let l = context.skols.len() - s_cnt;
                context.skols.truncate(l);

                if context.in_rhs {
                    op.add_rhs(next);
                } else {
                    op.add_lhs(next);
                }
                sent.add_particle(op);
                Ok(ptr)
            } else {
                let res = walk_ast(&ast.next, sent, context);
                // drop local scope vars from context
                let l = context.vars.len() - v_cnt;
                context.vars.truncate(l);
                let l = context.skols.len() - s_cnt;
                context.skols.truncate(l);
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
                let first: *mut Particle = match walk_ast(&nodes[0], sent, context) {
                    Ok(opt) => opt,
                    Err(err) => return Err(err),
                };
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
                        let ptr = match walk_ast(&nodes[i], sent, context) {
                            Ok(ptr) => ptr,
                            Err(err) => return Err(err),
                        };
                        let a = unsafe { &mut *(ptr) };
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
                        let ptr = match walk_ast(&nodes[i], sent, context) {
                            Ok(ptr) => ptr,
                            Err(err) => return Err(err),
                        };
                        let a = unsafe { &mut *(ptr) };
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

fn _link_sent_childs(sent: &mut LogSentence) {
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
    use std::collections::HashMap;
    let mut childs: HashMap<usize, (Option<usize>, Option<usize>)> = HashMap::new();
    for a in &sent.particles {
        if !addresses.contains(&(&**a as *const Particle)) {
            sent.root = Some(&**a as *const Particle);
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

#[test]
fn icond_exprs() {
    let source = String::from("
        # Err:
        ((let x y z)
         ( ( cde[x,u=1] |> fn::fgh[y,u>0.5;x;z] ) |> hij[y,u=1] )
        )

        # Err: chekear pq no pilla
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
    use lang::ParserState;
    let state = ParserState::Tell;
    let tree = Parser::parse(source, &state);
    assert!(tree.is_ok());
    let mut tree = tree.unwrap();

    unsafe {
        let sent = match tree.pop().unwrap() {
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

    let sent = match tree.pop() {
        Some(ParseTree::IExpr(sent)) => sent,
        _ => panic!(),
    };

    let sent = match tree.pop() {
        Some(ParseTree::ParseErr(ParseErrF::ICondLHS)) => {}
        _ => panic!(),
    };

    let sent = match tree.pop() {
        Some(ParseTree::ParseErr(ParseErrF::ICondLHS)) => {}
        _ => panic!(),
    };
}
