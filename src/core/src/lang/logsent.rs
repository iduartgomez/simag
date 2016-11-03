use lang::parser::*;

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
#[derive(Debug)]
pub struct LogSentence {
    particles: Vec<Box<Particle>>,
    produced: Vec<Box<LogSentence>>,
    created: usize,
    vars: Option<Vec<Box<Var>>>,
    skolem: Option<Vec<Box<Skolem>>>,
}

impl LogSentence {
    pub fn new(ast: &Next, context: &mut Context) -> Result<LogSentence, ParseErrF> {
        let mut sent = LogSentence {
            particles: Vec::new(),
            produced: Vec::new(),
            created: 0,
            skolem: None,
            vars: None,
        };
        let r = walk_ast(ast, &mut sent, context);
        if r.is_err() {
            Err(r.unwrap_err())
        } else {
            Ok(sent)
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
}

pub struct Context {
    pub vars: Vec<*const Var>,
    pub skols: Vec<*const Skolem>,
    ancestor: Option<*const Particle>,
}

impl<'a> Context {
    pub fn new() -> Context {
        Context {
            vars: Vec::new(),
            skols: Vec::new(),
            ancestor: None,
        }
    }
}

fn walk_ast(ast: &Next,
            sent: &mut LogSentence,
            context: &mut Context)
            -> Result<Option<*mut Particle>, ParseErrF> {
    match *ast {
        Next::Assert(ref decl) => {
            let mut particle = Box::new(match decl {
                &AssertBorrowed::ClassDecl(ref decl) => {
                    let pred = Assert::ClassDecl(ClassDecl::from(decl, context));
                    let atom = LogicAtom::new(context.ancestor, pred);
                    Particle::Atom(atom)
                }
                &AssertBorrowed::FuncDecl(ref decl) => {
                    let pred = Assert::FuncDecl(FuncDecl::from(decl, context));
                    let atom = LogicAtom::new(context.ancestor, pred);
                    Particle::Atom(atom)
                }
            });
            let res = &mut *particle as *mut Particle;
            sent.add_particle(particle);
            Ok(Some(res))
        }
        Next::ASTNode(ref ast) => {
            let mut v_cnt = 0;
            let mut s_cnt = 0;
            // make vars and add to sent, also add them to local scope context
            if ast.vars.is_some() {
                for v in ast.vars.as_ref().unwrap() {
                    match *v {
                        VarDeclBorrowed::Var(ref v) => {
                            let var = Box::new(Var::from(v, context));
                            context.vars.push(&*var);
                            v_cnt += 1;
                            sent.add_var(var);
                        }
                        VarDeclBorrowed::Skolem(ref s) => {
                            let skolem = Box::new(Skolem::from(s, context));
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
                        Particle::Condition(LogicIndCond::new(context.ancestor))
                    }
                    &LogicOperator::And => {
                        Particle::Conjunction(LogicConjunction::new(context.ancestor))
                    }
                    &LogicOperator::Or => {
                        Particle::Disjunction(LogicDisjunction::new(context.ancestor))
                    }
                    &LogicOperator::Implication => {
                        Particle::Implication(LogicImplication::new(context.ancestor))
                    }
                    &LogicOperator::Biconditional => {
                        Particle::Equivalence(LogicEquivalence::new(context.ancestor))
                    }
                });
                let ptr = &mut *op as *mut Particle;
                context.ancestor = Some(ptr);
                let next = match walk_ast(&ast.next, sent, context) {
                    Ok(opt) => opt,
                    Err(err) => return Err(err),
                };
                // drop local scope vars from context
                let l = context.vars.len() - v_cnt;
                context.vars.truncate(l);
                let l = context.skols.len() - s_cnt;
                context.skols.truncate(l);

                if next.is_some() {
                    op.add_lhs(next.unwrap());
                    sent.add_particle(op);
                    Ok(Some(ptr))
                } else {
                    Ok(None)
                }
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
                let rhs = match walk_ast(&nodes[1], sent, context) {
                    Ok(opt) => opt,
                    Err(err) => return Err(err),
                };
                match rhs {
                    Some(ptr) => {
                        context.ancestor = Some(ptr);
                        let lhs = match walk_ast(&nodes[0], sent, context) {
                            Ok(opt) => opt,
                            Err(err) => return Err(err),
                        };
                        match lhs {
                            Some(p) => {
                                let mut rhs: &mut Particle = unsafe { &mut *ptr };
                                rhs.add_lhs(p);
                            }
                            None => {}
                        };
                    }
                    None => {}
                }
                Ok(rhs)
            } else {
                let len = nodes.len() - 1;
                let first: *mut Particle = match walk_ast(&nodes[0], sent, context) {
                    Ok(opt) => opt.unwrap(),
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
                context.ancestor = Some(first);
                let mut prev = first;
                if operator.is_and() {
                    for i in 1..len {
                        let ptr = match walk_ast(&nodes[i], sent, context) {
                            Ok(opt) => opt.unwrap(),
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
                            context.ancestor = Some(prev);
                        }
                    }
                } else {
                    for i in 1..len {
                        let ptr = match walk_ast(&nodes[i], sent, context) {
                            Ok(opt) => opt.unwrap(),
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
                            context.ancestor = Some(prev);
                        }
                    }
                }
                let last = walk_ast(&nodes[len], sent, context).unwrap().unwrap();
                let r = unsafe { &mut *prev };
                r.add_rhs(last);
                Ok(Some(first))
            }
        }
        Next::None => Ok(None),
    }
}

enum SolveErr {
    AssertionError(String),
}

#[derive(Debug, Clone, Copy)]
enum Solution {
    None,
}

#[derive(Debug)]
enum Particle {
    Atom(LogicAtom),
    Conjunction(LogicConjunction),
    Disjunction(LogicDisjunction),
    Implication(LogicImplication),
    Equivalence(LogicEquivalence),
    Condition(LogicIndCond),
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

    fn add_rhs(&mut self, next: *const Particle) {
        match *self {
            Particle::Conjunction(ref mut p) => p.next.push(next),
            Particle::Disjunction(ref mut p) => p.next.push(next),
            Particle::Implication(ref mut p) => p.next.push(next),
            Particle::Equivalence(ref mut p) => p.next.push(next),
            Particle::Condition(ref mut p) => p.next.push(next),
            _ => panic!("simag: expected an operator, found a predicate instead"),
        };
    }

    fn add_lhs(&mut self, next: *const Particle) {
        match *self {
            Particle::Conjunction(ref mut p) => {
                if p.next.len() == 1 {
                    p.next.insert(0, next)
                } else {
                    p.next.push(next)
                }
            }
            Particle::Disjunction(ref mut p) => {
                if p.next.len() == 1 {
                    p.next.insert(0, next)
                } else {
                    p.next.push(next)
                }
            }
            Particle::Implication(ref mut p) => {
                if p.next.len() == 1 {
                    p.next.insert(0, next)
                } else {
                    p.next.push(next)
                }
            }
            Particle::Equivalence(ref mut p) => {
                if p.next.len() == 1 {
                    p.next.insert(0, next)
                } else {
                    p.next.push(next)
                }
            }
            Particle::Condition(ref mut p) => {
                if p.next.len() == 1 {
                    p.next.insert(0, next)
                } else {
                    p.next.push(next)
                }
            }
            _ => panic!("simag: expected an operator, found a predicate instead"),
        };
    }
}

#[derive(Debug, Clone)]
struct LogicIndCond {
    parent: Option<*const Particle>,
    results: [Solution; 2],
    next: Vec<*const Particle>,
}

impl LogicIndCond {
    fn new(parent: Option<*const Particle>) -> LogicIndCond {
        let parent = match parent {
            Some(ptr) => Some(ptr),
            None => None,
        };
        LogicIndCond {
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

#[derive(Debug, Clone)]
struct LogicEquivalence {
    parent: Option<*const Particle>,
    results: [Solution; 2],
    next: Vec<*const Particle>,
}

impl LogicEquivalence {
    fn new(parent: Option<*const Particle>) -> LogicEquivalence {
        let parent = match parent {
            Some(ptr) => Some(ptr),
            None => None,
        };
        LogicEquivalence {
            parent: parent,
            results: [Solution::None; 2],
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
}

#[derive(Debug, Clone)]
struct LogicImplication {
    parent: Option<*const Particle>,
    results: [Solution; 2],
    next: Vec<*const Particle>,
}

impl LogicImplication {
    fn new(parent: Option<*const Particle>) -> LogicImplication {
        let parent = match parent {
            Some(ptr) => Some(ptr),
            None => None,
        };
        LogicImplication {
            parent: parent,
            results: [Solution::None; 2],
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
}

#[derive(Debug, Clone)]
struct LogicConjunction {
    parent: Option<*const Particle>,
    results: [Solution; 2],
    next: Vec<*const Particle>,
}

impl LogicConjunction {
    fn new(parent: Option<*const Particle>) -> LogicConjunction {
        let parent = match parent {
            Some(ptr) => Some(ptr),
            None => None,
        };
        LogicConjunction {
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

#[derive(Debug, Clone)]
struct LogicDisjunction {
    parent: Option<*const Particle>,
    results: [Solution; 2],
    next: Vec<*const Particle>,
}

impl LogicDisjunction {
    fn new(parent: Option<*const Particle>) -> LogicDisjunction {
        let parent = match parent {
            Some(ptr) => Some(ptr),
            None => None,
        };
        LogicDisjunction {
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

#[derive(Debug, Clone)]
struct LogicAtom {
    parent: Option<*const Particle>,
    pred: Assert,
}

impl LogicAtom {
    fn new(parent: Option<*const Particle>, term: Assert) -> LogicAtom {
        let parent = match parent {
            Some(ptr) => Some(ptr),
            None => None,
        };
        LogicAtom {
            parent: parent,
            pred: term,
        }
    }

    fn solve_proof(&self, proof: usize) {}

    fn substitute(&mut self, proof: usize, args: Option<Vec<&str>>) -> Result<bool, SolveErr> {
        Ok(true)
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Parent {
    Func(Func),
    Class(Class),
}

#[derive(Debug, PartialEq, Clone)]
struct Func {
    name: String,
}

#[derive(Debug, PartialEq, Clone)]
struct Class {
    name: String,
}

#[derive(Debug, PartialEq, Clone)]
struct GroundedTerm {
    term: String,
    value: f32,
    operator: CompOperator,
    parent: Parent,
    dates: Option<Vec<i32>>,
}

impl GroundedTerm {
    fn new<'a>(term: String, uval: UVal, parent: Parent, dates: Option<Vec<i32>>) -> GroundedTerm {
        let val = match uval.val {
            Number::UnsignedFloat(val) => val,
            _ => panic!(),
        };
        let op = match uval.op {
            CompOperator::Equal => CompOperator::Equal,
            _ => panic!(),
        };
        GroundedTerm {
            term: term,
            value: val,
            operator: op,
            parent: parent,
            dates: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct FreeTerm {
    term: String,
    value: f32,
    operator: CompOperator,
    parent: Parent,
    dates: Option<Vec<i32>>,
}

impl FreeTerm {
    fn new<'a>(term: String, uval: UVal, parent: Parent, dates: Option<Vec<i32>>) -> FreeTerm {
        let val = match uval.val {
            Number::UnsignedFloat(val) => val,
            _ => panic!(),
        };
        FreeTerm {
            term: term,
            value: val,
            operator: uval.op,
            parent: parent,
            dates: None,
        }
    }
}

#[test]
fn parse_tree() {
    let source = String::from("
        (   ( let x y z )
            (
                ( american[x,u=1] && weapon[y,u=1] && fn::sells[y,u>0.5;x;z] && hostile[z,u=1] )
                |> criminal[x,u=1]
            )
        )
    ");
    use lang::ParserState;
    let state = ParserState::Tell;
    let p = Parser::parse(source, &state);
    println!("{:?}", p);
    assert!(p.is_ok());
}
