//! Inference infrastructure
#![allow(or_fun_call)]
#![allow(mutex_atomic)]

use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::FromIterator;
use std::hash::{Hash, Hasher};
use std::sync::{RwLock, Mutex};
use std::rc::Rc;

use chrono::{UTC, DateTime};
use scoped_threadpool::Pool;

use lang;
use lang::{ParseTree, ParseErrF, GroundedClsMemb, GroundedFunc, LogSentence};
use super::repr::*;

type Date = DateTime<UTC>;

pub struct Inference<'a> {
    query: QueryProcessed<'a>,
    kb: &'a Representation,
    ignore_current: bool,
    nodes: RwLock<HashMap<Rc<String>, Vec<Box<ProofNode>>>>,
    queue: RwLock<HashMap<*const ProofNode, HashSet<PArgVal>>>,
    results: InfResults<'a>,
    args: RwLock<Vec<Rc<ProofArgs>>>,
    available_threads: Mutex<u32>,
}

type PArgVal = Vec<usize>;
type ObjName<'a> = &'a str;
type QueryPredicate<'a> = &'a str;
type QueryResult<'a> = HashMap<QueryPredicate<'a>,
                               HashMap<ObjName<'a>, Option<(bool, Option<Date>)>>>;
type QueryResMemb<'a> = HashMap<Rc<lang::Var>, HashMap<ObjName<'a>, Vec<Rc<GroundedClsMemb>>>>;
type QueryResRels<'a> = HashMap<Rc<lang::Var>, HashMap<ObjName<'a>, Vec<Rc<GroundedFunc>>>>;

/// A succesful query will return an `InfResult` which contains all the answer data.
/// The data can be manipulated and filtered throught various methods returning
/// whatever is requested by the consumer.
#[derive(Debug)]
pub struct InfResults<'a> {
    grounded_queries: RwLock<QueryResult<'a>>,
    membership: RwLock<QueryResMemb<'a>>,
    relationships: RwLock<QueryResRels<'a>>,
}

impl<'a> InfResults<'a> {
    fn new() -> InfResults<'a> {
        InfResults {
            grounded_queries: RwLock::new(HashMap::new()),
            membership: RwLock::new(HashMap::new()),
            relationships: RwLock::new(HashMap::new()),
        }
    }

    fn add_membership(&self, var: Rc<lang::Var>, name: &'a str, membership: Rc<GroundedClsMemb>) {
        let mut lock = self.membership.write().unwrap();
        lock.entry(var)
            .or_insert(HashMap::new())
            .entry(name)
            .or_insert(vec![])
            .push(membership);
    }

    fn add_relationships(&self, var: Rc<lang::Var>, relationships: &[Rc<GroundedFunc>]) {
        let mut lock = self.relationships.write().unwrap();
        for func in relationships {
            for obj in func.get_args_names() {
                let name = unsafe { &*(&*obj as *const String) as &'a str };
                lock.entry(var.clone())
                    .or_insert(HashMap::new())
                    .entry(name)
                    .or_insert(vec![])
                    .push(func.clone());
            }
        }
    }

    fn add_grounded(&self, obj: &'a str, pred: &'a str, res: Option<(bool, Option<Date>)>) {
        let mut lock = self.grounded_queries.write().unwrap();
        lock.entry(pred).or_insert(HashMap::new()).insert(obj, res);
    }

    pub fn get_results_single(&self) -> Option<bool> {
        let results = self.grounded_queries.read().unwrap();
        if results.len() == 0 {
            return None;
        }
        for r0 in results.values() {
            for r1 in r0.values() {
                if let Some((false, _)) = *r1 {
                    return Some(false);
                } else if r1.is_none() {
                    return None;
                }
            }
        }
        Some(true)
    }

    pub fn get_results_multiple(self) -> QueryResult<'a> {
        self.grounded_queries.into_inner().unwrap()
    }

    pub fn get_memberships(&'a self) -> HashMap<ObjName, Vec<&'a GroundedClsMemb>> {
        let lock = self.membership.read().unwrap();
        let mut res = HashMap::new();
        for preds in lock.values() {
            for members in preds.values() {
                for gr in members {
                    let gr = unsafe { &*(&**gr as *const GroundedClsMemb) as &'a GroundedClsMemb };
                    let name = unsafe { &*(&*gr.get_name() as *const String) as &'a str };
                    res.entry(name).or_insert(vec![]).push(gr);
                }
            }
        }
        res
    }

    pub fn get_relationships(&'a self) -> HashMap<ObjName, Vec<&'a GroundedFunc>> {
        let lock = self.relationships.read().unwrap();
        let mut res = HashMap::new();
        for relations in lock.values() {
            for relation_ls in relations.values() {
                for grfunc in relation_ls {
                    for name in grfunc.get_args_names() {
                        let name = unsafe { &*(&*name as *const String) as &'a str };
                        res.entry(name)
                            .or_insert(HashSet::new())
                            .insert(&**grfunc as *const GroundedFunc);
                    }
                }
            }
        }
        HashMap::from_iter(res.iter().map(|(k, l)| {
            (*k, l.iter().map(|v| unsafe { &**v as &'a GroundedFunc }).collect::<Vec<_>>())
        }))
    }
}

/// Carries the inference results payload or the error type in case the query
/// failed. A query can fail because it's either incomprehensible, in which case
/// it will return a `QueryErr` variant, or because parsing of the ask request failed
/// in which case it will return a `ParseErr` variant and it's payload.
#[derive(Debug)]
pub enum Answer<'a> {
    Results(InfResults<'a>),
    QueryErr,
    ParseErr(ParseErrF),
}

impl<'a> Answer<'a> {
    pub fn get_results_single(self) -> Option<bool> {
        match self {
            Answer::Results(result) => result.get_results_single(),
            _ => panic!("simag: tried to unwrap a result from an error"),
        }
    }

    pub fn get_results_multiple(self) -> QueryResult<'a> {
        match self {
            Answer::Results(result) => result.get_results_multiple(),
            _ => panic!("simag: tried to unwrap a result from an error"),
        }
    }

    pub fn get_memberships(&'a self) -> HashMap<ObjName, Vec<&'a GroundedClsMemb>> {
        match *self {
            Answer::Results(ref result) => result.get_memberships(),
            _ => panic!("simag: tried to unwrap a result from an error"),
        }
    }

    pub fn get_relationships(&'a self) -> HashMap<ObjName, Vec<&'a GroundedFunc>> {
        match *self {
            Answer::Results(ref result) => result.get_relationships(),
            _ => panic!("simag: tried to unwrap a result from an error"),
        }
    }

    pub fn is_err(&self) -> bool {
        match *self {
            Answer::Results(_) => false,
            _ => true,
        }
    }
}

impl<'a> Inference<'a> {
    pub fn new(agent: &'a Representation,
               query_input: QueryInput,
               ignore_current: bool)
               -> Result<Box<Inference<'a>>, ()> {
        let query = QueryProcessed::new().get_query(query_input)?;
        Ok(Box::new(Inference {
            query: query,
            kb: agent,
            ignore_current: ignore_current,
            nodes: RwLock::new(HashMap::new()),
            queue: RwLock::new(HashMap::new()),
            results: InfResults::new(),
            args: RwLock::new(vec![]),
            available_threads: Mutex::new(4_u32),
        }))
    }

    pub fn get_results(self) -> Answer<'a> {
        Answer::Results(self.results)
    }

    /// Inference function from first-order logic sentences.
    ///
    /// Gets a query from an 'ask' call, encapsulates the query subtitutions,
    /// processes it (including caching of partial results or tracking
    /// var substitution) and returns the answer to the query. If new
    /// knowledge is produced then it's passed to an other procedure for
    /// addition to the KB.
    pub fn infer_facts(&'a mut self) {
        fn query_cls<'a>(inf: &'a Inference<'a>, query: Rc<String>, actv_query: ActiveQuery<'a>) {
            let mut pass = Box::new(InfTrial::new(inf, actv_query));
            pass.get_rules(vec![query.clone()]);
            {
                let mut lock = inf.nodes.write().unwrap();
                for nodes in lock.values_mut() {
                    nodes.sort_by(|a, b| a.proof.created.cmp(&b.proof.created));
                }
            }
            // run the query, if there is no result and there is an update,
            // then loop again, else stop
            loop {
                pass.unify(query.clone(), VecDeque::new(), HashSet::new());
                {
                    let lock0 = pass.updated.lock().unwrap();
                    let lock1 = pass.feedback.lock().unwrap();
                    if !lock0.contains(&true) || !*lock1 {
                        break;
                    }
                }
                pass.updated = Mutex::new(vec![]);
            }
        }

        let inf_ptr = &*self as *const Inference as usize;
        let mut pool = Pool::new(*self.available_threads.lock().unwrap());

        pool.scoped(|scope| {
            for (obj, preds) in &self.query.cls_queries_grounded {
                let obj: &str = &**obj;
                for pred in preds {
                    let pred_r = *pred as *const GroundedClsMemb as usize;
                    let query = unsafe { &*(&*pred.get_parent() as *const String) as &'static str };
                    scope.execute(move || {
                        let inf: &Inference;
                        let pred: &GroundedClsMemb;
                        unsafe {
                            inf = &*(inf_ptr as *const Inference);
                            pred = &*(pred_r as *const GroundedClsMemb);
                        }
                        let result = if !inf.ignore_current {
                            inf.kb.class_membership(pred)
                        } else {
                            None
                        };
                        if result.is_some() {
                            inf.results.add_grounded(obj, query, Some((result.unwrap(), None)));
                        } else {
                            inf.results.add_grounded(obj, query, None);
                            // if no result was found from the kb directly
                            // make an inference from a grounded fact
                            let actv_query = ActiveQuery::Class(obj, &*query, pred);
                            query_cls(inf, pred.get_parent(), actv_query);
                        }
                    });
                }
            }
        });

        pool.scoped(|scope| {
            for pred in &self.query.func_queries_grounded {
                let pred_r = &**pred as *const GroundedFunc as usize;
                scope.execute(move || {
                    let inf: &Inference;
                    let pred: &GroundedFunc;
                    let query: &str;
                    unsafe {
                        inf = &*(inf_ptr as *const Inference);
                        pred = &*(pred_r as *const GroundedFunc);
                        query = &*(&*pred.name as *const String) as &'static str;
                    }
                    let mut result = None;
                    for arg in &pred.args {
                        let obj = unsafe { &*(&*arg.get_name() as *const String) as &'static str };
                        if !inf.ignore_current {
                            result = inf.kb.has_relationship(pred, arg.get_name());
                        }
                        if result.is_some() {
                            inf.results.add_grounded(&*obj, query, Some((result.unwrap(), None)));
                        } else {
                            inf.results.add_grounded(&*obj, query, None);
                            let actv_query = ActiveQuery::Func(&*obj, &*query, pred);
                            query_cls(inf, pred.get_name(), actv_query);
                        }
                    }
                });
            }
        });

        for (var, classes) in &self.query.cls_queries_free {
            for cls in classes {
                let cls_name = cls.get_parent();
                let lock = self.kb.classes.read().unwrap();
                if let Some(cls_curr) = lock.get(&cls_name) {
                    let members: Vec<Rc<GroundedClsMemb>> = cls_curr.get_members(cls);
                    for m in members {
                        let name = unsafe { &*(&*m.get_name() as *const String) as &'a str };
                        self.results.add_membership(var.clone(), name, m);
                    }
                }
            }
        }

        for (var, funcs) in &self.query.func_queries_free {
            for func in funcs {
                let func_name = func.get_name();
                let lock = self.kb.classes.read().unwrap();
                if let Some(cls_curr) = lock.get(&func_name) {
                    let members: Vec<Rc<GroundedFunc>> = cls_curr.get_funcs(func);
                    self.results.add_relationships(var.clone(), &members);
                }
            }
        }

        pool.scoped(|scope| {
            for (var, objs) in &self.query.cls_memb_query {
                let var_r = var as *const Rc<lang::Var> as usize;
                for obj in objs {
                    let obj_r = *obj as *const lang::FreeClsOwner as usize;
                    scope.execute(move || {
                        let inf: &Inference;
                        let obj: &lang::FreeClsOwner;
                        let var: Rc<lang::Var>;
                        unsafe {
                            inf = &*(inf_ptr as *const Inference);
                            obj = &*(obj_r as *const lang::FreeClsOwner);
                            var = (&*(var_r as *const Rc<lang::Var>)).clone();
                        }
                        let name = unsafe { &*(&*obj.term as *const String) as &'a str };
                        let member_of = inf.kb.get_class_membership(obj);
                        for m in member_of {
                            inf.results.add_membership(var.clone(), name, m);
                        }
                    });
                }
            }
        });

        pool.scoped(|scope| {
            for (var, funcs) in &self.query.func_memb_query {
                let var_r = &*var as *const Rc<lang::Var> as usize;
                for func in funcs {
                    let func_r = func as *const Rc<lang::FuncDecl> as usize;
                    scope.execute(move || {
                        let inf: &Inference;
                        let func: &Rc<lang::FuncDecl>;
                        let var: Rc<lang::Var>;
                        unsafe {
                            inf = &*(inf_ptr as *const Inference);
                            func = &*(func_r as *const Rc<lang::FuncDecl>);
                            var = (&*(var_r as *const Rc<lang::Var>)).clone();
                        }
                        let relationships = inf.kb.get_relationships(func);
                        for funcs in relationships.values() {
                            inf.results.add_relationships(var.clone(), funcs);
                        }
                    });
                }
            }
        });
    }
}

struct InfTrial<'a> {
    kb: &'a Representation,
    actv: ActiveQuery<'a>,
    updated: Mutex<Vec<bool>>,
    feedback: Mutex<bool>,
    valid: Mutex<Option<(*const ProofNode, Rc<ProofArgs>)>>,
    nodes: &'a RwLock<HashMap<Rc<String>, Vec<Box<ProofNode>>>>,
    queue: &'a RwLock<HashMap<*const ProofNode, HashSet<PArgVal>>>,
    results: &'a InfResults<'a>,
    args: &'a RwLock<Vec<Rc<ProofArgs>>>,
    available_threads: Mutex<u32>,
}

enum ActiveQuery<'a> {
    // `(obj_name, pred_name, fn/cls decl)`
    Class(&'a str, &'a str, &'a GroundedClsMemb),
    Func(&'a str, &'a str, &'a GroundedFunc),
}

impl<'a> ActiveQuery<'a> {
    #[inline]
    fn get_func(&self) -> &GroundedFunc {
        match *self {
            ActiveQuery::Func(_, _, gf) => gf,
            _ => panic!(),
        }
    }

    #[inline]
    fn get_cls(&self) -> &GroundedClsMemb {
        match *self {
            ActiveQuery::Class(_, _, gt) => gt,
            _ => panic!(),
        }
    }
}

type ProofArgs = Vec<(Rc<lang::Var>, Rc<VarAssignment>)>;

#[derive(Debug)]
pub struct ProofResult {
    pub result: Option<bool>,
    args: Rc<ProofArgs>,
    node: *const ProofNode,
    pub grounded: Vec<(lang::Grounded, Date)>,
}

impl ProofResult {
    fn new(args: Rc<ProofArgs>, node: &ProofNode) -> ProofResult {
        ProofResult {
            result: None,
            args: args,
            node: node as *const ProofNode,
            grounded: vec![],
        }
    }
}

impl<'a> InfTrial<'a> {
    pub fn new(inf: &'a Inference<'a>, actv_query: ActiveQuery<'a>) -> InfTrial<'a> {
        InfTrial {
            kb: inf.kb,
            actv: actv_query,
            updated: Mutex::new(vec![]),
            feedback: Mutex::new(true),
            valid: Mutex::new(None),
            nodes: &inf.nodes,
            queue: &inf.queue,
            results: &inf.results,
            args: &inf.args,
            available_threads: Mutex::new(4_u32),
        }
    }

    fn unify(&self,
             mut parent: Rc<String>,
             mut chk: VecDeque<Rc<String>>,
             mut done: HashSet<Rc<String>>) {

        fn scoped_exec(inf_ptr: usize, node_r: usize, args_r: usize) {
            let inf: &InfTrial;
            let node: &ProofNode;
            let args: Rc<ProofArgs>;
            unsafe {
                inf = &*(inf_ptr as *const InfTrial);
                node = &*(node_r as *const ProofNode);
                args = Rc::new(*Box::from_raw(args_r as *mut ProofArgs));
            }
            let arg_hash = arg_hash_val(&args);
            let node_r = node as *const ProofNode;
            let args_done = {
                if let Some(queued) = inf.queue.read().unwrap().get(&(node_r)) {
                    queued.contains(&arg_hash)
                } else {
                    false
                }
            };
            if !args_done {
                let mut n_args: HashMap<Rc<lang::Var>, &VarAssignment> =
                    HashMap::with_capacity(args.len());
                for &(ref k, ref v) in args.iter() {
                    n_args.insert(k.clone(), &*v);
                }
                let mut context = ProofResult::new(args.clone(), node);
                node.proof.solve(inf.kb, Some(n_args), &mut context);
                if context.result.is_some() {
                    {
                        let mut lock0 = inf.updated.lock().unwrap();
                        lock0.push(true);
                        let mut lock1 = inf.queue.write().unwrap();
                        lock1.entry(node_r)
                            .or_insert(HashSet::new())
                            .insert(arg_hash);
                        let mut lock3 = inf.args.write().unwrap();
                        lock3.push(args.clone());
                    }
                    inf.add_result(context);
                }
            }
        };

        let inf_ptr = &*self as *const InfTrial as usize;
        loop {
            let mut pool = Pool::new(*self.available_threads.lock().unwrap());
            {
                *self.valid.lock().unwrap() = None;
            }
            // for each node in the subtitution tree unifify variables
            // and try every possible substitution until (if) a solution is found
            // the proofs are tried in order of addition to the KB
            if let Some(nodes) = self.nodes.read().unwrap().get(&parent) {
                // the node for each rule is stored in an efficient sorted list
                // by rule creation datetime, from newest to oldest
                // as the newest rules take precedence
                for node in nodes.iter() {
                    // recursively try unifying all possible argument with the
                    // operating logic sentence:
                    // get all the entities/classes from the kb that meet the proof requeriments
                    let assignments = meet_sent_req(self.kb, node.proof.var_req.as_ref().unwrap());
                    if assignments.is_none() {
                        for e in node.antecedents.clone() {
                            if !done.contains(&e) && !chk.contains(&e) {
                                chk.push_back(e);
                            }
                        }
                        continue;
                    }
                    // lazily iterate over all possible combinations of the substitutions
                    let mapped = ArgsProduct::product(assignments.unwrap());
                    if mapped.is_some() {
                        let mapped = mapped.unwrap();
                        pool.scoped(|scope| {
                            let node_r = &**node as *const ProofNode as usize;
                            for args in mapped {
                                {
                                    let lock = self.valid.lock().unwrap();
                                    if lock.is_some() {
                                        break;
                                    }
                                }
                                let args_r = Box::into_raw(Box::new(args)) as usize;
                                scope.execute(move || scoped_exec(inf_ptr, node_r, args_r));
                            }
                        });
                    }
                    let lock = self.feedback.lock().unwrap();
                    if *lock {
                        for e in node.antecedents.clone() {
                            if !done.contains(&e) && !chk.contains(&e) {
                                chk.push_back(e);
                            }
                        }
                    }
                }
            }
            let lock = self.feedback.lock().unwrap();
            if !*lock {
                return;
            }
            if !chk.is_empty() {
                done.insert(parent);
                self.get_rules(Vec::from_iter(chk.iter().cloned()));
                let p = chk.pop_front().unwrap();
                parent = p;
            } else {
                return;
            }
        }
    }

    fn add_result(&self, mut context: ProofResult) {
        // add category/function to the object dictionary
        // and to results dict if is the result for the query
        let (query_obj, query_pred, is_func) = match self.actv {
            ActiveQuery::Class(obj, query_pred, _) => (obj, query_pred, false),
            ActiveQuery::Func(obj, query_pred, _) => (obj, query_pred, true),
        };
        if let Some(false) = context.result {
            self.results.add_grounded(query_obj, query_pred, Some((false, None)));
            return;
        }
        for subst in context.grounded.drain(..) {
            match subst {
                (lang::Grounded::Function(gf), date) => {
                    let gf: &GroundedFunc = &*gf;
                    if is_func {
                        let query_func = self.actv.get_func();
                        if query_func.comparable(gf) {
                            let val = query_func == gf;
                            let mut d = self.results.grounded_queries.write().unwrap();
                            let mut d = d.entry(query_pred).or_insert(HashMap::new());
                            if d.contains_key(query_obj) {
                                let cond_ok;
                                if let Some(&Some((_, Some(ref cdate)))) = d.get(query_obj) {
                                    if &date >= cdate {
                                        cond_ok = true;
                                    } else {
                                        cond_ok = false;
                                    }
                                } else {
                                    cond_ok = true;
                                }
                                if cond_ok {
                                    d.insert(query_obj, Some((val, Some(date))));
                                    let mut lock = self.valid.lock().unwrap();
                                    *lock = Some((context.node, context.args.clone()));
                                }
                            } else {
                                d.insert(query_obj, Some((val, Some(date))));
                                let mut lock = self.valid.lock().unwrap();
                                *lock = Some((context.node, context.args.clone()));
                            }
                            let mut lock = self.feedback.lock().unwrap();
                            *lock = false;
                        }
                    }
                }
                (lang::Grounded::Terminal(gt), date) => {
                    if !is_func {
                        let query_cls = self.actv.get_cls();
                        if query_cls.comparable(&gt) {
                            let gt: &GroundedClsMemb = &*gt;
                            let val = query_cls == gt;
                            let mut d = self.results.grounded_queries.write().unwrap();
                            let mut d = d.entry(query_pred).or_insert(HashMap::new());
                            if d.contains_key(query_obj) {
                                let cond_ok;
                                if let Some(&Some((_, Some(ref cdate)))) = d.get(query_obj) {
                                    if &date >= cdate {
                                        cond_ok = true;
                                    } else {
                                        cond_ok = false;
                                    }
                                } else {
                                    cond_ok = true;
                                }
                                if cond_ok {
                                    let mut lock = self.valid.lock().unwrap();
                                    d.insert(query_obj, Some((val, Some(date))));
                                    *lock = Some((context.node, context.args.clone()));
                                }
                            } else {
                                d.insert(query_obj, Some((val, Some(date))));
                                let mut lock = self.valid.lock().unwrap();
                                *lock = Some((context.node, context.args.clone()));
                            }
                            let mut lock = self.feedback.lock().unwrap();
                            *lock = false;
                        }
                    }
                }
            }
        }
    }

    fn get_rules(&self, cls_ls: Vec<Rc<String>>) {
        let mut rules = HashSet::new();
        for vrules in self.nodes.read().unwrap().values() {
            for r in vrules {
                rules.insert(r.proof.clone());
            }
        }
        for cls in cls_ls {
            if let Some(stored) = self.kb.classes.read().unwrap().get(&cls) {
                let lock = stored.beliefs.read().unwrap();
                let a: HashSet<Rc<LogSentence>> =
                    HashSet::from_iter(lock.get(&cls).unwrap().iter().cloned());
                for sent in a.difference(&rules) {
                    let mut antecedents = vec![];
                    for p in sent.get_lhs_predicates() {
                        antecedents.push(p.get_name())
                    }
                    let node = Box::new(ProofNode::new(sent.clone(), antecedents.clone()));
                    for pred in sent.get_rhs_predicates() {
                        let name = pred.get_name();
                        let mut lock = self.nodes.write().unwrap();
                        let mut ls = lock.entry(name).or_insert(vec![]);
                        if ls.iter()
                            .map(|x| x.proof.get_id())
                            .find(|x| *x == sent.get_id())
                            .is_none() {
                            ls.push(node.clone());
                        }
                    }
                }
            }
        }
    }
}

pub fn meet_sent_req(rep: &Representation,
                     req: &HashMap<Rc<lang::Var>, Vec<Rc<lang::Assert>>>)
                     -> Option<HashMap<Rc<lang::Var>, Vec<Rc<VarAssignment>>>> {
    let mut results: HashMap<Rc<lang::Var>, Vec<Rc<VarAssignment>>> = HashMap::new();
    for (var, asserts) in req.iter() {
        let mut class_list = Vec::new();
        let mut funcs_list = Vec::new();
        for a in asserts {
            match **a {
                lang::Assert::FuncDecl(ref f) => {
                    funcs_list.push(f);
                }
                lang::Assert::ClassDecl(ref c) => {
                    class_list.push(c.get_name());
                }
            }
        }
        // meet_cls_req: HashMap<Rc<String>, Vec<Rc<GroundedClsMemb>>>
        let meet_cls_req = rep.by_class(&class_list);
        // meet_func_req: HashMap<Rc<String>, HashMap<Rc<String>, Vec<Rc<GroundedFunc>>>>
        let mut meet_func_req = rep.by_relationship(funcs_list.as_slice());
        let mut i0: HashMap<Rc<String>, usize> = HashMap::new();
        for v in meet_cls_req.values() {
            for name in v.iter().map(|x| x.get_name()) {
                let cnt: &mut usize = i0.entry(name).or_insert(0);
                *cnt += 1;
            }
        }
        let mut i1: HashMap<Rc<String>, usize> = HashMap::new();
        for v in meet_func_req.values() {
            for name in v.iter().map(|(name, _)| name) {
                let cnt: &mut usize = i1.entry(name.clone()).or_insert(0);
                *cnt += 1;
            }
        }
        let i2: Vec<_>;
        let cls_filter = i0.iter()
            .filter(|&(_, cnt)| *cnt == class_list.len())
            .map(|(k, _)| k.clone());
        let func_filter = i1.iter()
            .filter(|&(_, cnt)| *cnt == funcs_list.len())
            .map(|(k, _)| k.clone());
        if !meet_func_req.is_empty() && !meet_cls_req.is_empty() {
            let c1: HashSet<Rc<String>> = cls_filter.collect();
            i2 = func_filter.filter_map(|n0| c1.get(&n0)).cloned().collect();
        } else if !meet_func_req.is_empty() {
            i2 = func_filter.collect();
        } else {
            i2 = cls_filter.collect();
        }
        for name in i2 {
            let mut gr_memb: HashMap<Rc<String>, Rc<GroundedClsMemb>> = HashMap::new();
            let mut gr_relations: HashMap<Rc<String>, Vec<Rc<GroundedFunc>>> = HashMap::new();
            for ls in meet_cls_req.values() {
                for e in ls {
                    if e.get_name() == name {
                        gr_memb.insert(e.get_parent(), e.clone());
                    }
                }
            }
            for (k, map) in &mut meet_func_req {
                let ls = map.remove(&name).unwrap();
                gr_relations.insert(k.clone(), ls);
            }
            if results.contains_key(var) {
                let v = results.get_mut(var).unwrap();
                v.push(Rc::new(VarAssignment {
                    name: name.clone(),
                    classes: gr_memb,
                    funcs: gr_relations,
                }))
            } else {
                results.insert(var.clone(),
                               vec![Rc::new(VarAssignment {
                                        name: name.clone(),
                                        classes: gr_memb,
                                        funcs: gr_relations,
                                    })]);
            }
        }
        if !results.contains_key(var) {
            return None;
        }
    }
    Some(results)
}

#[derive(Debug)]
pub struct ArgsProduct {
    indexes: HashMap<Rc<lang::Var>, (usize, bool)>,
    input: HashMap<Rc<lang::Var>, Vec<Rc<VarAssignment>>>,
    curr: Rc<lang::Var>,
}

impl ArgsProduct {
    pub fn product(input: HashMap<Rc<lang::Var>, Vec<Rc<VarAssignment>>>) -> Option<ArgsProduct> {
        let mut indexes = HashMap::new();
        let mut curr = None;
        let mut first = true;
        for k in input.keys() {
            if first {
                curr = Some(k.clone());
                first = false;
            }
            indexes.insert(k.clone(), (0_usize, false));
        }
        if curr.is_some() {
            Some(ArgsProduct {
                indexes: indexes,
                input: input,
                curr: curr.unwrap(),
            })
        } else {
            None
        }
    }
}

impl ::std::iter::Iterator for ArgsProduct {
    type Item = ProofArgs;

    fn next(&mut self) -> Option<ProofArgs> {
        let mut row_0 = vec![];
        for (k1, v1) in &self.input {
            let idx_1 = self.indexes[k1];
            let e = (k1.clone(), v1[idx_1.0].clone());
            row_0.push(e);
        }
        if self.completed_iter() {
            None
        } else {
            Some(row_0)
        }
    }
}

impl ArgsProduct {
    fn completed_iter(&mut self) -> bool {
        let mut max = 0;
        for v in self.indexes.values() {
            if v.1 {
                max += 1;
            }
        }
        if max == self.indexes.len() {
            true
        } else {
            let p;
            {
                let l = self.input[&self.curr].len();
                let curr = self.indexes[&self.curr];
                if curr.0 < l - 1 {
                    p = true;
                } else {
                    p = false;
                }
            }
            if p {
                let mut i = 0;
                for (j, (k, v)) in self.indexes.iter_mut().enumerate() {
                    i = j;
                    let l = self.input[k].len();
                    if (*k != self.curr) && (v.0 < l - 1) {
                        v.0 += 1;
                        break;
                    }
                }
                let lidx = self.indexes.len() - 1;
                let mut curr = self.indexes.get_mut(&self.curr).unwrap();
                let l = self.input[&self.curr].len();
                if (i == lidx) && (curr.0 < l) {
                    curr.0 += 1;
                }
            } else {
                for k in self.indexes.keys() {
                    if *k != self.curr {
                        self.curr = k.clone();
                        break;
                    }
                }
                let mut curr = self.indexes.get_mut(&self.curr).unwrap();
                curr.1 = true;
                curr.0 = 0;
            }
            false
        }
    }
}

fn arg_hash_val(input: &[(Rc<lang::Var>, Rc<VarAssignment>)]) -> Vec<usize> {
    // ProofArgs: Vec<(Rc<lang::Var>, Rc<VarAssignment>)>;
    let mut v = vec![];
    for &(ref var, ref assigned) in input {
        v.push(&**var as *const lang::Var as usize);
        v.push(&*assigned.name as *const String as usize);
    }
    v
}

#[derive(Debug, Clone)]
struct ProofNode {
    proof: Rc<LogSentence>,
    antecedents: Vec<Rc<String>>,
}

impl ProofNode {
    fn new(proof: Rc<LogSentence>, antecedents: Vec<Rc<String>>) -> ProofNode {
        ProofNode {
            proof: proof.clone(),
            antecedents: antecedents,
        }
    }
}

impl<'a> ::std::cmp::PartialEq for ProofNode {
    fn eq(&self, other: &ProofNode) -> bool {
        self.proof.get_id() == other.proof.get_id()
    }
}

impl<'a> ::std::cmp::Eq for ProofNode {}

impl<'a> Hash for ProofNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.proof.get_id().hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct VarAssignment {
    pub name: Rc<String>,
    classes: HashMap<Rc<String>, Rc<GroundedClsMemb>>,
    funcs: HashMap<Rc<String>, Vec<Rc<GroundedFunc>>>,
}

impl VarAssignment {
    #[inline]
    pub fn get_class(&self, name: Rc<String>) -> Option<&Rc<GroundedClsMemb>> {
        self.classes.get(&name)
    }

    #[inline]
    pub fn get_relationship(&self, func: &GroundedFunc) -> Option<&Rc<GroundedFunc>> {
        if let Some(funcs) = self.funcs.get(&func.get_name()) {
            for owned_f in funcs {
                if owned_f.comparable(func) {
                    return Some(owned_f);
                }
            }
        }
        None
    }
}

impl ::std::cmp::PartialEq for VarAssignment {
    fn eq(&self, other: &VarAssignment) -> bool {
        *self.name == *other.name
    }
}

impl ::std::cmp::Eq for VarAssignment {}

impl Hash for VarAssignment {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (*self.name).hash(state);
    }
}

#[derive(Debug)]
pub enum QueryInput {
    AskRelationalFunc(Rc<GroundedFunc>),
    AskClassMember(Rc<GroundedClsMemb>),
    ManyQueries(VecDeque<ParseTree>),
}

#[derive(Debug)]
struct QueryProcessed<'a> {
    cls_queries_free: HashMap<Rc<lang::Var>, Vec<&'a lang::FreeClsMemb>>,
    cls_queries_grounded: HashMap<Rc<String>, Vec<&'a lang::GroundedClsMemb>>,
    cls_memb_query: HashMap<Rc<lang::Var>, Vec<&'a lang::FreeClsOwner>>,
    func_queries_free: HashMap<Rc<lang::Var>, Vec<Rc<lang::FuncDecl>>>,
    func_queries_grounded: Vec<Rc<lang::GroundedFunc>>,
    func_memb_query: HashMap<Rc<lang::Var>, Vec<Rc<lang::FuncDecl>>>,
    vars: Vec<Rc<lang::Var>>,
    cls: Vec<Rc<lang::ClassDecl>>,
    func: Vec<Rc<lang::FuncDecl>>,
}

impl<'a> QueryProcessed<'a> {
    fn new() -> QueryProcessed<'a> {
        QueryProcessed {
            cls_queries_free: HashMap::new(),
            cls_queries_grounded: HashMap::new(),
            cls_memb_query: HashMap::new(),
            func_queries_free: HashMap::new(),
            func_queries_grounded: vec![],
            func_memb_query: HashMap::new(),
            vars: vec![],
            cls: vec![],
            func: vec![],
        }
    }

    fn get_query(mut self, prequery: QueryInput) -> Result<QueryProcessed<'a>, ()> {
        fn assert_memb(query: &mut QueryProcessed, cdecl: Rc<lang::ClassDecl>) -> Result<(), ()> {
            let cdecl = unsafe { &*(&cdecl as *const Rc<lang::ClassDecl>) as &Rc<lang::ClassDecl> };
            match *(cdecl.get_parent()) {
                lang::Terminal::GroundedTerm(_) => {
                    for a in cdecl.get_args() {
                        match *a {
                            lang::Predicate::FreeClsMemb(ref t) => {
                                query.push_to_clsquery_free(t.get_var(), t);
                            }
                            lang::Predicate::GroundedClsMemb(ref t) => {
                                query.push_to_clsquery_grounded(t.get_name(), t);
                            }
                            _ => return Err(()), // not happening ever
                        }
                    }
                }
                lang::Terminal::FreeTerm(_) => {
                    for a in cdecl.get_args() {
                        match *a {
                            lang::Predicate::FreeClsOwner(ref t) => {
                                query.ask_class_memb(t);
                            }
                            _ => return Err(()), // not happening ever
                        }
                    }
                }
                _ => return Err(()),
            }
            Ok(())
        }

        fn assert_rel(query: &mut QueryProcessed, fdecl: Rc<lang::FuncDecl>) -> Result<(), ()> {
            let fdecl = unsafe { &*(&fdecl as *const Rc<lang::FuncDecl>) as &Rc<lang::FuncDecl> };
            match *(fdecl.get_parent()) {
                lang::Terminal::GroundedTerm(_) => {
                    if fdecl.is_grounded() {
                        query.push_to_fnquery_grounded(fdecl.as_ref().clone().into_grounded());
                    } else {
                        for a in fdecl.get_args() {
                            if let lang::Predicate::FreeClsMemb(ref t) = *a {
                                query.push_to_fnquery_free(t.get_var(), fdecl.clone());
                            }
                        }
                    }
                }
                lang::Terminal::FreeTerm(_) => query.ask_relationships(fdecl.clone()),
                _ => return Err(()), // keyword: incomprenhensible
            }
            Ok(())
        }

        match prequery {
            QueryInput::AskClassMember(cdecl) => {
                let cdecl = unsafe { &*(&*cdecl as *const GroundedClsMemb) };
                self.push_to_clsquery_grounded(cdecl.get_name(), cdecl);
            }
            QueryInput::AskRelationalFunc(fdecl) => {
                self.func_queries_grounded.push(fdecl);
            }
            QueryInput::ManyQueries(trees) => {
                for parsetree in trees {
                    match parsetree {
                        lang::ParseTree::Assertion(assertions) => {
                            for a in assertions {
                                if let Err(()) = match a {
                                    lang::Assert::ClassDecl(cdecl) => {
                                        self.cls.push(Rc::new(cdecl));
                                        let cdecl = self.cls.last().unwrap().clone();
                                        assert_memb(&mut self, cdecl)
                                    }
                                    lang::Assert::FuncDecl(fdecl) => {
                                        self.func.push(Rc::new(fdecl));
                                        let fdecl = self.func.last().unwrap().clone();
                                        assert_rel(&mut self, fdecl)
                                    }
                                } {
                                    return Err(());
                                }
                            }
                        }
                        lang::ParseTree::Expr(expr) => {
                            let (mut vars, preds) = expr.extract_all_predicates();
                            self.vars.append(&mut vars);
                            for a in preds {
                                if let Err(()) = match *a {
                                    lang::Assert::ClassDecl(ref cdecl) => {
                                        self.cls.push(Rc::new(cdecl.clone()));
                                        let cdecl = self.cls.last().unwrap().clone();
                                        assert_memb(&mut self, cdecl)
                                    }
                                    lang::Assert::FuncDecl(ref fdecl) => {
                                        self.func.push(Rc::new(fdecl.clone()));
                                        let fdecl = self.func.last().unwrap().clone();
                                        assert_rel(&mut self, fdecl)
                                    }
                                } {
                                    return Err(());
                                }
                            }
                        }
                        _ => return Err(()), // an iexpr: incomprehensible
                    }
                }
            }
        }
        Ok(self)
    }

    #[inline]
    fn push_to_clsquery_grounded(&mut self, term: Rc<String>, cls: &'a lang::GroundedClsMemb) {
        self.cls_queries_grounded.entry(term).or_insert(vec![]).push(cls);
    }

    #[inline]
    fn push_to_clsquery_free(&mut self, term: Rc<lang::Var>, cls: &'a lang::FreeClsMemb) {
        self.cls_queries_free.entry(term).or_insert(vec![]).push(cls);
    }

    #[inline]
    fn push_to_fnquery_grounded(&mut self, func: lang::GroundedFunc) {
        self.func_queries_grounded.push(Rc::new(func))
    }

    #[inline]
    fn push_to_fnquery_free(&mut self, term: Rc<lang::Var>, func: Rc<lang::FuncDecl>) {
        self.func_queries_free.entry(term).or_insert(vec![]).push(func);
    }

    #[inline]
    fn ask_class_memb(&mut self, term: &'a lang::FreeClsOwner) {
        self.cls_memb_query.entry(term.parent.clone()).or_insert(vec![]).push(term);
    }

    #[inline]
    fn ask_relationships(&mut self, term: Rc<lang::FuncDecl>) {
        self.func_memb_query.entry(term.get_parent().get_var()).or_insert(vec![]).push(term);
    }
}

#[cfg(test)]
mod test {
    use agent::kb::repr::Representation;
    use std::collections::HashSet;

    #[test]
    fn temp() {
        let test_07 = String::from("
            # retrieve all objs which fit to a criteria
            (fn::produce[milk,u=1;$Lulu])
            (cow[$Lucy,u=1])
            (goat[$Vicky,u=1])
            ((let x) ((cow[x,u=1] || goat[x,u=1]) |> (female[x,u=1] && animal[x,u=1])))
            ((let x) ((female[x,u>0] && animal[x,u>0]) |> fn::produce[milk,u=1;x]))
        ");
        let q07_01 = "((let x) (fn::produce[milk,u>0;x]))".to_string();
        let rep = Representation::new();
        rep.tell(test_07).unwrap();
        let answ = rep.ask(q07_01);
        let a07_01 = answ.get_relationships();
        println!("\nANSWER:\n{:?}", a07_01);
        assert!(a07_01.contains_key("$Lulu"));
        assert!(a07_01.contains_key("$Lucy"));
        assert!(a07_01.contains_key("$Vicky"));
    }

    // #[test]
    fn ask_pred() {
        let test_01 = String::from("
            ( professor[$Lucy,u=1] )
        ");
        let q01_01 = "(professor[$Lucy,u=1] && person[$Lucy,u=1])".to_string();
        let q01_02 = "(professor[$Lucy,u=1])".to_string();
        let rep = Representation::new();
        rep.tell(test_01).unwrap();
        assert_eq!(rep.ask(q01_01).get_results_single(), None);
        assert_eq!(rep.ask(q01_02).get_results_single(), Some(true));

        let test_02 = String::from("
            ( professor[$Lucy,u=1] )
            ( dean[$John,u=1] )
            ( ( let x ) ( dean[x,u=1] |> professor[x,u=1] ) )
            ( ( let x ) ( professor[x,u=1] |> person[x,u=1] ) )
        ");
        let q02_01 = "(professor[$Lucy,u>0] && person[$Lucy,u<1])".to_string();
        let q02_02 = "(person[$John,u=1])".to_string();
        let rep = Representation::new();
        rep.tell(test_02).unwrap();
        assert_eq!(rep.ask(q02_01).get_results_single(), Some(false));
        assert_eq!(rep.ask(q02_02).get_results_single(), Some(true));

        let test_03 = String::from("
            ( fn::owns[$M1,u=1;$Nono] )
            ( missile[$M1,u=1] )
            ( american[$West,u=1] )
            ( fn::enemy[$Nono,u=1;$America] )
            (( let x, y, z )
             (( american[x,u=1] && weapon[y,u=1] && fn::sells[y,u=1;x;z] && hostile[z,u=1]  )
                 |> criminal[x,u=1] ))
            (( let x )
             (( fn::owns[x,u=1;$Nono] && missile[x,u=1] ) |> fn::sells[x,u=1;$West;$Nono] ))
            (( let x ) ( missile[x,u=1] |> weapon[x,u=1] ) )
            (( let x ) ( fn::enemy[x,u=1;$America] |> hostile[x,u=1] ) )
        ");
        let q03_01 = "(criminal[$West,u=1]) && hostile[$Nono,u=1] && weapon[$M1,u=1]".to_string();
        let rep = Representation::new();
        rep.tell(test_03).unwrap();
        assert_eq!(rep.ask(q03_01).get_results_single(), Some(true));

        let _test_04 = String::from("
            (( let x, y, t1:time, t2:time=\"*now\" )
             (( dog[x,u=1] && meat[y,u=1] && fn::eat(t1=time)[y,u=1;x] && fn::time_calc(t1<t2) )
              |> fat(time=t2)[x,u=1] ))
            ( dog[$Pancho,u=1] )
            ( meat[$M1,u=1] )
            ( fn::eat(time=\"2015.07.05.10.25\")[$M1,u=1;$Pancho] )
        ");
        let _q04_01 = "(fat(t='*now')[$Pancho,u=1])".to_string();

        let _test_05 = String::from("
            (( let x, y, t1:time, t2:time=\"2016.01.01\" )
             (( (dog[x,u=1] && meat[y,u=1] && fn::eat(t1=time)[y,u=1;x]) && fn::time_calc(t1<t2) )
              |> fat(time=t2)[x,u=1] ))
            ( dog[$Pancho,u=1] )
            ( meat[$M1,u=1] )
            ( fn::eat(time=\"2015.07.05.10.25\")[$M1,u=1;$Pancho] )
        ");
        let _q05_01 = "(fat(t='*now')[$Pancho,u=1])".to_string();

        let test_06 = String::from("
            # query for all 'professor'
            ( professor[$Lucy,u=1] )
            ( dean[$John,u=1] )
            ((let x) (dean[x,u=1] |> professor[x,u=1]))
        ");
        let q06_01 = "((let x) (professor[x,u=1]))".to_string();
        let rep = Representation::new();
        rep.tell(test_06).unwrap();
        let answ = rep.ask(q06_01);
        let a06_01 = answ.get_memberships();
        assert!(a06_01.contains_key("$Lucy"));
        assert!(a06_01.contains_key("$John"));

        let test_07 = String::from("
            # query for all classes '$Lucy' is member of
            (professor[$Lucy,u=1])
        	((let x) (professor[x,u=1] |> person[x,u=1]))
        	(ugly[$Lucy,u=0.2])
        ");
        let q07_01 = "((let x) (x[$Lucy,u>0.5]))".to_string();
        let rep = Representation::new();
        rep.tell(test_07).unwrap();
        let mut results = HashSet::new();
        results.insert("professor");
        results.insert("person");
        let answ = rep.ask(q07_01);
        let a07_01 = answ.get_memberships();
        let mut cnt = 0;
        for a in a07_01.get("$Lucy").unwrap() {
            cnt += 1;
            assert!(results.contains(&**a.get_parent()));
            assert!(&**a.get_parent() != "ugly");
        }
        assert_eq!(cnt, 2)
    }

    // #[test]
    fn ask_func() {
        let test_01 = String::from("
            ( professor[$Lucy,u=1] )
            ( dean[$John,u=1] )
            ( fn::criticize[$John,u=1;$Lucy] )
        ");
        let q01_01 = "(fn::criticize[$John,u=1;$Lucy])".to_string();
        let rep = Representation::new();
        rep.tell(test_01).unwrap();
        assert_eq!(rep.ask(q01_01).get_results_single(), Some(true));

        let test_02 = String::from("
            ( animal[cow,u=1] )
            ( female[cow,u=1] )
            ( (animal[cow,u=1] && female[cow,u=1]) |> fn::produce[milk,u=1;cow] )
        ");
        let q02_01 = "(fn::produce[milk,u=1;cow])".to_string();
        let rep = Representation::new();
        rep.tell(test_02).unwrap();
        assert_eq!(rep.ask(q02_01).get_results_single(), Some(true));

        let test_03 = String::from("
            ( professor[$Lucy,u=1] )
            ( dean[$John,u=1] )
            ( fn::criticize[$John,u=1;$Lucy] )
            ( (let x) ( dean[x,u=1] |> professor[x,u=1] ) )
            ( (let x) ( professor[x,u=1] |> person[x,u=1] ) )
            ( (let x, y)
              (( person[x,u=1] && person[y,u=1] && dean[y,u=1] && fn::criticize[y,u=1;x] )
                 |> fn::friend[x,u=0;y] ))
        ");
        let q03_01 = "(fn::friend[$Lucy,u=0;$John])".to_string();
        let rep = Representation::new();
        rep.tell(test_03).unwrap();
        assert_eq!(rep.ask(q03_01).get_results_single(), Some(true));

        let _test_05 = String::from("
            (( let x, y, t1: time=\"2015.01.01\", t2: time=\"2015.02.01\" )
             ( ( dog[x,u=1] && meat[y,u=1] && fat(time=t2)[x,u=1] && fn::time_calc(t1<t2) )
               |> fn::eat(time=t1)[y,u=1;x]
             )
            )
            ( dog[$Pancho,u=1] )
            ( meat[$M1,u=1] )
            ( fat[$Pancho,u=1] )
        ");
        let _q05_01 = "(fn::eat[$M1,u=1;$Pancho])".to_string();

        let _test_06 = String::from("
        	(( let x, y, t1: time=\"2015.01.01\", t2: time )
             ( ( dog[x,u=1] && meat[y,u=1] && fat(time=t2)[x,u=1] && fn::time_calc(t1<t2) )
               |> fn::eat(time=t1)[y,u=1;x]
             )
            )
            ( dog[$Pancho,u=1] )
            ( meat[$M1,u=1] )
            ( fat(time=\"2015.12.01\")[$Pancho,u=1] )
        ");
        let _q06_01 = "(fn::eat[$M1,u=1;$Pancho])".to_string();

        let _test_07 = String::from("
            # retrieve all objs which fit to a criteria
            (cow[$Lucy,u=1])
            (goat[$Vicky,u=1])
            ((let x) ((cow[x,u=1] || goat[x,u=1]) |> (female[x,u=1] && animal[x,u=1])))
            ((let x) ((female[x,u>0] && animal[x,u>0]) |> fn::produce[milk,u=1;x]))
        ");
        let _q07_01 = "((let x) (fn::produce[milk,u>0;x]))".to_string();

        let test_08 = String::from("
            # retrieve all relations between objects
            (fn::loves[$Vicky,u=1;$Lucy])
            (fn::worships[$Vicky,u=1;cats])
            (fn::hates[$Vicky,u=0;dogs])
        ");

        let q08_01 = "((let x) (fn::x[$Vicky,u>0;$Lucy]))".to_string();
        let rep = Representation::new();
        rep.tell(test_08).unwrap();
        let mut results = HashSet::new();
        results.insert("loves");
        results.insert("worships");
        let answ = rep.ask(q08_01);
        let a08_01 = answ.get_relationships();
        let mut cnt = 0;
        for a in a08_01.get("$Vicky").unwrap() {
            cnt += 1;
            assert!(results.contains(&**a.get_name()));
            assert!(&**a.get_name() != "hates");
        }
        assert_eq!(cnt, 2);

        let q08_02 = "((let x, y) (fn::x[$Vicky,u=0;y]))".to_string();
        let answ = rep.ask(q08_02);
        let a08_02 = answ.get_relationships();
        let mut cnt = 0;
        for a in a08_02.get("$Vicky").unwrap() {
            cnt += 1;
            assert!(&**a.get_name() == "hates");
        }
        assert_eq!(cnt, 1);
    }
}
