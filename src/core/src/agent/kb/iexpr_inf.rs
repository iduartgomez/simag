//! Inference infrastructure for indicative conditional expressions and
//! representation querying.
//!
//! ## Safety
//! There is some unsafe code on this module, the unsafe code performs to taks:
//!     * Assure the compiler that data being referenced will have the proper
//!       lifetimes to satisfy the compiler.
//!     * Cheat the compiler on ```Send + Sync``` impl in certain types.
//!
//! Both of those are safe because a Representation owns, uniquely, all its
//! knowledge by the duration of it's own lifetime (data is never dropped, while
//! the representation is alive), thereby is safe to point to the data being
//! referenced from the representation or the query (for the duration of the query).

use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::mem;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use rayon;
use rayon::prelude::*;

use super::repr::{Answer, Representation};
use super::VarAssignment;
use crate::agent::lang::{
    Assert, ClassDecl, FreeClsMemb, FreeClsOwner, FuncDecl, Grounded, GroundedFunc, GroundedMemb,
    LogSentence, ParseTree, Predicate, ProofResContext, SentID, Terminal, Time, Var, VarKind,
};

type ObjName<'a> = &'a str;
type QueryPred = String;
/// Whether there is a result or not, if it's true or false, and from what point in time
pub type GroundedResult = Option<(bool, Option<Time>)>;
type GroundedResults<'a> = HashMap<ObjName<'a>, GroundedResult>;
type QueryResMemb<'a> = HashMap<ObjName<'a>, Vec<Arc<GroundedMemb>>>;
type QueryResRels<'a> = HashMap<ObjName<'a>, Vec<Arc<GroundedFunc>>>;

/// A succesful query will return an `InfResult` which contains all the answer data.
/// The data can be manipulated and filtered throught various methods returning
/// whatever is requested by the consumer.
#[derive(Debug)]
pub(super) struct InfResults<'b> {
    grounded_queries: RwLock<HashMap<QueryPred, GroundedResults<'b>>>,
    membership: RwLock<HashMap<&'b Var, QueryResMemb<'b>>>,
    relationships: RwLock<HashMap<&'b Var, QueryResRels<'b>>>,
    query: Arc<QueryProcessed<'b>>,
}

impl<'b> InfResults<'b> {
    fn new(query: Arc<QueryProcessed<'b>>) -> InfResults<'b> {
        InfResults {
            grounded_queries: RwLock::new(HashMap::new()),
            membership: RwLock::new(HashMap::new()),
            relationships: RwLock::new(HashMap::new()),
            query,
        }
    }

    fn add_membership(&self, var: &'b Var, name: &'b str, membership: Arc<GroundedMemb>) {
        let mut lock = self.membership.write().unwrap();
        lock.entry(var)
            .or_insert_with(HashMap::new)
            .entry(name)
            .or_insert_with(Vec::new)
            .push(membership);
    }

    fn add_relationships(&self, var: &'b Var, rel: &[Arc<GroundedFunc>]) {
        let mut lock = self.relationships.write().unwrap();
        for func in rel {
            for obj in func.get_args_names() {
                let obj = unsafe { std::mem::transmute::<&str, &'b str>(obj) };
                lock.entry(var)
                    .or_insert_with(HashMap::new)
                    .entry(obj)
                    .or_insert_with(Vec::new)
                    .push(func.clone());
            }
        }
    }

    fn add_grounded(&self, obj: &str, pred: &str, res: Option<(bool, Option<Time>)>) {
        let obj = unsafe { std::mem::transmute::<&str, &'b str>(obj) };
        let mut lock = self.grounded_queries.write().unwrap();
        lock.entry(pred.to_string())
            .or_insert_with(HashMap::new)
            .insert(obj, res);
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

    pub fn get_results_multiple(self) -> HashMap<QueryPred, HashMap<String, GroundedResult>> {
        // WARNING: ObjName<'a> may (truly) outlive the content, own the &str first
        let orig: &mut HashMap<QueryPred, GroundedResults<'b>> =
            &mut *self.grounded_queries.write().unwrap();
        let mut res = HashMap::new();
        for (qpred, r) in orig.drain() {
            let r = HashMap::from_iter(r.into_iter().map(|(k, v)| (k.to_string(), v)));
            res.insert(qpred, r);
        }
        res
    }

    pub fn get_memberships(&self) -> HashMap<ObjName<'b>, Vec<&'b GroundedMemb>> {
        unsafe {
            let lock = self.membership.read().unwrap();
            let mut res: HashMap<ObjName<'b>, Vec<&GroundedMemb>> = HashMap::new();
            for preds in lock.values() {
                for members in preds.values() {
                    for gr in members {
                        let gr = &*(&**gr as *const GroundedMemb) as &'b GroundedMemb;
                        if res.contains_key(gr.get_name()) {
                            let prev = res.get_mut(gr.get_name()).unwrap();
                            prev.push(gr);
                        } else {
                            let mut new = vec![];
                            new.push(gr);
                            res.insert(gr.get_name(), new);
                        }
                    }
                }
            }
            res
        }
    }

    pub fn get_relationships(&self) -> HashMap<ObjName<'b>, Vec<&'b GroundedFunc>> {
        unsafe {
            let lock = self.relationships.read().unwrap();
            let mut res: HashMap<ObjName<'b>, HashSet<*const GroundedFunc>> = HashMap::new();
            for relations in lock.values() {
                for relation_ls in relations.values() {
                    for grfunc in relation_ls {
                        for name in grfunc.get_args_names() {
                            let name = mem::transmute::<&str, &'b str>(name);
                            if res.contains_key(name) {
                                let prev = res.get_mut(name).unwrap();
                                prev.insert(&**grfunc as *const GroundedFunc);
                            } else {
                                let mut new = HashSet::new();
                                new.insert(&**grfunc as *const GroundedFunc);
                                res.insert(name, new);
                            }
                        }
                    }
                }
            }
            HashMap::from_iter(res.into_iter().map(|(k, l)| {
                (
                    k,
                    l.into_iter()
                        .map(|v| &*v as &'b GroundedFunc)
                        .collect::<Vec<_>>(),
                )
            }))
        }
    }
}

pub(super) struct Inference<'a> {
    query: Arc<QueryProcessed<'a>>,
    kb: &'a Representation,
    depth: usize,
    ignore_current: bool,
    nodes: RwLock<HashMap<&'a str, Vec<ProofNode<'a>>>>,
    queue: RwLock<HashMap<usize, HashSet<PArgVal>>>, // K: *const ProofNode<'a>
    results: InfResults<'a>,
    tpool: rayon::ThreadPool,
}

impl<'a> Inference<'a> {
    pub fn try_new(
        agent: &'a Representation,
        query_input: QueryInput,
        depth: usize,
        ignore_current: bool,
        num_threads: usize,
    ) -> Result<Inference<'a>, ()> {
        let query = Arc::new(QueryProcessed::new().get_query(query_input)?);
        Ok(Inference {
            query: query.clone(),
            kb: agent,
            depth,
            ignore_current,
            nodes: RwLock::new(HashMap::new()),
            queue: RwLock::new(HashMap::new()),
            results: InfResults::new(query),
            tpool: rayon::ThreadPoolBuilder::new()
                .num_threads(num_threads)
                .build()
                .unwrap(),
        })
    }

    pub fn get_results(self) -> Answer<'a> {
        // TODO: filter results based on query composition
        Answer::new(self.results)
    }

    /// Inference function from first-order logic sentences.
    ///
    /// Gets a query from an 'ask' call, encapsulates the query subtitutions,
    /// processes it (including caching of partial results or tracking
    /// var substitution) and returns the answer to the query. If new
    /// knowledge is produced then it's passed to an other procedure for
    /// addition to the KB.
    pub fn infer_facts(&self) {
        self.tpool.install(|| self.query_cls_gr());
        self.tpool.install(|| self.query_func_gr());
        self.tpool.install(|| self.query_cls_free());
        self.tpool.install(|| self.query_func_free());
        self.tpool.install(|| self.query_cls_memb());
        self.tpool.install(|| self.query_func_memb());
    }

    fn queue_query(&self, query: &str, actv_query: &ActiveQuery) {
        let mut pass = InfTrial::new(self, actv_query);
        pass.get_rules(vec![query]);
        // run the query, if there is no result and there is an update,
        // then loop again, else stop
        loop {
            pass.unify(query, VecDeque::new(), HashSet::new());
            {
                let lock0 = pass.updated.lock().unwrap();
                let lock1 = pass.feedback.load(Ordering::SeqCst);
                if !lock0.contains(&true) || !lock1 {
                    break;
                }
            }
            pass.updated = Mutex::new(vec![]);
        }
        let obj = actv_query.get_obj();
        let pred = actv_query.get_pred();
        if pass.valid.lock().unwrap().is_none() {
            self.results.add_grounded(obj, pred, None);
        } else {
            let l = pass.valid.lock().unwrap();
            let valid = l.as_ref().unwrap();
            let node: &ProofNode = unsafe { &*(valid.node as *const ProofNode) };
            let mut context = IExprResult::new(valid.args.clone(), node);
            node.proof
                .solve(self.kb, Some(&valid.args.as_proof_input()), &mut context);
        }
    }

    #[inline]
    /// Find if a grounded class membership is true, false or unknown.
    /// ie. (person[$John,u=1])
    fn query_cls_gr(&self) {
        self.query
            .cls_queries_grounded
            .par_iter()
            .for_each(|(obj, preds)| {
                for pred in preds {
                    let query = pred.get_parent();
                    let result = if !self.ignore_current {
                        // FIXME: only checks for the current value, not for intervals
                        self.kb.class_membership_query(pred)
                    } else {
                        None
                    };
                    if result.is_some() {
                        self.results
                            .add_grounded(obj, query, Some((result.unwrap(), None)));
                    } else {
                        self.results.add_grounded(obj, query, None);
                        // if no result was found from the kb directly
                        // make an inference from a grounded fact
                        let actv_query = ActiveQuery::new_with_class(pred.clone());
                        self.queue_query(query, &actv_query);
                    }
                }
            });
    }

    #[inline]
    /// Find if a grounded function is true, false or unknown.
    /// ie. (fn::friend[$Lucy,u=0;$John])
    fn query_func_gr(&self) {
        self.query
            .func_queries_grounded
            .par_iter()
            .for_each(|pred| {
                let query: &str = pred.get_name();
                let mut result = None;
                for (i, arg) in pred.get_args().iter().enumerate() {
                    let obj = arg.get_name();
                    if !self.ignore_current {
                        result = self.kb.has_relationship(pred, obj);
                    }
                    if result.is_some() {
                        self.results
                            .add_grounded(obj, query, Some((result.unwrap(), None)));
                    } else {
                        self.results.add_grounded(obj, query, None);
                        let actv_query = ActiveQuery::new_with_func(i, pred.clone());
                        self.queue_query(query, &actv_query);
                    }
                }
            });
    }

    #[inline]
    /// Find all members (subclasses or entities) of a given class.
    /// ie. ((let x) (professor[x,u=1]))
    fn query_cls_free(&self) {
        self.query
            .cls_queries_free
            .par_iter()
            .for_each(|(var, classes)| {
                for cls in classes {
                    let cls_name = cls.get_parent();
                    let lock = self.kb.classes.read().unwrap();
                    if let Some(cls_curr) = lock.get(cls_name) {
                        let members: Vec<Arc<GroundedMemb>> = cls_curr.get_members(cls);
                        for m in members {
                            let t = unsafe { &*(&*m as *const GroundedMemb) as &GroundedMemb };
                            self.results.add_membership(var, t.get_name(), m.clone());
                        }
                    }
                }
            });
    }

    #[inline]
    /// Find all relationships of a given function type.
    /// ie. ((let x) (fn::produce[milk,u>0;x]))
    fn query_func_free(&self) {
        self.query
            .func_queries_free
            .par_iter()
            .for_each(|(var, funcs)| {
                for func in funcs {
                    let func_name = func.get_name();
                    let lock = &self.kb.classes.read().unwrap();
                    if let Some(cls_curr) = lock.get(func_name) {
                        let members: Vec<Arc<GroundedFunc>> = cls_curr.get_funcs(func);
                        self.results.add_relationships(var, &members);
                    }
                }
            });
    }

    #[inline]
    /// Find class membership of a grounded entity or class.
    /// ie. ((let x) (x[$Lucy,u>0.5]))
    fn query_cls_memb(&self) {
        self.query
            .cls_memb_query
            .par_iter()
            .for_each(|(var, objs)| {
                for obj in objs {
                    let member_of = self.kb.get_class_membership(obj);
                    for m in member_of {
                        self.results.add_membership(var, obj.get_name(), m.clone());
                    }
                }
            });
    }

    #[inline]
    /// Find relationships of a grounded entity or class.
    /// ie. ((let x, y) (fn::x[$Vicky,u=0;y]))
    fn query_func_memb(&self) {
        self.query
            .func_memb_query
            .par_iter()
            .for_each(|(var, funcs)| {
                for func in funcs {
                    let relationships = self.kb.get_relationships(func);
                    for funcs in relationships.values() {
                        self.results.add_relationships(var, funcs);
                    }
                }
            });
    }
}

#[derive(Clone)]
enum ActiveQuery {
    // (obj_name, pred_name, fn/cls decl)
    Class(Arc<GroundedMemb>),
    Func(usize, Arc<GroundedFunc>),
}

impl ActiveQuery {
    fn new_with_func(obj_arg: usize, decl: Arc<GroundedFunc>) -> ActiveQuery {
        ActiveQuery::Func(obj_arg, decl)
    }

    fn new_with_class(decl: Arc<GroundedMemb>) -> ActiveQuery {
        ActiveQuery::Class(decl)
    }

    #[inline]
    fn get_obj(&self) -> &str {
        match *self {
            ActiveQuery::Class(ref decl) => decl.get_name(),
            ActiveQuery::Func(pos, ref decl) => decl.get_arg_name(pos),
        }
    }

    #[inline]
    fn get_pred(&self) -> &str {
        match *self {
            ActiveQuery::Class(ref decl) => decl.get_parent(),
            ActiveQuery::Func(_, ref decl) => decl.get_name(),
        }
    }

    #[inline]
    fn is_func(&self) -> bool {
        match *self {
            ActiveQuery::Class(..) => false,
            ActiveQuery::Func(..) => true,
        }
    }

    #[inline]
    fn get_func(&self) -> &GroundedFunc {
        match *self {
            ActiveQuery::Func(_, ref gf) => &*gf,
            ActiveQuery::Class(_) => panic!(),
        }
    }

    #[inline]
    fn get_cls(&self) -> &GroundedMemb {
        match *self {
            ActiveQuery::Class(ref gt) => &*gt,
            ActiveQuery::Func(_, _) => panic!(),
        }
    }
}

#[derive(Debug)]
struct ProofArgs {
    ptr: usize, // *mut Vec<(&'a Var, Arc<VarAssignment<'a>>)
    hash_val: usize,
}

type PArgVal = usize;

impl ProofArgs {
    fn new<'a>(input: Vec<(&Var, Arc<VarAssignment<'a>>)>) -> ProofArgs {
        let hash_val = ProofArgs::arg_hash_val(&input[..]);
        let ptr = Box::into_raw(Box::new(input));
        ProofArgs {
            ptr: ptr as usize,
            hash_val,
        }
    }

    fn arg_hash_val(input: &[(&Var, Arc<VarAssignment>)]) -> usize {
        use std::collections::hash_map::DefaultHasher;
        let mut v = vec![];
        for &(var, ref assigned) in input {
            let mut var = format!("{:?}", var as *const Var).into_bytes();
            v.append(&mut var);
            let mut s = Vec::from_iter(assigned.name.as_bytes().iter().cloned());
            v.append(&mut s);
        }
        let mut s = DefaultHasher::new();
        v.hash(&mut s);
        s.finish() as usize
    }

    fn as_proof_input(&self) -> HashMap<&Var, &VarAssignment> {
        let data = unsafe {
            let data = self.ptr as *mut Vec<(&Var, Arc<VarAssignment>)>;
            &*data as &Vec<(&Var, Arc<VarAssignment>)>
        };
        let mut n_args = HashMap::with_capacity(data.len());
        for &(k, ref v) in data {
            n_args.insert(k, &**v);
        }
        n_args
    }
}

impl std::clone::Clone for ProofArgs {
    fn clone(&self) -> ProofArgs {
        unsafe {
            let data = self.ptr as *mut Vec<(&Var, Arc<VarAssignment>)>;
            let data = &*data as &Vec<(&Var, Arc<VarAssignment>)>;
            ProofArgs::new(data.clone())
        }
    }
}

impl std::ops::Drop for ProofArgs {
    fn drop(&mut self) {
        unsafe {
            Box::from_raw(self.ptr as *mut Vec<(&Var, Arc<VarAssignment>)>);
        }
    }
}

struct ValidAnswer {
    node: usize, // *const ProofNode<'a>,
    args: ProofArgs,
    newest_grfact: Time,
}

#[derive(Debug)]
pub(super) struct IExprResult {
    result: Option<bool>,
    newest_grfact: Time,
    antecedents: Vec<Grounded>,
    grounded_func: Vec<(GroundedFunc, Time)>,
    grounded_cls: Vec<(GroundedMemb, Time)>,
    sent_id: SentID,
    args: ProofArgs,
    node: usize, // *const ProofNode<'a>
    sub_mode: bool,
}

impl IExprResult {
    fn new(args: ProofArgs, node: &ProofNode) -> IExprResult {
        IExprResult {
            result: None,
            args,
            node: node as *const ProofNode as usize,
            newest_grfact: chrono::MIN_DATE.and_hms(0, 0, 0),
            sent_id: node.proof.get_id(),
            antecedents: vec![],
            grounded_func: vec![],
            grounded_cls: vec![],
            sub_mode: false,
        }
    }
}

impl ProofResContext for IExprResult {
    fn substituting(&mut self) {
        self.sub_mode = true;
    }

    fn is_substituting(&self) -> bool {
        self.sub_mode
    }

    fn set_inconsistent(&mut self, _val: bool) {}

    fn is_inconsistent(&self) -> bool {
        false
    }

    fn set_result(&mut self, res: Option<bool>) {
        self.result = res;
    }

    fn get_id(&self) -> SentID {
        self.sent_id
    }

    fn push_grounded_func(&mut self, grounded: GroundedFunc, time: Time) {
        self.grounded_func.push((grounded, time));
    }

    fn push_grounded_cls(&mut self, grounded: GroundedMemb, time: Time) {
        self.grounded_cls.push((grounded, time));
    }

    fn newest_grfact(&self) -> Time {
        self.newest_grfact
    }

    fn set_newest_grfact(&mut self, time: Time) {
        self.newest_grfact = time;
    }

    fn get_antecedents(&self) -> &[Grounded] {
        &self.antecedents
    }

    fn push_antecedents(&mut self, grounded: Grounded) {
        self.antecedents.push(grounded);
    }

    fn push_false_fn_assert(&mut self, _func: Arc<GroundedFunc>) {}

    fn push_false_cls_assert(&mut self, _cls: Arc<GroundedMemb>) {}

    fn compare_relation(&self, _func: &GroundedFunc) -> bool {
        false
    }

    fn compare_cls(&self, _cls: &GroundedMemb) -> bool {
        false
    }

    fn has_relationship(&self, _func: &GroundedFunc) -> Option<bool> {
        None
    }

    fn has_cls_memb(&self, _cls: &GroundedMemb) -> Option<bool> {
        None
    }
}

struct InfTrial<'a> {
    kb: &'a Representation,
    actv: ActiveQuery,
    updated: Mutex<Vec<bool>>,
    feedback: AtomicBool,
    valid: Mutex<Option<ValidAnswer>>,
    nodes: usize, // &'a RwLock<HashMap<&'a str, Vec<ProofNode<'a>>>>,
    queue: &'a RwLock<HashMap<usize, HashSet<PArgVal>>>, // K: *const ProofNode<'a>
    results: usize, // &'a InfResults<'a>,
    depth: usize,
    depth_cnt: RwLock<usize>,
    tpool: &'a rayon::ThreadPool,
}

impl<'a> InfTrial<'a> {
    fn new(inf: &'a Inference, actv_query: &ActiveQuery) -> InfTrial<'a> {
        let nodes = &inf.nodes as *const RwLock<_> as usize;
        let results = &inf.results as *const InfResults as usize;
        InfTrial {
            kb: inf.kb,
            actv: actv_query.clone(),
            updated: Mutex::new(vec![]),
            feedback: AtomicBool::new(true),
            valid: Mutex::new(None),
            nodes,
            queue: &inf.queue,
            results,
            depth: inf.depth,
            depth_cnt: RwLock::new(0_usize),
            tpool: &inf.tpool,
        }
    }

    fn unify(&self, mut parent: &'a str, mut chk: VecDeque<&'a str>, mut done: HashSet<&'a str>) {
        fn scoped_exec(inf: &InfTrial, node: &ProofNode, args: &ProofArgs) {
            let node_raw = node as *const ProofNode as usize;
            let args_done = {
                if let Some(queued) = inf.queue.read().unwrap().get(&node_raw) {
                    queued.contains(&args.hash_val)
                } else {
                    false
                }
            };
            if !args_done {
                let n_args = &args.as_proof_input();
                let mut context = IExprResult::new(args.clone(), node);
                node.proof.solve(inf.kb, Some(n_args), &mut context);
                if context.result.is_some() {
                    {
                        let mut lock0 = inf.updated.lock().unwrap();
                        lock0.push(true);
                        let mut lock1 = inf.queue.write().unwrap();
                        lock1
                            .entry(node_raw)
                            .or_insert_with(HashSet::new)
                            .insert(args.hash_val);
                    }
                    inf.add_result(context);
                }
            }
        };

        loop {
            {
                *self.valid.lock().unwrap() = None;
            }
            // for each node in the subtitution tree unifify variables
            // and try every possible substitution until (if) a solution is found
            // the proofs are tried in order of addition to the KB
            let nodes = unsafe { &*(self.nodes as *const RwLock<HashMap<&str, Vec<ProofNode>>>) };
            if let Some(nodes) = nodes.read().unwrap().get(parent) {
                // the node for each rule is stored in an efficient sorted list
                // by rule creation datetime, from newest to oldest
                // as the newest rules take precedence
                for node in nodes.iter() {
                    // recursively try unifying all possible argument with the
                    // operating logic sentence:
                    // get all the entities/classes from the kb that meet the proof requeriments
                    for var_req in node.proof.get_lhs_predicates().into_sent_req() {
                        let assignments = meet_sent_req(self.kb, &var_req);
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
                        if let Some(mapped) = mapped {
                            for args in mapped {
                                if let Some(ref valid) = *self.valid.lock().unwrap() {
                                    if valid.node == &*node as *const ProofNode as usize {
                                        break;
                                    }
                                }
                                let args: ProofArgs = ProofArgs::new(args);
                                self.tpool.install(|| scoped_exec(self, node, &args));
                            }
                        }
                        if self.feedback.load(Ordering::SeqCst) {
                            for e in node.antecedents.clone() {
                                if !done.contains(&e) && !chk.contains(&e) {
                                    chk.push_back(e);
                                }
                            }
                        }
                    }
                }
            }
            if !self.feedback.load(Ordering::SeqCst) {
                return;
            }
            if !chk.is_empty() && (*self.depth_cnt.read().unwrap() < self.depth) {
                done.insert(parent);
                self.get_rules(Vec::from_iter(chk.iter().cloned()));
                let p1 = chk.pop_front().unwrap();
                parent = p1;
            } else {
                let mut c = self.depth_cnt.write().unwrap();
                *c += 1;
                return;
            }
        }
    }

    fn add_as_last_valid(
        &self,
        context: &IExprResult,
        r_dict: &mut GroundedResults<'a>,
        time: Time,
        value: bool,
    ) {
        let query_obj = unsafe {
            let obj = self.actv.get_obj();
            std::mem::transmute::<&str, &'a str>(obj)
        };
        let mut lock = self.valid.lock().unwrap();
        if let Some(ref prev_answ) = *lock {
            if prev_answ.newest_grfact >= context.newest_grfact {
                return;
            }
        }
        r_dict.insert(query_obj, Some((value, Some(time))));
        let answ = ValidAnswer {
            node: context.node,
            args: context.args.clone(),
            newest_grfact: context.newest_grfact,
        };
        *lock = Some(answ);
    }

    fn add_result(&self, mut context: IExprResult) {
        // add category/function to the object dictionary
        // and to results dict if is the result for the query
        let query_obj = self.actv.get_obj();
        let query_pred = self.actv.get_pred();
        let is_func = self.actv.is_func();
        let results = unsafe { &*(self.results as *const InfResults) };

        let gr_cls: Vec<_> = context.grounded_cls.drain(..).collect();
        for (gt, time) in gr_cls {
            if !is_func {
                let query_cls = self.actv.get_cls();
                if query_cls.comparable(&gt) {
                    let val = query_cls == &gt;
                    let mut d = results.grounded_queries.write().unwrap();
                    let gr_results_dict = {
                        if d.contains_key(query_pred) {
                            d.get_mut(query_pred).unwrap()
                        } else {
                            let new = HashMap::new();
                            d.insert(query_pred.to_string(), new);
                            d.get_mut(query_pred).unwrap()
                        }
                    };
                    if gr_results_dict.contains_key(query_obj) {
                        let cond_ok;
                        if let Some(&Some((_, Some(cdate)))) = gr_results_dict.get(query_obj) {
                            if time >= cdate {
                                cond_ok = true;
                            } else {
                                cond_ok = false;
                            }
                        } else {
                            cond_ok = true;
                        }
                        if cond_ok {
                            self.add_as_last_valid(&context, gr_results_dict, time, val);
                        }
                    } else {
                        self.add_as_last_valid(&context, gr_results_dict, time, val);
                    }
                    self.feedback.store(false, Ordering::SeqCst);
                }
            }
        }

        let gr_func: Vec<_> = context.grounded_func.drain(..).collect();
        for (gf, time) in gr_func {
            if is_func {
                let query_func = self.actv.get_func();
                if query_func.comparable(&gf) {
                    let val = query_func == &gf;
                    let mut d = results.grounded_queries.write().unwrap();
                    let gr_results_dict = {
                        if d.contains_key(query_pred) {
                            d.get_mut(query_pred).unwrap()
                        } else {
                            let new = HashMap::new();
                            d.insert(query_pred.to_string(), new);
                            d.get_mut(query_pred).unwrap()
                        }
                    };
                    if gr_results_dict.contains_key(query_obj) {
                        let cond_ok;
                        if let Some(&Some((_, Some(cdate)))) = gr_results_dict.get(query_obj) {
                            if time >= cdate {
                                cond_ok = true;
                            } else {
                                cond_ok = false;
                            }
                        } else {
                            cond_ok = true;
                        }
                        if cond_ok {
                            self.add_as_last_valid(&context, gr_results_dict, time, val);
                        }
                    } else {
                        self.add_as_last_valid(&context, gr_results_dict, time, val);
                    }
                    self.feedback.store(false, Ordering::SeqCst);
                }
            }
        }
    }

    fn get_rules(&self, cls_ls: Vec<&str>) {
        let nodes = unsafe { &*(self.nodes as *const RwLock<HashMap<&str, Vec<ProofNode>>>) };
        let mut rules: HashSet<Arc<LogSentence>> = HashSet::new();
        for vrules in nodes.read().unwrap().values() {
            for r in vrules {
                rules.insert(r.proof.clone());
            }
        }
        for cls in cls_ls {
            if let Some(stored) = self.kb.classes.read().unwrap().get(cls) {
                let comp: HashSet<Arc<LogSentence>> = {
                    let lock = stored.beliefs.read().unwrap();
                    if let Some(beliefs) = lock.get(cls) {
                        HashSet::from_iter(beliefs.iter().cloned())
                    } else {
                        HashSet::new()
                    }
                };
                for sent in comp.difference(&rules) {
                    let mut antecedents = vec![];
                    for p in sent.get_all_lhs_predicates() {
                        let p = unsafe { &*(p as *const Assert) as &'a Assert };
                        antecedents.push(p.get_name())
                    }
                    let node = ProofNode::new(sent, antecedents);
                    for pred in sent.get_rhs_predicates() {
                        let pred = unsafe { &*(pred as *const Assert) as &'a Assert };
                        let name = pred.get_name();
                        let mut lock = nodes.write().unwrap();
                        let ls = lock.entry(name).or_insert_with(Vec::new);
                        if ls
                            .iter()
                            .map(|x| x.proof.get_id())
                            .find(|x| *x == sent.get_id())
                            .is_none()
                        {
                            ls.push(node.clone());
                        }
                        ls.sort_by(|a, b| a.proof.created.cmp(&b.proof.created).reverse());
                    }
                }
            }
        }
    }
}

pub(super) fn meet_sent_req<'a>(
    rep: &'a Representation,
    req: &'a HashMap<&Var, Vec<&'a Assert>>,
) -> Option<HashMap<&'a Var, Vec<Arc<VarAssignment<'a>>>>> {
    let mut results: HashMap<&Var, Vec<Arc<VarAssignment>>> = HashMap::new();
    for (var, asserts) in req.iter() {
        if asserts.is_empty() {
            continue;
        }
        match var.kind {
            VarKind::Time | VarKind::TimeDecl => continue,
            _ => {}
        }
        let mut cl = Vec::new();
        let mut fl = Vec::new();
        for a in asserts {
            match **a {
                Assert::FuncDecl(ref f) => {
                    fl.push(f);
                }
                Assert::ClassDecl(ref c) => {
                    cl.push(c.get_name());
                }
            }
        }
        let (class_list, funcs_list) = unsafe {
            let cl = mem::transmute::<&[&str], &'a [&'a str]>(&cl[..]);
            let fl = mem::transmute::<&[&FuncDecl], &'a [&'a FuncDecl]>(&fl[..]);
            (cl, fl)
        };
        // meet_cls_req: HashMap<&str, Vec<Arc<GroundedMemb>>>
        let meet_cls_req = rep.by_class(class_list);
        // meet_func_req: HashMap<&str, HashMap<&str, Vec<Arc<GroundedFunc>>>>
        let mut meet_func_req = rep.by_relationship(funcs_list);
        let mut i0: HashMap<&str, usize> = HashMap::new();
        for v in meet_cls_req.values() {
            for name in v.iter().map(|x| unsafe {
                let t = &*(&**x as *const GroundedMemb);
                t.get_name()
            }) {
                let cnt: &mut usize = i0.entry(name).or_insert(0);
                *cnt += 1;
            }
        }
        let mut i1: HashMap<&str, usize> = HashMap::new();
        for v in meet_func_req.values() {
            for name in v.iter().map(|(name, _)| name) {
                let cnt: &mut usize = i1.entry(name).or_insert(0);
                *cnt += 1;
            }
        }
        let i2: Vec<_>;
        let cls_filter = i0
            .iter()
            .filter(|&(_, cnt)| *cnt == class_list.len())
            .map(|(k, _)| *k);
        let func_filter = i1
            .iter()
            .filter(|&(_, cnt)| *cnt == funcs_list.len())
            .map(|(k, _)| *k);
        if !meet_func_req.is_empty() && !meet_cls_req.is_empty() {
            let c1: HashSet<&str> = cls_filter.collect();
            i2 = func_filter.filter_map(|n0| c1.get(&n0)).cloned().collect();
        } else if !meet_func_req.is_empty() {
            i2 = func_filter.collect();
        } else {
            i2 = cls_filter.collect();
        }
        for name in i2 {
            let mut gr_memb: HashMap<&str, Arc<GroundedMemb>> = HashMap::new();
            let mut gr_relations: HashMap<&str, Vec<Arc<GroundedFunc>>> = HashMap::new();
            for ls in meet_cls_req.values() {
                for e in ls {
                    if e.get_name() == name {
                        let t = unsafe { &*(&**e as *const GroundedMemb) };
                        gr_memb.insert(t.get_parent(), e.clone());
                    }
                }
            }
            for (k, map) in &mut meet_func_req {
                let ls = map.remove(name).unwrap();
                gr_relations.insert(k, ls);
            }
            if results.contains_key(var) {
                let v = results.get_mut(var).unwrap();
                v.push(Arc::new(VarAssignment {
                    name,
                    classes: gr_memb,
                    funcs: gr_relations,
                }))
            } else {
                results.insert(
                    var,
                    vec![Arc::new(VarAssignment {
                        name,
                        classes: gr_memb,
                        funcs: gr_relations,
                    })],
                );
            }
        }
        if !results.contains_key(var) {
            return None;
        }
    }
    Some(results)
}

#[derive(Debug)]
pub(super) struct ArgsProduct<'a> {
    indexes: HashMap<&'a Var, (usize, bool)>,
    input: HashMap<&'a Var, Vec<Arc<VarAssignment<'a>>>>,
    curr: &'a Var,
    done: HashSet<Vec<(*const Var, &'a str)>>,
}

impl<'a> ArgsProduct<'a> {
    pub fn product(
        input: HashMap<&'a Var, Vec<Arc<VarAssignment<'a>>>>,
    ) -> Option<ArgsProduct<'a>> {
        let mut indexes = HashMap::new();
        let mut curr = None;
        let mut first = true;
        for k in input.keys() {
            if first {
                curr = Some(*k);
                first = false;
            }
            indexes.insert(*k, (0_usize, false));
        }
        if curr.is_some() {
            Some(ArgsProduct {
                indexes,
                input,
                curr: curr.unwrap(),
                done: HashSet::new(),
            })
        } else {
            None
        }
    }
}

impl<'a> std::iter::Iterator for ArgsProduct<'a> {
    type Item = Vec<(&'a Var, Arc<VarAssignment<'a>>)>;

    fn next(&mut self) -> Option<Vec<(&'a Var, Arc<VarAssignment<'a>>)>> {
        loop {
            let mut row_0 = vec![];
            let mut val = vec![];
            for (k1, v1) in &self.input {
                let idx_1 = self.indexes[k1];
                let assign = v1[idx_1.0].clone();
                val.push((*k1 as *const Var, assign.name));
                row_0.push((*k1, assign));
            }
            if self.completed_iter() {
                return None;
            } else if !self.done.contains(&val) {
                self.done.insert(val);
                return Some(row_0);
            }
        }
    }
}

impl<'a> ArgsProduct<'a> {
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
                let curr = self.indexes.get_mut(&self.curr).unwrap();
                let l = self.input[&self.curr].len();
                if (i == lidx) && (curr.0 < l) {
                    curr.0 += 1;
                }
            } else {
                for (k, v) in &self.indexes {
                    if *k != self.curr && !v.1 {
                        self.curr = k;
                        break;
                    }
                }
                let curr = self.indexes.get_mut(&self.curr).unwrap();
                curr.1 = true;
                curr.0 = 0;
            }
            false
        }
    }
}

#[derive(Debug, Clone)]
struct ProofNode<'a> {
    proof: Arc<LogSentence>,
    antecedents: Vec<&'a str>,
}

impl<'a> ProofNode<'a> {
    fn new(proof: &Arc<LogSentence>, antecedents: Vec<&'a str>) -> ProofNode<'a> {
        ProofNode {
            proof: proof.clone(),
            antecedents,
        }
    }
}

impl<'a> std::cmp::PartialEq for ProofNode<'a> {
    fn eq(&self, other: &ProofNode) -> bool {
        self.proof.get_id() == other.proof.get_id()
    }
}

impl<'a> std::cmp::Eq for ProofNode<'a> {}

impl<'a> Hash for ProofNode<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.proof.get_id().hash(state);
    }
}

#[derive(Debug)]
pub(in crate::agent) enum QueryInput {
    AskRelationalFunc(Arc<GroundedFunc>),
    AskClassMember(Arc<GroundedMemb>),
    ManyQueries(VecDeque<ParseTree>),
}

#[derive(Debug)]
struct QueryProcessed<'b> {
    cls_queries_free: HashMap<&'b Var, Vec<&'b FreeClsMemb>>,
    cls_queries_grounded: HashMap<&'b str, Vec<Arc<GroundedMemb>>>,
    cls_memb_query: HashMap<&'b Var, Vec<&'b FreeClsOwner>>,
    func_queries_free: HashMap<&'b Var, Vec<&'b FuncDecl>>,
    func_queries_grounded: Vec<Arc<GroundedFunc>>,
    func_memb_query: HashMap<&'b Var, Vec<&'b FuncDecl>>,
    cls: Vec<Rc<ClassDecl>>,
    func: Vec<Rc<FuncDecl>>,
}

unsafe impl<'b> std::marker::Sync for QueryProcessed<'b> {}
unsafe impl<'b> std::marker::Send for QueryProcessed<'b> {}

impl<'b> QueryProcessed<'b> {
    fn new() -> QueryProcessed<'b> {
        QueryProcessed {
            cls_queries_free: HashMap::new(),
            cls_queries_grounded: HashMap::new(),
            cls_memb_query: HashMap::new(),
            func_queries_free: HashMap::new(),
            func_queries_grounded: vec![],
            func_memb_query: HashMap::new(),
            cls: vec![],
            func: vec![],
        }
    }

    fn get_query(mut self, prequery: QueryInput) -> Result<QueryProcessed<'b>, ()> {
        #![allow(clippy::needless_pass_by_value)]

        fn assert_memb(query: &mut QueryProcessed, cdecl: Rc<ClassDecl>) -> Result<(), ()> {
            let cdecl = unsafe { &*(&*cdecl as *const ClassDecl) as &ClassDecl };
            match *cdecl.get_parent() {
                Terminal::GroundedTerm(_) => {
                    for a in cdecl.get_args() {
                        match *a {
                            Predicate::FreeClsMemb(ref t) => {
                                query.push_to_clsquery_free(&*t.get_var_ref(), t);
                            }
                            Predicate::GroundedMemb(ref t) => {
                                if let Some(times) = cdecl.get_time_payload(t.get_value()) {
                                    t.overwrite_time_data(&times);
                                }
                                query.push_to_clsquery_grounded(t.get_name(), Arc::new(t.clone()));
                            }
                            _ => return Err(()), // not happening ever
                        }
                    }
                }
                Terminal::FreeTerm(_) => {
                    for a in cdecl.get_args() {
                        match *a {
                            Predicate::FreeClsOwner(ref t) => {
                                if let Some(times) = cdecl.get_time_payload(None) {
                                    t.overwrite_time_data(&times);
                                }
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

        fn assert_rel(query: &mut QueryProcessed, fdecl: Rc<FuncDecl>) -> Result<(), ()> {
            let fdecl = unsafe { &*(&*fdecl as *const FuncDecl) as &FuncDecl };
            match *fdecl.get_parent() {
                Terminal::GroundedTerm(_) => {
                    if fdecl.is_grounded() {
                        query.push_to_fnquery_grounded(fdecl.clone().into_grounded());
                    } else {
                        for a in fdecl.get_args() {
                            if let Predicate::FreeClsMemb(ref t) = *a {
                                query.push_to_fnquery_free(t.get_var_ref(), fdecl);
                            }
                        }
                    }
                }
                Terminal::FreeTerm(_) => query.ask_relationships(fdecl),
                _ => return Err(()), // keyword: incomprenhensible
            }
            Ok(())
        }

        match prequery {
            QueryInput::AskClassMember(cdecl) => {
                let t = unsafe { &*(&*cdecl as *const GroundedMemb) as &GroundedMemb };
                let name = t.get_name();
                self.push_to_clsquery_grounded(name, cdecl);
            }
            QueryInput::AskRelationalFunc(fdecl) => {
                self.func_queries_grounded.push(fdecl);
            }
            QueryInput::ManyQueries(trees) => {
                for parsetree in trees {
                    match parsetree {
                        ParseTree::Assertion(assertions) => {
                            for a in assertions {
                                if let Err(()) = match a {
                                    Assert::ClassDecl(cdecl) => {
                                        self.cls.push(Rc::new(cdecl));
                                        let cdecl = self.cls.last().unwrap().clone();
                                        assert_memb(&mut self, cdecl)
                                    }
                                    Assert::FuncDecl(fdecl) => {
                                        self.func.push(Rc::new(fdecl));
                                        let fdecl = self.func.last().unwrap().clone();
                                        assert_rel(&mut self, fdecl)
                                    }
                                } {
                                    return Err(());
                                }
                            }
                        }
                        ParseTree::Expr(expr) => {
                            let (_, preds) = expr.extract_all_predicates();
                            for a in preds {
                                if let Err(()) = match *a {
                                    Assert::ClassDecl(ref cdecl) => {
                                        self.cls.push(Rc::new(cdecl.clone()));
                                        let cdecl = self.cls.last().unwrap().clone();
                                        assert_memb(&mut self, cdecl)
                                    }
                                    Assert::FuncDecl(ref fdecl) => {
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
    fn push_to_clsquery_grounded(&mut self, term: &'b str, cls: Arc<GroundedMemb>) {
        self.cls_queries_grounded
            .entry(term)
            .or_insert_with(Vec::new)
            .push(cls);
    }

    #[inline]
    fn push_to_clsquery_free(&mut self, term: &'b Var, cls: &'b FreeClsMemb) {
        self.cls_queries_free
            .entry(term)
            .or_insert_with(Vec::new)
            .push(cls);
    }

    #[inline]
    fn push_to_fnquery_grounded(&mut self, func: GroundedFunc) {
        self.func_queries_grounded.push(Arc::new(func))
    }

    #[inline]
    fn push_to_fnquery_free(&mut self, term: &'b Var, func: &'b FuncDecl) {
        self.func_queries_free
            .entry(term)
            .or_insert_with(Vec::new)
            .push(func);
    }

    #[inline]
    fn ask_class_memb(&mut self, term: &'b FreeClsOwner) {
        self.cls_memb_query
            .entry(term.get_var_ref())
            .or_insert_with(Vec::new)
            .push(term);
    }

    #[inline]
    fn ask_relationships(&mut self, term: &'b FuncDecl) {
        self.func_memb_query
            .entry(term.get_parent().get_var_ref())
            .or_insert_with(Vec::new)
            .push(term);
    }
}
