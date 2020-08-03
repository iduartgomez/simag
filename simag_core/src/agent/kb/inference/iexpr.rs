//! Inference infrastructure for indicative conditional expressions and
//! representation querying.
//!
//! ## Safety
//! Unsafe in this module is safe because a Representation owns, uniquely, all its
//! knowledge by the duration of it's own lifetime (data is never dropped while
//! the representation is alive), thereby is safe to point to the data being
//! referenced from the representation or the query (for the duration of the query).

use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::mem;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
};

use crate::agent::kb::{
    bms::{HasBms, OverwriteBms},
    class::Class,
    entity::Entity,
    inference::results::{GroundedResults, InfResults},
    repr::{Answer, Representation},
    VarAssignment,
};
use crate::agent::lang::{
    Assert, BuiltIns, ClassDecl, FreeClassMembership, FreeMembershipToClass, FuncDecl,
    GrTerminalKind, Grounded, GroundedFunc, GroundedMemb, LocFn, LogSentence, ParseTree, Point,
    Predicate, ProofResContext, SentID, SentVarReq, SpatialOps, Terminal, Time, TimeOps, TypeDef,
    Var,
};
use chrono::Utc;
use dashmap::DashMap;
use rayon::prelude::*;

pub(in crate::agent::kb) struct Inference<'rep> {
    query: Arc<QueryProcessed>,
    kb: &'rep Representation,
    depth: usize,
    ignore_current: bool,
    nodes: DashMap<&'rep str, Vec<ProofNode<'rep>>>,
    queue: DashMap<ProofNodeId, HashSet<PArgVal>>,
    results: InfResults<'rep>,
    tpool: &'rep rayon::ThreadPool,
}

impl<'rep> Inference<'rep> {
    pub fn try_new(
        agent: &'rep Representation,
        query_input: QueryInput,
        depth: usize,
        ignore_current: bool,
        tpool: &'rep rayon::ThreadPool,
    ) -> Result<Inference<'rep>, ()> {
        let query = Arc::new(QueryProcessed::new().get_query(query_input)?);
        Ok(Inference {
            query: query.clone(),
            kb: agent,
            depth,
            ignore_current,
            nodes: DashMap::new(),
            queue: DashMap::new(),
            results: InfResults::new(query),
            tpool,
        })
    }

    pub fn get_results(self) -> Answer<'rep> {
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
        if !self.query.cls_queries_grounded.is_empty() {
            self.tpool.install(|| self.query_cls_gr());
        }

        if !self.query.func_queries_grounded.is_empty() {
            self.tpool.install(|| self.query_func_gr());
        }

        if !self.query.cls_queries_free.is_empty() {
            self.tpool.install(|| self.query_cls_free());
        }

        if !self.query.func_queries_free.is_empty() {
            self.tpool.install(|| self.query_func_free());
        }

        if !self.query.cls_memb_query.is_empty() {
            self.tpool.install(|| self.query_cls_memb());
        }

        if !self.query.func_memb_query.is_empty() {
            self.tpool.install(|| self.query_func_memb());
        }

        if !self.query.loc_query.is_empty() {
            self.tpool.install(|| self.query_loc())
        }
    }

    /// Find if a grounded class membership is true, false or unknown.
    /// ie. (person[$John=1])
    fn query_cls_gr(&self) {
        self.query
            .cls_queries_grounded
            .par_iter()
            .for_each(|(obj, preds)| {
                for pred in preds {
                    let query = pred.get_parent();
                    let result = if !self.ignore_current {
                        // FIXME: only checks for the current value, not for time intervals
                        self.kb.class_membership_query(pred)
                    } else {
                        None
                    };
                    if let Some(result) = result {
                        self.results.add_grounded(obj, query, Some((result, None)));
                    } else {
                        self.results.add_grounded(obj, query, None);
                        // if no result was found from the kb directly
                        // make an inference from a grounded fact
                        let actv_query = ActiveQuery::new_with_class(pred.clone());
                        self.queue_query(query, actv_query);
                    }
                }
            });
    }

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
                    let obj = arg.get_name().into();
                    if !self.ignore_current {
                        result = self.kb.has_relationship(pred, obj);
                    }
                    if let Some(result) = result {
                        self.results.add_grounded(obj, query, Some((result, None)));
                    } else {
                        self.results.add_grounded(obj, query, None);
                        let actv_query = ActiveQuery::new_with_func(i, pred.clone());

                        #[cfg(debug_assertions)]
                        {
                            log::trace!("Start querying for: {}", &**pred);
                        }

                        self.queue_query(query, actv_query);
                    }
                }
            });
    }

    fn queue_query<'inf>(&self, query: &'inf str, actv_query: ActiveQuery) {
        let mut pass = InfTrial::new(self, actv_query.clone());
        pass.get_rules(std::iter::once(&query));
        // run the query, if there is no result and there is an update,
        // then loop again, else stop
        loop {
            pass.unify(query, VecDeque::new(), HashSet::new());
            if !pass.updated.contains(&true) || !pass.feedback {
                break;
            }
            pass.updated = vec![];
        }
        let obj = actv_query.get_obj();
        let pred = actv_query.get_pred();
        if pass.valid.is_none() {
            #[cfg(debug_assertions)]
            {
                log::trace!("No unification found");
            }
            self.results.add_grounded(obj, pred, None);
        } else {
            let valid = pass.valid.as_ref().unwrap();
            let context = IExprResult::new(valid.args.clone(), &valid.node);
            let proof_input = valid.args.as_proof_input();
            #[cfg(debug_assertions)]
            {
                log::trace!("Unification found, trying to solve: {:?}", proof_input);
            }
            valid.node.proof.solve(self.kb, Some(&proof_input), context);
        }
    }

    /// Find all members (subclasses or entities) of a given class.
    /// ie. ((let x) (professor[x,u=1]))
    fn query_cls_free(&self) {
        self.query
            .cls_queries_free
            .par_iter()
            .for_each(|(var, classes)| {
                for cls in classes {
                    let cls_name = cls.get_parent();
                    if let Some(cls_curr) = self.kb.classes.get(cls_name) {
                        let members: Vec<Arc<GroundedMemb>> = cls_curr.get_members(cls);
                        for m in members {
                            let t = unsafe { &*(&*m as *const GroundedMemb) as &'rep GroundedMemb };
                            self.results.add_membership(
                                var.clone(),
                                t.get_name().into(),
                                m.clone(),
                            );
                        }
                    }
                }
            });
    }

    /// Find all relationships of a given function type.
    /// ie. ((let x) (fn::produce[milk,u>0;x]))
    fn query_func_free(&self) {
        self.query
            .func_queries_free
            .par_iter()
            .for_each(|(var, funcs)| {
                for func in funcs {
                    let func_name = func.get_name();
                    let lock = &self.kb.classes;
                    if let Some(cls_curr) = lock.get(func_name) {
                        let members: Vec<Arc<GroundedFunc>> = cls_curr.get_funcs(func);
                        self.results.add_relationships(var.clone(), &members);
                    }
                }
            });
    }

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
                        let t = unsafe { &*(&*m as *const GroundedMemb) as &'rep GroundedMemb };
                        self.results
                            .add_membership(var.clone(), t.get_name().into(), m.clone());
                    }
                }
            });
    }

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
                        self.results.add_relationships(var.clone(), funcs);
                    }
                }
            });
    }

    /// Find or asserts if objects are positioned in a specified location.
    /// ie. (fn::location($John at '1.1.0'))
    fn query_loc(&self) {
        self.query.loc_query.par_iter().for_each(|loc_fn| {
            // find out objects by location
            let objs = self.kb.find_objs_by_loc(loc_fn.locate_objects());
            {
                let not_retrieved_objs =
                    objs.iter()
                        .filter_map(|(loc, term, was_located)| match was_located {
                            Some(false) | None => Some((loc, term)),
                            _ => None,
                        });

                for (point, obj) in not_retrieved_objs {
                    match &**obj {
                        GrTerminalKind::Entity(ent) => {
                            if let Some(ent) = self.kb.entities.get(ent.as_str()) {
                                let trial = MoveInfTrial {
                                    kb: self.kb,
                                    obj: Movable::Entity(&*ent),
                                    term: obj,
                                    nodes: DashMap::new(),
                                    loc: *point,
                                };
                                self.results.add_objs_in_loc(trial.unify().into_iter());
                            }
                        }
                        GrTerminalKind::Class(cls) => {
                            if let Some(cls) = self.kb.classes.get(cls.as_str()) {
                                let trial = MoveInfTrial {
                                    kb: self.kb,
                                    obj: Movable::Class(&*cls),
                                    term: obj,
                                    nodes: DashMap::new(),
                                    loc: *point,
                                };
                                self.results.add_objs_in_loc(trial.unify().into_iter());
                            }
                        }
                    }
                }
            }
            let initially_retrieved =
                objs.into_iter()
                    .filter(|(_, _, res)| if let Some(true) = res { true } else { false });
            self.results.add_objs_in_loc(initially_retrieved);

            // find all objects which are in a location
            loc_fn.free_locations().for_each(|(var, p)| {
                let objs = self.kb.find_all_objs_in_loc(p);
                self.results.add_objs_by_loc(p, var.clone(), objs);
            })
        });
    }
}

struct MoveInfTrial<'rep> {
    kb: &'rep Representation,
    obj: Movable<'rep>,
    term: &'rep Arc<GrTerminalKind<String>>,
    loc: &'rep Point,
    nodes: DashMap<&'rep str, Vec<ProofNode<'rep>>>,
}

enum Movable<'rep> {
    Class(&'rep Class),
    Entity(&'rep Entity),
}

impl<'rep> MoveInfTrial<'rep> {
    #![allow(clippy::type_complexity)]

    fn unify(self) -> Option<(&'rep Point, Arc<GrTerminalKind<String>>, Option<bool>)> {
        match self.obj {
            Movable::Entity(ent) => {
                for sent in ent.move_beliefs.read().iter().rev() {
                    let trial = self.unification_trial(sent);
                    if trial.is_some() {
                        return trial;
                    }
                }
            }
            Movable::Class(cls) => {
                for sent in cls.move_beliefs.read().iter().rev() {
                    let trial = self.unification_trial(sent);
                    if trial.is_some() {
                        return trial;
                    }
                }
            }
        }
        None
    }

    fn unification_trial(
        &self,
        sent: &Arc<LogSentence>,
    ) -> Option<(&'rep Point, Arc<GrTerminalKind<String>>, Option<bool>)> {
        // try unifying from last sentence to first
        add_rule_node(sent, &self.nodes);
        if self.run_trial(sent) {
            // find out if this moved the obj
            let it = std::iter::once((self.term, self.loc));
            if let Some(res) = self
                .kb
                .find_objs_by_loc(it)
                .into_iter()
                .find(|(_, _, res)| res.is_some())
            {
                return Some(res);
            }
        }
        None
    }

    fn run_trial(&self, sent: &Arc<LogSentence>) -> bool {
        let sent_req: SentVarReq = sent.get_lhs_predicates().into();
        for var_requirements in sent_req {
            if let Some(assignments) = meet_sent_requirements(self.kb, &var_requirements) {
                if let Some(mapped) = ArgsProduct::product(assignments) {
                    for args in mapped {
                        let args: ProofArgs = ProofArgs::new(args);
                        let n_args = &args.as_proof_input();
                        let mut antecedents = vec![];
                        for p in sent.get_all_lhs_predicates() {
                            antecedents.push(p.get_name())
                        }
                        let node = ProofNode::new(sent, antecedents);
                        let context = IExprResult::new(args.clone(), &node);
                        let solved_proof = node.proof.solve(self.kb, Some(n_args), context);
                        if solved_proof.result.is_some() {
                            return true;
                        }
                    }
                }
            }
        }
        false
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
            ActiveQuery::Class(ref decl) => decl.get_name().into(),
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
            ActiveQuery::Class(_) => unreachable!(),
        }
    }

    #[inline]
    fn get_cls(&self) -> &GroundedMemb {
        match *self {
            ActiveQuery::Class(ref gt) => &*gt,
            ActiveQuery::Func(_, _) => unreachable!(),
        }
    }
}

type PArgVal = u64;

#[derive(Clone, Debug)]
struct ProofArgs<'rep> {
    assignments: Vec<(&'rep Var, Arc<VarAssignment<'rep>>)>,
    hash_val: PArgVal,
}

impl<'rep> ProofArgs<'rep> {
    fn new(assignments: Vec<(&'rep Var, Arc<VarAssignment<'rep>>)>) -> ProofArgs<'rep> {
        let hash_val = ProofArgs::arg_hash_val(&assignments[..]);
        ProofArgs {
            assignments,
            hash_val,
        }
    }

    fn arg_hash_val(input: &[(&Var, Arc<VarAssignment>)]) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        let mut s = DefaultHasher::new();
        for &(var, ref assigned) in input {
            var.name.as_bytes().hash(&mut s);
            assigned.name.as_bytes().hash(&mut s);
        }
        s.finish()
    }

    fn as_proof_input(&self) -> HashMap<&Var, &VarAssignment> {
        let mut n_args = HashMap::with_capacity(self.assignments.len());
        for &(k, ref v) in &self.assignments {
            n_args.insert(k, &**v);
        }
        n_args
    }
}

struct ValidAnswer<'rep, 'inf> {
    node: &'inf ProofNode<'rep>,
    args: ProofArgs<'rep>,
    newest_grfact: Time,
}

#[derive(Debug)]
pub struct IExprResult<'rep: 'inf, 'inf> {
    result: Option<bool>,
    newest_grfact: Time,
    antecedents: Vec<Grounded>,
    grounded_func: Vec<(GroundedFunc, Time)>,
    grounded_cls: Vec<(GroundedMemb, Time)>,
    sent: &'inf LogSentence,
    global_subtitution_time: Time,
    args: ProofArgs<'rep>,
    node: &'inf ProofNode<'rep>,
    sub_mode: bool,
}

impl<'rep, 'inf> IExprResult<'rep, 'inf> {
    fn new(args: ProofArgs<'rep>, node: &'inf ProofNode<'rep>) -> IExprResult<'rep, 'inf> {
        IExprResult {
            result: None,
            args,
            node,
            newest_grfact: chrono::MIN_DATE.and_hms(0, 0, 0),
            sent: &node.proof,
            global_subtitution_time: Utc::now(),
            antecedents: vec![],
            grounded_func: vec![],
            grounded_cls: vec![],
            sub_mode: false,
        }
    }
}

impl<'rep, 'inf> ProofResContext for IExprResult<'rep, 'inf> {
    fn sent(&self) -> &LogSentence {
        self.sent
    }

    fn get_production_time(&self) -> Time {
        self.global_subtitution_time
    }

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
        self.sent.id
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

struct InfTrial<'rep, 'inf> {
    kb: &'rep Representation,
    actv: ActiveQuery,
    updated: Vec<bool>,
    feedback: bool,
    valid: Option<ValidAnswer<'rep, 'inf>>,
    nodes: &'inf DashMap<&'rep str, Vec<ProofNode<'rep>>>,
    queue: &'inf DashMap<ProofNodeId, HashSet<PArgVal>>,
    results: &'inf InfResults<'rep>,
    depth: usize,
    depth_cnt: AtomicUsize,
}

impl<'rep, 'inf> InfTrial<'rep, 'inf> {
    fn new(inf: &'inf Inference<'rep>, actv_query: ActiveQuery) -> InfTrial<'rep, 'inf> {
        InfTrial {
            kb: inf.kb,
            actv: actv_query,
            updated: vec![],
            feedback: true,
            valid: None,
            nodes: &inf.nodes,
            queue: &inf.queue,
            results: &inf.results,
            depth: inf.depth,
            depth_cnt: AtomicUsize::new(0_usize),
        }
    }

    fn unify<'a>(
        &mut self,
        mut parent: &'a str,
        mut chk: VecDeque<&'a str>,
        mut done: HashSet<&'a str>,
    ) {
        loop {
            self.valid = None;
            // for each node in the subtitution tree unifify variables
            // and try every possible substitution until (if) a solution is found
            // the proofs are tried in order of addition to the KB

            if let Some(nodes) = self.nodes.get(parent) {
                // the node for each rule is stored in an efficient sorted list
                // by rule creation datetime, from newest to oldest
                // as the newest rules take precedence
                for node in nodes.value().iter() {
                    #[cfg(debug_assertions)]
                    {
                        log::trace!("Querying sentence: {}", &*node.proof);
                    }

                    // Safety: lifetimes of node fit perfectly fine here because both:
                    // a) node ref does not escape this scope
                    // b) the contents of node have lifetime 'rep
                    let node = unsafe { std::mem::transmute::<&ProofNode, &ProofNode<'_>>(node) };

                    // recursively try unifying all possible argument with the
                    // operating logic sentence:
                    // get all the entities/classes from the kb that meet the proof requeriments
                    let sent_req: SentVarReq = node.proof.get_lhs_predicates().into();
                    for var_requirements in sent_req {
                        if let Some(assignments) =
                            meet_sent_requirements(self.kb, &var_requirements)
                        {
                            self.iterate_argument_permutations(node, assignments);
                            if self.feedback {
                                for e in node.antecedents.clone() {
                                    if !done.contains(&e) && !chk.contains(&e) {
                                        chk.push_back(e);
                                    }
                                }
                            }
                        } else {
                            for e in node.antecedents.clone() {
                                if !done.contains(&e) && !chk.contains(&e) {
                                    chk.push_back(e);
                                }
                            }
                        }
                    }
                }
            }

            if !self.feedback {
                return;
            }

            if !chk.is_empty() && (self.depth_cnt.load(Ordering::Acquire) < self.depth) {
                done.insert(parent);
                self.get_rules(chk.iter());
                let p1 = chk.pop_front().unwrap();
                parent = p1;
            } else {
                self.depth_cnt.fetch_add(1, Ordering::SeqCst);
                return;
            }
        }
    }

    /// Lazily iterate over all possible combinations of the substitutions
    fn iterate_argument_permutations(
        &mut self,
        node: &'inf ProofNode<'rep>,
        assignments: HashMap<&'rep Var, Vec<Arc<VarAssignment<'rep>>>>,
    ) {
        if let Some(mapped) = ArgsProduct::product(assignments) {
            for args in mapped {
                if let Some(ref valid) = self.valid {
                    if valid.node == node {
                        break;
                    }
                }
                #[cfg(debug_assertions)]
                {
                    let permutation: Vec<_> = args.iter().map(|(v, a)| (v, &*a)).collect();
                    log::trace!(
                        "Params (hash: {}): {:?}",
                        ProofArgs::arg_hash_val(&args),
                        &*permutation
                    );
                }
                let args: ProofArgs = ProofArgs::new(args);
                self.unification_trial(node, args);
            }
        }
    }

    fn unification_trial(&mut self, node: &'inf ProofNode<'rep>, args: ProofArgs<'rep>) {
        let args_done = {
            if let Some(queued) = self.queue.get(&node.proof.id) {
                queued.contains(&args.hash_val)
            } else {
                false
            }
        };
        if !args_done {
            let n_args = &args.as_proof_input();
            let context = IExprResult::new(args.clone(), node);
            let solved_proof = node.proof.solve(self.kb, Some(n_args), context);
            if solved_proof.result.is_some() {
                self.updated.push(true);
                self.queue
                    .entry(node.proof.id)
                    .or_insert_with(HashSet::new)
                    .insert(args.hash_val);
                self.add_result(solved_proof);
            }
        }
    }

    fn add_as_last_valid(
        &mut self,
        context: &IExprResult<'rep, 'inf>,
        r_dict: &mut GroundedResults<'rep>,
        time: Time,
        value: bool,
    ) {
        let query_obj = unsafe {
            let obj = self.actv.get_obj();
            std::mem::transmute::<&str, &'rep str>(obj)
        };
        if let Some(ref prev_answ) = self.valid {
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
        self.valid = Some(answ);
    }

    fn add_result(&mut self, mut context: IExprResult<'rep, 'inf>) {
        // add category/function to the object dictionary
        // and to results dict if is the result for the query
        let is_func = self.actv.is_func();

        let gr_cls: Vec<_> = context.grounded_cls.drain(..).collect();
        for (gt, time) in gr_cls {
            if !is_func {
                let query_cls = self.actv.get_cls();
                if query_cls.comparable(&gt) {
                    let val = query_cls.compare_ignoring_times(&gt);
                    let d = &self.results.grounded_queries;
                    let mut gr_results_dict = {
                        if d.contains_key(self.actv.get_pred()) {
                            d.get_mut(self.actv.get_pred()).unwrap()
                        } else {
                            let new = HashMap::new();
                            d.insert(self.actv.get_pred().to_string(), new);
                            d.get_mut(self.actv.get_pred()).unwrap()
                        }
                    };
                    if gr_results_dict.contains_key(self.actv.get_obj()) {
                        let cond_ok;
                        if let Some(&Some((_, Some(cdate)))) =
                            gr_results_dict.get(self.actv.get_obj())
                        {
                            if time >= cdate {
                                cond_ok = true;
                            } else {
                                cond_ok = false;
                            }
                        } else {
                            cond_ok = true;
                        }
                        if cond_ok {
                            self.add_as_last_valid(
                                &context,
                                gr_results_dict.value_mut(),
                                time,
                                val,
                            );
                        }
                    } else {
                        self.add_as_last_valid(&context, gr_results_dict.value_mut(), time, val);
                    }
                    self.feedback = false;
                }
            }
        }

        let gr_func: Vec<_> = context.grounded_func.drain(..).collect();
        for (gf, time) in gr_func {
            if is_func {
                let query_func = self.actv.get_func();
                if query_func.comparable(&gf) {
                    let val = query_func.compare_ignoring_times(&gf);
                    let d = &self.results.grounded_queries;
                    let mut gr_results_dict = {
                        if d.contains_key(self.actv.get_pred()) {
                            d.get_mut(self.actv.get_pred()).unwrap()
                        } else {
                            let new = HashMap::new();
                            d.insert(self.actv.get_pred().to_string(), new);
                            d.get_mut(self.actv.get_pred()).unwrap()
                        }
                    };
                    if gr_results_dict.contains_key(self.actv.get_obj()) {
                        let cond_ok;
                        if let Some(&Some((_, Some(cdate)))) =
                            gr_results_dict.get(self.actv.get_obj())
                        {
                            if time >= cdate {
                                cond_ok = true;
                            } else {
                                cond_ok = false;
                            }
                        } else {
                            cond_ok = true;
                        }
                        if cond_ok {
                            self.add_as_last_valid(
                                &context,
                                gr_results_dict.value_mut(),
                                time,
                                val,
                            );
                        }
                    } else {
                        self.add_as_last_valid(&context, gr_results_dict.value_mut(), time, val);
                    }
                    self.feedback = false;
                }
            }
        }
    }

    fn get_rules<'a, T, U>(&self, cls_ls: T)
    where
        T: Iterator<Item = U>,
        U: std::ops::Deref<Target = &'a str>,
    {
        let mut rules: HashSet<Arc<LogSentence>> = HashSet::new();
        for vrules in self.nodes.iter() {
            for r in vrules.value() {
                rules.insert(r.proof.clone());
            }
        }
        for cls in cls_ls {
            if let Some(stored) = self.kb.classes.get(*cls) {
                let comp: HashSet<Arc<LogSentence>> = {
                    if let Some(beliefs) = stored.beliefs.get(*cls) {
                        HashSet::from_iter(beliefs.iter().cloned())
                    } else {
                        HashSet::new()
                    }
                };
                for sent in comp.difference(&rules) {
                    add_rule_node(sent, &self.nodes);
                }
            }
        }
    }
}

fn add_rule_node<'rep>(sent: &Arc<LogSentence>, nodes: &DashMap<&'rep str, Vec<ProofNode<'rep>>>) {
    let mut antecedents = vec![];
    for p in sent.get_all_lhs_predicates() {
        let p = unsafe { &*(p as *const Assert) as &'rep Assert };
        antecedents.push(p.get_name())
    }
    let node = ProofNode::new(sent, antecedents);
    for pred in sent.get_rhs_predicates() {
        let pred = unsafe { &*(pred as *const Assert) as &'rep Assert };
        let name = pred.get_name();
        let mut ls = nodes.entry(name).or_insert_with(Vec::new);
        if ls
            .iter()
            .map(|x| x.proof.id)
            .find(|x| *x == sent.id)
            .is_none()
        {
            ls.push(node.clone());
        }
        ls.sort_by(|a, b| a.proof.created.cmp(&b.proof.created).reverse());
    }
}

pub(in crate::agent::kb) fn meet_sent_requirements<'rep>(
    rep: &'rep Representation,
    req: &HashMap<&'rep Var, Vec<&'rep Assert>>,
) -> Option<HashMap<&'rep Var, Vec<Arc<VarAssignment<'rep>>>>> {
    let mut results: HashMap<&Var, Vec<Arc<VarAssignment>>> = HashMap::new();
    for (var, asserts) in req.iter() {
        if asserts.is_empty() {
            continue;
        }
        match var.ty {
            TypeDef::Time | TypeDef::TimeDecl | TypeDef::Location | TypeDef::LocDecl => continue,
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
                _ => unreachable!(),
            }
        }
        let (class_list, funcs_list) = unsafe {
            let cl = mem::transmute::<&[&str], &'rep [&'rep str]>(&cl[..]);
            let fl = mem::transmute::<&[&FuncDecl], &'rep [&'rep FuncDecl]>(&fl[..]);
            (cl, fl)
        };

        // meet_cls_req: HashMap<&str, Vec<Arc<GroundedMemb>>>
        let meet_cls_req = rep.by_class(class_list);
        let mut meeting_class_req: HashMap<&str, usize> = HashMap::new();
        for v in meet_cls_req.values() {
            for name in v.iter().map(|x| unsafe {
                let t = &*(&**x as *const GroundedMemb);
                t.get_name()
            }) {
                let cnt: &mut usize = meeting_class_req.entry(name.into()).or_insert(0);
                *cnt += 1;
            }
        }

        // meet_func_req: HashMap<&str, HashMap<&str, Vec<Arc<GroundedFunc>>>>
        let mut meet_func_req = rep.by_relationship(funcs_list);
        let mut meeting_func_req: HashMap<&str, usize> = HashMap::new();
        for v in meet_func_req.values() {
            for name in v.iter().map(|(name, _)| name) {
                let cnt: &mut usize = meeting_func_req.entry(name).or_insert(0);
                *cnt += 1;
            }
        }

        let filtered_entities: Vec<_> = {
            let cls_filter = meeting_class_req
                .iter()
                .filter(|&(_, cnt)| *cnt == class_list.len())
                .map(|(k, _)| *k);
            let func_filter = meeting_func_req
                .iter()
                .filter(|&(_, cnt)| *cnt == funcs_list.len())
                .map(|(k, _)| *k);
            if !meet_func_req.is_empty() && !meet_cls_req.is_empty() {
                let c1: HashSet<&str> = cls_filter.collect();
                func_filter.filter_map(|n0| c1.get(&n0)).cloned().collect()
            } else if !meet_func_req.is_empty() {
                func_filter.collect()
            } else {
                cls_filter.collect()
            }
        };

        for name in filtered_entities {
            let mut gr_memb: HashMap<&str, Arc<GroundedMemb>> = HashMap::new();
            let mut gr_relations: HashMap<&str, Vec<Arc<GroundedFunc>>> = HashMap::new();
            for ls in meet_cls_req.values() {
                for e in ls {
                    let e_name: &str = e.get_name().into();
                    if e_name == name {
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
                    name: GrTerminalKind::from(name),
                    classes: gr_memb,
                    funcs: gr_relations,
                }))
            } else {
                results.insert(
                    var,
                    vec![Arc::new(VarAssignment {
                        name: GrTerminalKind::from(name),
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
pub(in crate::agent::kb) struct ArgsProduct<'a> {
    var_assignments: Vec<(&'a Var, Vec<Arc<VarAssignment<'a>>>)>,
    grouping: usize,
    counter: Vec<usize>,
    total_combos: usize,
    current: usize,
}

impl<'a> ArgsProduct<'a> {
    /// Panics if any of the assignments is empty
    pub fn product(
        input: HashMap<&'a Var, Vec<Arc<VarAssignment<'a>>>>,
    ) -> Option<ArgsProduct<'a>> {
        let grouping = input.len();
        let mut var_assignments = Vec::with_capacity(input.len());
        let mut counter = Vec::with_capacity(input.len());
        let mut total_combos = 1;

        for (var, assignments) in input {
            assert!(!assignments.is_empty());
            total_combos *= assignments.len();
            counter.push(0);
            var_assignments.push((var, assignments));
        }

        // This ensures the lazy iterator algorithm later works correctly
        var_assignments.sort_by_key(|k| (k.1.len(), &k.0.name));

        if !var_assignments.is_empty() {
            Some(ArgsProduct {
                grouping,
                var_assignments,
                counter,
                total_combos,
                current: 0,
            })
        } else {
            None
        }
    }

    fn iter(&mut self) -> <Self as Iterator>::Item {
        let mut group = Vec::with_capacity(self.grouping);
        let mut advanced = false;
        for selected in 0..self.grouping {
            let idx = self.counter[selected];
            let (var, assigned_to_var) = &self.var_assignments[selected];
            let assignment = assigned_to_var[idx].clone();
            group.push((*var, assignment));

            let max_idx = assigned_to_var.len() - 1;
            let next = selected + 1;

            if next < self.grouping && self.counter[next] < self.var_assignments[next].1.len() {
                // peeked forward, is ok to advance next, so don't advance this item
                advanced = false;
            } else if idx < max_idx && !advanced {
                // peeking forward failed, so advance this if possible
                self.counter[selected] = idx + 1;
                advanced = true;
            } else if idx == max_idx {
                // iterated over all possible positions of this, try to advance something else
                self.counter[selected] = 0; // reset this item position
                advanced = false;
                if selected > 0 {
                    // first item cannot advance other items
                    let previous = selected - 1;
                    if self.counter[previous] < (self.var_assignments[previous].1.len() - 1) {
                        // peeked backwards and was ok to advance
                        self.counter[previous] += 1;
                        advanced = true;
                    } else if next == self.grouping
                        && self.counter[0] < (self.var_assignments[0].1.len() - 1)
                    {
                        // peeked first and was ok to advance
                        self.counter[0] += 1;
                        advanced = true;
                        // set rest of items to first position to start over
                        for i in 1..self.grouping {
                            self.counter[i] = 0;
                        }
                    }
                }
            }
        }

        group
    }
}

impl<'a> std::iter::Iterator for ArgsProduct<'a> {
    type Item = Vec<(&'a Var, Arc<VarAssignment<'a>>)>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.total_combos {
            self.current += 1;
            Some(self.iter())
        } else {
            None
        }
    }
}

type ProofNodeId = u64;

#[derive(Debug, Clone)]
struct ProofNode<'rep> {
    proof: Arc<LogSentence>,
    antecedents: Vec<&'rep str>,
}

impl<'rep> ProofNode<'rep> {
    fn new(proof: &Arc<LogSentence>, antecedents: Vec<&'rep str>) -> ProofNode<'rep> {
        ProofNode {
            proof: proof.clone(),
            antecedents,
        }
    }
}

impl<'rep> std::cmp::PartialEq for ProofNode<'rep> {
    fn eq(&self, other: &ProofNode) -> bool {
        self.proof.id == other.proof.id
    }
}

impl<'rep> std::cmp::Eq for ProofNode<'rep> {}

impl<'rep> Hash for ProofNode<'rep> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.proof.id.hash(state);
    }
}

#[derive(Debug)]
pub(in crate::agent) enum QueryInput {
    AskRelationalFunc(Arc<GroundedFunc>),
    AskClassMember(Arc<GroundedMemb>),
    AskLocation(LocFn<String>),
    ManyQueries(VecDeque<ParseTree>),
}

#[derive(Debug)]
pub(in crate::agent::kb) struct QueryProcessed {
    cls_queries_free: HashMap<Arc<Var>, Vec<FreeMembershipToClass>>,
    cls_queries_grounded: HashMap<String, Vec<Arc<GroundedMemb>>>,
    cls_memb_query: HashMap<Arc<Var>, Vec<FreeClassMembership>>,
    func_queries_free: HashMap<Arc<Var>, Vec<Arc<FuncDecl>>>,
    func_queries_grounded: Vec<Arc<GroundedFunc>>,
    func_memb_query: HashMap<Arc<Var>, Vec<Arc<FuncDecl>>>,
    loc_query: Vec<LocFn<String>>,
}

impl QueryProcessed {
    fn new() -> QueryProcessed {
        QueryProcessed {
            cls_queries_free: HashMap::new(),
            cls_queries_grounded: HashMap::new(),
            cls_memb_query: HashMap::new(),
            func_queries_free: HashMap::new(),
            func_queries_grounded: Vec::new(),
            func_memb_query: HashMap::new(),
            loc_query: Vec::new(),
        }
    }

    fn get_query(mut self, prequery: QueryInput) -> Result<QueryProcessed, ()> {
        fn build_query_bms<T>(cdecl: &ClassDecl, query: &T) -> Result<(), ()>
        where
            T: HasBms,
        {
            let locs = cdecl.get_spatial_payload();
            let times = cdecl.get_time_payload(query.get_value());
            match (times, locs) {
                (Some(times), None) => {
                    if let Some(bms) = query.get_bms() {
                        bms.overwrite_data(times.into())?;
                    }
                }
                (None, Some(locs)) => {
                    if let Some(bms) = query.get_bms() {
                        bms.overwrite_loc_data(locs)?;
                    }
                }
                (Some(times), Some(locs)) => {
                    let merged = times.merge_spatial_data(locs)?;
                    if let Some(bms) = query.get_bms() {
                        bms.overwrite_data(merged)?;
                    }
                }
                _ => {}
            }
            Ok(())
        }

        fn assert_memb(query: &mut QueryProcessed, mut cdecl: Arc<ClassDecl>) -> Result<(), ()> {
            for a in Arc::get_mut(&mut cdecl).ok_or_else(|| ())?.get_args_mut() {
                if let Predicate::GroundedMemb(t) = a {
                    if let Some(bms) = t.bms.as_mut() {
                        Arc::get_mut(bms).ok_or_else(|| ())?.of_predicate();
                    }
                }
            }
            match cdecl.get_parent() {
                Terminal::GroundedTerm(_) => {
                    for a in cdecl.get_args() {
                        match a {
                            Predicate::FreeMembershipToClass(t) => {
                                query.push_to_clsquery_free(t.get_var(), t.clone());
                            }
                            Predicate::GroundedMemb(t) => {
                                build_query_bms(&*cdecl, t)?;
                                let cls: &str = t.get_name().into();
                                query
                                    .push_to_clsquery_grounded(cls.to_owned(), Arc::new(t.clone()));
                            }
                            _ => return Err(()), // should not happen ever
                        }
                    }
                }
                Terminal::FreeTerm(_) => {
                    for a in cdecl.get_args() {
                        match a {
                            Predicate::FreeClassMembership(t) => {
                                build_query_bms(&*cdecl, t)?;
                                query.ask_class_memb(t.clone());
                            }
                            _ => return Err(()), // should not happen ever
                        }
                    }
                }
            }
            Ok(())
        }

        fn assert_rel(query: &mut QueryProcessed, fdecl: Arc<FuncDecl>) -> Result<(), ()> {
            match *fdecl.get_parent() {
                Terminal::GroundedTerm(_) => {
                    if fdecl.is_grounded() {
                        let mut fgr: GroundedFunc = Arc::try_unwrap(fdecl).map_err(|_| ())?.into();
                        Arc::get_mut(&mut fgr.bms).unwrap().of_predicate();
                        query.push_to_fnquery_grounded(fgr);
                    } else {
                        for a in fdecl.get_args() {
                            if let Predicate::FreeMembershipToClass(ref t) = *a {
                                query.push_to_fnquery_free(t.get_var(), fdecl.clone());
                            }
                        }
                    }
                }
                Terminal::FreeTerm(_) => query.ask_relationships(fdecl),
            }
            Ok(())
        }

        match prequery {
            QueryInput::AskClassMember(cdecl) => {
                let name: &str = cdecl.get_name().into();
                self.push_to_clsquery_grounded(name.to_owned(), cdecl);
            }
            QueryInput::AskRelationalFunc(fdecl) => {
                self.func_queries_grounded.push(fdecl);
            }
            QueryInput::AskLocation(loc_fn) => self.loc_query.push(loc_fn),
            QueryInput::ManyQueries(trees) => {
                for parsetree in trees {
                    match parsetree {
                        ParseTree::Assertion(assertions) => {
                            for a in assertions {
                                match a {
                                    Assert::ClassDecl(cdecl) => {
                                        let cdecl = Arc::new(cdecl);
                                        assert_memb(&mut self, cdecl)?;
                                    }
                                    Assert::FuncDecl(fdecl) => {
                                        let fdecl = Arc::new(fdecl);
                                        assert_rel(&mut self, fdecl)?;
                                    }
                                    Assert::SpecialFunc(BuiltIns::Location(loc_fn)) => {
                                        self.loc_query.push(loc_fn)
                                    }
                                    _ => unreachable!(),
                                }
                            }
                        }
                        ParseTree::Expr(expr) => {
                            let (_, preds) = expr.extract_all_predicates();
                            for a in preds {
                                match a {
                                    Assert::ClassDecl(cdecl) => {
                                        let cdecl = Arc::new(cdecl);
                                        assert_memb(&mut self, cdecl)?;
                                    }
                                    Assert::FuncDecl(fdecl) => {
                                        let fdecl = Arc::new(fdecl);
                                        assert_rel(&mut self, fdecl)?;
                                    }
                                    _ => unreachable!(),
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
    fn push_to_clsquery_grounded(&mut self, term: String, cls: Arc<GroundedMemb>) {
        self.cls_queries_grounded
            .entry(term)
            .or_insert_with(Vec::new)
            .push(cls);
    }

    #[inline]
    fn push_to_clsquery_free(&mut self, term: Arc<Var>, cls: FreeMembershipToClass) {
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
    fn push_to_fnquery_free(&mut self, term: Arc<Var>, func: Arc<FuncDecl>) {
        self.func_queries_free
            .entry(term)
            .or_insert_with(Vec::new)
            .push(func);
    }

    #[inline]
    fn ask_class_memb(&mut self, term: FreeClassMembership) {
        self.cls_memb_query
            .entry(term.get_var())
            .or_insert_with(Vec::new)
            .push(term);
    }

    #[inline]
    fn ask_relationships(&mut self, term: Arc<FuncDecl>) {
        self.func_memb_query
            .entry(term.get_parent().get_var())
            .or_insert_with(Vec::new)
            .push(term);
    }
}

#[test]
fn args_iterator() {
    let x = Var::from("x");

    let y = Var::from("y");

    let z = Var::from("z");

    let a = Arc::new(VarAssignment {
        name: GrTerminalKind::Class("A"),
        classes: HashMap::new(),
        funcs: HashMap::new(),
    });

    let b = Arc::new(VarAssignment {
        name: GrTerminalKind::Class("B"),
        classes: HashMap::new(),
        funcs: HashMap::new(),
    });

    let c = Arc::new(VarAssignment {
        name: GrTerminalKind::Class("C"),
        classes: HashMap::new(),
        funcs: HashMap::new(),
    });

    fn compare(expecting: &HashSet<u64>, input: HashMap<&Var, Vec<Arc<VarAssignment>>>) {
        let actual = ArgsProduct::product(input)
            .unwrap()
            .map(|a| ProofArgs::arg_hash_val(&a))
            .collect::<HashSet<_>>();
        assert_eq!(expecting, &actual);
    }

    // different sized assignments
    {
        let hash0 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, a.clone()), (&z, a.clone())]);
        let hash1 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, b.clone()), (&z, a.clone())]);
        let hash2 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, a.clone()), (&z, b.clone())]);
        let hash3 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, b.clone()), (&z, b.clone())]);
        let hash4 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, a.clone()), (&z, c.clone())]);
        let hash5 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, b.clone()), (&z, c.clone())]);
        let expecting = HashSet::from_iter(vec![hash0, hash1, hash2, hash3, hash4, hash5]);

        for _ in 0..100 {
            // repeat multiple times to guarantee different ordering in the inputs
            let mut input1 = HashMap::with_capacity(3);
            input1.insert(&x, vec![a.clone()]);
            input1.insert(&y, vec![a.clone(), b.clone()]);
            input1.insert(&z, vec![a.clone(), b.clone(), c.clone()]);

            compare(&expecting, input1);
        }
    }

    // equally sized assignments
    {
        let hash0 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, a.clone())]);
        let hash1 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, b.clone())]);
        let hash2 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, c.clone())]);
        let hash3 = ProofArgs::arg_hash_val(&[(&x, b.clone()), (&y, a.clone())]);
        let hash4 = ProofArgs::arg_hash_val(&[(&x, b.clone()), (&y, b.clone())]);
        let hash5 = ProofArgs::arg_hash_val(&[(&x, b.clone()), (&y, c.clone())]);
        let hash6 = ProofArgs::arg_hash_val(&[(&x, c.clone()), (&y, a.clone())]);
        let hash7 = ProofArgs::arg_hash_val(&[(&x, c.clone()), (&y, b.clone())]);
        let hash8 = ProofArgs::arg_hash_val(&[(&x, c.clone()), (&y, c.clone())]);
        let expecting = HashSet::from_iter(vec![
            hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7, hash8,
        ]);

        for _ in 0..100 {
            // repeat multiple times to guarantee different ordering in the inputs
            let mut input2 = HashMap::with_capacity(3);
            input2.insert(&x, vec![a.clone(), b.clone(), c.clone()]);
            input2.insert(&y, vec![a.clone(), b.clone(), c.clone()]);

            compare(&expecting, input2);
        }
    }

    // multiple equally sized assignments
    {
        let hash0 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, a.clone()), (&z, a.clone())]);
        let hash1 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, a.clone()), (&z, b.clone())]);
        let hash2 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, b.clone()), (&z, a.clone())]);
        let hash3 = ProofArgs::arg_hash_val(&[(&x, a.clone()), (&y, b.clone()), (&z, b.clone())]);
        let hash4 = ProofArgs::arg_hash_val(&[(&x, b.clone()), (&y, a.clone()), (&z, a.clone())]);
        let hash5 = ProofArgs::arg_hash_val(&[(&x, b.clone()), (&y, a.clone()), (&z, b.clone())]);
        let hash6 = ProofArgs::arg_hash_val(&[(&x, b.clone()), (&y, b.clone()), (&z, a.clone())]);
        let hash7 = ProofArgs::arg_hash_val(&[(&x, b.clone()), (&y, b.clone()), (&z, b.clone())]);
        let expecting =
            HashSet::from_iter(vec![hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7]);

        for _ in 0..100 {
            // repeat multiple times to guarantee different ordering in the inputs
            let mut input3 = HashMap::with_capacity(3);
            input3.insert(&x, vec![a.clone(), b.clone()]);
            input3.insert(&y, vec![a.clone(), b.clone()]);
            input3.insert(&z, vec![a.clone(), b.clone()]);

            compare(&expecting, input3);
        }
    }
}
