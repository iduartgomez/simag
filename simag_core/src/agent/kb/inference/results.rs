use parking_lot::RwLock;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::mem;
use std::sync::Arc;

use crate::agent::kb::inference::iexpr::QueryProcessed;
use crate::agent::lang::{GroundedFunc, GroundedMemb, Time, Var};

type ObjName<'a> = &'a str;
type QueryPred = String;
/// Whether there is a result or not, if it's true or false, and from what point in time
pub type GroundedResult = Option<(bool, Option<Time>)>;
pub type GroundedResults<'a> = HashMap<ObjName<'a>, GroundedResult>;
type QueryResMemb<'a> = HashMap<ObjName<'a>, Vec<Arc<GroundedMemb>>>;
type QueryResRels<'a> = HashMap<ObjName<'a>, Vec<Arc<GroundedFunc>>>;

/// A succesful query will return an `InfResult` which contains all the answer data.
/// The data can be manipulated and filtered throught various methods returning
/// whatever is requested by the consumer.
#[derive(Debug)]
pub(in crate::agent::kb) struct InfResults<'b> {
    pub grounded_queries: RwLock<HashMap<QueryPred, GroundedResults<'b>>>,
    membership: RwLock<HashMap<&'b Var, QueryResMemb<'b>>>,
    relationships: RwLock<HashMap<&'b Var, QueryResRels<'b>>>,
    query: Arc<QueryProcessed<'b>>,
}

impl<'b> InfResults<'b> {
    pub fn new(query: Arc<QueryProcessed<'b>>) -> InfResults<'b> {
        InfResults {
            grounded_queries: RwLock::new(HashMap::new()),
            membership: RwLock::new(HashMap::new()),
            relationships: RwLock::new(HashMap::new()),
            query,
        }
    }

    pub fn add_membership(&self, var: &'b Var, name: &'b str, membership: Arc<GroundedMemb>) {
        let mut lock = self.membership.write();
        lock.entry(var)
            .or_insert_with(HashMap::new)
            .entry(name)
            .or_insert_with(Vec::new)
            .push(membership);
    }

    pub fn add_relationships(&self, var: &'b Var, rel: &[Arc<GroundedFunc>]) {
        let mut lock = self.relationships.write();
        for func in rel {
            for obj in func.get_args_names() {
                // obj lives for the duration of var as both are borrowed
                // from a repr further up the stack
                let obj = unsafe { std::mem::transmute::<&str, &'b str>(obj) };
                lock.entry(var)
                    .or_insert_with(HashMap::new)
                    .entry(obj)
                    .or_insert_with(Vec::new)
                    .push(func.clone());
            }
        }
    }

    pub fn add_grounded(&self, obj: &str, pred: &str, res: Option<(bool, Option<Time>)>) {
        // obj lives for the duration of var as both are borrowed
        // from a repr further up the stack
        let obj = unsafe { std::mem::transmute::<&str, &'b str>(obj) };
        let mut lock = self.grounded_queries.write();
        lock.entry(pred.to_string())
            .or_insert_with(HashMap::new)
            .insert(obj, res);
    }

    pub fn get_results_single(&self) -> Option<bool> {
        let results = self.grounded_queries.read();
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
            &mut *self.grounded_queries.write();
        let mut res = HashMap::new();
        for (qpred, r) in orig.drain() {
            let r = HashMap::from_iter(r.into_iter().map(|(k, v)| (k.to_string(), v)));
            res.insert(qpred, r);
        }
        res
    }

    pub fn get_memberships(&self) -> HashMap<ObjName<'b>, Vec<&'b GroundedMemb>> {
        let lock = self.membership.read();
        let mut res: HashMap<ObjName<'b>, Vec<&GroundedMemb>> = HashMap::new();
        for preds in lock.values() {
            for members in preds.values() {
                for gr in members {
                    // this is safe because gr lives for the duration of a repr further
                    // up the stack that outlives self
                    let gr = unsafe { &*(&**gr as *const GroundedMemb) as &'b GroundedMemb };
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

    pub fn get_relationships(&self) -> HashMap<ObjName<'b>, Vec<&'b GroundedFunc>> {
        let lock = self.relationships.read();
        let mut res: HashMap<ObjName<'b>, HashSet<*const GroundedFunc>> = HashMap::new();
        for relations in lock.values() {
            for relation_ls in relations.values() {
                for grfunc in relation_ls {
                    for name in grfunc.get_args_names() {
                        unsafe {
                            // this is safe because gr lives for the duration of a repr further
                            // up the stack that outlives self
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
        }
        HashMap::from_iter(res.into_iter().map(|(k, l)| {
            (
                k,
                l.into_iter()
                    .map(|v| unsafe { &*v as &'b GroundedFunc })
                    .collect::<Vec<_>>(),
            )
        }))
    }
}
