use dashmap::DashMap;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::mem;
use std::{hash::Hash, sync::Arc};

use crate::agent::kb::inference::iexpr::QueryProcessed;
use crate::agent::lang::{GrTerminalKind, GroundedFunc, GroundedMemb, Point, Time, Var};

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
pub(in crate::agent::kb) struct InfResults<'rep> {
    pub grounded_queries: DashMap<QueryPred, GroundedResults<'rep>>,
    membership: DashMap<Arc<Var>, QueryResMemb<'rep>>,
    relationships: DashMap<Arc<Var>, QueryResRels<'rep>>,
    objs_by_loc: DashMap<Point, HashSet<LocResult>>,
    query: Arc<QueryProcessed>,
}

#[derive(Debug)]
struct LocResult(Arc<GrTerminalKind<String>>, bool);

impl Hash for LocResult {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl PartialEq for LocResult {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for LocResult {}

impl<'rep> InfResults<'rep> {
    pub fn new(query: Arc<QueryProcessed>) -> InfResults<'rep> {
        InfResults {
            grounded_queries: DashMap::new(),
            membership: DashMap::new(),
            relationships: DashMap::new(),
            objs_by_loc: DashMap::new(),
            query,
        }
    }

    pub fn add_membership(&self, var: Arc<Var>, name: &'rep str, membership: Arc<GroundedMemb>) {
        self.membership
            .entry(var)
            .or_insert_with(HashMap::new)
            .entry(name)
            .or_insert_with(Vec::new)
            .push(membership);
    }

    pub fn add_relationships(&self, var: Arc<Var>, rel: &[Arc<GroundedFunc>]) {
        for func in rel {
            for obj in func.get_args_names() {
                // Safety: guaranteed this lives as long as Self<'rep> where 'rep is the lifetime of the owning Rep
                let obj = unsafe { std::mem::transmute::<&str, &'rep str>(obj) };
                self.relationships
                    .entry(var.clone())
                    .or_insert_with(HashMap::new)
                    .entry(obj)
                    .or_insert_with(Vec::new)
                    .push(func.clone());
            }
        }
    }

    pub fn add_grounded(&self, obj: &str, pred: &str, res: Option<(bool, Option<Time>)>) {
        // Safety: guaranteed this lives as long as Self<'rep> where 'rep is the lifetime of the owning Rep
        let obj = unsafe { std::mem::transmute::<&str, &'rep str>(obj) };
        self.grounded_queries
            .entry(pred.to_string())
            .or_insert_with(HashMap::new)
            .insert(obj, res);
    }

    pub fn add_objs_by_loc<'a: 'b, 'b>(
        &'b self,
        objs: impl Iterator<Item = (&'a Point, Arc<GrTerminalKind<String>>, bool)>,
    ) {
        for (loc, obj, val) in objs {
            if let Some(mut entries) = self.objs_by_loc.get_mut(loc) {
                entries.insert(LocResult(obj, val));
            } else {
                self.objs_by_loc.insert(loc.clone(), {
                    let mut hs = HashSet::new();
                    hs.insert(LocResult(obj, val));
                    hs
                });
            }
        }
    }

    pub fn get_results_single(&self) -> Option<bool> {
        if self.grounded_queries.is_empty() && self.objs_by_loc.is_empty() {
            return None;
        }

        for r0 in self.grounded_queries.iter() {
            for r1 in r0.values() {
                if let Some((false, _)) = *r1 {
                    return Some(false);
                } else if r1.is_none() {
                    return None;
                }
            }
        }

        for objs in self.objs_by_loc.iter() {
            if objs.iter().any(|obj| !obj.1) {
                return Some(false);
            }
        }

        Some(true)
    }

    //pub type GroundedResult = Option<(bool, Option<Time>)>;
    pub fn get_results_multiple(self) -> HashMap<QueryPred, HashMap<String, GroundedResult>> {
        // TODO: change collection of results to iterator over owned values when dashmap exposes a method
        // Safety WARNING: ObjName<'rep> may (truly) outlive the content, own the &str first
        let mut res = HashMap::new();
        for entry in self.grounded_queries.iter() {
            let qpred = entry.key().to_owned();
            let owned_values = entry.value().iter().map(|(k, v)| ((*k).to_owned(), *v));
            let r = HashMap::from_iter(owned_values);
            res.insert(qpred, r);
        }
        res
    }

    pub fn get_memberships(&self) -> HashMap<ObjName<'rep>, Vec<&'rep GroundedMemb>> {
        let mut res: HashMap<ObjName<'rep>, Vec<&GroundedMemb>> = HashMap::new();
        for preds in self.membership.iter() {
            for members in preds.values() {
                for gr in members {
                    // Safety: guaranteed this lives as long as Self<'rep> where 'rep is the lifetime of the owning Rep
                    let gr = unsafe { &*(&**gr as *const GroundedMemb) };
                    let gr_name: &str = gr.get_name().into();
                    res.entry(gr_name).or_insert_with(Vec::new).push(gr);
                }
            }
        }
        res
    }

    pub fn get_relationships(&self) -> HashMap<ObjName<'rep>, Vec<&'rep GroundedFunc>> {
        let mut res: HashMap<ObjName<'rep>, HashSet<*const GroundedFunc>> = HashMap::new();
        for relations in self.relationships.iter() {
            for relation_ls in relations.values() {
                for grfunc in relation_ls {
                    for name in grfunc.get_args_names() {
                        #[allow(clippy::mutable_key_type)]
                        unsafe {
                            // Safety: guaranteed this lives as long as Self<'rep> where 'rep is the lifetime of the owning Rep
                            let name = mem::transmute::<&str, &'rep str>(name);
                            if let Some(prev) = res.get_mut(name) {
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
                    .map(
                        |v| // Safety: guaranteed this lives as long as Self<'rep> where 'rep is the lifetime of the owning Rep 
                        unsafe { std::mem::transmute::<&GroundedFunc, &'rep GroundedFunc>(&*v) },
                    )
                    .collect::<Vec<_>>(),
            )
        }))
    }
}
