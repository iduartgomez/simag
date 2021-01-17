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
type LocationByVar = (Point, Arc<GrTerminalKind<String>>);

/// A succesful query will return an `InfResult` which contains all the answer data.
/// The data can be manipulated and filtered throught various methods returning
/// whatever is requested by the consumer.
#[derive(Debug)]
pub(in crate::agent::kb) struct InfResults<'rep> {
    pub grounded_queries: DashMap<QueryPred, GroundedResults<'rep>>,
    membership: DashMap<Arc<Var>, QueryResMemb<'rep>>,
    relationships: DashMap<Arc<Var>, QueryResRels<'rep>>,
    objs_by_loc: DashMap<Point, HashSet<LocResult>>,
    all_objs_in_loc: DashMap<LocationByVar, Vec<GrTerminalKind<String>>>,
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
            all_objs_in_loc: DashMap::new(),
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

    pub fn add_objs_in_loc<'a: 'b, 'b>(
        &'b self,
        objs: impl Iterator<Item = (&'a Point, Arc<GrTerminalKind<String>>, Option<bool>)>,
    ) {
        for (loc, obj, val) in objs {
            if let Some(mut entries) = self.objs_by_loc.get_mut(loc) {
                entries.insert(LocResult(obj, val.unwrap_or_else(|| false)));
            } else {
                self.objs_by_loc.insert(loc.clone(), {
                    let mut hs = HashSet::new();
                    hs.insert(LocResult(obj, val.unwrap_or_else(|| false)));
                    hs
                });
            }
        }
    }

    pub fn add_objs_by_loc(
        &self,
        point: &Point,
        var: Arc<GrTerminalKind<String>>,
        objs: Vec<GrTerminalKind<String>>,
    ) {
        let mut curr_entries = self
            .all_objs_in_loc
            .entry((point.clone(), var))
            .or_insert_with(|| Vec::with_capacity(objs.len()));
        curr_entries.extend(objs.into_iter());
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
        // Safety WARNING: ObjName<'rep> may (truly) outlive the content, own the &str first
        self.grounded_queries
            .into_iter()
            .map(|(k, v)| {
                (
                    k.to_owned(),
                    v.into_iter().map(|(k, v)| ((*k).to_owned(), v)).collect(),
                )
            })
            .collect()
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

    pub fn get_located_objects(&self) -> HashMap<Point, Vec<ObjName<'rep>>> {
        let mut located = HashMap::from_iter(self.objs_by_loc.iter().map(|e| {
            let k = e.key();
            let v: Vec<_> = e
                .iter()
                .filter_map(|e| {
                    if e.1 {
                        let obj = (&*e.0).into();
                        Some(unsafe { std::mem::transmute::<&str, &'rep str>(obj) })
                    } else {
                        None
                    }
                })
                .collect();
            (k.clone(), v)
        }));
        self.all_objs_in_loc.iter().for_each(|entry| {
            let (loc, _) = entry.key();
            let in_loc = entry.value();
            let entries = located.entry(loc.clone()).or_insert_with(Vec::new);
            {
                entries.extend(in_loc.iter().map(|obj| {
                    let obj = (&*obj).into();
                    unsafe { std::mem::transmute::<&str, &'rep str>(obj) }
                }))
            }
        });
        located
    }
}
