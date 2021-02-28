use crate::agent::kb::{
    bms,
    repr::{lookahead_rules, BackgroundTask, Representation},
};
use crate::agent::lang::{
    FreeClassMembership, Grounded, GroundedFunc, GroundedMemb, GroundedRef, LogSentence, Operator,
    Point, Predicate, ProofResContext, Time,
};
use crate::FLOAT_EQ_ULPS;
use bms::{BmsWrapper, RecordHistory};
use crossbeam::channel::Sender;
use dashmap::DashMap;
use float_cmp::ApproxEqUlps;
use parking_lot::RwLock;
use std::collections::HashMap;
use std::sync::Arc;

#[cfg(feature = "persistence")]
pub(super) use serialization::EntityMetadata;

/// An entity is a singleton, the unique member of it's own class.
///
/// Represents an object which can pertain to multiple classes or sets.
/// It's an abstraction owned by an agent, the internal representation
/// of the object, not the object itself.
///
/// An entity inherits the properties of the classes it belongs to,
/// and has some implicit attributes which are unique to itself.
///
/// Membership to a class is denoted (following fuzzy sets) by a real number
/// between 0 and 1. If the number is one, the object will always belong to
/// the set, if it's zero, it will never belong to the set.
///
/// For example, an object can belong to the set "cold" with a degree of
/// 0.9 (in natural language then it would be 'very cold') or 0.1
/// (then it would be "a bit cold", the subjective adjectives are defined
/// in the class itself).
///
/// Attributes:
///     * name: unique name to identify the entity.
///     * classes: categories to which the object belongs. Includes the degree of membership
///       (ie. ("cold", 0.9)).
///     * attr: implicit attributes of the object, unique to itself.
///     * beliefs: hese are the cognitions attributed to the object by the agent owning this
///       representation.
///     * relations: Functions between objects and/or classes.
///
pub(in crate::agent) struct Entity {
    pub name: String,
    pub location: BmsWrapper<RecordHistory>,
    /// LogSentence are shared amongst all the predicate members. Shared ptr has to be preserved.
    pub(super) move_beliefs: RwLock<Vec<Arc<LogSentence>>>,
    /// The GroundedMemb is shared with the parent class. Shared ptr relationship  has to be preserved.
    classes: DashMap<String, Arc<GroundedMemb>>,
    /// The GroundedFunc is shared with the represented relationship and every member of the relationship.
    /// Shared ptr relationship has to be preserved.
    relations: DashMap<String, Vec<Arc<GroundedFunc>>>,
    /// LogSentence are shared amongst all the predicate members. Shared ptr has to be preserved.
    beliefs: DashMap<String, Vec<Arc<LogSentence>>>,
    svc_queue: Sender<BackgroundTask>,
}

impl Entity {
    pub(in crate::agent::kb) fn new(name: String, svc_queue: Sender<BackgroundTask>) -> Entity {
        Entity {
            name,
            location: BmsWrapper::<RecordHistory>::new(),
            move_beliefs: RwLock::new(Vec::new()),
            classes: DashMap::new(),
            relations: DashMap::new(),
            beliefs: DashMap::new(),
            svc_queue,
        }
    }

    /// Updates this class location.
    pub(in crate::agent::kb) fn with_location(
        &self,
        loc: Point,
        was_produced: Option<(u64, Time)>,
    ) {
        self.location
            .add_new_record(None, Some(loc), None, was_produced);
    }

    pub(in crate::agent::kb) fn belongs_to_class(
        &self,
        class_name: &str,
        interval: bool,
    ) -> Option<Arc<GroundedMemb>> {
        match self.classes.get(class_name) {
            Some(r) if r.get_value().is_some() || interval => Some(r.clone()),
            Some(_) | None => None,
        }
    }

    pub(in crate::agent::kb) fn get_class_membership(
        &self,
        compare: &FreeClassMembership,
    ) -> Vec<Arc<GroundedMemb>> {
        self.classes
            .iter()
            .filter_map(|r| {
                if compare.filter_grounded(r.value()) {
                    Some(r.value().clone())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    /// Add a new class membership for this class. Returns whether this was added or updated.
    pub(in crate::agent::kb) fn add_or_updated_class_membership<T: ProofResContext>(
        &self,
        agent: &Representation,
        grounded: &Arc<GroundedMemb>,
        context: Option<&T>,
    ) -> bool {
        let name = grounded.get_parent();
        if let Some(context) = context {
            if !context.is_substituting()
                && lookahead_rules(agent, name, &GroundedRef::Class(&*grounded))
            {
                return false;
            }
        } else if lookahead_rules(agent, name, &GroundedRef::Class(&*grounded)) {
            return false;
        }
        if let Some(current) = self.classes.get(name) {
            if let Some(context) = context {
                #[cfg(debug_assertions)]
                {
                    log::trace!(
                        "Updated existing `{}` class membership {} with context:\n    {}",
                        grounded,
                        self.name,
                        &*current
                    );
                }
                current.update(
                    agent,
                    &*grounded,
                    Some((context.get_id(), context.get_production_time())),
                );
                bms::update_producers(&Grounded::Class(Arc::downgrade(&current.clone())), context);
            } else {
                #[cfg(debug_assertions)]
                {
                    log::trace!(
                        "Updated existing `{}` class membership {} w/o context:\n    {}",
                        grounded,
                        self.name,
                        &*current
                    );
                }
                current.update(agent, &*grounded, None);
            }
            if let Some(bms) = &current.bms {
                if bms.mark_for_sweep() {
                    self.svc_queue
                        .send(BackgroundTask::CompactBmsLog(&**bms as *const _))
                        .expect("background service crashed");
                }
            }
            false
        } else {
            if let Some(context) = context {
                let bms = grounded.bms.as_ref().unwrap();
                bms.set_last_rec_producer(Some((context.get_id(), context.get_production_time())));
                bms::update_producers(&Grounded::Class(Arc::downgrade(&grounded.clone())), context);
            }
            #[cfg(debug_assertions)]
            {
                log::trace!(
                    "Inserting new class membership for {}: {}",
                    self.name,
                    grounded
                );
            }
            self.classes.insert(name.to_string(), grounded.clone());
            true
        }
    }

    pub(in crate::agent::kb) fn has_relationship(
        &self,
        func: &GroundedFunc,
    ) -> Option<Arc<GroundedFunc>> {
        if let Some(relation_type) = self.relations.get(func.get_name()) {
            for rel in relation_type.value() {
                if rel.comparable(func) {
                    return Some(rel.clone());
                }
            }
        }
        None
    }

    pub(in crate::agent::kb) fn get_relationships(
        &self,
        pos: usize,
        compare: &Predicate,
    ) -> HashMap<&str, Vec<Arc<GroundedFunc>>> {
        let mut res = HashMap::new();
        let (op, val) = compare.get_uval();
        let at_time = compare.get_last_date();
        for relations in self.relations.iter() {
            for f in relations.value() {
                if f.name_in_pos(&*self.name, pos) {
                    // Safety: guaranteed this lives as long as self and mutable borrows don't leak
                    // return type is: (&'a str, Arc<GrFunc>) where &'a str lives as long as Arc<GrFunc>
                    // &'a str must NOT outlive (nor leak from) this Repr, this would be UB,
                    let rel_name = unsafe { &*(f.get_name() as *const str) };
                    let val_at_time = Self::get_value_at_time(f, at_time).unwrap();
                    match op {
                        None => res.entry(rel_name).or_insert_with(Vec::new).push(f.clone()),
                        Some(Operator::Equal) => {
                            if val_at_time.approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS) {
                                res.entry(rel_name).or_insert_with(Vec::new).push(f.clone())
                            }
                        }
                        Some(Operator::More) => {
                            if *val.as_ref().unwrap() < val_at_time {
                                res.entry(rel_name).or_insert_with(Vec::new).push(f.clone())
                            }
                        }
                        Some(Operator::Less) => {
                            if *val.as_ref().unwrap() > val_at_time {
                                res.entry(rel_name).or_insert_with(Vec::new).push(f.clone())
                            }
                        }
                        Some(Operator::LessEqual) => {
                            if *val.as_ref().unwrap() > val_at_time
                                || val_at_time.approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS)
                            {
                                res.entry(rel_name).or_insert_with(Vec::new).push(f.clone())
                            }
                        }
                        Some(Operator::MoreEqual) => {
                            if *val.as_ref().unwrap() < val_at_time
                                || val_at_time.approx_eq_ulps(val.as_ref().unwrap(), FLOAT_EQ_ULPS)
                            {
                                res.entry(rel_name).or_insert_with(Vec::new).push(f.clone())
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
        res
    }

    /// Adds a new relationship for the entity.
    /// Returns 'true' in case the relationship didn't exist previously,
    /// 'false' otherwise. If it already existed, it's value is updated.
    pub(in crate::agent::kb) fn add_relationship<T: ProofResContext>(
        &self,
        agent: &Representation,
        func: &Arc<GroundedFunc>,
        context: Option<&T>,
    ) -> bool {
        let name = func.get_name();
        if let Some(context) = context {
            if !context.is_substituting()
                && lookahead_rules(agent, name, &GroundedRef::Function(&*func))
            {
                return false;
            }
        } else if lookahead_rules(agent, name, &GroundedRef::Function(&*func)) {
            return false;
        }
        if let Some(rels) = self.relations.get(name) {
            let mut found_rel = false;
            {
                for f in rels.value() {
                    if f.comparable(&*func) {
                        if let Some(context) = context {
                            f.update(
                                agent,
                                &*func,
                                Some((context.get_id(), context.get_production_time())),
                            );
                            bms::update_producers(
                                &Grounded::Function(Arc::downgrade(&f.clone())),
                                context,
                            );
                            #[cfg(debug_assertions)]
                            {
                                log::trace!(
                                    "Updated existing relation from context for {}: {}",
                                    self.name,
                                    func
                                );
                            }
                        } else {
                            f.update(agent, &*func, None);
                            #[cfg(debug_assertions)]
                            {
                                log::trace!(
                                    "Updated existing relation w/o contex for {}: {}",
                                    self.name,
                                    func
                                );
                            }
                        }
                        found_rel = true;
                        if f.bms.mark_for_sweep() {
                            self.svc_queue
                                .send(BackgroundTask::CompactBmsLog(
                                    &*f.bms as *const BmsWrapper<_>,
                                ))
                                .expect("background service crashed");
                        }
                        break;
                    }
                }
            }
            if !found_rel {
                if let Some(context) = context {
                    func.bms.set_last_rec_producer(Some((
                        context.get_id(),
                        context.get_production_time(),
                    )));
                    bms::update_producers(
                        &Grounded::Function(Arc::downgrade(&func.clone())),
                        context,
                    );
                }
                let mut funcs_type = self.relations.get_mut(name).unwrap();
                funcs_type.push(func.clone());
                true
            } else {
                false
            }
        } else {
            if let Some(context) = context {
                func.bms
                    .set_last_rec_producer(Some((context.get_id(), context.get_production_time())));
                bms::update_producers(&Grounded::Function(Arc::downgrade(&func.clone())), context);
            }
            #[cfg(debug_assertions)]
            {
                log::trace!("Inserting new relation for {}: {}", self.name, func);
            }
            self.relations.insert(name.to_string(), vec![func.clone()]);
            true
        }
    }

    pub(in crate::agent::kb) fn add_belief(&self, belief: Arc<LogSentence>, parent: &str) {
        if let Some(mut ls) = self.beliefs.get_mut(parent) {
            ls.push(belief)
        } else {
            self.beliefs.insert(parent.to_string(), vec![belief]);
        }
    }

    pub(in crate::agent::kb) fn add_move_belief(&self, belief: &Arc<LogSentence>) {
        self.move_beliefs.write().push(belief.clone());
    }

    fn get_value_at_time(f: &Arc<GroundedFunc>, at_time: Option<Time>) -> Option<f32> {
        if let Some(at_time) = at_time {
            f.bms.get_record_at_time(at_time).0
        } else {
            f.get_value()
        }
    }

    #[cfg(feature = "persistence")]
    pub(super) fn persist(&self) -> bincode::Result<serialization::EntityBlob> {
        let blob = serialization::serialize(self)?;
        Ok(blob)
    }
}

#[cfg(feature = "persistence")]
mod serialization {
    use super::*;
    use crate::agent::kb::storage::{
        BinGrFuncRecord, BinGrMembRecord, BinLogSentRecord, BinMoveRecord, Mapped, MemAddr,
        Metadata, NonMapped, ToBinaryObject,
    };
    use bincode::Result;
    use std::{collections::HashSet, ops::Deref};

    pub(in crate::agent::kb) struct EntityMetadata {
        key: MemAddr<NonMapped>,
        pub stored_data: HashSet<MemAddr<Mapped>>,
    }

    impl EntityMetadata {
        pub fn new(blob: &EntityBlob) -> Self {
            EntityMetadata {
                key: MemAddr::from(blob.name.as_ptr() as u64),
                stored_data: HashSet::new(),
            }
        }
    }

    impl Metadata for EntityMetadata {
        fn metadata_key(&self) -> MemAddr<NonMapped> {
            self.key
        }

        fn mapped_objects(&self) -> Box<dyn Iterator<Item = MemAddr<Mapped>> + '_> {
            Box::new(self.stored_data.iter().copied())
        }
    }

    /// Deserializable blob of data representing all the information to construct an Entity.
    pub(in crate::agent::kb) struct EntityBlob {
        pub name: String,
        pub location: BmsWrapper<RecordHistory>,
        pub classes: Vec<BinGrMembRecord>,
        pub relations: Vec<Vec<BinGrFuncRecord>>,
        pub beliefs: Vec<Vec<BinLogSentRecord>>,
        pub move_beliefs: Vec<BinMoveRecord>,
    }

    pub(super) fn serialize(entity: &Entity) -> Result<EntityBlob> {
        let classes: Result<Vec<_>> = entity
            .classes
            .iter()
            .map(|kv| {
                let address = Arc::as_ptr(kv.value()) as u64;
                BinGrMembRecord::build(address.into(), kv.key(), kv.value())
            })
            .collect();

        let relations: Result<Vec<_>> = entity
            .relations
            .iter()
            .map(|kv| {
                kv.value()
                    .iter()
                    .map(|f| {
                        let address = Arc::as_ptr(f) as u64;
                        BinGrFuncRecord::build(address.into(), kv.key(), f.deref())
                    })
                    .collect()
            })
            .collect();

        let beliefs: Result<Vec<_>> = entity
            .beliefs
            .iter()
            .map(|kv| {
                kv.value()
                    .iter()
                    .map(|f| {
                        let address = Arc::as_ptr(f) as u64;
                        BinLogSentRecord::build(address.into(), kv.key(), f.deref())
                    })
                    .collect()
            })
            .collect();

        let move_beliefs: Result<Vec<_>> = {
            let lock = entity.move_beliefs.read();
            lock.iter()
                .map(|s| {
                    let address = Arc::as_ptr(s) as u64;
                    BinMoveRecord::build(address.into(), "", s.deref())
                })
                .collect()
        };

        let blob = EntityBlob {
            name: entity.name.clone(),
            location: entity.location.clone(),
            classes: classes?,
            relations: relations?,
            beliefs: beliefs?,
            move_beliefs: move_beliefs?,
        };

        Ok(blob)
    }
}
