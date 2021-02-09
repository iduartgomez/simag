//! Belief Maintenance System for the agent system.
//!
//! This module adds methods and types for:
//! 1) Recording how a belief came to existence to the agent.
//! 2) Detecting inconsistences between new and old beliefs.
//! 3) Fixing those inconsitences.
pub(in crate::agent) use self::errors::BmsError;
use super::{inference::QueryInput, repr::Representation};
use crate::agent::{
    lang::{
        ClassDecl, Grounded, GroundedMemb, GroundedRef, Point, ProofResContext, SentID, SpatialOps,
        Time, TimeOps,
    },
    ParseErrF,
};
use chrono::Utc;
use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::mem;
use std::{cmp::Ordering as CmpOrdering, time::Duration};
use std::{
    collections::HashMap,
    convert::TryFrom,
    marker::PhantomData,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

/// Trigger threshold for log compation event.
const LOG_COMPACT_THRESHOLD: usize = 100;
/// Max number of records to keep after compacting the log.
const LOG_MAX_SIZE: usize = 20;
const WRITE_LOCK_RETRIES: usize = 100;

/// Acts as a wrapper for the Belief Maintenance System for a given agent.
///
/// Serves to keep the believes alive in memory, fix inconsistencies and
/// serialize any information.
#[derive(Debug)]
pub(in crate::agent) struct BmsWrapper<T>
where
    T: BmsKind,
{
    /// The semantics of this collection depends on the type of the wrapper. For the record history type variant
    /// is the log of values, sorted by increasing register time, associated with the owning object.
    records: RwLock<Vec<BmsRecord>>,
    /// set if it's a predicate with the time of query execution
    pred: Option<Time>,
    /// When this flag is set, it means that if this wrapper is received as an updating wrapper
    /// it will replace all the existing records of the wrapper being updated.
    ///
    /// When this is done all the previous records will be re-examined with any new knowledge
    /// being checked, which could produce new facts or update existing facts rolling back
    /// previous knowledge that no longer applies.
    overwrite: AtomicBool,
    _kind: PhantomData<T>,
}

pub(in crate::agent) trait BmsKind {}

pub(in crate::agent) trait HasBms {
    type BmsType: OverwriteBms;

    fn get_bms(&self) -> Option<&Self::BmsType>;

    fn get_value(&self) -> Option<f32>;
}

pub(in crate::agent) trait OverwriteBms {
    /// Drops all the records and writes the records from the incoming BMS.
    fn overwrite_data(&self, other: BmsWrapper<RecordHistory>) -> Result<(), ()>;

    fn overwrite_loc_data(&self, spatial_data: BmsWrapper<IsSpatialData>) -> Result<(), ()>;
}

#[inline]
fn internal_overwrite_data<T: BmsKind>(
    first: &BmsWrapper<T>,
    other: BmsWrapper<RecordHistory>,
) -> Result<(), ()> {
    let lock = &mut *first.acquire_write_lock();
    // drop old records
    lock.truncate(0);
    // insert new records
    for rec in &*other.acquire_read_lock() {
        lock.push(rec.clone())
    }
    first
        .overwrite
        .store(other.overwrite.load(Ordering::Acquire), Ordering::Release);
    Ok(())
}

#[inline]
fn internal_overwrite_loc_data<T: BmsKind>(
    first: &BmsWrapper<T>,
    spatial_data: BmsWrapper<IsSpatialData>,
) -> Result<(), ()> {
    let spatial_data = &*spatial_data.acquire_read_lock();
    let location = match spatial_data.last() {
        Some(loc) if spatial_data.len() == 1 => loc.location.clone(),
        _ => return Err(()),
    };

    for rec in &mut *first.acquire_write_lock() {
        rec.location = location.clone();
    }

    Ok(())
}

#[derive(Debug, Clone)]
pub(in crate::agent) struct IsSpatialData;
impl BmsKind for IsSpatialData {}

#[derive(Debug, Clone)]
pub(in crate::agent) struct IsTimeData;
impl BmsKind for IsTimeData {}

impl Into<BmsWrapper<RecordHistory>> for BmsWrapper<IsTimeData> {
    fn into(self) -> BmsWrapper<RecordHistory> {
        let BmsWrapper {
            records,
            pred,
            overwrite,
            ..
        } = self;
        BmsWrapper {
            _kind: PhantomData,
            records,
            pred,
            overwrite,
        }
    }
}

#[derive(Debug, Clone)]
pub(in crate::agent) struct RecordHistory;
impl BmsKind for RecordHistory {}

impl<T: BmsKind> AsRef<BmsWrapper<T>> for BmsWrapper<T> {
    fn as_ref(&self) -> &BmsWrapper<T> {
        self
    }
}

impl TryFrom<&BmsWrapper<RecordHistory>> for BmsWrapper<IsTimeData> {
    type Error = ();

    fn try_from(value: &BmsWrapper<RecordHistory>) -> Result<Self, Self::Error> {
        let values = &*value.acquire_read_lock();
        match values.len() {
            1 if values[0].value.is_some() => {}
            2 if values[0].value.is_some() && values[1].value.is_some() => {}
            _ => {
                //
                return Err(());
            }
        }

        Ok(BmsWrapper {
            records: RwLock::new(values.clone()),
            pred: value.pred,
            overwrite: AtomicBool::new(value.overwrite.load(Ordering::Acquire)),
            _kind: PhantomData,
        })
    }
}

impl TryFrom<&BmsWrapper<RecordHistory>> for BmsWrapper<IsSpatialData> {
    type Error = ();

    fn try_from(value: &BmsWrapper<RecordHistory>) -> Result<Self, Self::Error> {
        let values = &*value.acquire_read_lock();
        // spatial data only should have exactly one record
        if values.len() != 1 {
            return Err(());
        }

        // spatial data record should have a location set
        if values[0].location.is_none() {
            return Err(());
        }

        Ok(BmsWrapper {
            records: RwLock::new(values.clone()),
            pred: value.pred,
            overwrite: AtomicBool::new(value.overwrite.load(Ordering::Acquire)),
            _kind: PhantomData,
        })
    }
}

impl<T: BmsKind> BmsWrapper<T> {
    /// Get the last time from which the record value applies.
    pub fn get_last_time(&self) -> Time {
        let lock = self.acquire_read_lock();
        let rec = lock.last().unwrap();
        rec.time
    }

    pub fn get_last_value(&self) -> (Option<f32>, Option<Point>) {
        let records = self.acquire_read_lock();
        if let Some(rec) = records.last() {
            (rec.value, rec.location.clone())
        } else {
            (None, None)
        }
    }

    pub fn with_ow_val(self, ow: bool) -> Self {
        self.overwrite.store(ow, Ordering::Release);
        self
    }

    /// Returns a guarded shared reference to the internal record log.
    #[inline]
    pub fn acquire_read_lock(&self) -> RwLockReadGuard<Vec<BmsRecord>> {
        self.records.read()
    }

    /// Returns a guarded exclusive reference to the internal record log.
    #[inline]
    fn acquire_write_lock(&self) -> RwLockWriteGuard<Vec<BmsRecord>> {
        for retry in 0..WRITE_LOCK_RETRIES {
            // optimistally try to lock to append to records; retry for a number of times waiting between retrials
            // under heavy stress otherwise deadlocks were observed when scheduling an exclusive lock request
            if let Some(records) = self.records.try_write_for(Duration::from_nanos(100)) {
                return records;
            }
            log::warn!(
                "retrying acquiring rec lock exclusive access, attempt #{}",
                retry + 1
            );
            std::thread::sleep(Duration::from_nanos(100));
        }
        unreachable!("deadlock detected!");
    }
}

impl BmsWrapper<RecordHistory> {
    pub fn new() -> Self {
        BmsWrapper {
            records: RwLock::new(Vec::with_capacity(2)),
            pred: None,
            overwrite: AtomicBool::new(false),
            _kind: PhantomData,
        }
    }

    /// Compare the last record on this bms to a given time and returns
    /// the newest of both.
    pub fn get_newest_date(&self, other: Time) -> Option<Time> {
        let lock = &*self.acquire_read_lock();
        let rec = lock.last().unwrap();
        if rec.time > other {
            Some(rec.time)
        } else {
            None
        }
    }

    /// Set a producer for the last truth value
    pub fn set_last_rec_producer(&self, produced: Option<(SentID, Time)>) {
        let records = &mut *self.acquire_write_lock();
        let last = records.last_mut().unwrap();
        last.was_produced = produced;
    }

    /// Given two different BMS, rollback one of them based on the time
    /// ordering of both of their records.
    ///
    /// This means that if A has been updated at a later or same moment as B,
    /// then A (along all the knowledge produced by this registry)
    /// will be rolled back to previous record. All produced records will
    /// be rolled back recursively.
    pub(in crate::agent) fn rollback_one_once(&self, other: &Self) {
        // Write lock to guarantee atomicity of the operation
        let lock_self = &mut *self.acquire_write_lock();
        let lock_other = &mut *other.acquire_write_lock();
        let own_is_newer = {
            let own_last_time = lock_self.last_mut().unwrap();
            let other_last_time = lock_other.last_mut().unwrap();

            match own_last_time.time.cmp(&other_last_time.time) {
                CmpOrdering::Greater | CmpOrdering::Equal => true,
                _ => false,
            }
        };

        if own_is_newer {
            Self::rollback_once(lock_self.pop().unwrap());
        } else {
            Self::rollback_once(lock_other.pop().unwrap());
        }
    }

    fn rollback_once(recordings: BmsRecord) {
        let (sent_id, at_time) = if let Some((sent_id, at_time)) = recordings.was_produced {
            (sent_id, at_time)
        } else {
            unreachable!("SIMAG - can't rollback records which were not produced")
        };

        for (record, _) in &recordings.produced {
            match record {
                Grounded::Function(func) => {
                    if let Some(f) = func.upgrade() {
                        f.bms.rollback(sent_id, &at_time);
                    }
                }
                Grounded::Class(class) => {
                    if let Some(c) = class.upgrade() {
                        if let Some(b) = &c.bms {
                            b.rollback(sent_id, &at_time);
                        }
                    }
                }
            }
        }
    }

    fn rollback(&self, sent_id: SentID, at_time: &Time) {
        let lock = &mut *self.acquire_write_lock();
        let rollback_this = {
            let last = lock.last().unwrap();
            if let Some((other_id, ref production_time)) = last.was_produced {
                other_id == sent_id && at_time == production_time
            } else {
                false
            }
        };
        if rollback_this {
            Self::rollback_once(lock.pop().unwrap());
        }
    }

    /// Compare the time of the last record of two BMS and return
    /// the ordering of self vs. other.
    pub fn cmp_by_time(&self, other: &Self) -> CmpOrdering {
        let lock0 = &*self.acquire_read_lock();
        let lock1 = &*other.acquire_read_lock();
        let own_last_time = &lock0.last().unwrap().time;
        let other_last_time = &lock1.last().unwrap().time;
        own_last_time.cmp(other_last_time)
    }

    /// Return the value valid at the given time, if any.
    pub fn get_record_at_time(&self, cmp_time: Time) -> (Option<f32>, Option<Point>) {
        if let Some(rec) = self
            .acquire_read_lock()
            .iter()
            .take_while(|rec| rec.time <= cmp_time)
            .last()
        {
            (rec.value, rec.location.clone())
        } else {
            (None, None)
        }
    }

    /// Add a new record to this BMS.
    pub fn add_new_record(
        &self,
        time: Option<Time>,
        location: Option<Point>,
        value: Option<f32>,
        was_produced: Option<(SentID, Time)>,
    ) {
        let mut records = self.acquire_write_lock();
        let production_time = match was_produced {
            Some((_, production_time)) => Some(production_time),
            None => None,
        };
        let time = match time {
            Some(_) if production_time.is_some() => production_time.unwrap(),
            Some(time) => time,
            None => Utc::now(),
        };

        let record = BmsRecord {
            produced: vec![],
            time,
            location,
            value,
            was_produced,
        };
        records.push(record);
        // FIXME: records always should be sorted by time, but this makes some tests fail
        records.sort_by(|a, b| a.time.cmp(&b.time));
    }

    /// Look for all the changes that were produced, before an update,
    /// due to this belief previous value and test if they still apply.
    /// If the facts no longer hold true rollback them.
    // FIXME: improve this so is not potentially racey:
    // inner checks for rollbacks could potentially provoque inconsistencies
    // when subsequent checks are recursively performed.
    pub fn update(
        &self,
        owner: &GroundedRef,
        agent: &Representation,
        data: &Self,
        was_produced: Option<(SentID, Time)>,
    ) {
        let ask_processed = |entry: &(Grounded, Option<f32>), cmp_rec: &Time| {
            match *entry {
                (Grounded::Function(ref func), ..) => {
                    let func = func.upgrade().unwrap();
                    // check if indeed the value was produced by this producer or is more recent
                    let ask = {
                        // avoid keeping the lock; since it may deadlock in the next iteration,
                        // after calling `ask_processed` again.
                        let func_lock = func.bms.acquire_read_lock();
                        let last = func_lock.last().unwrap();
                        if last.time > *cmp_rec {
                            // if it was produced, run again a test against the kb to check
                            // if it is still valid
                            true
                        } else {
                            false
                        }
                    };
                    if ask {
                        let answ = agent
                            .ask_processed(QueryInput::AskRelationalFunc(func.clone()), 0, true)
                            .unwrap()
                            .get_results_single();
                        if answ.is_none() {
                            let bms = &func.bms;
                            let mut time = None;
                            let mut value = None;
                            let mut loc = None;
                            {
                                let recs = &*func.bms.acquire_read_lock();
                                for rec in recs.iter().rev() {
                                    if rec.was_produced.is_none() {
                                        time = Some(rec.time);
                                        value = rec.value;
                                        loc = rec.location.clone();
                                        break;
                                    }
                                }
                                func.update_value(value);
                            }
                            bms.add_new_record(time, loc, value, None);
                        }
                    }
                }
                (Grounded::Class(ref cls), ..) => {
                    let cls = cls.upgrade().unwrap();
                    let ask = {
                        // avoid keeping the lock; since it may deadlock in the next iteration,
                        // after calling `ask_processed` again.
                        let recs = &*cls.bms.as_ref().unwrap().acquire_read_lock();
                        let last = recs.last().unwrap();
                        if last.time > *cmp_rec {
                            true
                        } else {
                            false
                        }
                    };
                    if ask {
                        let answ = agent
                            .ask_processed(QueryInput::AskClassMember(cls.clone()), 0, true)
                            .unwrap()
                            .get_results_single();
                        if answ.is_none() {
                            let bms = cls.bms.as_ref().unwrap();
                            let mut time = None;
                            let mut value = None;
                            let mut loc = None;
                            {
                                let recs = bms.acquire_read_lock();
                                for rec in recs.iter().rev() {
                                    if rec.was_produced.is_none() {
                                        time = Some(rec.time);
                                        value = rec.value;
                                        loc = rec.location.clone();
                                        break;
                                    }
                                }
                            }

                            cls.update_value(value);
                            bms.add_new_record(time, loc, value, None);
                        }
                    }
                }
            }
        };

        if data.overwrite.load(Ordering::Acquire) {
            let old_recs;
            {
                let mut new_recs: Vec<BmsRecord> = vec![];
                for rec in &*data.acquire_read_lock() {
                    new_recs.push(rec.clone())
                }
                let prev_recs = &mut *self.acquire_write_lock();
                mem::swap(prev_recs, &mut new_recs);
                old_recs = new_recs;
            }
            for rec in old_recs {
                for entry in &rec.produced {
                    ask_processed(entry, &rec.time);
                }
            }
            return;
        }

        let (up_rec_value, up_rec_date, up_rec_loc) = {
            let lock = data.acquire_read_lock();
            let last = lock.last().unwrap();
            (last.value, last.time, last.location.clone())
        };
        let (last_rec_value, last_rec_date, last_rec_loc) = {
            let lock = self.acquire_read_lock();
            let last = lock.last().unwrap();
            (last.value, last.time, last.location.clone())
        };

        if (last_rec_date == up_rec_date)
            && (last_rec_value == up_rec_value)
            && (last_rec_loc == up_rec_loc)
        {
            return;
        }

        // create a new record with the new data
        owner.update_value(up_rec_value);
        self.add_new_record(Some(up_rec_date), up_rec_loc, up_rec_value, was_produced);

        // check if there are any inconsistencies with the knowledge produced with the previous value
        if up_rec_date < last_rec_date {
            // new value is older, in face of new information all previously
            // produced knowledge must be checked to see if it still holds true
            let old_recs = {
                let mut new_recs: Vec<BmsRecord> = Vec::with_capacity(1);
                let records = &mut *self.acquire_write_lock();
                let newest_rec = records.pop().unwrap();
                new_recs.push(newest_rec);
                mem::swap(records, &mut new_recs);
                new_recs
            };
            for entry in old_recs.iter().map(|rec| rec.produced.iter()).flatten() {
                ask_processed(entry, &last_rec_date);
            }
        } else if up_rec_date > last_rec_date || up_rec_value != last_rec_value {
            // new value is more recent, check only the last produced values.
            // if both times are the same there is an incongruency.
            //
            // replace previous record value and check that all produced knowledge
            // with that value still holds true
            let old_record = {
                // in order to avoid deadlocks, remove the previous record temporarily
                // and push it back after the necessary checks, then sort by age
                let mut records = self.acquire_write_lock();
                if records.len() > 1 {
                    let pos = records.len() - 2;
                    Some(records.remove(pos))
                } else {
                    None
                }
            };
            for entry in old_record.iter().map(|r| r.produced.iter()).flatten() {
                ask_processed(entry, &last_rec_date);
            }
            {
                let records = &mut *self.acquire_write_lock();
                if let Some(old_record) = old_record {
                    records.push(old_record);
                }
                records.sort_by(|a, b| a.time.cmp(&b.time));
            }
        }
        self.compact_record_log()
    }

    pub fn records_log_size(&self) -> usize {
        self.acquire_read_lock().len()
    }

    pub fn of_predicate(&mut self) {
        if self.pred.is_none() {
            self.pred = Some(Utc::now());
        }
    }

    pub fn is_predicate(&self) -> Option<&Time> {
        self.pred.as_ref()
    }

    /// Get records at a given point space if any at the provided time.
    //  If the provided time is none, then only the last record will be retrieved
    pub fn get_record_at_location(&self, loc: &Point, time: Option<Time>) -> Vec<Option<f32>> {
        if let Some(_time) = time {
            todo!()
        } else {
            if let (rec_val, Some(rec_loc)) = self.get_last_value() {
                if &rec_loc == loc {
                    return vec![rec_val];
                }
            }
            Vec::new()
        }
    }

    fn add_produced_entry(&self, produced: Grounded, with_val: Option<f32>) {
        let mut records = self.acquire_write_lock();
        let record = records.last_mut().unwrap();
        record.add_produced_entry((produced, with_val));
    }

    // TODO: this function is to be called by the background cleaning service thread
    /// Compacts the record log, this function is lossy and information may be potentially lost
    /// and irrecoverable, meaning that rollbacks won't be possible for those values lost.
    fn compact_record_log(&self) {
        let records = &mut *self.acquire_write_lock();
        if records.len() < LOG_COMPACT_THRESHOLD {
            // only perform a cleanup if more than 100 records have been registered
            return;
        }

        // potentially remove any rec which didn't produce anything itself
        let mut remove_ls: Vec<usize> = vec![];
        // if necessary, rm the oldest records
        let mut maybe_rm = vec![];
        let need_to_clean = records.len() - LOG_MAX_SIZE;
        let mut clean_counter = need_to_clean;
        // keep the last two records regardless
        let last_two = records.len() - 2;
        for (i, rec) in records[..last_two].iter().enumerate() {
            if clean_counter == 0 {
                break;
            }
            if rec.produced.is_empty() {
                remove_ls.push(i);
                clean_counter -= 1;
            } else if remove_ls.len() < need_to_clean && clean_counter > 0 {
                maybe_rm.push(i);
                clean_counter -= 1;
            }
        }

        if remove_ls.len() < records.len() - LOG_MAX_SIZE {
            let required = records.len() - remove_ls.len() - LOG_MAX_SIZE;
            let extra_to_rm = if maybe_rm.len() < required {
                maybe_rm.drain(..)
            } else {
                maybe_rm.drain(..required)
            };
            remove_ls.extend(extra_to_rm);
            remove_ls.sort();
        }
        remove_ls.reverse();
        for pos in remove_ls {
            records.remove(pos);
        }
    }
}

impl OverwriteBms for BmsWrapper<RecordHistory> {
    fn overwrite_data(&self, other: BmsWrapper<RecordHistory>) -> Result<(), ()> {
        internal_overwrite_data(self, other)
    }

    fn overwrite_loc_data(&self, spatial_data: BmsWrapper<IsSpatialData>) -> Result<(), ()> {
        internal_overwrite_loc_data(self, spatial_data)
    }
}

impl BmsWrapper<IsTimeData> {
    pub fn new(time: Option<Time>, value: Option<f32>) -> Self {
        let time = match time {
            Some(time) => time,
            None => Utc::now(),
        };

        let record = BmsRecord {
            produced: vec![],
            time,
            location: None,
            value,
            was_produced: None,
        };

        BmsWrapper {
            records: RwLock::new(vec![record]),
            pred: None,
            overwrite: AtomicBool::new(false),
            _kind: PhantomData,
        }
    }

    /// Merge an other bmswrapper with this one, the belief then will be true
    /// between the time interval [start, end) of both records, after which it will be set
    /// as `unknown`.
    ///
    /// This operation is meant to be used when asserting new facts.
    pub fn merge_since_until(&mut self, until: &Self) -> Result<(), errors::BmsError> {
        let mut self_records = self.acquire_write_lock();
        let other_records = until.acquire_read_lock();

        if self_records.is_empty() || other_records.is_empty() {
            return Err(errors::BmsError::EmptyRecordList);
        } else if self_records.len() > 1 || other_records.len() > 1 {
            return Err(errors::BmsError::IllegalMerge(
                "More than one record found".to_string(),
            ));
        }

        let until = other_records.get(0).unwrap();
        let since = self_records.get_mut(0).unwrap();

        let mut until = until.clone();
        if until.time < since.time {
            return Err(errors::BmsError::IllegalMerge(
                "First time variable assignment can't be older than second".to_string(),
            ));
        }
        until.value = None;
        self_records.push(until);
        Ok(())
    }

    /// Replaces the value of the last, active, record.
    pub fn replace_value(&self, val: Option<f32>) {
        let records = &mut *self.acquire_write_lock();
        let last = records.last_mut().unwrap();
        last.value = val;
    }

    pub fn get_time_interval(&self) -> Result<[(Time, Option<f32>); 2], ()> {
        let values: Vec<_> = self
            .acquire_read_lock()
            .iter()
            .take(2)
            .map(|r| (r.time, r.value))
            .collect();
        if values.len() != 2 {
            Err(())
        } else {
            Ok([values[0], values[1]])
        }
    }

    pub fn merge_spatial_data(
        self,
        spatial_data: BmsWrapper<IsSpatialData>,
    ) -> Result<BmsWrapper<RecordHistory>, ()> {
        spatial_data.merge_time_data(self)
    }
}

impl OverwriteBms for BmsWrapper<IsTimeData> {
    fn overwrite_data(&self, other: BmsWrapper<RecordHistory>) -> Result<(), ()> {
        internal_overwrite_data(self, other)
    }

    fn overwrite_loc_data(&self, spatial_data: BmsWrapper<IsSpatialData>) -> Result<(), ()> {
        internal_overwrite_loc_data(self, spatial_data)
    }
}

impl BmsWrapper<IsSpatialData> {
    pub fn new(location: Option<Point>) -> Self {
        let record = BmsRecord {
            produced: vec![],
            time: Utc::now(),
            location,
            value: None,
            was_produced: None,
        };

        BmsWrapper {
            records: RwLock::new(vec![record]),
            pred: None,
            overwrite: AtomicBool::new(false),
            _kind: PhantomData,
        }
    }

    /// Merges a wrapper with just time data with an other with just spatial data.
    ///
    /// In order for the merge to function the following conditions must hold:
    /// - Both of the wrappers must be of the same length.
    pub fn merge_time_data(
        self,
        time_data: BmsWrapper<IsTimeData>,
    ) -> Result<BmsWrapper<RecordHistory>, ()> {
        let overwrite = AtomicBool::new(
            self.overwrite.load(Ordering::Acquire) || time_data.overwrite.load(Ordering::Acquire),
        );

        let mut spatial_recs = self.records.into_inner();
        let time_data_recs = time_data.records.into_inner();

        if spatial_recs.len() != 1 && spatial_recs.len() != 2 {
            return Err(());
        }

        let records;
        if spatial_recs.len() == time_data_recs.len() {
            records = RwLock::new(
                time_data_recs
                    .into_iter()
                    .zip(spatial_recs.into_iter())
                    .map(|(time_rec, space_rec)| BmsRecord {
                        produced: vec![],
                        time: time_rec.time,
                        location: space_rec.location,
                        value: time_rec.value,
                        was_produced: None,
                    })
                    .collect(),
            );
        } else if spatial_recs.len() == 1 && time_data_recs.len() == 2 {
            if let Some(spatial_rec) = spatial_recs.pop() {
                records = RwLock::new(
                    time_data_recs
                        .into_iter()
                        .map(|time_rec| BmsRecord {
                            produced: vec![],
                            time: time_rec.time,
                            location: spatial_rec.location.clone(),
                            value: time_rec.value,
                            was_produced: None,
                        })
                        .collect(),
                );
            } else {
                return Err(());
            }
        } else {
            return Err(());
        }

        Ok(BmsWrapper {
            overwrite,
            pred: None,
            records,
            _kind: PhantomData,
        })
    }

    /// Merge an other spatial wrapper with this one, this represents a movement between two positions
    /// for the owner of the wrapper. The 'from' portion is this object, the other is the 'to' portion.
    ///
    /// This operation is meant to be used with the `move` operation.
    pub fn merge_from_to(&mut self, to: &Self) -> Result<(), errors::BmsError> {
        let mut self_records = self.acquire_write_lock();
        let other_records = to.acquire_read_lock();

        if self_records.is_empty() || other_records.is_empty() {
            return Err(errors::BmsError::EmptyRecordList);
        } else if self_records.len() > 1 || other_records.len() > 1 {
            return Err(errors::BmsError::IllegalMerge(
                "More than one record found".to_string(),
            ));
        }

        let to = other_records.get(0).unwrap();

        let mut to = to.clone();
        to.value = None;
        self_records.push(to);
        Ok(())
    }
}

impl<T: BmsKind> std::clone::Clone for BmsWrapper<T> {
    fn clone(&self) -> BmsWrapper<T> {
        let recs = &*self.acquire_read_lock();
        BmsWrapper {
            records: RwLock::new(recs.clone()),
            pred: self.pred,
            overwrite: AtomicBool::new(self.overwrite.load(Ordering::Acquire)),
            _kind: PhantomData,
        }
    }
}

impl<T: BmsKind> std::fmt::Display for BmsWrapper<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let record = self.get_last_time();
        let val = self.get_last_value();
        write!(f, "Bms(.., {} = {:?})", record, val)
    }
}

/// Record of how a belief became to existence and what other believes
/// it has produced since then.
#[derive(Debug, Clone)]
pub(in crate::agent) struct BmsRecord {
    produced: Vec<(Grounded, Option<f32>)>,
    time: Time,
    location: Option<Point>,
    value: Option<f32>,
    /// whether this record was a byproduct of a sentence evaluation or a declaration
    was_produced: Option<(SentID, Time)>,
}

impl BmsRecord {
    fn add_produced_entry(&mut self, entry: (Grounded, Option<f32>)) {
        self.produced.push(entry);
    }
}

pub(in crate::agent) fn update_producers<C: ProofResContext>(owner: &Grounded, context: &C) {
    // add the produced knowledge to each producer in case it comes
    // from a logic sentence resolution
    for a in context.get_antecedents() {
        match *a {
            Grounded::Class(ref cls) => {
                let cls = cls.upgrade().unwrap();
                let value = cls.get_value();
                cls.bms
                    .as_ref()
                    .unwrap()
                    .add_produced_entry(owner.clone(), value);
            }
            Grounded::Function(ref func) => {
                let func = func.upgrade().unwrap();
                let value = func.get_value();
                func.bms.add_produced_entry(owner.clone(), value);
            }
        }
    }
}

pub(in crate::agent) fn build_declaration_bms(
    cls_decl: ClassDecl,
) -> Result<impl Iterator<Item = GroundedMemb>, ParseErrF> {
    let ta = HashMap::new();
    let time_data = cls_decl.get_own_time_data(&ta, None);
    let la = HashMap::new();
    let loc_data = cls_decl.get_own_spatial_data(&la)?;
    let f_bms = loc_data
        .merge_time_data(time_data)
        .map_err(|_| ParseErrF::WrongDef)?;

    Ok(cls_decl.into_iter().map(move |mut a| {
        let val = a.get_value();
        if let Some(bms) = a.bms.as_mut() {
            let t = f_bms.clone();
            {
                // replace first because it may be an interval
                let records = &mut *t.acquire_write_lock();
                records.first_mut().unwrap().value = val;
            }
            *bms = Arc::new(t);
            if a.is_time_interval() {
                // if it's a time interval make sure the last value is None
                a.update_value(None);
            }
        };
        a
    }))
}

/// Adds location data produced from a move fn into a bms.
pub(in crate::agent) fn add_loc_from_move_fn<C: ProofResContext>(
    context: &C,
    bms: &BmsWrapper<RecordHistory>,
    loc_data: BmsWrapper<RecordHistory>,
) {
    // extract records from loc_data:
    for rec in loc_data.records.into_inner() {
        let BmsRecord { time, location, .. } = rec;
        bms.add_new_record(
            Some(time),
            location,
            None,
            Some((context.get_id(), context.get_production_time())),
        )
    }
}

/// Adds location data produced from a move fn into a bms, w/o any time data.
pub(in crate::agent) fn add_loc_from_spatial_data<C: ProofResContext>(
    context: &C,
    bms: &BmsWrapper<RecordHistory>,
    loc_data: BmsWrapper<IsSpatialData>,
) {
    // extract records from loc_data:
    for rec in loc_data.records.into_inner() {
        let BmsRecord { time, location, .. } = rec;
        bms.add_new_record(
            Some(time),
            location,
            None,
            Some((context.get_id(), context.get_production_time())),
        )
    }
}

pub(in crate::agent) mod errors {
    #[derive(Debug, PartialEq)]
    pub enum BmsError {
        IllegalMerge(String),
        EmptyRecordList,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    #[ignore]
    fn bms_rollback() {
        let rep = Representation::new(1);

        let fol = "
            (ugly[$Pancho=0])
            (dog[$Pancho=1])
            (meat[$M1=1])
            (fn::eat[$M1=1,$Pancho])
            
            (let x, y in
             ((dog[x=1] and meat[y=1] and fn::eat[y=1,x]) 
              := fat[x=1]))
            
            (let x in
             ((fat[x=1] and dog[x=1]) := (ugly[x=1] and sad[x=1])))
        ";
        rep.tell(fol).unwrap();
        {
            let answ = rep.ask("(fat[$Pancho=1] and sad[$Pancho=1])");
            assert_eq!(answ.unwrap().get_results_single(), Some(true));
        }

        log::info!("NEW FACTS!!!!");
        let fol = "
            (run[$Pancho=1])
            (let x in
             ((run[x=1] and dog[x=1]) := fat[x=0]))
        ";
        rep.tell(fol).unwrap();
        let answ0 = rep.ask("(fat[$Pancho=0])");
        assert_eq!(answ0.unwrap().get_results_single(), Some(true));
        let answ2 = rep.ask("(sad[$Pancho=0])");
        assert_eq!(answ2.unwrap().get_results_single(), None);
        let answ1 = rep.ask("(ugly[$Pancho=0])");
        assert_eq!(answ1.unwrap().get_results_single(), Some(true));
    }

    #[test]
    #[ignore]
    fn bms_review_after_change() {
        let rep = Representation::new(1);

        let fol = "            
            ( meat[$M1=1] )
            ( dog[$Pancho=1] )
            ( fn::eat[$M1=1,$Pancho] )
            ( let x, y in
              ( ( dog[x=1] and meat[y=1] and fn::eat[y=1,x] ) 
                := fat[x=1] ) )
        ";
        rep.tell(fol).unwrap();
        {
            let answ = rep.ask("(fat[$Pancho=1])");
            assert_eq!(answ.unwrap().get_results_single(), Some(true));
        }

        let fol = "
            ( run[$Pancho=1] )
            ( let x in (( dog[x=1] and run[x=1] ) := fat[x=0] ))
        ";
        rep.tell(fol).unwrap();
        {
            let answ = rep.ask("(fat[$Pancho=0])");
            assert_eq!(answ.unwrap().get_results_single(), Some(true));
        }

        rep.tell("(fn::eat[$M1=1,$Pancho])").unwrap();
        {
            let answ = rep.ask("(fat[$Pancho=1])");
            assert_eq!(answ.unwrap().get_results_single(), Some(true));
        }
    }

    #[test]
    fn log_compaction() {
        // clean up to the maintenace limit, when all records didn't produce anything
        {
            let bms = BmsWrapper::<RecordHistory>::new();
            (0..LOG_COMPACT_THRESHOLD + 10).for_each(|_| {
                bms.add_new_record(None, None, None, None);
            });

            bms.compact_record_log();
            assert_eq!(bms.acquire_read_lock().len(), LOG_MAX_SIZE);
        }

        // should only remove as many as necessary produced to meet the cleanup threshold
        {
            let bms = BmsWrapper::<RecordHistory>::new();
            (0..LOG_COMPACT_THRESHOLD + 10).for_each(|_| {
                bms.add_new_record(None, None, None, None);
            });
            {
                let recs = &mut *bms.acquire_write_lock();
                let mock_gr_cls = Arc::new(GroundedMemb::gen_mock());
                for (i, rec) in recs.iter_mut().enumerate() {
                    if i % 2 == 0 {
                        let mock_produced =
                            (Grounded::Class(Arc::downgrade(&mock_gr_cls.clone())), None);
                        rec.produced.push(mock_produced);
                    }
                }
            }

            bms.compact_record_log();
            assert_eq!(bms.acquire_read_lock().len(), LOG_MAX_SIZE);
        }
    }
}
