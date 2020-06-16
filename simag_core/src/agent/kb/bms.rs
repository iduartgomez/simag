//! Belief Maintenance System for the agent system.
//!
//! This module adds methods and types for:
//! 1) Recording how a belief came to existence to the agent.
//! 2) Detecting inconsistences between new and old beliefs.
//! 3) Fixing those inconsitences.

pub(in crate::agent) use self::errors::BmsError;
use super::{inference::QueryInput, repr::Representation};
use crate::agent::lang::{Grounded, GroundedRef, ProofResContext, SentID, Time};

use chrono::Utc;
use parking_lot::{RwLock, RwLockReadGuard};

use std::cmp::Ordering as CmpOrdering;
use std::mem;
use std::sync::atomic::{AtomicBool, Ordering};

/// Acts as a wrapper for the Belief Maintenance System for a given agent.
///
/// Serves to keep the believes alive in memory, fix inconsistencies and
/// serialize any information.
#[derive(Debug)]
pub(in crate::agent) struct BmsWrapper {
    records: RwLock<Vec<BmsRecord>>,
    /// set if it's a predicate with the time of query execution
    pred: Option<Time>,
    /// When this flag is set, it means that if this wrapper is received as an updating wrapper
    /// it will replace all the existing records of the wrapper being updated.
    ///
    /// When this is done all the previous records will be re-examined with any new knowledge
    /// being checked, which could produce new facts or update existing facts rolling back
    /// previous knowledge that no longer applies.
    pub overwrite: AtomicBool,
}

impl BmsWrapper {
    pub fn new(overwrite: bool) -> BmsWrapper {
        BmsWrapper {
            records: RwLock::new(vec![]),
            pred: None,
            overwrite: AtomicBool::new(overwrite),
        }
    }

    /// Add a new record to this BMS.
    pub fn new_record(
        &self,
        time: Option<Time>,
        value: Option<f32>,
        was_produced: Option<(SentID, Time)>,
    ) {
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
            value,
            was_produced,
        };
        let mut records = self.records.write();
        records.push(record);
    }

    /// Look for all the changes that were produced, before an update,
    /// due to this belief previous value and test if they still apply.
    /// If the facts no longer hold true rollback them.
    ///
    /// Caveat: this implementation is actually not solid and is potentially racey
    /// because the inner checks for rollbacks could potentially provoque inconsistencies
    /// when subsequent checks are recursively performed. This is hard to provoque because
    /// a Representation is performing one query at the same time and is guaranteed
    /// to be uniquely hold as both `tell` and `ask` require a unique reference, but
    /// with the right combination of queries it could happen.
    pub fn update(
        &self,
        owner: &GroundedRef,
        agent: &Representation,
        data: &BmsWrapper,
        was_produced: Option<(SentID, Time)>,
    ) {
        let ask_processed = |entry: &(Grounded, Option<f32>), cmp_rec: &Time| {
            match *entry {
                (Grounded::Function(ref func), ..) => {
                    let func = func.upgrade().unwrap();
                    // check if indeed the value was produced by this producer or is more recent
                    let mut ask = false;
                    let func_lock = func.bms.records.read();
                    let last = func_lock.last().unwrap();
                    if last.time > *cmp_rec {
                        // if it was produced, run again a test against the kb to check
                        // if it is still valid
                        ask = true;
                    }
                    if ask {
                        let answ = agent
                            .ask_processed(QueryInput::AskRelationalFunc(func.clone()), 0, true)
                            .unwrap()
                            .get_results_single();
                        if answ.is_none() {
                            let bms = &func.bms;
                            let mut time: Option<Time> = None;
                            let mut value: Option<f32> = None;
                            let recs = (*func_lock).iter();
                            for rec in recs.rev() {
                                if rec.was_produced.is_none() {
                                    time = Some(rec.time);
                                    value = rec.value;
                                    break;
                                }
                            }
                            func.update_value(value);
                            bms.new_record(time, value, None);
                        }
                    }
                }
                (Grounded::Class(ref cls), ..) => {
                    let cls = cls.upgrade().unwrap();
                    let mut ask = false;
                    {
                        let lock = &*cls.bms.as_ref().unwrap().records.read();
                        let last = lock.last().unwrap();
                        if last.time > *cmp_rec {
                            ask = true;
                        }
                    }
                    if ask {
                        let answ = agent
                            .ask_processed(QueryInput::AskClassMember(cls.clone()), 0, true)
                            .unwrap()
                            .get_results_single();
                        if answ.is_none() {
                            let bms = cls.bms.as_ref().unwrap();
                            let mut time: Option<Time> = None;
                            let mut value: Option<f32> = None;
                            {
                                let lock = bms.records.read();
                                let recs = (*lock).iter();
                                for rec in recs.rev() {
                                    if rec.was_produced.is_none() {
                                        time = Some(rec.time);
                                        value = rec.value;
                                        break;
                                    }
                                }
                            }

                            cls.update_value(value);
                            bms.new_record(time, value, None);
                        }
                    }
                }
            }
        };

        if data.overwrite.load(Ordering::Acquire) {
            let old_recs;
            {
                let mut new_recs: Vec<BmsRecord> = vec![];
                for rec in &*data.records.read() {
                    new_recs.push(rec.clone())
                }
                let prev_recs = &mut *self.records.write();
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

        let (up_rec_value, up_rec_date) = {
            let lock = data.records.read();
            let last = lock.last().unwrap();
            (last.value, last.time)
        };
        let (last_rec_value, last_rec_date) = {
            let lock = self.records.read();
            let last = lock.last().unwrap();
            (last.value, last.time)
        };

        if (last_rec_date == up_rec_date) && (last_rec_value == up_rec_value) {
            return;
        }

        // create a new record with the new data
        owner.update_value(up_rec_value);
        self.new_record(Some(up_rec_date), up_rec_value, was_produced);

        // check if there are any inconsistencies with the knowledge produced with
        // the previous value
        if up_rec_date < last_rec_date {
            // new value is older, in face of new information all previously
            // produced knowledge must be checked to see if it still holds true
            let old_recs;
            {
                let mut new_recs: Vec<BmsRecord> = vec![];
                let records = &mut *self.records.write();
                let newest_rec = records.pop().unwrap();
                new_recs.push(newest_rec);
                mem::swap(records, &mut new_recs);
                old_recs = new_recs;
            }
            for rec in old_recs {
                for entry in &rec.produced {
                    ask_processed(entry, &last_rec_date);
                }
            }
        } else if up_rec_date > last_rec_date || up_rec_value != last_rec_value {
            // new value is more recent, check only the last produced values
            //
            // if both times are the same there is an incongruency
            // replace previous record value and check that all produced knowledge
            // with that value still holds true
            let last = {
                // in order to avoid deadlocks, pop the old record temporarily
                // and push it back after the necessary checks, then sort by age
                let mut records = self.records.write();
                let pos = records.len() - 2;
                records.remove(pos)
            };
            for entry in &last.produced {
                ask_processed(entry, &last_rec_date);
            }
            {
                let records = &mut *self.records.write();
                records.push(last);
                records.sort_by(|a, b| a.time.cmp(&b.time));
            }
        }
        self.cleanup_records()
    }

    fn cleanup_records(&self) {
        let records = &mut *self.records.write();
        let l = records.len() - 2;
        let mut remove_ls: Vec<usize> = vec![];
        for (i, rec) in records[..l].iter().enumerate() {
            if rec.produced.is_empty() && rec.was_produced.is_some() {
                remove_ls.push(i);
            }
        }
        remove_ls.reverse();
        for pos in remove_ls {
            records.remove(pos);
        }
    }

    fn add_entry(&self, produced: Grounded, with_val: Option<f32>) {
        let mut records = self.records.write();
        let record = records.last_mut().unwrap();
        record.add_entry((produced, with_val));
    }

    pub fn update_producers<T: ProofResContext>(owner: &Grounded, context: &T) {
        // add the produced knowledge to each producer in case it comes
        // from a logic sentence resolution
        for a in context.get_antecedents() {
            match *a {
                Grounded::Class(ref cls) => {
                    let cls = cls.upgrade().unwrap();
                    let value = cls.get_value();
                    cls.bms.as_ref().unwrap().add_entry(owner.clone(), value);
                }
                Grounded::Function(ref func) => {
                    let func = func.upgrade().unwrap();
                    let value = func.get_value();
                    func.bms.add_entry(owner.clone(), value);
                }
            }
        }
    }

    /// Drops all the records and writes the records from the incoming BMS.
    pub fn overwrite_data(&self, other: &BmsWrapper) {
        let lock = &mut *self.records.write();
        // drop old records
        lock.truncate(0);
        // insert new records
        for rec in &*other.records.read() {
            lock.push(rec.clone())
        }
        self.overwrite
            .store(other.overwrite.load(Ordering::Acquire), Ordering::Release);
    }

    pub fn record_len(&self) -> usize {
        self.records.read().len()
    }

    /// Compare the last record on this bms to a given time and returns
    /// the newest of both.
    pub fn get_newest_date(&self, other: Time) -> Option<Time> {
        let lock = &*self.records.read();
        let rec = lock.last().unwrap();
        if rec.time > other {
            Some(rec.time)
        } else {
            None
        }
    }

    /// Get the last record date.
    pub fn get_last_date(&self) -> Time {
        let lock = self.records.read();
        let rec = lock.last().unwrap();
        rec.time
    }

    pub fn get_last_value(&self) -> Option<f32> {
        let records = self.records.read();
        if let Some(rec) = records.last() {
            rec.value
        } else {
            None
        }
    }

    /// Replaces a record when creating one of two records.
    pub fn replace_value(&self, val: Option<f32>, mode: ReplaceMode) {
        let records = &mut *self.records.write();
        match mode {
            ReplaceMode::Substitute => {
                let last = records.last_mut().unwrap();
                last.value = val;
            }
            ReplaceMode::Tell => {
                // can be an interval, so we replace the first value
                let first = records.first_mut().unwrap();
                first.value = val;
            }
        }
    }

    /// Set a producer for the last truth value
    pub fn set_last_rec_producer(&self, produced: Option<(SentID, Time)>) {
        let records = &mut *self.records.write();
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
    pub(in crate::agent) fn rollback_one_once(&self, other: &BmsWrapper) {
        // Write lock to guarantee atomicity of the operation
        let lock_self = &mut *self.records.write();
        let lock_other = &mut *other.records.write();
        let own_is_newer = {
            let own_last_time = lock_self.last_mut().unwrap();
            let other_last_time = lock_other.last_mut().unwrap();

            match own_last_time.time.cmp(&other_last_time.time) {
                CmpOrdering::Greater | CmpOrdering::Equal => true,
                _ => false,
            }
        };

        if own_is_newer {
            BmsWrapper::rollback_once(lock_self.pop().unwrap());
        } else {
            BmsWrapper::rollback_once(lock_other.pop().unwrap());
        }
    }

    fn rollback_once(recordings: BmsRecord) {
        let (sent_id, at_time) = if let Some((sent_id, at_time)) = recordings.was_produced {
            (sent_id, at_time)
        } else {
            unreachable!(format!(
                "SIMAG - {}:{} - unreachable: can't rollback records which were not produced",
                file!(),
                line!()
            ))
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
        let lock = &mut *self.records.write();
        let rollback_this = {
            let last = lock.last().unwrap();
            if let Some((other_id, ref production_time)) = last.was_produced {
                other_id == sent_id && at_time == production_time
            } else {
                false
            }
        };
        if rollback_this {
            BmsWrapper::rollback_once(lock.pop().unwrap());
        }
    }

    /// Compare the time of the last record of two BMS and return
    /// the ordering of self vs. other.
    pub fn cmp_by_time(&self, other: &BmsWrapper) -> CmpOrdering {
        let lock0 = &*self.records.read();
        let lock1 = &*other.records.read();
        let own_last_time = &lock0.last().unwrap().time;
        let other_last_time = &lock1.last().unwrap().time;
        own_last_time.cmp(other_last_time)
    }

    /// Merge an other bmswrapper with this one, the belief then will be true
    /// between the time interval [start, end) of both records, after which it will be set
    /// as `unknown`.
    ///
    /// This operation is meant to be used when asserting new facts.
    pub fn merge_from_until(&mut self, until: &BmsWrapper) -> Result<(), errors::BmsError> {
        let mut self_records = self.records.write();
        let other_records = until.records.read();

        if self_records.is_empty() || other_records.is_empty() {
            return Err(errors::BmsError::EmptyRecordList);
        } else if self_records.len() > 1 || other_records.len() > 1 {
            return Err(errors::BmsError::IllegalMerge(
                "More than one record found".to_string(),
            ));
        }

        let until = other_records.get(0).unwrap();
        let from = self_records.get_mut(0).unwrap();
        if until.value != from.value {
            return Err(errors::BmsError::IllegalMerge(
                "Different values while merging".to_string(),
            ));
        }
        let mut until = until.clone();
        if until.time < from.time {
            return Err(errors::BmsError::IllegalMerge(
                "First time variable assignment can't be older than second".to_string(),
            ));
        }
        until.value = None;
        self_records.push(until);
        Ok(())
    }

    /// Return the value valid at the given time, if any.
    #[allow(clippy::op_ref)]
    pub fn get_record_at_time(&self, cmp_time: Time) -> Option<f32> {
        let mut lastest = None;
        for rec in self
            .records
            .read()
            .iter()
            .take_while(|rec| &rec.time <= &cmp_time)
        {
            lastest = rec.value;
        }
        lastest
    }

    pub fn iter_values(&self) -> TimeValueIterator {
        let values: Vec<_> = self
            .records
            .read()
            .iter()
            .map(|r| (r.time, r.value))
            .rev()
            .collect();
        TimeValueIterator { values }
    }

    pub fn of_predicate(&mut self) {
        self.pred = Some(Utc::now());
    }

    pub fn is_predicate(&self) -> Option<&Time> {
        self.pred.as_ref()
    }

    pub fn acquire_read_lock(&self) -> RwLockReadGuard<Vec<BmsRecord>> {
        self.records.read()
    }
}

impl std::clone::Clone for BmsWrapper {
    fn clone(&self) -> BmsWrapper {
        let recs = &*self.records.read();
        BmsWrapper {
            records: RwLock::new(recs.clone()),
            pred: self.pred,
            overwrite: AtomicBool::new(self.overwrite.load(Ordering::Acquire)),
        }
    }
}

pub struct TimeValueIterator {
    values: Vec<(Time, Option<f32>)>,
}

impl std::iter::Iterator for TimeValueIterator {
    type Item = (Time, Option<f32>);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.pop()
    }
}

/// Record of how a belief became to existence and what other believes
/// it has produced since then.
#[derive(Debug, Clone)]
pub(in crate::agent) struct BmsRecord {
    produced: Vec<(Grounded, Option<f32>)>,
    time: Time,
    value: Option<f32>,
    was_produced: Option<(SentID, Time)>,
}

impl BmsRecord {
    fn add_entry(&mut self, entry: (Grounded, Option<f32>)) {
        self.produced.push(entry);
    }
}

#[derive(Debug, Clone, Copy)]
pub(in crate::agent) enum ReplaceMode {
    Tell,
    Substitute,
}

mod errors {
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
    fn bms_rollback() {
        let rep = Representation::default();

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

        let fol = "
            (run[$Pancho=1])
            (let x in 
             ((run[x=1] and dog[x=1]) := fat[x=0]))
        ";
        rep.tell(fol).unwrap();
        let answ0 = rep.ask("(fat[$Pancho=0])");
        assert_eq!(answ0.unwrap().get_results_single(), Some(true));
        let answ1 = rep.ask("(ugly[$Pancho=0])");
        assert_eq!(answ1.unwrap().get_results_single(), Some(true));
        let answ2 = rep.ask("(sad[$Pancho=0])");
        assert_eq!(answ2.unwrap().get_results_single(), None);
    }

    #[test]
    #[ignore]
    fn bms_maybe_rollback() {
        todo!()
    }

    #[test]
    fn bms_review_after_change() {
        let rep = Representation::default();

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
}
