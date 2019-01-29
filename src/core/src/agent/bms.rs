//! Belief Maintenance System for the agent system.
//!
//! This module adds methods and types for:
//! 1) Recording how a belief came to existence to the agent.
//! 2) Detecting inconsistences between new and old beliefs.
//! 3) Fixing those inconsitences.

use super::kb::QueryInput;
use super::Representation;
use crate::lang::{Grounded, GroundedRef, ProofResContext, SentID, Time};
pub(crate) use self::errors::BmsError;

use chrono::Utc;

use std::cmp::Ordering as CmpOrdering;
use std::mem;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::RwLock;

/// Acts as a wrapper for the Belief Maintenance System for a given agent.
///
/// Serves to keep the believes alive in memory, fix inconsistencies and
/// serialize any information.
#[derive(Debug)]
pub(crate) struct BmsWrapper {
    records: RwLock<Vec<BmsRecord>>,
    pub overwrite: AtomicBool,
}

impl BmsWrapper {
    pub fn new(overwrite: bool) -> BmsWrapper {
        BmsWrapper {
            records: RwLock::new(vec![]),
            overwrite: AtomicBool::new(overwrite),
        }
    }

    pub fn new_record(&self, time: Option<Time>, value: Option<f32>, was_produced: Option<SentID>) {
        let time = match time {
            Some(time) => time,
            None => Utc::now(),
        };
        let record = BmsRecord {
            produced: vec![],
            time,
            value,
            was_produced,
        };
        let mut records = self.records.write().unwrap();
        records.push(record);
    }

    /// Look for all the changes that were produced, before an update,
    /// due to this belief previous value and test if they still apply.
    /// If the facts no longer hold true rollback them.
    pub fn update(
        &self,
        owner: &GroundedRef,
        agent: &Representation,
        data: &BmsWrapper,
        was_produced: Option<SentID>,
    ) {
        let ask_processed = |entry: &(Grounded, Option<f32>), cmp_rec: &Time| {
            match *entry {
                (Grounded::Function(ref func), ..) => {
                    let func = func.upgrade().unwrap();
                    // check if indeed the value was produced by this producer or is more recent
                    let mut ask = false;
                    {
                        let lock = func.bms.records.read().unwrap();
                        let last = lock.last().unwrap();
                        if last.time > *cmp_rec {
                            // if it was produced, run again a test against the kb to check
                            // if it is still valid
                            ask = true;
                        }
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
                            {
                                let lock = bms.records.read().unwrap();
                                let recs = (*lock).iter();
                                for rec in recs.rev() {
                                    if rec.was_produced.is_none() {
                                        time = Some(rec.time);
                                        value = rec.value;
                                        break;
                                    }
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
                        let lock = &*cls.bms.as_ref().unwrap().records.read().unwrap();
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
                                let lock = bms.records.read().unwrap();
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
                for rec in &*data.records.read().unwrap() {
                    new_recs.push(rec.clone())
                }
                let prev_recs = &mut *self.records.write().unwrap();
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
            let lock = data.records.read().unwrap();
            let last = lock.last().unwrap();
            (last.value, last.time)
        };
        let (last_rec_value, last_rec_date) = {
            let lock = self.records.read().unwrap();
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
                let records = &mut *self.records.write().unwrap();
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
                let mut records = self.records.write().unwrap();
                let pos = records.len() - 2;
                records.remove(pos)
            };
            for entry in &last.produced {
                ask_processed(entry, &last_rec_date);
            }
            {
                let records = &mut *self.records.write().unwrap();
                records.push(last);
                records.sort_by(|a, b| a.time.cmp(&b.time));
            }
        }
        self.cleanup_records()
    }

    fn cleanup_records(&self) {
        let records = &mut *self.records.write().unwrap();
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
        let mut records = self.records.write().unwrap();
        let record = records.last_mut().unwrap();
        record.add_entry((produced, with_val));
    }

    pub fn update_producers<T: ProofResContext>(&self, owner: &Grounded, context: &T) {
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

    pub fn overwrite_data(&self, other: &BmsWrapper) {
        let lock = &mut *self.records.write().unwrap();
        // drop old records
        lock.truncate(0);
        // insert new records
        for rec in &*other.records.read().unwrap() {
            lock.push(rec.clone())
        }
        self.overwrite
            .store(other.overwrite.load(Ordering::Acquire), Ordering::Release);
    }

    pub fn record_len(&self) -> usize {
        self.records.read().unwrap().len()
    }

    pub fn newest_date(&self, other: Time) -> Option<Time> {
        let lock = &*self.records.read().unwrap();
        let rec = lock.last().unwrap();
        if rec.time > other {
            Some(rec.time)
        } else {
            None
        }
    }

    pub fn get_last_date(&self) -> Time {
        let lock = &*self.records.read().unwrap();
        let rec = lock.last().unwrap();
        rec.time
    }

    pub fn replace_last_val(&self, val: Option<f32>) {
        let records = &mut *self.records.write().unwrap();
        let last = records.last_mut().unwrap();
        last.value = val;
    }

    pub fn last_was_produced(&self, produced: Option<SentID>) {
        let records = &mut *self.records.write().unwrap();
        let last = records.last_mut().unwrap();
        last.was_produced = produced;
    }

    pub fn rollback_once(&self) {
        unimplemented!()
    }

    pub fn cmp_by_time(&self, other: &BmsWrapper) -> CmpOrdering {
        let lock0 = &*self.records.read().unwrap();
        let own_last_time = &lock0.last().unwrap().time;
        let lock1 = &*other.records.read().unwrap();
        let other_last_time = &lock1.last().unwrap().time;
        own_last_time.cmp(other_last_time)
    }

    /// Merge an other bmswrapper with this one, the belief then will be true
    /// between the time interval [start, end) of both records, after which it will be set 
    /// as `unknown`.
    /// 
    /// This operation is meant to be used when asserting new facts.
    pub fn merge_from_until(&mut self, until: &BmsWrapper) -> Result<(), errors::BmsError> {
        let mut self_records = self.records.write().unwrap();
        let other_records = until.records.read().unwrap();

        if self_records.is_empty() || other_records.is_empty() {
            return Err(errors::BmsError::EmptyRecordList);
        } else if self_records.len() > 1 || other_records.len() > 1 {
            return Err(errors::BmsError::IllegalMerge("More than one record found".to_string()));
        }

        let until = other_records.get(0).unwrap();
        let from = self_records.get_mut(0).unwrap();
        if until.value != from.value {
            return Err(errors::BmsError::IllegalMerge("Different values while merging".to_string()));
        }
        let mut until = until.clone();
        if until.time < from.time {
            std::mem::swap(&mut until, from);
        }
        until.value = None;
        self_records.push(until);
        Ok(())
    }
}

impl std::clone::Clone for BmsWrapper {
    fn clone(&self) -> BmsWrapper {
        let recs = &*self.records.read().unwrap();
        BmsWrapper {
            records: RwLock::new(recs.clone()),
            overwrite: AtomicBool::new(self.overwrite.load(Ordering::Acquire)),
        }
    }
}

/// Record of how a belief became to existence and what other believes
/// it has produced since then.
#[derive(Debug, Clone)]
struct BmsRecord {
    produced: Vec<(Grounded, Option<f32>)>,
    time: Time,
    value: Option<f32>,
    was_produced: Option<SentID>,
}

impl BmsRecord {
    fn add_entry(&mut self, entry: (Grounded, Option<f32>)) {
        self.produced.push(entry);
    }
}

mod errors {
    #[derive(Debug, PartialEq)]
    pub enum BmsError {
        IllegalMerge(String),
        EmptyRecordList
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bms_rollback() {
        let mut rep = Representation::new();

        let fol = "
            (ugly[$Pancho,u=0])
            (dog[$Pancho,u=1])
            (meat[$M1,u=1])
            (fn::eat[$M1,u=1;$Pancho])
            
            ((let x y)
             ((dog[x,u=1] && meat[y,u=1] && fn::eat[y,u=1;x]) 
              := fat[x,u=1]))
            
            ((let x)
             ((fat[x,u=1] && dog[x,u=1]) := (ugly[x,u=1] && sad[x,u=1])))
        ";
        rep.tell(fol).unwrap();
        {
            let answ = rep.ask("(fat[$Pancho,u=1] && sad[$Pancho,u=1])");
            assert_eq!(answ.unwrap().get_results_single(), Some(true));
        }

        let fol = "
            (run[$Pancho,u=1])
            ((let x) 
             ((run[x,u=1] && dog[x,u=1]) := fat[x,u=0]))
        ";
        rep.tell(fol).unwrap();
        let answ0 = rep.ask("(fat[$Pancho,u=0])");
        assert_eq!(answ0.unwrap().get_results_single(), Some(true));
        let answ1 = rep.ask("(ugly[$Pancho,u=0])");
        assert_eq!(answ1.unwrap().get_results_single(), Some(true));
        let answ2 = rep.ask("(sad[$Pancho,u=0])");
        assert_eq!(answ2.unwrap().get_results_single(), None);
    }

    #[test]
    fn bms_review_after_change() {
        let mut rep = Representation::new();

        let fol = "            
            ( meat[$M1,u=1] )
            ( dog[$Pancho,u=1] )
            ( fn::eat[$M1,u=1;$Pancho] )
            ( ( let x, y )
              ( ( dog[x,u=1] && meat[y,u=1] && fn::eat[y,u=1;x] ) 
                := fat[x,u=1] ) )
        ";
        rep.tell(fol).unwrap();
        {
            let answ = rep.ask("(fat[$Pancho,u=1])");
            assert_eq!(answ.unwrap().get_results_single(), Some(true));
        }

        let fol = "
            ( run[$Pancho,u=1] )
            (( let x ) (( dog[x,u=1] && run[x,u=1] ) := fat[x,u=0] ))
        ";
        rep.tell(fol).unwrap();
        {
            let answ = rep.ask("(fat[$Pancho,u=0])");
            assert_eq!(answ.unwrap().get_results_single(), Some(true));
        }

        rep.tell("(fn::eat[$M1,u=1;$Pancho])").unwrap();
        {
            let answ = rep.ask("(fat[$Pancho,u=1])");
            assert_eq!(answ.unwrap().get_results_single(), Some(true));
        }
    }
}
