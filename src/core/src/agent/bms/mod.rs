//! Belief Maintenance System for the agent system.
//!
//! This module adds methods and types for:
//! 1) Recording how a belief came to existence to the agent.
//! 2) Detecting inconsistences between new and old beliefs.
//! 3) Fixing those inconsitences.

use super::{ProofResult, Representation};
use super::kb::QueryInput;
use lang::{Date, Grounded};

use chrono::UTC;

use std::mem;
use std::sync::RwLock;
use std::sync::atomic::{AtomicBool, Ordering};

#[derive(Debug)]
pub struct BmsWrapper {
    records: RwLock<Vec<*mut BmsRecord>>,
    read: AtomicBool,
    pub overwrite: RwLock<Option<bool>>,
}

impl BmsWrapper {
    pub fn new(overwrite: Option<bool>) -> BmsWrapper {
        let wrapper = BmsWrapper {
            records: RwLock::new(vec![]),
            read: AtomicBool::new(true),
            overwrite: RwLock::new(overwrite),
        };
        wrapper
    }

    pub fn new_record(&self, date: Option<Date>, value: Option<f32>) {
        let date = match date {
            Some(date) => date,
            None => UTC::now(),
        };
        let record = BmsRecord {
            locked: AtomicBool::new(false),
            produced: vec![],
            date: date,
            value: value,
        };
        let raw_rec = Box::into_raw(Box::new(record));
        let mut records = self.records.write().unwrap();
        // if there is a previous record, lock it
        if records.len() > 0 {
            let raw = records.last_mut().unwrap();
            let last = unsafe { &mut **raw as &mut BmsRecord };
            last.lock();
        }
        records.push(raw_rec);
    }

    pub fn update(&self, agent: &Representation, data: &BmsWrapper) {
        let ask_processed = |entry: &(Grounded, Option<f32>), cmp_rec: &Date| {
            match *entry {
                (Grounded::Function(ref func), ..) => {
                    // check if indeed the value was produced by this producer or is more recent
                    let mut ask = false;
                    {
                        let lock = func.bms.records.read().unwrap();
                        let last = unsafe { &**lock.last().unwrap() as &BmsRecord };
                        if last.date > *cmp_rec {
                            // if it was produced, run again a test against the kb to check
                            // if it is still valid
                            ask = true;
                        }
                    }
                    if ask {
                        agent.ask_processed(QueryInput::AskRelationalFunc(func.clone()), true);
                    }
                }
                (Grounded::Terminal(ref cls), ..) => {
                    let mut ask = false;
                    {
                        let lock = cls.bms.as_ref().unwrap().records.read().unwrap();
                        let last = unsafe { &**lock.last().unwrap() as &BmsRecord };
                        if last.date > *cmp_rec {
                            ask = true;
                        }
                    }
                    if ask {
                        agent.ask_processed(QueryInput::AskClassMember(cls.clone()), true);
                    }
                }
            }
        };

        // safety lock to avoid duplicate processing of the same data
        if !data.read.load(Ordering::Acquire) {
            return;
        } else {
            data.read.store(false, Ordering::Release);
        }

        match *data.overwrite.read().unwrap() {
            Some(val) if val => {
                let mut recs: Vec<*mut BmsRecord> = vec![];
                for rec in &*data.records.read().unwrap() {
                    recs.push(rec.clone())
                }
                {
                    let new_rec = &mut *self.records.write().unwrap();
                    let pre_rec = &mut recs;
                    mem::swap(pre_rec, new_rec);
                    let last = unsafe { &mut **pre_rec.last_mut().unwrap() as &mut BmsRecord };
                    last.lock();
                }
                for record in &recs {
                    let record = unsafe { &**record as &BmsRecord };
                    for entry in record.get_old_entries() {
                        ask_processed(entry, &record.date);
                    }
                }
                return;
            }
            _ => {}
        }

        // check if there are any inconsistencies with the knowledge produced with
        // the previous value
        let update_rec = unsafe {
            let up_lock = data.records.read().unwrap();
            &**up_lock.last().unwrap() as &BmsRecord
        };           
        // create a new record with the new data and lock the last one
        self.new_record(Some(update_rec.date.clone()), update_rec.value.clone());
        // get a reference to the last record before the new one was inserted
        let last_record = unsafe {
            let lock = &*self.records.read().unwrap();
            let l = lock.len() - 2;
            &**lock.get(l).unwrap() as &BmsRecord
        };
        if (update_rec.date > last_record.date) && (update_rec.value != last_record.value) {
            // new value is more recent, check only the last produced values and
            // append a new entry to the end of the record
            for entry in last_record.get_old_entries() {
                ask_processed(entry, &last_record.date);
            }
        } else if (update_rec.date > last_record.date) && (update_rec.value != last_record.value) {
            // new value is older, in face of new information all previously
            // produced knowledge must be checked to see if it still holds true
        } else if update_rec.value != last_record.value {
            // if both dates are the same there is an incongruency
            // replace previous record value and check that all produced knowledge
            // with that value still holds true
        }
    }

    pub fn update_producers(&self, owner: Grounded, context: &ProofResult) {
        // add the produced knowledge to each producer in case it comes
        // from a logic sentence resolution
        for a in &context.antecedents {
            match *a {
                Grounded::Terminal(ref cls) => {
                    let value = cls.get_value();
                    cls.bms
                        .as_ref()
                        .unwrap()
                        .add_entry(owner.clone(), value);
                }
                Grounded::Function(ref func) => {
                    let value = func.get_value();
                    func.bms.add_entry(owner.clone(), value);
                }
            }
        }
    }

    fn add_entry(&self, produced: Grounded, with_val: Option<f32>) {
        let mut records = self.records.write().unwrap();
        let raw = records.last_mut().unwrap();
        let record = unsafe { &mut **raw as &mut BmsRecord };
        record.add_entry((produced, with_val));
    }

    pub fn overwrite_data(&self, other: &BmsWrapper) {
        let mut lock = &mut *self.records.write().unwrap();
        lock.truncate(0);
        for rec in &*other.records.read().unwrap() {
            lock.push(rec.clone())
        }
        let mut lock = &mut *self.overwrite.write().unwrap();
        *lock = other.overwrite.read().unwrap().clone();
    }

    pub fn record_len(&self) -> usize {
        self.records.read().unwrap().len()
    }

    pub fn get_last_date(&self) -> &Date {
        let rec = unsafe {
            let records = self.records.read().unwrap();
            &**records.last().unwrap() as &BmsRecord
        };
        &rec.date
    }

    pub fn replace_last_val(&self, val: Option<f32>) {
        let mut records = &mut self.records.write().unwrap();
        let last = unsafe { &mut **records.last_mut().unwrap() as &mut BmsRecord };
        last.value = val;
    }
}

impl ::std::clone::Clone for BmsWrapper {
    fn clone(&self) -> BmsWrapper {
        let recs = self.records.read().unwrap();
        let mut data = vec![];
        for rec in &*recs {
            let rec = unsafe { &**rec as &BmsRecord };
            data.push(Box::into_raw(Box::new(rec.clone())));
        }
        let ow_value = self.overwrite.read().unwrap().clone();
        BmsWrapper {
            read: AtomicBool::new(self.read.load(Ordering::SeqCst)),
            records: RwLock::new(data),
            overwrite: RwLock::new(ow_value),
        }
    }
}

/*
impl ::std::ops::Drop for BmsWrapper {
    fn drop(&mut self) {
        let recs = self.records.write().unwrap();
        for rec in &*recs {
            unsafe { Box::from_raw(*rec) }; // drop each owned record
        }
    }
}
*/

#[derive(Debug)]
struct BmsRecord {
    locked: AtomicBool,
    produced: Vec<(Grounded, Option<f32>)>,
    date: Date,
    value: Option<f32>,
}

impl BmsRecord {
    fn add_entry(&mut self, entry: (Grounded, Option<f32>)) {
        self.produced.push(entry);
    }

    fn get_old_entries(&self) -> BmsRecIterator {
        if self.locked.load(Ordering::Relaxed) == true {
            BmsRecIterator::new(&self.produced)
        } else {
            panic!()
        }
    }

    fn lock(&mut self) {
        *self.locked.get_mut() = true;
    }
}

impl ::std::clone::Clone for BmsRecord {
    fn clone(&self) -> BmsRecord {
        BmsRecord {
            locked: AtomicBool::new(self.locked.load(Ordering::Acquire)),
            produced: self.produced.clone(),
            date: self.date.clone(),
            value: self.value.clone(),
        }
    }
}

struct BmsRecIterator<'a> {
    cnt: usize,
    data: &'a Vec<(Grounded, Option<f32>)>,
}

impl<'a> BmsRecIterator<'a> {
    fn new(data: &'a Vec<(Grounded, Option<f32>)>) -> BmsRecIterator<'a> {
        BmsRecIterator {
            data: data,
            cnt: 0,
        }
    }
}

impl<'a> ::std::iter::Iterator for BmsRecIterator<'a> {
    type Item = &'a (Grounded, Option<f32>);

    fn next(&mut self) -> Option<&'a (Grounded, Option<f32>)> {
        let out = self.data.get(self.cnt);
        self.cnt += 1;
        out
    }
}
