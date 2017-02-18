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
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

#[derive(Debug)]
pub struct BmsWrapper {
    records: RwLock<Vec<*mut BmsRecord>>,
    read: AtomicBool,
    pub overwrite: RwLock<bool>,
}

impl BmsWrapper {
    pub fn new(overwrite: bool) -> BmsWrapper {
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
            refcnt: AtomicUsize::new(1),
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

        if *data.overwrite.read().unwrap() {
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
            for rec in recs {
                unsafe {
                    let r =  &*rec as &BmsRecord;
                    for entry in r.get_old_entries() {
                        ask_processed(entry, &r.date);
                    }
                    // if the record is not owned by an other object, it's safe to drop
                    if r.refcnt.load(Ordering::SeqCst) == 1 {
                        Box::from_raw(rec);
                    }
                }
            }
            return;
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
        // drop previous records
        for rec in lock.drain(..) {
            unsafe {
                let r = &*rec as &BmsRecord;             
                if r.refcnt.fetch_sub(1, Ordering::SeqCst) == 0 {
                    Box::from_raw(rec);
                }
            }
        }
        // insert new records
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
            unsafe {
                let r = &**rec as &BmsRecord;
                r.refcnt.fetch_add(1, Ordering::SeqCst);
            }
            data.push(rec.clone());
        }
        let ow_value = *self.overwrite.read().unwrap();
        BmsWrapper {
            read: AtomicBool::new(self.read.load(Ordering::Acquire)),
            records: RwLock::new(data),
            overwrite: RwLock::new(ow_value),
        }
    }
}

impl ::std::ops::Drop for BmsWrapper {
    fn drop(&mut self) {
        unsafe {
            let mut lock = &mut *self.records.write().unwrap();
            // drop previous records
            for rec in lock.drain(..) {
                let r = &*rec as &BmsRecord;             
                if r.refcnt.fetch_sub(1, Ordering::SeqCst) == 0 {
                    Box::from_raw(rec);
                }
            }
        }
    }
}

#[derive(Debug)]
struct BmsRecord {
    locked: AtomicBool,
    produced: Vec<(Grounded, Option<f32>)>,
    date: Date,
    value: Option<f32>,
    refcnt: AtomicUsize,
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bms_rollback() {
        let rep = Representation::new();

        let fol = String::from("
            (ugly[$Pancho,u=0])
            (dog[$Pancho,u=1])
            (meat[$M1,u=1])
            (fn::eat[$M1,u=1;$Pancho])
            
            ((let x y)
             ((dog[x,u=1] && meat[y,u=1] && fn::eat[y,u=1;x]) 
              |> fat[x,u=1]))
            
            ((let x)
             ((fat[x,u=1] && dog[x,u=1]) |> (ugly[x,u=1] && sad[x,u=1])))
        ");
        rep.tell(fol).unwrap();
        let answ = rep.ask("(fat[$Pancho,u=1] && sad[$Pancho,u=1])".to_string());
        assert_eq!(answ.get_results_single(), Some(true));

        let fol = String::from("
            (run[$Pancho,u=1])
            ((let x) 
             ((run[x,u=1] && dog[x,u=1]) |> fat[x,u=0]))
        ");
        rep.tell(fol).unwrap();
        let answ0 = rep.ask("(fat[$Pancho,u=0] && ugly[$Pancho,u=0])".to_string());
        assert_eq!(answ0.get_results_single(), Some(true));
        let answ1 = rep.ask("(sad[$Pancho,u=0])".to_string());
        assert_eq!(answ1.get_results_single(), None);
    }

    #[test]
    fn bms_review_after_change() {
        let rep = Representation::new();

        let fol = String::from("            
            ( meat[$M1,u=1] )
            ( dog[$Pancho,u=1] )
            ( fn::eat[$M1,u=1;$Pancho] )
            ( ( let x, y )
              ( ( dog[x,u=1] && meat[y,u=1] && fn::eat[y,u=1;x] ) 
                |> fat[x,u=1] ) )
        ");
        rep.tell(fol).unwrap();
        let answ = rep.ask("(fat[$Pancho,u=1])".to_string());
        assert_eq!(answ.get_results_single(), Some(true));

        let fol = String::from("
            ( run[$Pancho,u=1] )
            (( let x ) (( dog[x,u=1] && run[x,u=1] ) |> fat[x,u=0] ))
        ");
        rep.tell(fol).unwrap();
        let answ = rep.ask("(fat[$Pancho,u=0])".to_string());
        assert_eq!(answ.get_results_single(), Some(true));

        rep.tell("(fn::eat[$M1,u=1;$Pancho])".to_string()).unwrap();
        let answ = rep.ask("(fat[$Pancho,u=1])".to_string());
        assert_eq!(answ.get_results_single(), Some(true));
    }
}
