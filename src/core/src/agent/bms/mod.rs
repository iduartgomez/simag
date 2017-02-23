//! Belief Maintenance System for the agent system.
//!
//! This module adds methods and types for:
//! 1) Recording how a belief came to existence to the agent.
//! 2) Detecting inconsistences between new and old beliefs.
//! 3) Fixing those inconsitences.

use super::{ProofResult, Representation};
use super::kb::QueryInput;
use lang::{Date, Grounded, GroundedRef};

use chrono::UTC;

use std::mem;
use std::sync::RwLock;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

#[derive(Debug)]
pub struct BmsWrapper {
    records: RwLock<Vec<*mut BmsRecord>>,
    read: AtomicBool,
    pub overwrite: AtomicBool,
}

impl BmsWrapper {
    pub fn new(overwrite: bool) -> BmsWrapper {
        BmsWrapper {
            records: RwLock::new(vec![]),
            read: AtomicBool::new(true),
            overwrite: AtomicBool::new(overwrite),
        }
    }

    pub fn new_record(&self, date: Option<Date>, value: Option<f32>, was_produced: Option<bool>) {
        let date = match date {
            Some(date) => date,
            None => UTC::now(),
        };
        let was_produced = match was_produced {
            Some(val) => val,
            None => false,
        };
        let record = BmsRecord {
            locked: AtomicBool::new(false),
            produced: vec![],
            date: date,
            value: value,
            was_produced: was_produced,
            refcnt: AtomicUsize::new(1),
        };
        let raw_rec = Box::into_raw(Box::new(record));
        let mut records = self.records.write().unwrap();
        // if there is a previous record, lock it
        if records.len() > 0 {
            let raw = records.last_mut().unwrap();
            let last = BmsRecord::ptr_as_mut(*raw);
            last.lock();
        }
        records.push(raw_rec);
    }

    pub fn update(&self,
                  owner: GroundedRef,
                  agent: &Representation,
                  data: &BmsWrapper,
                  was_produced: bool) {
        let ask_processed = |entry: &(Grounded, Option<f32>), cmp_rec: &Date| {
            match *entry {
                (Grounded::Function(ref func), ..) => {
                    // check if indeed the value was produced by this producer or is more recent
                    let mut ask = false;
                    {
                        let lock = func.bms.records.read().unwrap();
                        let last = BmsRecord::ptr_as_ref(*lock.last().unwrap());
                        if last.date > *cmp_rec {
                            // if it was produced, run again a test against the kb to check
                            // if it is still valid
                            ask = true;
                        }
                    }
                    if ask {
                        let answ =
                            agent.ask_processed(QueryInput::AskRelationalFunc(func.clone()),
                                               0,
                                               true)
                                .get_results_single();
                        if answ.is_none() {
                            let bms = &func.bms;
                            let mut date: Option<Date> = None;
                            let mut value: Option<f32> = None;
                            {
                                let lock = bms.records.read().unwrap();
                                let recs = (*lock).iter();
                                for rec in recs.rev() {
                                    let r = BmsRecord::ptr_as_ref(*rec);
                                    if !r.was_produced {
                                        date = Some(r.date);
                                        value = r.value;
                                        break;
                                    }
                                }
                            }
                            func.update_value(value);
                            bms.new_record(date, value, Some(false));
                        }
                    }
                }
                (Grounded::Class(ref cls), ..) => {
                    let mut ask = false;
                    {
                        let lock = cls.bms.as_ref().unwrap().records.read().unwrap();
                        let last = BmsRecord::ptr_as_ref(*lock.last().unwrap());
                        if last.date > *cmp_rec {
                            ask = true;
                        }
                    }
                    if ask {
                        let answ =
                            agent.ask_processed(QueryInput::AskClassMember(cls.clone()), 0, true)
                                .get_results_single();
                        if answ.is_none() {
                            let bms = cls.bms.as_ref().unwrap();
                            let mut date: Option<Date> = None;
                            let mut value: Option<f32> = None;
                            {
                                let lock = bms.records.read().unwrap();
                                let recs = (*lock).iter();
                                for rec in recs.rev() {
                                    let r = BmsRecord::ptr_as_ref(*rec);
                                    if !r.was_produced {
                                        date = Some(r.date);
                                        value = r.value;
                                        break;
                                    }
                                }
                            }
                            cls.update_value(value);
                            bms.new_record(date, value, Some(false));
                        }
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

        if data.overwrite.load(Ordering::Acquire) {
            let old_recs;
            {
                let mut new_recs: Vec<*mut BmsRecord> = vec![];
                for rec in &*data.records.read().unwrap() {
                    new_recs.push(*rec)
                }
                let prev_recs = &mut *self.records.write().unwrap();
                let last = BmsRecord::ptr_as_mut(*prev_recs.last_mut().unwrap());
                last.lock();
                mem::swap(prev_recs, &mut new_recs);
                old_recs = new_recs;
            }
            for rec in old_recs {
                let r = BmsRecord::ptr_as_ref(rec);
                for entry in r.get_old_entries() {
                    ask_processed(entry, &r.date);
                }
                // if the record is not owned by an other object, it's safe to drop
                if r.refcnt.load(Ordering::SeqCst) == 1 {
                    unsafe { Box::from_raw(rec) };
                }
            }
            return;
        }

        // check if there are any inconsistencies with the knowledge produced with
        // the previous value
        let update_rec = {
            let up_lock = data.records.read().unwrap();
            BmsRecord::ptr_as_ref(*up_lock.last().unwrap())
        };
        // create a new record with the new data and lock the last one
        let date = update_rec.date;
        let value = update_rec.value;
        self.new_record(Some(date), value, Some(was_produced));
        owner.update_value(update_rec.value);
        // get a reference to the last record before the new one was inserted
        let last_record = {
            let lock = &*self.records.read().unwrap();
            let l = lock.len() - 2;
            BmsRecord::ptr_as_ref(lock[l])
        };
        if update_rec.date < last_record.date {
            // new value is older, in face of new information all previously
            // produced knowledge must be checked to see if it still holds true
            let old_recs;
            {
                let mut new_recs: Vec<*mut BmsRecord> = vec![];
                let records = &mut *self.records.write().unwrap();
                let newest_rec = records.pop().unwrap();
                new_recs.push(newest_rec);
                mem::swap(records, &mut new_recs);
                old_recs = new_recs;
            }
            for rec in old_recs {
                let rec = BmsRecord::ptr_as_ref(rec);
                for entry in rec.get_old_entries() {
                    ask_processed(entry, &last_record.date);
                }
            }
        } else if update_rec.date > last_record.date || update_rec.value != last_record.value {
            // new value is more recent, check only the last produced values
            //
            // if both dates are the same there is an incongruency
            // replace previous record value and check that all produced knowledge
            // with that value still holds true
            for entry in last_record.get_old_entries() {
                ask_processed(entry, &last_record.date);
            }
        }
    }

    pub fn update_producers(&self, owner: Grounded, context: &ProofResult) {
        // add the produced knowledge to each producer in case it comes
        // from a logic sentence resolution
        for a in &context.antecedents {
            match *a {
                Grounded::Class(ref cls) => {
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
        let record = BmsRecord::ptr_as_mut(*raw);
        record.add_entry((produced, with_val));
    }

    pub fn overwrite_data(&self, other: &BmsWrapper) {
        let mut lock = &mut *self.records.write().unwrap();
        // drop previous records
        for rec in lock.drain(..) {
            let r = BmsRecord::ptr_as_ref(rec);
            if r.refcnt.fetch_sub(1, Ordering::SeqCst) == 0 {
                unsafe { Box::from_raw(rec) };
            }
        }
        // insert new records
        for rec in &*other.records.read().unwrap() {
            lock.push(*rec)
        }
        self.overwrite.store(other.overwrite.load(Ordering::Acquire), Ordering::Release);
    }

    pub fn record_len(&self) -> usize {
        self.records.read().unwrap().len()
    }

    pub fn newest_date(&self, other: &Date) -> Option<Date> {
        let lock = self.records.read().unwrap();
        let rec = BmsRecord::ptr_as_ref(*lock.last().unwrap());
        if &rec.date > other {
            Some(rec.date)
        } else {
            None
        }
    }

    pub fn get_last_date(&self) -> Date {
        let lock = self.records.read().unwrap();
        let rec = BmsRecord::ptr_as_ref(*lock.last().unwrap());
        rec.date
    }

    pub fn replace_last_val(&self, val: Option<f32>) {
        let mut records = &mut self.records.write().unwrap();
        let last = BmsRecord::ptr_as_mut(*records.last_mut().unwrap());
        last.value = val;
    }

    pub fn last_was_produced(&self) {
        let mut records = &mut self.records.write().unwrap();
        let last = BmsRecord::ptr_as_mut(*records.last_mut().unwrap());
        last.was_produced = true;
    }
}

impl ::std::clone::Clone for BmsWrapper {
    fn clone(&self) -> BmsWrapper {
        let recs = self.records.read().unwrap();
        let mut data = vec![];
        for rec in &*recs {
            let r = BmsRecord::ptr_as_ref(*rec);
            r.refcnt.fetch_add(1, Ordering::SeqCst);
            data.push(*rec);
        }
        BmsWrapper {
            read: AtomicBool::new(self.read.load(Ordering::Acquire)),
            records: RwLock::new(data),
            overwrite: AtomicBool::new(self.overwrite.load(Ordering::Acquire)),
        }
    }
}

impl ::std::ops::Drop for BmsWrapper {
    fn drop(&mut self) {
        let mut lock = &mut *self.records.write().unwrap();
        // drop previous records
        for rec in lock.drain(..) {
            let r = BmsRecord::ptr_as_ref(rec);
            if r.refcnt.fetch_sub(1, Ordering::SeqCst) == 0 {
                unsafe { Box::from_raw(rec) };
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
    was_produced: bool,
}

impl BmsRecord {
    fn add_entry(&mut self, entry: (Grounded, Option<f32>)) {
        self.produced.push(entry);
    }

    fn get_old_entries(&self) -> BmsRecIterator {
        if self.locked.load(Ordering::SeqCst) {
            BmsRecIterator::new(&self.produced)
        } else {
            panic!()
        }
    }

    fn lock(&mut self) {
        self.locked.store(true, Ordering::SeqCst);
    }

    fn ptr_as_ref<'a>(ptr: *mut BmsRecord) -> &'a BmsRecord {
        unsafe { &*ptr as &BmsRecord }
    }

    fn ptr_as_mut<'a>(ptr: *mut BmsRecord) -> &'a mut BmsRecord {
        unsafe { &mut *ptr as &mut BmsRecord }
    }
}

struct BmsRecIterator<'a> {
    cnt: usize,
    data: &'a [(Grounded, Option<f32>)],
}

impl<'a> BmsRecIterator<'a> {
    fn new(data: &'a [(Grounded, Option<f32>)]) -> BmsRecIterator<'a> {
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
        let answ0 = rep.ask("(fat[$Pancho,u=0])".to_string());
        assert_eq!(answ0.get_results_single(), Some(true));
        let answ1 = rep.ask("(ugly[$Pancho,u=0])".to_string());
        assert_eq!(answ1.get_results_single(), Some(true));
        let answ2 = rep.ask("(sad[$Pancho,u=0])".to_string());
        assert_eq!(answ2.get_results_single(), None);
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
