//! Belief Maintenance System for the agent system.
//!
//! This module adds methods and types for:
//! 1) Recording how a belief came to existence to the agent.
//! 2) Detecting inconsistences between new and old beliefs.
//! 3) Fixing those inconsitences.

use super::{ProofResult, Representation};
use super::kb::QueryInput;

use chrono::UTC;

use lang::{Date, Grounded};
use std::mem;
use std::sync::RwLock;

#[derive(Debug)]
pub struct BmsWrapper {
    records: RwLock<Vec<BmsRecord>>,
    pub overwrite: RwLock<Option<bool>>,
}

impl ::std::clone::Clone for BmsWrapper {
    fn clone(&self) -> BmsWrapper {
        let data = self.records.read().unwrap().clone();
        let ow_value = self.overwrite.read().unwrap().clone();
        BmsWrapper {
            records: RwLock::new(data),
            overwrite: RwLock::new(ow_value),
        }
    }
}

impl BmsWrapper {
    pub fn new(overwrite: Option<bool>) -> BmsWrapper {
        let wrapper = BmsWrapper {
            records: RwLock::new(vec![]),
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
            produced: vec![],
            date: date,
            value: value,
        };
        let mut records = self.records.write().unwrap();
        records.push(record);
    }

    pub fn add_entry(&self, produced: Grounded, with_val: Option<f32>) {
        let mut records = self.records.write().unwrap();
        records.last_mut().unwrap().produced.push((produced, with_val));
    }

    pub fn record_len(&self) -> usize {
        self.records.read().unwrap().len()
    }

    pub fn update(&self, agent: &Representation, data: &BmsWrapper) {
        let ask_processed = |entry: &(Grounded, Option<f32>), last_record: &BmsRecord| {
            match *entry {
                (Grounded::Function(ref func), ..) => {
                    // check if indeed the value was produced by this producer or is more recent
                    let lock = func.bms.records.read().unwrap();
                    let ref prod_date = lock.last().unwrap().date;
                    if *prod_date <= last_record.date {
                        // if it was produced, run again a test against the kb to check
                        // if it is still valid
                        agent.ask_processed(QueryInput::AskRelationalFunc(func.clone()), true);
                    }
                }
                (Grounded::Terminal(ref cls), ..) => {
                    let lock = cls.bms.as_ref().unwrap().records.read().unwrap();
                    let ref prod_date = lock.last().unwrap().date;
                    if *prod_date <= last_record.date {
                        agent.ask_processed(QueryInput::AskClassMember(cls.clone()), true);
                    }
                }
            }
        };

        if let Some(_) = *data.overwrite.read().unwrap() {
            let mut new_records: Vec<BmsRecord> = vec![];
            for rec in &*data.records.read().unwrap() {
                new_records.push(rec.clone())
            }

            let new_rec = &mut *self.records.write().unwrap();
            let prev_rec = &mut new_records;
            mem::swap(prev_rec, new_rec);
            for record in &*prev_rec {
                println!("rec: {:?}\n", record);
                for entry in &record.produced {
                    ask_processed(entry, record);
                }
            }
            return;
        }

        // check if there are any inconsistencies with the knowledge produced with
        // the previous value
        let up_lock = data.records.read().unwrap();
        let update_rec = up_lock.last().unwrap().clone();
        let last_date = self.last_date();
        if update_rec.date > last_date {
            // new value is more recent, check only the last produced values and
            // append a new entry to the end of the record
            let lock = &mut *self.records.write().unwrap();
            lock.push(update_rec);
            let last_pos = lock.len() - 2;
            let last_record = lock.get(last_pos).unwrap();
            for entry in &last_record.produced {
                ask_processed(entry, last_record);
            }
        } else if update_rec.date < last_date {
            // new value is older, in face of new information all previously
            // produced knowledge must be checked to see if it still holds true
        } else {
            // if both dates are the same there is an incongruency
            // replace previous record value and check that all produced knowledge
            // with that value still holds true
        }
    }

    pub fn update_producers(&self, owner: Grounded, context: &ProofResult) {
        // add the produced knowledge to each producer in case it comes
        // from a logic sentence resolution
        println!("\nfrom: {}", owner.get_name());
        for a in &context.antecedents {
            match *a {
                Grounded::Terminal(ref cls) => {
                    println!("add to: {}", &*cls.get_parent());
                    let value = cls.get_value();
                    cls.bms
                        .as_ref()
                        .unwrap()
                        .add_entry(owner.clone(), value);
                }
                Grounded::Function(ref func) => {
                    println!("add to: {}", &*func.get_name());
                    let value = func.get_value();
                    func.bms.add_entry(owner.clone(), value);
                }
            }
        }
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

    pub fn last_date(&self) -> Date {
        self.records.read().unwrap().last().unwrap().date.clone()
    }

    pub fn replace_last_val(&self, val: Option<f32>) {
        let mut records = &mut self.records.write().unwrap();
        records.last_mut().unwrap().value = val;
    }
}

#[derive(Debug, Clone)]
struct BmsRecord {
    produced: Vec<(Grounded, Option<f32>)>,
    date: Date,
    value: Option<f32>,
}
