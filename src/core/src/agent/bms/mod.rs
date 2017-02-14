//! Belief Maintenance System for the agent system.
//!
//! This module adds methods and types for:
//! 1) Recording how a belief came to existence to the agent.
//! 2) Detecting inconsistences between new and old beliefs.
//! 3) Fixing those inconsitences.
#![allow(dead_code)]

use std::sync::RwLock;
use std::rc::Rc;

use chrono::UTC;

use lang::{Date, Grounded, GroundedClsMemb, GroundedFunc};
use super::{ProofResult, Representation};
use super::kb::QueryInput;

#[derive(Debug)]
pub struct BmsWrapper {
    records: RwLock<Vec<BmsRecord>>,
}

impl ::std::clone::Clone for BmsWrapper {
    fn clone(&self) -> BmsWrapper {
        let data = self.records.read().unwrap().clone();
        BmsWrapper { records: RwLock::new(data) }
    }
}

impl BmsWrapper {
    pub fn new(value: Option<f32>) -> BmsWrapper {
        let wrapper = BmsWrapper { records: RwLock::new(vec![]) };
        // add first record
        wrapper.new_record(None, value);
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

    pub fn update(&self,
                  agent: &Representation,
                  owner: Grounded,
                  data: &BmsWrapper,
                  context: Option<&ProofResult>) {
        // self.override_data(data);
        // check if there are any inconsistencies with the knowledge produced with
        // the previous value
        let lock = self.records.read().unwrap();
        let last_record = lock.last().unwrap();
        for record in &last_record.produced {
            match *record {
                (Grounded::Function(ref func), ..) => {
                    // check if indeed the value was produced by this producer or is more recent
                    let prod_date = (&*func).bms.last_date();
                    if prod_date <= last_record.date {
                        // if it was produced, run again a test against the kb to check
                        // if it is still valid
                        agent.ask_processed(QueryInput::AskRelationalFunc(func.clone()),
                                            true);
                    }
                }
                (Grounded::Terminal(ref cls), ..) => {
                    // check if indeed the value was produced by this producer or is more recent
                    let prod_date = (&*cls).bms.as_ref().unwrap().last_date();
                    if prod_date <= last_record.date {
                        // if it was produced, run again a test against the kb to check
                        // if it is still valid
                        agent.ask_processed(QueryInput::AskClassMember(cls.clone()), true);
                    }
                }
            }
        }
        // add the produced knowledge to each producer
        if let Some(context) = context {
            for a in &context.antecedents {
                match *a {
                    Grounded::Terminal(ref cls) => {
                        let value = cls.get_value();
                        cls.bms
                            .as_ref()
                            .unwrap()
                            .add_entry(owner.clone(), value)
                    }
                    Grounded::Function(ref func) => {
                        let value = func.get_value();
                        func.bms.add_entry(owner.clone(), Some(value))
                    }
                }
            }
        }
    }

    pub fn override_data(&self, other: &BmsWrapper) {
        let mut lock = &mut *self.records.write().unwrap();
        lock.truncate(0);
        for rec in &*other.records.read().unwrap() {
            lock.push(rec.clone())
        }
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
