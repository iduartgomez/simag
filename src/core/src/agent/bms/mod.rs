//! Belief Maintenance System for the agent system.
//!
//! This module adds methods and types for:
//! 1) Recording how a belief came to existence to the agent.
//! 2) Detecting inconsistences between new and old beliefs.
//! 3) Fixing those inconsitences.
#![allow(dead_code)]

use std::sync::RwLock;

use chrono::UTC;

use lang::{Date, GroundedClsMemb, GroundedFunc};

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

    pub fn add_entry(&self, produced: Grounded) {
        let mut records = self.records.write().unwrap();
        records.last_mut().unwrap().produced.push(produced);
    }

    pub fn update(&self, data: &BmsWrapper) {
        // stub: must check for inconsistencies and rollback if necessary
        self.override_data(data);

    }

    pub fn override_data(&self, other: &BmsWrapper) {
        let mut lock = &mut *self.records.write().unwrap();
        lock.truncate(0);
        for rec in &*other.records.read().unwrap() {
            lock.push(rec.clone())
        }
    }

    pub fn last(&self) -> Date {
        self.records.read().unwrap().last().unwrap().date.clone()
    }

    pub fn replace_last_val(&self, val: Option<f32>) {
        let mut records = &mut self.records.write().unwrap();
        records.last_mut().unwrap().value = val;
    }
}

#[derive(Debug, Clone)]
struct BmsRecord {
    produced: Vec<Grounded>,
    date: Date,
    value: Option<f32>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Grounded {
    Function(*const GroundedFunc),
    Class(*const GroundedClsMemb),
}

impl Grounded {
    pub fn from_grounded_func(func_ref: &GroundedFunc) -> Grounded {
        Grounded::Function(func_ref as *const GroundedFunc)
    }

    pub fn from_grounded_cls(cls_ref: &GroundedClsMemb) -> Grounded {
        Grounded::Class(cls_ref as *const GroundedClsMemb)
    }
}
