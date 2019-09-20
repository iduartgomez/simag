//! Main knowledge-base logic module, in this module reside the different
//! types that transform and store the data for the individual agents and
//! serve as representations of the different objects and the relationships
//! between them.
pub(in crate::agent) mod bms;
mod inference;
pub(super) mod repr;

#[cfg(test)]
mod test;

use std::collections::HashMap;
use std::sync::Arc;
use super::lang::{GroundedFunc, GroundedMemb};

#[derive(Debug, Clone)]
pub(in crate::agent) struct VarAssignment<'a> {
    pub name: &'a str,
    classes: HashMap<&'a str, Arc<GroundedMemb>>,
    funcs: HashMap<&'a str, Vec<Arc<GroundedFunc>>>,
}

impl<'a> VarAssignment<'a> {
    #[inline]
    pub fn get_class(&self, name: &str) -> Option<&Arc<GroundedMemb>> {
        self.classes.get(name)
    }

    #[inline]
    pub fn get_relationship(&self, func: &GroundedFunc) -> Option<&Arc<GroundedFunc>> {
        if let Some(funcs) = self.funcs.get(func.get_name()) {
            for owned_f in funcs {
                if owned_f.comparable(func) {
                    return Some(owned_f);
                }
            }
        }
        None
    }
}

impl<'a> std::cmp::PartialEq for VarAssignment<'a> {
    fn eq(&self, other: &VarAssignment) -> bool {
        *self.name == *other.name
    }
}

impl<'a> std::cmp::Eq for VarAssignment<'a> {}

impl<'a> std::hash::Hash for VarAssignment<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (*self.name).hash(state);
    }
}
