//! Main knowledge-base logic module, in this module reside the different
//! types that transform and store the data for the individual agents and
//! serve as representations of the different objects and the relationships
//! between them.
pub(in crate::agent) mod bms;
pub(self) mod class;
pub(self) mod entity;
mod inference;
#[cfg(feature = "persistence")]
mod persist;
pub(super) mod repr;

#[cfg(test)]
mod test;

use super::lang::{GrTerminalKind, GroundedFunc, GroundedMemb};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone)]
pub(in crate::agent) struct VarAssignment<'rep> {
    pub name: GrTerminalKind<&'rep str>,
    classes: HashMap<&'rep str, Arc<GroundedMemb>>,
    funcs: HashMap<&'rep str, Vec<Arc<GroundedFunc>>>,
}

impl<'rep> VarAssignment<'rep> {
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

impl<'rep> std::cmp::PartialEq for VarAssignment<'rep> {
    fn eq(&self, other: &VarAssignment) -> bool {
        *self.name == *other.name
    }
}

impl<'rep> std::cmp::Eq for VarAssignment<'rep> {}

impl<'rep> std::hash::Hash for VarAssignment<'rep> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (*self.name).hash(state);
    }
}

impl<'rep> std::fmt::Display for VarAssignment<'rep> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'rep> std::fmt::Debug for VarAssignment<'rep> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
