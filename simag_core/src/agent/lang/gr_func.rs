use std::collections::HashMap;
use std::str;
use std::sync::Arc;

use super::{
    common::{GroundedRef, Predicate},
    fn_decl::FuncDecl,
    logsent::SentID,
    GroundedMemb, Terminal, TimeOps, Var,
};
use crate::agent::{
    kb::bms::{BmsWrapper, IsTimeData, RecordHistory},
    kb::{repr::Representation, VarAssignment},
    lang::Time,
};

/// Grounded relational functions describe relations between two objects,
/// or two objets and a third indirect object.
///
/// Are not meant to be instantiated directly, but asserted from logic
/// sentences or processed from `FuncDecl` on `tell` mode.
pub(in crate::agent) struct GroundedFunc {
    pub(in crate::agent::lang) name: String,
    pub(in crate::agent::lang) args: [GroundedMemb; 2],
    pub(in crate::agent::lang) third: Option<GroundedMemb>,
    pub bms: Arc<BmsWrapper<RecordHistory>>,
}

impl GroundedFunc {
    #[allow(unused_variables)]
    pub fn compare(&self, pred: &GroundedFunc) -> Option<bool> {
        // block both BMS for the duration of the comparison
        let self_lock = &*self.bms.acquire_read_lock();
        let pred_lock = &*pred.bms.acquire_read_lock();

        if let Some(time) = pred.bms.is_predicate() {
            let time_pred = pred.bms.get_last_date();
            let (val_lhs, loc_lhs) = pred.bms.get_last_value();
            let (val_rhs, loc_rhs) = if time_pred < *time {
                self.bms.get_record_at_time(time_pred)
            } else {
                self.bms.get_last_value()
            };
            let op_rhs = self.args[0].operator.unwrap();
            let op_lhs = pred.args[0].operator.unwrap();
            val_rhs?;
            if loc_lhs != loc_rhs {
                return Some(false);
            }
            match (&self.third, &pred.third) {
                (Some(arg0), Some(arg1)) => {
                    if arg0.compare_ignoring_times(arg1) {
                        return Some(false);
                    }
                }
                (None, None) => {}
                _ => return Some(false),
            };
            if !(self.name == pred.name
                && self.args[0].term == pred.args[0].term
                && self.args[1].term == pred.args[1].term)
            {
                return Some(false);
            }

            Some(GroundedMemb::compare_two_grounded_eq(
                val_lhs, val_rhs, op_lhs, op_rhs,
            ))
        } else {
            Some(self.compare_ignoring_times(pred))
        }
    }

    pub(in crate::agent) fn compare_ignoring_times(&self, other: &GroundedFunc) -> bool {
        if self.name != other.name {
            return false;
        }

        for (a, b) in (&self.args)
            .iter()
            .chain(self.third.as_ref())
            .zip(other.args.iter().chain(other.third.as_ref()))
        {
            match (a, b) {
                (arg0, arg1) if !arg0.compare_ignoring_times(arg1) => {
                    return false;
                }
                _ => {}
            }
        }
        true
    }

    pub fn from_free(
        free: &FuncDecl,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
    ) -> Result<GroundedFunc, ()> {
        if !free.variant.is_relational() || free.args.as_ref().unwrap().len() < 2 {
            return Err(());
        }
        let name = if let Terminal::GroundedTerm(ref name) = free.name {
            name.clone()
        } else {
            return Err(());
        };
        let mut first = None;
        let mut second = None;
        let mut third = None;
        let mut value = None;
        for (i, a) in free.args.as_ref().unwrap().iter().enumerate() {
            let n_a = match *a {
                Predicate::FreeMembershipToClass(ref free) => {
                    let assigned = if let Some(assignments) = assignments {
                        assignments
                    } else {
                        return Err(());
                    };
                    if let Some(entity) = assigned.get(&*free.term) {
                        GroundedMemb::from_free(free, &*entity.name)
                    } else {
                        return Err(());
                    }
                }
                Predicate::GroundedMemb(ref term) => term.clone(),
                _ => return Err(()),
            };
            if i == 0 {
                value = n_a.get_value();
                first = Some(n_a)
            } else if i == 1 {
                second = Some(n_a)
            } else {
                third = Some(n_a)
            }
        }
        let time_data = free.get_own_time_data(time_assign, value);
        Ok(GroundedFunc {
            name,
            args: [first.unwrap(), second.unwrap()],
            third,
            bms: Arc::new(time_data.into()),
        })
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn get_value(&self) -> Option<f32> {
        *self.args[0].value.read()
    }

    pub fn get_args(&self) -> Vec<&GroundedMemb> {
        let mut v = Vec::with_capacity(3);
        v.push(&self.args[0]);
        v.push(&self.args[1]);
        if let Some(ref arg) = self.third {
            v.push(arg);
        }
        v
    }

    pub fn get_args_names(&self) -> Vec<&str> {
        let mut v = Vec::with_capacity(3);
        v.push(self.args[0].get_name().into());
        v.push(self.args[1].get_name().into());
        if let Some(ref arg) = self.third {
            v.push(arg.get_name().into());
        }
        v
    }

    pub fn get_arg_name(&self, pos: usize) -> &str {
        match pos {
            0 => self.args[0].get_name().into(),
            1 => self.args[1].get_name().into(),
            _ => {
                if let Some(ref arg) = self.third {
                    arg.get_name().into()
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub fn name_in_pos(&self, name: &str, pos: usize) -> bool {
        if (pos < 2) && (Into::<&str>::into(self.args[pos].get_name()) == name) {
            return true;
        }
        if let Some(ref o_name) = self.third {
            if Into::<&str>::into(o_name.get_name()) == name {
                return true;
            }
        }
        false
    }

    pub fn comparable(&self, other: &GroundedFunc) -> bool {
        if other.name != self.name {
            return false;
        }
        if !self.args[0].comparable(&other.args[0]) {
            return false;
        }
        if !self.args[1].comparable(&other.args[1]) {
            return false;
        }
        if self.third.is_some() && other.third.is_some() {
            let st = self.third.as_ref().unwrap();
            let ot = other.third.as_ref().unwrap();
            st.comparable(ot)
        } else {
            self.third.is_none() && other.third.is_none()
        }
    }

    pub fn update(
        &self,
        agent: &Representation,
        data: &GroundedFunc,
        was_produced: Option<(SentID, Time)>,
    ) {
        {
            let mut value_lock = self.args[0].value.write();
            *value_lock = *data.args[0].value.read();
        }
        let data_bms = &data.bms;
        self.bms
            .update(&GroundedRef::Function(self), agent, data_bms, was_produced);
    }

    /// Ensure this only gets called from BMS update method.
    pub fn update_value(&self, val: Option<f32>) {
        let mut value_lock = self.args[0].value.write();
        *value_lock = val;
    }
}

impl std::clone::Clone for GroundedFunc {
    fn clone(&self) -> GroundedFunc {
        GroundedFunc {
            name: self.name.clone(),
            args: [self.args[0].clone(), self.args[1].clone()],
            third: self.third.clone(),
            bms: Arc::new((&*self.bms).clone()),
        }
    }
}

impl std::fmt::Debug for GroundedFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for GroundedFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let comp_op = if let Some(op) = self.args[0].operator {
            format!("{}", op)
        } else {
            "".to_owned()
        };
        let third = if let Some(third) = &self.third {
            format!(",{}", third.get_name())
        } else {
            String::from("")
        };
        write!(
            f,
            "GrFunc {{ {}[{}{}{:?},{}{}] @ `{}` }}",
            self.name,
            self.args[0].get_name(),
            comp_op,
            self.bms.get_last_value(),
            self.args[1].get_name(),
            third,
            self.bms.get_last_date()
        )
    }
}
