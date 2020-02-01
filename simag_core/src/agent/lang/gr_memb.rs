use super::{FreeClsMemb, GroundedRef};
use float_cmp::ApproxEqUlps;
use parking_lot::RwLock;
use std::str;
use std::sync::Arc;

use super::{
    errors::ParseErrF,
    logsent::{ParseContext, SentID},
    parser::{CompOperator, Number, UVal},
    GrTerminalKind, Time,
};
use crate::agent::{kb::bms::BmsWrapper, kb::repr::Representation};
use crate::FLOAT_EQ_ULPS;

/// Grounded membership of an entity to a class.
///
/// Not meant to be instantiated directly, but asserted from logic
/// sentences or processed from `ClassDecl` on `tell` mode.
pub(in crate::agent) struct GroundedMemb {
    pub(in crate::agent::lang) term: GrTerminalKind<String>,
    pub(in crate::agent::lang) value: RwLock<Option<f32>>,
    pub(in crate::agent::lang) operator: Option<CompOperator>,
    pub(in crate::agent::lang) parent: String,
    pub bms: Option<Arc<BmsWrapper>>,
}

impl GroundedMemb {
    //! Internally the mutable parts are wrapped in `RwLock` types, as they can be accessed
    //! from a multithreaded environment. This provides enough atomicity so most of
    //! the time it won't be blocking other reads.

    pub fn try_new(
        term: String,
        uval: Option<UVal>,
        parent: String,
        times: Option<Vec<(Time, Option<f32>)>>,
        context: &ParseContext,
    ) -> Result<GroundedMemb, ParseErrF> {
        let val;
        let op;
        let bms;
        if let Some(uval) = uval {
            let UVal { val: val0, op: op0 } = uval;
            val = Some(match val0 {
                Number::UnsignedInteger(val) => {
                    if val == 0 || val == 1 {
                        let t_bms = BmsWrapper::new(false);
                        if let Some(times) = times {
                            for (time, val) in times {
                                t_bms.new_record(Some(time), val, None);
                            }
                        } else {
                            t_bms.new_record(None, Some(val as f32), None);
                        }
                        bms = Some(Arc::new(t_bms));
                        val as f32
                    } else {
                        return Err(ParseErrF::IUVal(val as f32));
                    }
                }
                Number::UnsignedFloat(val) => {
                    if val >= 0. && val <= 1. {
                        let t_bms = BmsWrapper::new(false);
                        if let Some(times) = times {
                            for (time, val) in times {
                                t_bms.new_record(Some(time), val, None);
                            }
                        } else {
                            t_bms.new_record(None, Some(val as f32), None);
                        }
                        bms = Some(Arc::new(t_bms));
                        val
                    } else {
                        return Err(ParseErrF::IUVal(val as f32));
                    }
                }
                Number::SignedFloat(val) => return Err(ParseErrF::IUVal(val as f32)),
                Number::SignedInteger(val) => return Err(ParseErrF::IUVal(val as f32)),
            });
            if context.in_assertion && context.is_tell {
                op = match op0 {
                    CompOperator::Equal => Some(CompOperator::Equal),
                    _ => return Err(ParseErrF::IUValComp),
                };
            } else {
                op = Some(op0);
            }
        } else {
            val = None;
            op = None;
            bms = None;
        }
        Ok(GroundedMemb {
            term: term.into(),
            value: RwLock::new(val),
            operator: op,
            parent,
            bms,
        })
    }

    pub(in crate::agent::lang) fn generate_uid(&self) -> Vec<u8> {
        let mut id: Vec<u8> = vec![];
        let term_str: &str = (&self.term).into();
        id.append(&mut Vec::from(term_str.as_bytes()));
        if let Some(ref cmp) = self.operator {
            cmp.generate_uid(&mut id);
        }
        if let Some(val) = *self.value.read() {
            let mut id_2 = format!("{}", val).into_bytes();
            id.append(&mut id_2);
        }
        id.append(&mut Vec::from(self.parent.as_bytes()));
        id
    }

    #[inline]
    pub fn get_name(&self) -> &GrTerminalKind<String> {
        &self.term
    }

    #[inline]
    pub fn get_parent(&self) -> &str {
        &*self.parent
    }

    #[inline]
    pub fn get_value(&self) -> Option<f32> {
        if let Some(val) = *self.value.read() {
            Some(val)
        } else {
            None
        }
    }

    pub fn update(
        &self,
        agent: &Representation,
        data: &GroundedMemb,
        was_produced: Option<(SentID, Time)>,
    ) {
        let new_val: Option<f32>;
        {
            let mut value_lock = self.value.write();
            new_val = *data.value.read();
            *value_lock = new_val;
        }
        if let Some(ref bms) = self.bms {
            if data.bms.is_some() {
                let data_bms = data.bms.as_ref().unwrap();
                bms.update(&GroundedRef::Class(self), agent, data_bms, was_produced)
            } else {
                let data_bms = BmsWrapper::new(false);
                data_bms.new_record(None, new_val, None);
                bms.update(&GroundedRef::Class(self), agent, &data_bms, was_produced)
            }
        }
    }

    pub fn update_value(&self, val: Option<f32>) {
        *self.value.write() = val;
    }

    pub fn from_free(free: &FreeClsMemb, assignment: &str) -> GroundedMemb {
        let bms;
        let val = if free.value.is_some() {
            let t_bms = BmsWrapper::new(false);
            t_bms.new_record(None, free.value, None);
            bms = Some(Arc::new(t_bms));
            Some(free.value.unwrap())
        } else {
            bms = None;
            None
        };
        let op = if free.value.is_some() {
            Some(free.operator.unwrap())
        } else {
            None
        };
        GroundedMemb {
            term: GrTerminalKind::from(assignment.to_string()),
            value: RwLock::new(val),
            operator: op,
            parent: free.parent.get_name().to_string(),
            bms,
        }
    }

    #[inline]
    pub fn comparable(&self, other: &GroundedMemb) -> bool {
        if &self.term != other.get_name() {
            return false;
        }
        if self.parent != other.parent {
            return false;
        }
        if self.operator.is_some() && other.operator.is_none() {
            return false;
        }
        if other.operator.is_some() && self.operator.is_none() {
            return false;
        }
        true
    }

    pub fn overwrite_time_data(&self, data: &BmsWrapper) {
        self.bms.as_ref().unwrap().overwrite_data(data);
    }

    /// An statement is a time interval if there are only
    pub fn is_time_interval(&self) -> bool {
        let bms = self.bms.as_ref().unwrap();
        bms.record_len() == 2 && bms.get_last_value().is_none()
    }

    #[allow(clippy::collapsible_if)]
    pub(in crate::agent::lang) fn compare_two_grounded_eq(
        val_lhs: Option<f32>,
        val_rhs: Option<f32>,
        op_lhs: CompOperator,
        op_rhs: CompOperator,
    ) -> bool {
        if (val_lhs.is_none() && val_rhs.is_some()) || (val_lhs.is_some() && val_rhs.is_none()) {
            return false;
        }
        match op_lhs {
            CompOperator::Equal => {
                if op_rhs.is_equal() {
                    if let Some(val_lhs) = val_lhs {
                        let val_rhs = val_rhs.as_ref().unwrap();
                        val_lhs.approx_eq_ulps(val_rhs, FLOAT_EQ_ULPS)
                    } else {
                        true
                    }
                } else if op_rhs.is_more() {
                    val_lhs > val_rhs
                } else if op_rhs.is_less() {
                    val_lhs < val_rhs
                } else if op_rhs.is_more_eq() {
                    if let Some(val_lhs) = val_lhs {
                        let val_rhs = val_rhs.unwrap();
                        val_lhs.approx_eq_ulps(&val_rhs, FLOAT_EQ_ULPS) || val_lhs > val_rhs
                    } else {
                        true
                    }
                } else {
                    if let Some(val_lhs) = val_lhs {
                        let val_rhs = val_rhs.unwrap();
                        val_lhs.approx_eq_ulps(&val_rhs, FLOAT_EQ_ULPS) || val_lhs < val_rhs
                    } else {
                        true
                    }
                }
            }
            CompOperator::More => val_lhs < val_rhs,
            CompOperator::Less => val_lhs > val_rhs,
            CompOperator::MoreEqual => val_lhs <= val_rhs,
            CompOperator::LessEqual => val_lhs >= val_rhs,
            CompOperator::Until | CompOperator::At | CompOperator::FromUntil => unreachable!(),
        }
    }

    /// Compare if a grounded membership is true at the times stated by the pred
    #[allow(unused_variables)]
    pub fn compare_at_time_intervals(&self, pred: &GroundedMemb) -> Option<bool> {
        if pred.bms.is_none() {
            return Some(self == pred);
        }
        let self_bms = &**self.bms.as_ref().unwrap();
        let pred_bms = &**pred.bms.as_ref().unwrap();
        // block both BMS for the duration of the comparison
        let self_lock = &*self_bms.acquire_read_lock();
        let pred_lock = &*pred_bms.acquire_read_lock();

        if let Some(time) = pred_bms.is_predicate() {
            let op_rhs = self.operator.unwrap();
            let op_lhs = pred.operator.unwrap();
            let time_pred = pred_bms.get_last_date();
            let val_lhs = pred_bms.get_last_value();
            let val_rhs = if time_pred < *time {
                self_bms.get_record_at_time(time_pred)
            } else {
                self_bms.get_last_value()
            };
            val_rhs?;
            Some(Self::compare_two_grounded_eq(
                val_lhs, val_rhs, op_lhs, op_rhs,
            ))
        } else {
            Some(self == pred)
        }
    }
}

impl std::cmp::PartialEq for GroundedMemb {
    fn eq(&self, other: &GroundedMemb) -> bool {
        if self.term != other.term || self.parent != other.parent {
            return false;
        }
        // Block the values being compared for the duration of the comparison
        let self_value_lock = &*self.value.read();
        let other_value_lock = &*other.value.read();
        let op_rhs;
        let op_lhs;
        if let Some(op) = other.operator {
            op_lhs = self.operator.unwrap();
            op_rhs = op;
        } else {
            return *self_value_lock == *other_value_lock;
        }
        let val_lhs: Option<f32> = *self_value_lock;
        let val_rhs: Option<f32> = *other_value_lock;
        Self::compare_two_grounded_eq(val_lhs, val_rhs, op_lhs, op_rhs)
    }
}

impl std::clone::Clone for GroundedMemb {
    fn clone(&self) -> GroundedMemb {
        GroundedMemb {
            term: self.term.clone(),
            value: RwLock::new(*self.value.read()),
            operator: self.operator,
            parent: self.parent.clone(),
            bms: self.bms.clone(),
        }
    }
}

impl std::fmt::Debug for GroundedMemb {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for GroundedMemb {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let comp_op = if let Some(op) = self.operator {
            format!("{}", op)
        } else {
            "".to_owned()
        };
        let (value, time) = if let Some(ref bms) = self.bms {
            (
                bms.get_last_value(),
                format!(" @ `{}` ", bms.get_last_date()),
            )
        } else {
            (self.get_value(), "".to_string())
        };
        write!(
            f,
            "GrMemb {{ {}[{},u{}{:?}{}] }}",
            self.parent, self.term, comp_op, value, time
        )
    }
}
