use super::{
    errors::ParseErrF,
    logsent::{ParseContext, SentID},
    parser::{Number, Operator, UVal},
    FreeMembershipToClass, GrTerminalKind, GroundedRef, Time,
};
use crate::agent::{
    kb::bms::{BmsWrapper, HasBms, RecordHistory},
    kb::repr::Representation,
};
use crate::FLOAT_EQ_ULPS;
use float_cmp::ApproxEqUlps;
use parking_lot::RwLock;
use std::{pin::Pin, str};

#[cfg(feature = "persistence")]
use crate::{agent::storage::*, storage};
#[cfg(feature = "persistence")]
use serde::{Deserialize, Serialize};

/// Grounded membership of an entity to a class.
///
/// Not meant to be instantiated directly, but asserted from logic
/// sentences or processed from `ClassDecl` on `tell` mode.
///
/// E.g.: abc\[$def=1\]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
pub(in crate::agent) struct GroundedMemb {
    pub(in crate::agent::lang) term: GrTerminalKind<String>,
    #[cfg_attr(
        feature = "persistence",
        serde(
            serialize_with = "storage::ser_locked",
            deserialize_with = "storage::deser_locked"
        )
    )]
    pub(in crate::agent::lang) value: RwLock<Option<f32>>,
    pub(in crate::agent::lang) operator: Option<Operator>,
    pub(in crate::agent::lang) parent: String,
    #[cfg_attr(
        feature = "persistence",
        serde(
            serialize_with = "ser_optional_bms",
            deserialize_with = "deser_optional_bms"
        )
    )]
    pub bms: Option<Pin<Box<BmsWrapper<RecordHistory>>>>,
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
        is_func: Option<usize>,
    ) -> Result<GroundedMemb, ParseErrF> {
        let mut val = None;
        let mut op = None;
        let mut bms = None;

        let make_bms = |val: f32| -> BmsWrapper<RecordHistory> {
            let t_bms = BmsWrapper::<RecordHistory>::new();
            if let Some(times) = times {
                for (time, val) in times {
                    t_bms.add_new_record(Some(time), None, val, None);
                }
            } else {
                t_bms.add_new_record(None, None, Some(val as f32), None);
            }
            t_bms
        };

        if let Some(uval) = uval {
            let UVal { val: val0, op: op0 } = uval;
            val = Some(match val0 {
                Number::UnsignedInteger(val) => {
                    if val == 0 || val == 1 {
                        bms = Some(Box::pin(make_bms(val as f32)));
                        val as f32
                    } else {
                        return Err(ParseErrF::IUVal(val as f32));
                    }
                }
                Number::UnsignedFloat(val) => {
                    if val >= 0. && val <= 1. {
                        bms = Some(Box::pin(make_bms(val)));
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
                    Operator::Equal => Some(Operator::Equal),
                    _ => return Err(ParseErrF::IUValComp),
                };
            } else {
                op = Some(op0);
            }
        } else if is_func.filter(|x| *x == 0).is_some() || is_func.is_none() {
            op = Some(Operator::Equal);
            val = Some(1.0);
            bms = Some(Box::pin(make_bms(1.0)));
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
        let mut id = Vec::from(b"gr_memb<".as_ref());
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
        id.push(b'>');
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
                let data_bms = BmsWrapper::<RecordHistory>::new();
                data_bms.add_new_record(None, None, new_val, None);
                bms.update(&GroundedRef::Class(self), agent, &data_bms, was_produced)
            }
        }
    }

    pub fn update_value(&self, val: Option<f32>) {
        *self.value.write() = val;
    }

    pub fn from_free(free: &FreeMembershipToClass, assignment: &str) -> GroundedMemb {
        let bms;
        let val = if free.value.is_some() {
            let t_bms = BmsWrapper::<RecordHistory>::new();
            t_bms.add_new_record(None, None, free.value, None);
            bms = Some(Box::pin(t_bms));
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

    /// An statement is a time interval if there are only two time records and the
    /// last one is none.
    pub fn is_time_interval(&self) -> bool {
        if let Some(bms) = &self.bms {
            bms.records_log_size() == 2 && bms.get_last_value().0.is_none()
        } else {
            false
        }
    }

    #[allow(clippy::collapsible_if)]
    pub(in crate::agent::lang) fn compare_two_grounded_eq(
        val_lhs: Option<f32>,
        val_rhs: Option<f32>,
        op_lhs: Operator,
        op_rhs: Operator,
    ) -> bool {
        if (val_lhs.is_none() && val_rhs.is_some()) || (val_lhs.is_some() && val_rhs.is_none()) {
            return false;
        }
        match op_lhs {
            Operator::Equal => {
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
            Operator::More => val_lhs < val_rhs,
            Operator::Less => val_lhs > val_rhs,
            Operator::MoreEqual => val_lhs <= val_rhs,
            Operator::LessEqual => val_lhs >= val_rhs,
            _ => unreachable!(),
        }
    }

    /// Compare if a grounded membership is true at the times stated by the pred
    #[allow(unused_variables)]
    pub fn compare(&self, pred: &GroundedMemb) -> Option<bool> {
        if pred.bms.is_none() {
            return Some(self.compare_ignoring_times(pred));
        }
        let self_bms = self.bms.as_ref().unwrap();
        let pred_bms = pred.bms.as_ref().unwrap();
        // block both BMS for the duration of the comparison
        let self_lock = &*self_bms.acquire_read_lock();
        let pred_lock = &*pred_bms.acquire_read_lock();

        if let Some(time) = pred_bms.is_predicate() {
            let op_rhs = self.operator.unwrap();
            let op_lhs = pred.operator.unwrap();
            let time_pred = pred_bms.get_last_time();
            let (val_lhs, loc_lhs) = pred_bms.get_last_value();
            let (val_rhs, loc_rhs) = if time_pred < *time {
                self_bms.get_record_at_time(time_pred)
            } else {
                self_bms.get_last_value()
            };
            val_rhs?;
            if loc_lhs != loc_rhs {
                return Some(false);
            }
            Some(Self::compare_two_grounded_eq(
                val_lhs, val_rhs, op_lhs, op_rhs,
            ))
        } else {
            Some(self.compare_ignoring_times(pred))
        }
    }

    pub(in crate::agent) fn compare_ignoring_times(&self, other: &GroundedMemb) -> bool {
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

    // TODO: change for an Arbitrary impl
    #[cfg(test)]
    pub fn gen_mock() -> Self {
        GroundedMemb {
            term: GrTerminalKind::Class("".to_owned()),
            value: RwLock::new(None),
            operator: None,
            parent: String::default(),
            bms: None,
        }
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
            " ? ".to_owned()
        };
        let (value, time) = if let Some(ref bms) = self.bms {
            (
                bms.get_last_value().0,
                format!(" @ `{}` ", bms.get_last_time()),
            )
        } else {
            (self.get_value(), "".to_string())
        };
        write!(
            f,
            "GrMemb {{ {}[{}{}{:?}{}] }}",
            self.parent, self.term, comp_op, value, time
        )
    }
}

impl HasBms for GroundedMemb {
    type BmsType = BmsWrapper<RecordHistory>;

    fn get_bms(&self) -> Option<&Self::BmsType> {
        self.bms.as_ref().map(|i| &**i)
    }

    fn get_value(&self) -> Option<f32> {
        self.get_value()
    }
}

#[cfg(feature = "persistence")]
impl storage::ToBinaryObject for GroundedMemb {
    fn get_type() -> storage::BinType {
        storage::BinType::GrMemb
    }
}
