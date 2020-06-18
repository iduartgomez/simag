use std::collections::HashMap;
use std::sync::{atomic::AtomicBool, Arc};

use super::TimeArg::*;
use crate::agent::kb::{bms::BmsWrapper, repr::Representation, VarAssignment};
use crate::agent::lang::{common::OpArg, OpArgsOps, Var};

pub(in crate::agent) trait TimeOps: OpArgsOps {
    /// If the class declaration has an overwrite flag then if there are any previous records
    /// those will be dropped
    fn get_own_time_data(
        &self,
        assignments: &HashMap<&Var, Arc<BmsWrapper>>,
        value: Option<f32>,
    ) -> BmsWrapper {
        if self.get_op_args().is_none() {
            let t_bms = BmsWrapper::new(false);
            t_bms.new_record(None, value, None);
            return t_bms;
        }
        let mut v = None;
        let mut ow = false;
        for arg in self.get_op_args().unwrap() {
            match arg {
                OpArg::Time(arg) if arg.contains_payload() => {
                    v = Some(arg.get_time_payload(assignments, value));
                }
                OpArg::OverWrite => {
                    ow = true;
                }
                _ => {}
            }
        }

        if let Some(mut bms) = v {
            bms.overwrite = AtomicBool::new(ow);
            bms
        } else {
            let bms = BmsWrapper::new(ow);
            bms.new_record(None, value, None);
            bms
        }
    }

    fn get_times(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> Option<Arc<BmsWrapper>>;

    fn get_time_decl(&self, var0: &Var) -> bool {
        if let Some(args) = self.get_op_args() {
            for arg in args {
                if let OpArg::Time(TimeVarSince(ref var1)) = *arg {
                    return var1.as_ref() == var0;
                }
            }
            false
        } else {
            false
        }
    }

    fn get_time_payload(&self, _value: Option<f32>) -> Option<BmsWrapper> {
        unimplemented!()
    }
}
