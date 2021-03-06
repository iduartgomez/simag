use std::collections::HashMap;
use std::sync::Arc;

use super::TimeArg::*;
use crate::agent::kb::{
    bms::{BmsWrapper, IsTimeData},
    repr::Representation,
    VarAssignment,
};
use crate::agent::lang::{common::OpArg, OpArgsOps, Var};

pub(in crate::agent) trait TimeOps: OpArgsOps {
    /// If the class declaration has an overwrite flag then if there are any previous records
    /// those will be dropped
    fn get_own_time_data(
        &self,
        assignments: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
        value: Option<f32>,
    ) -> BmsWrapper<IsTimeData> {
        let op_args = if let Some(args) = self.get_op_args() {
            args
        } else {
            return BmsWrapper::<IsTimeData>::new(None, value);
        };

        let mut v = None;
        let mut ow = false;
        for arg in op_args {
            match arg {
                OpArg::Time(arg) if arg.contains_payload() => {
                    if v.is_some() {
                        unreachable!("SIMAG - {}:{}: can only set time value once")
                    }
                    v = Some(arg.get_time_payload(assignments, value));
                }
                OpArg::Time(SinceVar(var)) => {
                    if v.is_some() {
                        unreachable!("SIMAG - can only set time value once")
                    }
                    if let Some(val) = assignments.get(&*var) {
                        v = Some((&**val).clone());
                    }
                }
                OpArg::Time(SinceVarUntilVar(_var0, _var1)) => unimplemented!(),
                OpArg::Time(SinceVarUntilTime(_var0, _val1)) => unimplemented!(),
                OpArg::Time(SinceTimeUntilVar(_val0, _var1)) => unimplemented!(),
                OpArg::Time(SinceTimeUntilTime(_val0, _val1)) => unimplemented!(),
                OpArg::OverWrite => {
                    ow = true;
                }
                _ => {}
            }
        }

        if let Some(bms) = v {
            bms.with_ow_val(ow)
        } else {
            let bms = BmsWrapper::<IsTimeData>::new(None, value);
            bms.with_ow_val(ow)
        }
    }

    fn get_times(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> Option<Arc<BmsWrapper<IsTimeData>>>;

    fn get_time_decl(&self, var0: &Var) -> bool {
        if let Some(args) = self.get_op_args() {
            for arg in args {
                if let OpArg::Time(AssignThisToVar(ref var1)) = *arg {
                    return var1 == var0;
                }
            }
            false
        } else {
            false
        }
    }

    fn get_time_payload(&self, _value: Option<f32>) -> Option<BmsWrapper<IsTimeData>> {
        unimplemented!()
    }
}
