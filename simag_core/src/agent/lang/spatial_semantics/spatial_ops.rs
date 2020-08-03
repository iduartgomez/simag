use std::{collections::HashMap, sync::Arc};

use super::SpatialFnErr;
use crate::agent::{
    kb::{
        bms::{BmsWrapper, IsSpatialData},
        VarAssignment,
    },
    lang::spatial_semantics::SpatialArg::*,
    lang::{common::OpArg, OpArgsOps, Var},
    ParseErrF, Representation,
};

pub(in crate::agent) trait SpatialOps: OpArgsOps {
    fn get_own_spatial_data(
        &self,
        _assignments: &HashMap<&Var, Arc<BmsWrapper<IsSpatialData>>>,
    ) -> Result<BmsWrapper<IsSpatialData>, ParseErrF> {
        let op_args = if let Some(args) = self.get_op_args() {
            args
        } else {
            return Ok(BmsWrapper::<IsSpatialData>::new(None));
        };

        let mut v = None;
        let mut ow = false;
        for arg in op_args {
            match arg {
                OpArg::Spatial(DeclLocation(val)) => {
                    if v.is_some() {
                        return Err(ParseErrF::SpatialFnErr(SpatialFnErr::MoreThanOneSpatialArg));
                    }
                    v = Some(val.clone());
                }
                OpArg::OverWrite => ow = true,
                _ => {}
            }
        }

        Ok(BmsWrapper::<IsSpatialData>::new(v).with_ow_val(ow))
    }

    fn get_location(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> Option<Arc<BmsWrapper<IsSpatialData>>>;

    fn get_loc_decl(&self, var0: &Var) -> bool {
        if let Some(args) = self.get_op_args() {
            for arg in args {
                if let OpArg::Spatial(AssignThisToVar(ref var1)) = *arg {
                    return var1.as_ref() == var0;
                }
            }
            false
        } else {
            false
        }
    }

    fn get_spatial_payload(&self) -> Option<BmsWrapper<IsSpatialData>> {
        unimplemented!()
    }
}
