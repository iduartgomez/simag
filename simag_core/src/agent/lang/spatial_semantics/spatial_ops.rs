use std::{collections::HashMap, sync::Arc};

use super::SpatialFnErr;
use crate::agent::{
    kb::bms::{BmsWrapper, IsSpatialData},
    lang::spatial_semantics::SpatialArg::*,
    lang::{common::OpArg, OpArgsOps, Var},
    ParseErrF,
};

pub(in crate::agent) trait SpatialOps: OpArgsOps {
    fn get_own_spatial_data(
        &self,
        _assignments: &HashMap<&Var, Arc<BmsWrapper<IsSpatialData>>>,
    ) -> Result<BmsWrapper<IsSpatialData>, ParseErrF> {
        let op_args = if let Some(args) = self.get_op_args() {
            args
        } else {
            let t_bms = BmsWrapper::new(false);
            t_bms.new_record(None, None, None, None);
            return Ok(t_bms);
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

        let bms = BmsWrapper::new(ow);
        bms.new_record(None, v, None, None);
        Ok(bms)
    }
}
