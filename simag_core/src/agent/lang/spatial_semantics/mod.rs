mod move_fn;
mod spatial_arg;
mod spatial_ops;

pub(super) use errors::SpatialFnErr;
pub(super) use move_fn::MoveFn;
pub(in crate::agent) use spatial_arg::Point;
pub(super) use spatial_arg::SpatialArg;
pub(in crate::agent) use spatial_ops::SpatialOps;

mod errors {
    use crate::agent::ParseErrF;

    #[derive(Debug, PartialEq, Eq)]
    pub enum SpatialFnErr {
        MoreThanOneSpatialArg,
        WrongDef,
    }

    impl Into<ParseErrF> for SpatialFnErr {
        fn into(self) -> ParseErrF {
            ParseErrF::SpatialFnErr(self)
        }
    }
}
