mod location_fn;
mod move_fn;
mod spatial_arg;
mod spatial_ops;

use std::convert::TryFrom;

use smallvec::SmallVec;

use super::ParseErrF;
pub(super) use errors::SpatialFnErr;
pub(super) use location_fn::LocFn;
pub(super) use move_fn::MoveFn;
pub(super) use spatial_arg::SpatialArg;
pub(in crate::agent) use spatial_ops::SpatialOps;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) struct Point(i64, i64, i64);

impl TryFrom<&str> for Point {
    type Error = ParseErrF;

    fn try_from(input: &str) -> Result<Self, Self::Error> {
        let p: Result<SmallVec<[i64; 3]>, _> = input
            .split('.')
            .map(|p| p.parse())
            .collect::<Result<_, _>>();
        match p {
            Ok(val) if val.len() == 3 => Ok(Point(val[0], val[1], val[2])),
            _ => Err(ParseErrF::WrongDef),
        }
    }
}

impl TryFrom<&[u8]> for Point {
    type Error = ParseErrF;

    fn try_from(input: &[u8]) -> Result<Self, Self::Error> {
        let input_str = std::str::from_utf8(input).unwrap();
        Self::try_from(input_str)
    }
}

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
