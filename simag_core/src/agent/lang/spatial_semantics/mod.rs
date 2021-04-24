use super::ParseErrF;
pub(super) use errors::SpatialFnErr;
pub(in crate::agent) use location_fn::LocFn;
pub(in crate::agent) use move_fn::MoveFn;
#[cfg(feature = "persistence")]
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
pub(super) use spatial_arg::SpatialArg;
pub(in crate::agent) use spatial_ops::SpatialOps;
use std::convert::TryFrom;

mod location_fn;
mod move_fn;
mod spatial_arg;
mod spatial_ops;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
pub struct Point(i64, i64, i64);

impl Point {
    pub fn new(x: i64, y: i64, z: i64) -> Self {
        Point(x, y, z)
    }

    pub fn generate_uid(&self) -> Vec<u8> {
        let mut repr: Vec<u8> = Vec::with_capacity(std::mem::size_of::<i64>() * 3);
        repr.extend(b"point<");
        repr.extend(std::array::IntoIter::new(self.0.to_le_bytes()));
        repr.extend(std::array::IntoIter::new(self.0.to_le_bytes()));
        repr.extend(std::array::IntoIter::new(self.0.to_le_bytes()));
        repr.push(b'>');
        repr
    }
}

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
        let input_str = std::str::from_utf8(input).expect("should be valid utf8");
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
