#[cfg(feature = "persistence")]
use serde::{Deserialize, Serialize};

#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(in crate::agent) enum TypeDef {
    Erased,
    //Class,
    //Entity,
    Time,
    TimeDecl,
    Location,
    LocDecl,
}

impl std::fmt::Display for TypeDef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeDef::Erased => write!(f, "Erased"),
            TypeDef::Time => write!(f, "Time"),
            TypeDef::TimeDecl => write!(f, "TimeDecl"),
            TypeDef::Location => write!(f, "Location"),
            TypeDef::LocDecl => write!(f, "SpatialDecl"),
        }
    }
}
