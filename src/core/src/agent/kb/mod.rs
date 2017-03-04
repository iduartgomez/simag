//! Main knowledge-base logic module, in this module reside the different
//! types that transform and store the data for the individual agents and
//! serve as representations of the different objects and the relationships
//! between them.

mod iexpr_inf;
mod rule_inf;
mod repr;

pub use self::iexpr_inf::*;
pub use self::repr::*;
