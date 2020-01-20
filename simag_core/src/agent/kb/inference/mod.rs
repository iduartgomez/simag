mod iexpr;
mod results;
mod rules;

pub(in crate::agent) use self::iexpr::QueryInput;
pub(in crate::agent::kb) use self::iexpr::{
    meet_sent_requirements, ArgsProduct, IExprResult, Inference,
};
pub(in crate::agent::kb) use self::results::{GroundedResult, InfResults};
pub(in crate::agent::kb) use self::rules::{rules_inference_lookahead, rules_inference_rollback};
