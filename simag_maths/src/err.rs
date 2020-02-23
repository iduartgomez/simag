//! Error messages for panic calls
use std::fmt::Debug;

pub enum ErrMsg {
    ContDistDiscNode,
    DiscDistContNode,
    DistNotInvertible,
    RGSLRngInstance,
    Closed01,
    PositiveReal,
}

impl ErrMsg {
    pub fn panic_msg_with_arg<T: Debug>(&self, arg: T) -> String {
        match *self {
            ErrMsg::ContDistDiscNode => format!(
                "simag: dist of type `{:?}` cannot be owned by a discrete node",
                arg
            ),
            ErrMsg::DiscDistContNode => format!(
                "simag: dist of type `{:?}` cannot be owned by a continuous node",
                arg
            ),
            ErrMsg::DistNotInvertible => format!(
                "simag: the distribution `{:?}` does not have an invertible cumulative \
                        function",
                arg
            ),
            ErrMsg::Closed01 => format!(
                "simag: expected a real number in the interval [0,1] when computing \
                         `{:?}`",
                arg
            ),
            ErrMsg::PositiveReal => format!(
                "simag: expected positive real number when computing `{:?}`",
                arg
            ),
            _ => "".to_string(),
        }
    }

    pub fn panic_msg(&self) -> String {
        match *self {
            ErrMsg::RGSLRngInstance => "simag: failed to instantiate the RNG from GSL".to_string(),
            _ => "".to_string(),
        }
    }
}
