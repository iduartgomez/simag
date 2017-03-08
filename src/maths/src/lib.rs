//! Support mathematical methods library for the simAG framework

mod model;
mod mcmc;

use model::Distribution;

pub enum VariableKind {
    Continuous,
    Discrete,
    Boolean,
}

pub type Continuous = f64;
pub type Discrete = isize;
pub type Boolean = bool;

pub enum EventObs {
    Continuous(Continuous),
    Discrete(Discrete),
    Boolean(Boolean),
}

impl Observation for EventObs {
    fn is_kind(&self) -> VariableKind {
        match *self {
            EventObs::Continuous(_) => VariableKind::Continuous,
            EventObs::Discrete(_) => VariableKind::Discrete,
            EventObs::Boolean(_) => VariableKind::Boolean,
        }
    }
}

pub trait Observation {
    fn is_kind(&self) -> VariableKind;
}

pub trait Sampler {
    type O: Observation;

    fn get_sample(input: &Distribution<Self::O>) -> Self::O;
}
