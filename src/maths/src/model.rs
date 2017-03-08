//! Infrastructure to instantiate an statistical model with a given set of parameters.

use super::*;
use super::mcmc::DefaultSampler;

pub struct Distribution<O: Observation> {
    kind: DistributionKind,
    parameters: Vec<Parameter>,
    observations: Vec<O>,
}

impl<O> Distribution<O>
    where O: Observation
{
    fn new(kind: VariableKind) -> Distribution<O> {
        let kind = match kind {
            VariableKind::Continuous => DistributionKind::Continuous,
            VariableKind::Discrete => DistributionKind::Discrete,
            VariableKind::Boolean => DistributionKind::Binomial,
        };

        Distribution {
            kind: kind,
            parameters: vec![],
            observations: Vec::new(),
        }
    }

    fn with_params(&mut self, params: Vec<Parameter>) -> Result<(), ()> {
        for p in &params {
            p.is_compatible(&self.kind)?;
        }
        self.parameters = params;
        Ok(())
    }
}

enum DistributionKind {
    Continuous,
    Discrete,
    Binomial,
}

enum Parameter {
    Mean,
    Median,
    Mode,
    Variance,
    Skewness,
    Kurtosis,
}

impl Parameter {
    fn is_compatible(&self, kind: &DistributionKind) -> Result<(), ()> {
        match *kind {
            DistributionKind::Binomial => {}
            DistributionKind::Continuous => {}
            DistributionKind::Discrete => {}
        }
        Ok(())
    }
}

pub struct Model<S>
    where S: Sampler
{
    vars: Vec<Distribution<S::O>>,
    n: usize,
    sampler: S,
}

impl<S> Model<S>
    where S: Sampler
{
    fn new() -> Model<DefaultSampler> {
        Model::with_sampler(DefaultSampler::new())
    }

    fn with_sampler(sampler: S) -> Model<S> {
        Model {
            vars: Vec::new(),
            n: 0,
            sampler: sampler,
        }
    }

    fn derive_sampler(&self) -> S {
        unimplemented!()
    }

    fn add_variable(&mut self, var: Distribution<S::O>) {
        self.vars.push(var);
        self.n += 1;
    }
}
