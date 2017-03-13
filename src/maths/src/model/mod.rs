//! Infrastructure to instantiate an statistical model with a given set of parameters.

mod bayesnet;

use std::marker::PhantomData;
use std::hash::Hash;

use ndarray::Array2;

use P;
use sampling::{DiscreteSampler, DefaultSampler};
use self::bayesnet::{BayesNet, NetIter};

pub use self::bayesnet::Node;

pub struct DiscreteModel<'a, D: 'a, O: 'a, S>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation,
          S: DiscreteSampler<D, O> + Clone
{
    vars: BayesNet<'a, D, O>,
    sampler: S,
    _obtype: PhantomData<O>,
}

impl<'a, D: 'a, O: 'a, S> DiscreteModel<'a, D, O, S>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation,
          S: DiscreteSampler<D, O> + Clone
{
    pub fn new(net: BayesNet<D, O>) -> DiscreteModel<D, O, DefaultSampler<D, O>> {
        DiscreteModel {
            _obtype: PhantomData,
            vars: net,
            sampler: DefaultSampler::new(),
        }
    }

    pub fn with_sampler(&mut self, sampler: S) {
        self.sampler = sampler;
    }

    pub fn add_variable(&mut self, var: D) {
        unimplemented!()
    }

    pub fn sample(&self) -> Vec<Vec<u8>> {
        let mut sampler = self.sampler.clone();
        sampler.get_samples(self)
    }

    pub fn iter_vars<'b>(&'b self) -> NetIter<'a, 'b, D, O> {
        self.vars.iter_vars()
    }

    pub fn var_numb(&self) -> usize {
        self.vars.nodes.len()
    }

    pub fn add_node(&mut self, dist: &'a D) {
        let pos = self.vars.nodes.len();
        let node = Node::new(dist, pos);
        self.vars.nodes.push(node);
    }

    pub fn add_parents_to_node(&mut self, node: &D, parents: (P, &[D])) {
        // find node and parents in the net
        let node = self.vars.nodes.last_mut().unwrap();
        // build the cpt for the node after adding new ones
        // combinations of the parent nodes random variables values
        let mut row_n = 1;
        for d in node.get_parents_dists() {
            row_n *= d.get_k_num();
        }
        // number of categories for the node
        let colum_n = node.dist.get_k_num();
        node.cpt = Array2::zeros((colum_n, row_n));
        unimplemented!()
    }
}

// public traits for models:

pub trait Observation {
    fn is_kind(&self) -> VariableKind;
}

pub trait DiscreteDist<O: Observation>: Distribution {
    /// Returns an slice of known observations for the variable of
    /// this distribution.
    fn get_observations(&self) -> &[O];

    /// Returns the exact form of the distribution, where the distribution
    /// type should be a discrete type.
    fn dist_type(&self) -> DType;

    /// Returns the number of categories for this discrete event
    fn get_k_num(&self) -> usize;
}

pub trait Distribution {
    type Observation: Observation;
}

// default implementations:

pub type Continuous = f32;
pub type Discrete = isize;
pub type Boolean = bool;

pub enum VariableKind {
    Continuous,
    Discrete,
    Boolean,
}

/// Unknown discrete distributions initially default to a categorical distribution,
/// in case k = 2 (k = number of categories) it will be treated as a Bernoulli distribution.
///
/// Continuous distributions default to a nonparametric distribution.
#[derive(Debug, Clone, Copy)]
pub enum DistributionKind {
    Continuous,
    Discrete,
}

use dists::Normal;

#[derive(Debug, Clone)]
pub enum DType {
    // continuous types:
    Normal(Normal),
    Beta,
    Exponential,
    Gamma,
    ChiSquared,
    StudentT,
    F,
    LogNormal,
    Logistic,
    LogLogistic,
    NonParametric,
    // discrete types:
    Bernoulli(usize),
    Categorical(usize),
    Binomial(usize),
    Poisson(usize),
    // multivariate, used as conjugate prior in inference:
    Dirichlet,
}

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

pub struct DefaultDiscrete<O: Observation> {
    kind: DistributionKind,
    dist: DType,
    observations: Vec<O>,
}

impl<O> DefaultDiscrete<O>
    where O: Observation
{
    pub fn new(kind: VariableKind) -> Result<DefaultDiscrete<O>, ()> {
        let (kind, dtype) = match kind {
            VariableKind::Discrete => (DistributionKind::Discrete, DType::Categorical(0)),
            VariableKind::Boolean => (DistributionKind::Discrete, DType::Bernoulli(0)),
            _ => return Err(()),
        };

        Ok(DefaultDiscrete {
            kind: kind,
            dist: dtype,
            observations: Vec::new(),
        })
    }

    pub fn as_dist(&mut self, dist: DType) -> Result<(), ()> {
        match *&dist {
            DType::Bernoulli(_) |
            DType::Binomial(_) |
            DType::Categorical(_) |
            DType::Poisson(_) => {}
            _ => return Err(()),
        }
        self.dist = dist;
        Ok(())
    }
}

impl<O> Distribution for DefaultDiscrete<O>
    where O: Observation
{
    type Observation = O;
}

impl<O> DiscreteDist<O> for DefaultDiscrete<O>
    where O: Observation
{
    fn get_observations(&self) -> &[O] {
        &self.observations
    }

    fn dist_type(&self) -> DType {
        unimplemented!()
    }

    fn get_k_num(&self) -> usize {
        unimplemented!()
    }
}
