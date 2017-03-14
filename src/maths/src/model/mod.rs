//! Infrastructure to instantiate an statistical model with a given set of parameters.
mod discrete;

use std::collections::{VecDeque, HashSet};
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::hash::Hash;
use std::rc::Rc;

use P;

use dists::{Categorical, Binomial};
pub use self::discrete::{DiscreteNode, DiscreteModel};

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
    fn get_k_num(&self) -> u8;
}

pub trait Distribution {
    type Observation: Observation;
}

pub trait Node<D, N>
    where D: Distribution + Hash + Eq,
          N: Node<D, N>
{
    fn get_dist(&self) -> &D;
    fn get_child(&self, pos: usize) -> &N;
    fn get_childs(&self) -> Vec<&N>;
}

// default implementations:

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
            VariableKind::Discrete => (DistributionKind::Discrete, DType::UnknownDisc),
            VariableKind::Boolean => (DistributionKind::Discrete, DType::UnknownDisc),
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
            DType::Binomial(_) |
            DType::Categorical(_) |
            DType::Poisson |
            DType::UnknownDisc => {}
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

    fn get_k_num(&self) -> u8 {
        unimplemented!()
    }
}

// support types:

pub type Continuous = f32;
pub type Discrete = isize;
pub type Boolean = bool;

pub enum VariableKind {
    Continuous,
    Discrete,
    Boolean,
}

/// Unknown discrete distributions initially default to a categorical distribution,
/// in case k = 2 (k = number of categories) it will be treated as a binomial distribution.
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
    NonParametricCont,
    // discrete types:
    Categorical(Categorical),
    Binomial(Binomial),
    Poisson,
    UnknownDisc,
    // multivariate, used as conjugate prior in inference:
    Dirichlet,
}

struct BayesNet<'a, D: 'a, O: 'a, N>
    where D: Distribution + Hash + Eq,
          O: Observation,
          N: Node<D, N>
{
    _disttype: PhantomData<&'a D>,
    _obtype: PhantomData<O>,
    nodes: Vec<Rc<N>>,
}

impl<'a, D: 'a, O: 'a, N> BayesNet<'a, D, O, N>
    where D: Distribution + Hash + Eq,
          O: Observation,
          N: Node<D, N>
{
    fn new() -> BayesNet<'a, D, O, N> {
        BayesNet {
            _disttype: PhantomData,
            _obtype: PhantomData,
            nodes: vec![],
        }
    }

    fn iter_vars<'b>(&'b self) -> NetIter<'a, 'b, D, N> {
        NetIter::new(&self.nodes)
    }
}

/// Bayesian Network iterator, visits all nodes from parents to childs
pub struct NetIter<'a: 'b, 'b, D: 'a, N: 'b>
    where D: Distribution + Hash + Eq,
          N: Node<D, N>
{
    unvisitted: VecDeque<&'b N>,
    processed: HashSet<&'a D>,
    queued: &'b N,
    childs_visitted: usize,
}

impl<'a, 'b, D, N> NetIter<'a, 'b, D, N>
    where D: Distribution + Hash + Eq,
          N: Node<D, N>
{
    fn new(nodes: &'b [Rc<N>]) -> NetIter<'a, 'b, D, N> {
        let mut processed = HashSet::new();
        let mut unvisitted = VecDeque::from_iter(nodes.iter().map(|x| &**x));
        let first = unvisitted.pop_front().unwrap();
        let dist = unsafe { &*(first.get_dist() as *const D) as &'a D };
        processed.insert(dist);
        NetIter {
            unvisitted: unvisitted,
            processed: processed,
            queued: first,
            childs_visitted: 0,
        }
    }
}

impl<'a, 'b, D, N> ::std::iter::Iterator for NetIter<'a, 'b, D, N>
    where D: Distribution + Hash + Eq,
          N: Node<D, N>
{
    type Item = &'b N;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            while self.childs_visitted < self.queued.get_childs().len() {
                let next = self.queued.get_child(self.childs_visitted);
                self.childs_visitted += 1;
                if !self.processed.contains(next.get_dist()) {
                    return Some(next);
                }
            }

            if self.unvisitted.is_empty() {
                return None;
            } else {
                // add all previously visitted to the list of processed
                for e in self.queued.get_childs() {
                    let e = unsafe { &*(e.get_dist() as *const D) as &'a D };
                    self.processed.insert(e);
                }
                let next = self.unvisitted.pop_front().unwrap();
                self.queued = next;
            }
        }
    }
}
