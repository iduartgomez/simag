//! Infrastructure to instantiate an statistical model with a given set of parameters.
mod discrete;

use std::collections::{LinkedList, VecDeque, HashSet};
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use uuid::Uuid;

use dists::{Categorical, Binomial};
pub use self::discrete::{DiscreteNode, DiscreteModel, CPT};

// public traits for models:

pub trait Observation {
    fn is_kind(&self) -> VariableKind;
}

pub trait DiscreteVar<O>: Variable
    where O: Observation
{
    /// Returns an slice of known observations for the variable of
    /// this distribution.
    fn get_observations(&self) -> &[O];

    /// Returns the exact form of the distribution, where the distribution
    /// type should be a discrete type.
    fn dist_type(&self) -> &DType;

    /// Returns the number of categories for this discrete event
    fn k_num(&self) -> u8;

    /// Returns a sample from the original variable, not talking into account
    /// the parents in the network.
    fn sample(&self) -> u8;
}

pub trait Variable: Hash + PartialEq + Eq {
    type Observation: Observation;
}

pub trait Node<D, N>
    where D: Variable,
          N: Node<D, N>
{
    fn get_dist(&self) -> &D;
    fn get_child(&self, pos: usize) -> &N;
    fn get_childs(&self) -> Vec<&N>;
    fn is_root(&self) -> bool;
    fn position(&self) -> usize;
    fn update_position(&self, pos: usize);
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
    dist: DType,
    observations: Vec<O>,
    id: Uuid,
}

impl<O> DefaultDiscrete<O>
    where O: Observation
{
    pub fn new(kind: VariableKind) -> Result<DefaultDiscrete<O>, ()> {
        let dtype = match kind {
            VariableKind::Discrete | VariableKind::Boolean => DType::UnknownDisc,
            _ => return Err(()),
        };

        Ok(DefaultDiscrete {
            dist: dtype,
            observations: Vec::new(),
            id: Uuid::new_v4(),
        })
    }

    pub fn with_dist(dist: DType) -> Result<DefaultDiscrete<O>, ()> {
        match dist {
            DType::Binomial(_) |
            DType::Categorical(_) |
            DType::Poisson |
            DType::UnknownDisc => {}
            _ => return Err(()),
        }
        Ok(DefaultDiscrete {
            dist: dist,
            observations: Vec::new(),
            id: Uuid::new_v4(),
        })
    }

    pub fn as_dist(&mut self, dist: DType) -> Result<(), ()> {
        match dist {
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

impl<O> Hash for DefaultDiscrete<O>
    where O: Observation
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<O> PartialEq for DefaultDiscrete<O>
    where O: Observation
{
    fn eq(&self, other: &DefaultDiscrete<O>) -> bool {
        self.id == other.id
    }
}

impl<O> Eq for DefaultDiscrete<O> where O: Observation {}

impl<O> Variable for DefaultDiscrete<O>
    where O: Observation
{
    type Observation = O;
}

impl<O> DiscreteVar<O> for DefaultDiscrete<O>
    where O: Observation
{
    fn get_observations(&self) -> &[O] {
        &self.observations
    }

    fn dist_type(&self) -> &DType {
        &self.dist
    }

    fn k_num(&self) -> u8 {
        match self.dist {
            DType::Categorical(ref dist) => dist.sample(),
            DType::Binomial(_) => 2,
            _ => panic!(),
        }
    }

    fn sample(&self) -> u8 {
        match self.dist {
            DType::Categorical(ref dist) => dist.sample(),
            DType::Binomial(ref dist) => dist.sample(),
            _ => panic!(),
        }
    }
}

// support types:

pub type Continuous = f64;
pub type Discrete = isize;
pub type Boolean = bool;

pub enum VariableKind {
    Continuous,
    Discrete,
    Boolean,
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

struct DAG<'a, D: 'a, O: 'a, N>
    where D: Variable,
          O: Observation,
          N: Node<D, N>
{
    _disttype: PhantomData<&'a D>,
    _obtype: PhantomData<O>,
    nodes: Vec<Rc<N>>,
}

impl<'a, D: 'a, O: 'a, N> DAG<'a, D, O, N>
    where D: Variable,
          O: Observation,
          N: Node<D, N>
{
    fn new() -> DAG<'a, D, O, N> {
        DAG {
            _disttype: PhantomData,
            _obtype: PhantomData,
            nodes: vec![],
        }
    }

    fn iter_vars<'b>(&'b self) -> NetIter<'a, 'b, D, N> {
        NetIter::new(&self.nodes)
    }

    /// Perform both topological sorting and acyclicality check in the same operation.
    /// Returns error if is not a DAG.
    fn topological_sort(&mut self) -> Result<(), ()> {
        let mut cycle_detect = DirectedCycle::new(self);
        cycle_detect.dfs(self, 0);
        if cycle_detect.has_cycle() {
            return Err(());
        }
        let mut priority = Vec::with_capacity(self.nodes.len());
        for (i, c) in cycle_detect.sorted.iter().enumerate() {
            let node = &self.nodes[*c];
            if node.position() != i {
                node.update_position(i);
            }
            priority.push((i, node.clone()));
        }
        priority.sort_by(|&(ref i, _), &(ref j, _)| i.cmp(j));
        let sorted: Vec<Rc<N>> = priority.into_iter()
            .map(|(i, e)| e)
            .collect();
        self.nodes = sorted;
        Ok(())
    }
}

struct DirectedCycle {
    marked: Vec<bool>,
    edge_to: Vec<usize>,
    cycle: Vec<usize>,
    on_stack: Vec<bool>,
    sorted: Vec<usize>,
}

impl DirectedCycle {
    fn new<D, O, N>(graph: &DAG<D, O, N>) -> DirectedCycle
        where D: Variable,
              O: Observation,
              N: Node<D, N>
    {
        DirectedCycle {
            marked: vec![false; graph.nodes.len()],
            on_stack: vec![false; graph.nodes.len()],
            edge_to: Vec::from_iter(0..graph.nodes.len()),
            cycle: Vec::with_capacity(graph.nodes.len()),
            sorted: Vec::with_capacity(graph.nodes.len()),
        }
    }

    fn dfs<D, O, N>(&mut self, graph: &DAG<D, O, N>, v: usize)
        where D: Variable,
              O: Observation,
              N: Node<D, N>
    {
        self.on_stack[v] = true;
        self.marked[v] = true;
        for c in graph.nodes[v].get_childs() {
            let w = c.position();
            if self.has_cycle() {
                return;
            } else if !self.marked[w] {
                self.edge_to[w] = v;
                self.dfs(graph, w);
            } else if self.on_stack[w] {
                let mut x = v;
                while x != w {
                    x = self.edge_to[x];
                    self.cycle.push(x);
                }
                self.cycle.push(w);
                self.cycle.push(v);
            }
            self.on_stack[v] = false;
        }
        self.sorted.push(v);
    }

    fn has_cycle(&self) -> bool {
        !self.cycle.is_empty()
    }

    fn cycle(&self) -> &[usize] {
        &self.cycle
    }
}

/// Bayesian Network iterator, visits all nodes from parents to childs
pub struct NetIter<'a: 'b, 'b, D: 'a, N: 'b>
    where D: Variable,
          N: Node<D, N>
{
    unvisitted: VecDeque<&'b N>,
    processed: HashSet<&'a D>,
    queued: &'b N,
    childs_visitted: usize,
}

impl<'a, 'b, D, N> NetIter<'a, 'b, D, N>
    where D: Variable,
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
    where D: Variable,
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
