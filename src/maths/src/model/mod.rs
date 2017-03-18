//! Infrastructure to instantiate an statistical model with a given set of parameters.
mod discrete;

use std::collections::{VecDeque, HashSet};
use std::iter::FromIterator;
use std::hash::Hash;
use std::rc::Rc;
use std::marker::PhantomData;

use dists::{Categorical, Binomial};
use sampling::DiscreteSampler;

pub use self::discrete::{DiscreteModel, DefDiscreteNode, DefDiscreteVar, CPT};
pub use self::discrete::{DiscreteNode, DiscreteVar};

// public traits for models:

pub trait Observation {
    fn is_kind(&self) -> VariableKind;
}

pub trait Variable: Hash + PartialEq + Eq {
    type O: Observation;
}

/// A node in the the DAG.
pub trait Node {
    fn get_child(&self, pos: usize) -> Rc<Self>;
    fn get_childs(&self) -> Vec<Rc<Self>>;
    fn is_root(&self) -> bool;
    fn position(&self) -> usize;

    /// Returns the node parents positions in the network.
    fn get_parents_positions(&self) -> Vec<usize>;

    /// Returns the node childs positions in the network.
    fn get_childs_positions(&self) -> Vec<usize>;

    /// Node implementors are behind a reference counted pointer in the network,
    /// so they must have an interior mutable field to keep track of their position
    /// in the network which can be update calling this method.
    fn set_position(&self, pos: usize);

    /// Returns the number of parents this node has.
    fn parents_num(&self) -> usize;
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

struct DAG<'a, N: 'a>
    where N: DiscreteNode<'a>
{
    _nlt: PhantomData<&'a N>,
    nodes: Vec<Rc<N>>,
}

impl<'a, N: 'a> DAG<'a, N>
    where N: DiscreteNode<'a>
{
    fn new(init: N) -> DAG<'a, N> {
        DAG {
            _nlt: PhantomData,
            nodes: vec![Rc::new(init)],
        }
    }

    /// Perform both topological sorting and acyclicality check in the same operation.
    /// Returns error if is not a DAG.
    fn topological_sort(&mut self) -> Result<(), ()> {
        let mut cycle_check = DirectedCycle::new::<N>(self);
        for i in 0..self.nodes.len() {
            cycle_check.dfs(self, i);
            if cycle_check.has_cycle() {
                return Err(());
            }
        }
        let mut priority = Vec::with_capacity(self.nodes.len());
        for (i, c) in cycle_check.sorted.iter().enumerate() {
            let node = &self.nodes[*c];
            if node.position() != i {
                node.set_position(i);
            }
            priority.push((i, node.clone()));
        }
        priority.sort_by(|&(ref i, _), &(ref j, _)| i.cmp(j));
        let sorted: Vec<Rc<N>> = priority.into_iter()
            .map(|(_, e)| e)
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
    fn new<'a, N: 'a>(graph: &DAG<'a, N>) -> DirectedCycle
        where N: DiscreteNode<'a>
    {
        DirectedCycle {
            marked: vec![false; graph.nodes.len()],
            on_stack: vec![false; graph.nodes.len()],
            edge_to: Vec::from_iter(0..graph.nodes.len()),
            cycle: Vec::with_capacity(graph.nodes.len()),
            sorted: Vec::with_capacity(graph.nodes.len()),
        }
    }

    fn dfs<'a, N: 'a>(&mut self, graph: &DAG<'a, N>, v: usize)
        where N: DiscreteNode<'a>
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
pub struct NetIter<'a, N: 'a>
    where N: DiscreteNode<'a>
{
    _nlt: PhantomData<&'a N>,
    unvisitted: VecDeque<Rc<N>>,
    processed: HashSet<usize>,
    queued: Rc<N>,
    childs_visitted: usize,
}

impl<'a, N: 'a> NetIter<'a, N>
    where N: DiscreteNode<'a>
{
    fn new(nodes: &[Rc<N>]) -> NetIter<'a, N> {
        let mut processed = HashSet::new();
        let mut unvisitted = VecDeque::from_iter(nodes.iter().cloned());
        let first = unvisitted.pop_front().unwrap();
        processed.insert(first.get_dist() as *const N::Var as usize);
        NetIter {
            _nlt: PhantomData,
            unvisitted: unvisitted,
            processed: processed,
            queued: first,
            childs_visitted: 0,
        }
    }
}

impl<'a, N: 'a> ::std::iter::Iterator for NetIter<'a, N>
    where N: DiscreteNode<'a>
{
    type Item = Rc<N>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            while self.childs_visitted < self.queued.get_childs().len() {
                let next = self.queued.get_child(self.childs_visitted);
                self.childs_visitted += 1;
                let d = next.get_dist() as *const N::Var as usize;
                if !self.processed.contains(&d) {
                    return Some(next);
                }
            }

            if self.unvisitted.is_empty() {
                return None;
            } else {
                // add all previously visitted to the list of processed
                for e in self.queued.get_childs() {
                    self.processed.insert(e.get_dist() as *const N::Var as usize);
                }
                let next = self.unvisitted.pop_front().unwrap();
                self.queued = next;
            }
        }
    }
}
