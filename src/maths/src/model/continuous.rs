use std::collections::{VecDeque, HashSet};
use std::hash::{Hash, Hasher};
use std::iter::{FromIterator, Iterator};
use std::sync::{Arc, Weak, RwLock};
use std::marker::PhantomData;

use uuid::Uuid;

use RGSLRng;
use super::{Node, Variable, Observation};
use super::{DType, Continuous};
use sampling::{ContSampler};
use dists::{Sample, Normalization};
use err::ErrMsg;

// public traits for models:

/// Node trait for a continuous model. This node is reference counted and inmutably shared
/// throught Arc, add interior mutability if necessary.
pub trait ContNode<'a>: Node + Sized
{
    type Var: 'a + ContVar + Normalization;

    /// Constructor method for the continuous node in the Bayesian net.
    fn new(dist: &'a Self::Var, pos: usize) -> Result<Self, ()>;

    /// Return the distribution of a node.
    fn get_dist(&self) -> &Self::Var;

    /// Returns a reference to the distributions of the parents·
    fn get_parents_dists(&self) -> Vec<&'a Self::Var>;

    /// Returns a reference to the distributions of the childs·
    fn get_childs_dists(&self) -> Vec<&'a Self::Var>;

    /// Sample from the prior distribution, usually called on roots of the tree
    /// to initialize each sampling steep.
    fn init_sample(&self, rng: &mut RGSLRng) -> f64;

    /// Add a new parent to this child with given rank correlation (if any).
    /// Does not add self as child implicitly!
    fn add_parent(&self, parent: Arc<Self>, rank_cr: Option<f64>);

    /// Remove a parent from this node. Does not remove self as child implicitly!
    fn remove_parent(&self, parent: &Self::Var);

    /// Add a child to this node, assumes rank correlation was provided to the child.
    /// Does not add self as parent implicitly!
    fn add_child(&self, child: Arc<Self>);

    /// Remove a child from this node. Does not remove self as parent implicitly!
    fn remove_child(&self, child: &Self::Var);

    /// Get the values of the edges of this node with its parents, representing the rank
    /// correlation between the nodes. Returns the position of the parent for each edge
    /// in the network.
    fn get_edges(&self) -> Vec<(Option<f64>, usize)>;
}

pub trait ContVar: Variable {
    type Event: Observation;

    /// Returns a sample from the original variable, not taking into consideration
    /// the parents in the network (if any).
    fn sample(&self, rng: &mut RGSLRng) -> f64;

    /// Returns an slice of known observations for the variable of
    /// this distribution.
    fn get_observations(&self) -> &[Self::Event];

    /// Push a new observation of the measured event/variable to the stack of obserbations.
    fn push_observation(&mut self, obs: Self::Event);

    /// Conversion of a double float to the associated Event type.
    fn float_into_event(f64) -> Self::Event;

    /// Get an obsevation value from the observations stack. Panics if it out of bound.
    fn get_obs_unchecked(&self, pos: usize) -> Self::Event;
}

pub type DefContModel<'a> = ContModel<'a, DefContNode<'a, DefContVar>>;

#[derive(Debug)]
pub struct ContModel<'a, N>
    where N: ContNode<'a>
{
    vars: ContDAG<'a, N>
}

impl<'a, N> ContModel<'a, N>
    where N: ContNode<'a>
{
    pub fn new() -> ContModel<'a, N> {
        ContModel {
            vars: ContDAG::new()
        }
    }

    /// Add a new variable to the model.
    pub fn add_var(&mut self, var: &'a <N as ContNode<'a>>::Var) -> Result<(), ()> {
        let pos = self.vars.nodes.len();
        let node = N::new(var, pos)?;
        self.vars.nodes.push(Arc::new(node));
        Ok(())
    }

    /// Adds a parent `dist` to a child `dist`, connecting both nodes directionally
    /// with an arc. The arc value represents the rank correlation.
    ///
    /// Takes the distribution of a variable, and the parent variable distribution
    /// as arguments and returns a result indicating if the parent was added properly.
    /// Both variables have to be added previously to the model.
    pub fn add_parent(&mut self,
                      node: &'a <N as ContNode<'a>>::Var,
                      parent: &'a <N as ContNode<'a>>::Var,
                      rank_cr: Option<f64>)
                      -> Result<(), ()> {
        // checks to perform:
        //  - both exist in the model
        //  - the theoretical child cannot be a parent of the theoretical parent
        //    as the network is a DAG
        // find node and parents in the net
        let node: Arc<N> = self.vars
            .nodes
            .iter()
            .find(|n| (&**n).get_dist() == node)
            .cloned()
            .ok_or(())?;
        let parent: Arc<N> = self.vars
            .nodes
            .iter()
            .find(|n| (&**n).get_dist() == parent)
            .cloned()
            .ok_or(())?;
        node.add_parent(parent.clone(), rank_cr);
        parent.add_child(node.clone());
        // check if it's a DAG and topologically sort the graph
        self.vars.topological_sort()
    }

    /// Remove a variable from the model, the childs will be disjoint if they don't
    /// have an other parent.
    pub fn remove_var(&mut self, var: &'a <N as ContNode<'a>>::Var) {
        if let Some(pos) = self.vars
               .nodes
               .iter()
               .position(|n| (&**n).get_dist() == var) {
            if pos < self.vars.nodes.len() - 1 {
                let parent = &self.vars.nodes[pos];
                for node in &self.vars.nodes[pos + 1..] {
                    node.set_position(node.position() - 1);
                    node.remove_parent(var);
                    parent.remove_child(node.get_dist());
                }
            }
            self.vars.nodes.remove(pos);
        };
    }

    /// Sample the model with a given sampler, with the current elicited probabilities.
    pub fn sample<S: ContSampler<'a>>(&self, sampler: S) -> Result<S::Output, S::Err> {
        sampler.get_samples(self)
    }

    /// Returns the total number of variables in the model.
    pub fn var_num(&self) -> usize {
        self.vars.nodes.len()
    }

    /// Iterate the model variables in topographical order.
    pub fn iter_vars(&self) -> BayesNetIter<'a, N> {
        BayesNetIter::new(&self.vars.nodes)
    }

    /// Get the node in the graph at position *i* unchecked.
    pub fn get_var(&self, i: usize) -> Arc<N> {
        self.vars.get_node(i)
    }
}

#[derive(Debug)]
struct ContDAG<'a, N>
    where N: ContNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    nodes: Vec<Arc<N>>,
}

dag_impl!(ContDAG, ContNode; [ContVar + Normalization]);

/// A node in the network representing a continuous random variable.
///
/// This type shouldn't be instantiated directly, instead add the random variable
/// distribution to the network.
pub struct DefContNode<'a, V: 'a>
    where V: ContVar
{
    pub dist: &'a V,
    childs: RwLock<Vec<Weak<DefContNode<'a, V>>>>,
    parents: RwLock<Vec<Arc<DefContNode<'a, V>>>>,
    edges: RwLock<Vec<Option<f64>>>, // rank correlations assigned to edges, if any
    pos: RwLock<usize>,
}

impl<'a, V: 'a + ContVar> ::std::fmt::Debug for DefContNode<'a, V> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f,
               "DefContNode {{ dist: {d:?}, childs: {c}, parents: {p}, pos: {pos}, edges: {e:?} }}",
               c = self.childs.read().unwrap().len(),
               p = self.parents.read().unwrap().len(),
               pos = *self.pos.read().unwrap(),
               d = self.dist,
               e = self.edges.read().unwrap())
    }
}

node_impl!(DefContNode, ContVar);

impl<'a, V: 'a> ContNode<'a> for DefContNode<'a, V>
    where V: ContVar + Normalization
{
    type Var = V;

    fn new(dist: &'a V, pos: usize) -> Result<Self, ()> {
        match *dist.dist_type() {
            DType::Normal(_) |
            DType::Beta(_) |
            DType::Exponential(_) |
            DType::Gamma(_) |
            DType::ChiSquared(_) |
            DType::TDist(_) |
            DType::FDist(_) |
            DType::Cauchy(_) |
            DType::LogNormal(_) |
            DType::Logistic(_) |
            DType::Pareto(_) => {}
            _ => return Err(()),
        }

        // get the probabilities from the dist and insert as default cpt
        Ok(DefContNode {
            dist: dist,
            childs: RwLock::new(vec![]),
            parents: RwLock::new(vec![]),
            edges: RwLock::new(vec![]),
            pos: RwLock::new(pos),
        })
    }

    fn get_dist(&self) -> &V {
        self.dist
    }

    fn init_sample(&self, rng: &mut RGSLRng) -> f64 {
        self.dist.sample(rng)
    }

    fn get_parents_dists(&self) -> Vec<&'a V> {
        let parents = &*self.parents.read().unwrap();
        let mut dists = Vec::with_capacity(parents.len());
        for p in parents {
            dists.push(p.dist);
        }
        dists
    }

    fn get_childs_dists(&self) -> Vec<&'a V> {
        let childs = &*self.childs.read().unwrap();
        let mut dists = Vec::with_capacity(childs.len());
        for c in childs {
            dists.push(c.upgrade().unwrap().dist);
        }
        dists
    }

    fn add_parent(&self, parent: Arc<Self>, rank_cr: Option<f64>) {
        let parents = &mut *self.parents.write().unwrap();
        let edges = &mut *self.edges.write().unwrap();
        // check for duplicates:
        if let Some(pos) = parents
                .iter()
                .position(|ref x| &*x.get_dist() == parent.get_dist()) {
            edges[pos] = rank_cr;
        } else {
            parents.push(parent);
            edges.push(rank_cr);
        };
    }

    fn remove_parent(&self, parent: &V) {
        let parents = &mut *self.parents.write().unwrap();
        if let Some(pos) = parents.iter().position(|ref x| &*x.get_dist() == parent) {
            parents.remove(pos);
            let edges = &mut *self.edges.write().unwrap();
            edges.remove(pos);
        }
    }

    fn add_child(&self, child: Arc<Self>) {
        let parent_childs = &mut *self.childs.write().unwrap();
        let pos = parent_childs
            .iter()
            .enumerate()
            .find(|&(_, x)| &*x.upgrade().unwrap().get_dist() == child.get_dist())
            .map(|(i, _)| i);
        if let None = pos {
            parent_childs.push(Arc::downgrade(&child));
        }
    }

    fn remove_child(&self, child: &V) {
        let childs = &mut *self.childs.write().unwrap();
        if let Some(pos) = childs
               .iter()
               .position(|ref x| &*x.upgrade().unwrap().get_dist() == child) {
            childs.remove(pos);
        }
    }

    fn get_edges(&self) -> Vec<(Option<f64>, usize)> {
        let edges = &*self.edges.read().unwrap();
        let parents = &*self.parents.read().unwrap();
        let mut edge_with_parent = Vec::with_capacity(parents.len());
        for (i, p) in parents.iter().enumerate() {
            edge_with_parent.push((edges[i], p.position()));
        }
        edge_with_parent
    }
}

#[derive(Debug)]
pub struct DefContVar {
    dist: DType,
    observations: Vec<Continuous>,
    id: Uuid,
}

fn validate_dist(dist: &DType) -> Result<(), ()> {
    match *dist {
        DType::Normal(_) |
        DType::Beta(_) |
        DType::Exponential(_) |
        DType::Gamma(_) |
        DType::ChiSquared(_) |
        DType::TDist(_) |
        DType::FDist(_) |
        DType::Cauchy(_) |
        DType::LogNormal(_) |
        DType::Logistic(_) |
        DType::Pareto(_) => Ok(()), 
        _ => Err(()),
    }
}

var_impl!(DefContVar);

impl ContVar for DefContVar {
    type Event = Continuous;

    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        match self.dist {
            DType::Normal(ref dist) => dist.sample(rng),
            DType::Beta(ref dist) => dist.sample(rng),
            DType::Exponential(ref dist) => dist.sample(rng),
            DType::Gamma(ref dist) => dist.sample(rng),
            DType::ChiSquared(ref dist) => dist.sample(rng),
            DType::TDist(ref dist) => dist.sample(rng),
            DType::FDist(ref dist) => dist.sample(rng),
            DType::Cauchy(ref dist) => dist.sample(rng),
            DType::LogNormal(ref dist) => dist.sample(rng),
            DType::Logistic(ref dist) => dist.sample(rng),
            DType::Pareto(ref dist) => dist.sample(rng),
            ref d => panic!(ErrMsg::DiscDistContNode.panic_msg_with_arg(d)),
        }
    }

    fn get_observations(&self) -> &[<Self as ContVar>::Event] {
        &self.observations
    }

    fn push_observation(&mut self, obs: Self::Event) {
        self.observations.push(obs)
    }

    #[inline]
    fn float_into_event(float: f64) -> Self::Event {
        float as Self::Event
    }

    fn get_obs_unchecked(&self, pos: usize) -> Self::Event {
        self.observations[pos]
    }
}

impl Normalization for DefContVar {
    #[inline]
    fn into_default(self) -> Self {
        self
    }
}
