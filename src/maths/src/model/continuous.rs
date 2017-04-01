use std::cell::RefCell;
use std::collections::{VecDeque, HashSet};
use std::hash::{Hash, Hasher};
use std::iter::{FromIterator, Iterator};
use std::rc::{Rc, Weak};
use std::marker::PhantomData;

use uuid::Uuid;

use RGSLRng;
use super::{Node, Variable, Observation};
use super::{DType, Continuous};
use sampling::{ContinuousSampler, DefContSampler};
use dists::{Sample, Gaussianization};

// public traits for models:

///  Node trait for a continuous model.
pub trait ContNode<'a>: Node
    where Self: Sized
{
    type Var: 'a + ContVar + Gaussianization;

    /// Constructor method for the continuous node in the Bayesian net.
    fn new(dist: &'a Self::Var, pos: usize) -> Result<Self, ()>;

    /// Return the distribution of a node.
    fn get_dist(&self) -> &'a Self::Var;

    /// Returns a reference to the distributions of the parents·
    fn get_parents_dists(&self) -> Vec<&'a Self::Var>;

    /// Returns a reference to the distributions of the childs·
    fn get_childs_dists(&self) -> Vec<&'a Self::Var>;

    /// Sample from the prior distribution, usually called on roots of the tree
    /// to initialize each sampling steep.
    fn init_sample(&self, rng: &mut RGSLRng) -> f64;

    /// Add a new parent to this child with given rank correlation.
    /// Does not add self as child implicitly!
    fn add_parent(&self, parent: Weak<Self>, rank_cr: f64);

    /// Add a child to this node, assumes rank correlation was provided to the child.
    /// Does not add self as parent implicitly!
    fn add_child(&self, child: Rc<Self>);

    /// Get the values of the edges of this node with its parents, representing the rank
    /// correlation between the nodes. Returns the position of the parent for each edge
    /// in the network.
    fn get_edges(&self) -> Vec<(f64, usize)>;
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

pub struct ContModel<'a, N: 'a, S>
    where N: ContNode<'a>,
          S: ContinuousSampler<'a> + Clone
{
    vars: ContDAG<'a, N>,
    sampler: S,
}

pub type DefContModel<'a> = ContModel<'a, DefContNode<'a, DefContVar>, DefContSampler<'a>>;

impl<'a, N, S> ContModel<'a, N, S>
    where N: ContNode<'a>,
          S: ContinuousSampler<'a> + Clone
{
    pub fn new(init: &'a <N as ContNode<'a>>::Var, sampler: S) -> Result<ContModel<'a, N, S>, ()> {
        let init = N::new(init, 0)?;
        let dag = ContDAG::new(init);
        Ok(ContModel {
            vars: dag,
            sampler: sampler,
        })
    }

    /// Add a new variable to the model.
    pub fn add_var(&mut self, var: &'a <N as ContNode<'a>>::Var) -> Result<(), ()> {
        let pos = self.vars.nodes.len();
        let node: N = Self::with_var(var, pos)?;
        self.vars.nodes.push(Rc::new(node));
        Ok(())
    }

    /// Make a new orphan node from a continuous random variable (not added to the model
    /// automatically).
    fn with_var<No: 'a>(dist: &'a <No as ContNode<'a>>::Var, pos: usize) -> Result<No, ()>
        where No: ContNode<'a>
    {
        let node = No::new(dist, pos)?;
        Ok(node)
    }

    /// Adds a parent `dist` to a child `dist`, connecting both nodes directionally.
    ///
    /// Takes the distribution of a variable, and the parent variable distribution
    /// as arguments and returns a result indicating if the parent was added properly.
    /// Both variables have to be added previously to the model.
    pub fn add_parent_to_var(&mut self,
                             node: &'a <N as ContNode<'a>>::Var,
                             parent_d: &'a <N as ContNode<'a>>::Var,
                             rank_cr: f64)
                             -> Result<(), ()> {
        // checks to perform:
        //  - both exist in the model
        //  - the theoretical child cannot be a parent of the theoretical parent
        //    as the network is a DAG
        // find node and parents in the net
        let node: Rc<N> = self.vars
            .nodes
            .iter()
            .find(|n| (&**n).get_dist() == node)
            .cloned()
            .ok_or(())?;
        let parent: Rc<N> = self.vars
            .nodes
            .iter()
            .find(|n| (&**n).get_dist() == parent_d)
            .cloned()
            .ok_or(())?;
        node.add_parent(Rc::downgrade(&parent.clone()), rank_cr);
        parent.add_child(parent.clone());
        // check if it's a DAG and topologically sort the graph
        self.vars.topological_sort()
    }

    /// Remove a variable from the model, the childs will be disjoint if they don't
    /// have an other parent.
    pub fn remove_var(&mut self, _node: &'a <N as ContNode<'a>>::Var) {
        unimplemented!()
    }

    /// Sample the model in
    pub fn sample(&self) -> Vec<Vec<f64>> {
        let sampler = self.sampler.clone();
        sampler.get_samples(self)
    }

    /// Returns the total number of variables in the model.
    pub fn var_num(&self) -> usize {
        self.vars.nodes.len()
    }

    /// Iterate the model variables in topographical order.
    pub fn iter_vars(&self) -> NetIter<'a, N> {
        NetIter::new(&self.vars.nodes)
    }

    /// Get the node in the graph at position **i** unchecked.
    pub fn get_var(&self, i: usize) -> Rc<N> {
        self.vars.get_node(i)
    }
}

struct ContDAG<'a, N>
    where N: ContNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    nodes: Vec<Rc<N>>,
}

dag_impl!(ContDAG, ContNode);

/// A node in the network representing a continuous random variable.
///
/// This type cannot be instantiated directly, instead add the random variable
/// distribution to the network.
pub struct DefContNode<'a, V: 'a>
    where V: ContVar
{
    pub dist: &'a V,
    childs: RefCell<Vec<Rc<DefContNode<'a, V>>>>,
    parents: RefCell<Vec<Weak<DefContNode<'a, V>>>>,
    edges: RefCell<Vec<f64>>, // rank correlations assigned to edges
    pos: RefCell<usize>,
}

node_impl!(DefContNode, ContVar);

impl<'a, V: 'a> ContNode<'a> for DefContNode<'a, V>
    where V: ContVar + Gaussianization
{
    type Var = V;

    fn new(dist: &'a V, pos: usize) -> Result<Self, ()> {
        match *dist.dist_type() {
            DType::Normal(_) |
            DType::Exponential(_) => {}
            _ => return Err(()),
        }

        // get the probabilities from the dist and insert as default cpt
        Ok(DefContNode {
            dist: dist,
            childs: RefCell::new(vec![]),
            parents: RefCell::new(vec![]),
            edges: RefCell::new(vec![]),
            pos: RefCell::new(pos),
        })
    }

    fn get_dist(&self) -> &'a V {
        self.dist
    }

    fn init_sample(&self, rng: &mut RGSLRng) -> f64 {
        self.dist.sample(rng)
    }

    fn get_parents_dists(&self) -> Vec<&'a V> {
        let parents = &*self.parents.borrow();
        let mut dists = Vec::with_capacity(parents.len());
        for p in parents {
            let p = p.upgrade().unwrap();
            dists.push(p.dist);
        }
        dists
    }

    fn get_childs_dists(&self) -> Vec<&'a V> {
        let childs = &*self.childs.borrow();
        let mut dists = Vec::with_capacity(childs.len());
        for c in childs {
            dists.push(c.dist);
        }
        dists
    }

    fn add_parent(&self, parent: Weak<Self>, rank_cr: f64) {
        let parents = &mut *self.parents.borrow_mut();
        let edges = &mut *self.edges.borrow_mut();
        parents.push(parent);
        edges.push(rank_cr);
    }

    fn add_child(&self, child: Rc<Self>) {
        let parent_childs = &mut *self.childs.borrow_mut();
        parent_childs.push(child);
    }

    fn get_edges(&self) -> Vec<(f64, usize)> {
        let edges = &*self.edges.borrow();
        let parents = &*self.parents.borrow();
        let mut edge_with_parent = Vec::with_capacity(parents.len());
        for (i, p) in parents.iter().enumerate() {
            let p = p.upgrade().unwrap();
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

var_impl!(DefContVar);

impl ContVar for DefContVar {
    type Event = Continuous;

    fn sample(&self, rng: &mut RGSLRng) -> f64 {
        match self.dist {
            DType::Normal(ref dist) => dist.sample(rng),
            DType::Exponential(ref dist) => dist.sample(rng),
            _ => panic!(),
        }
    }
    fn get_observations(&self) -> &[<Self as ContVar>::Event] {
        &self.observations
    }

    fn push_observation(&mut self, obs: Self::Event) {
        self.observations.push(obs)
    }

    #[inline(always)]
    fn float_into_event(float: f64) -> Self::Event {
        float as Self::Event
    }

    fn get_obs_unchecked(&self, pos: usize) -> Self::Event {
        self.observations[pos]
    }
}

impl Gaussianization for DefContVar {
    #[inline(always)]
    fn into_default(self) -> Self {
        self
    }
}
