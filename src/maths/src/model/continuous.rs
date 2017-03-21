use std::cell::RefCell;
use std::collections::{VecDeque, HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::iter::{FromIterator, Iterator};
use std::ops::Deref;
use std::rc::{Rc, Weak};
use std::marker::PhantomData;

use uuid::Uuid;

use super::{Node, Variable, Observation};
use super::{DType, Continuous};
use sampling::{ContinuousSampler, DefContSampler};

// public traits for models:

///  Node trait for a continuous model.
pub trait ContNode<'a>: Node
    where Self: Sized
{
    type Var: 'a + ContVar;

    fn new(dist: &'a Self::Var, pos: usize) -> Result<Self, ()>;

    /// Return the distribution of a node.
    fn get_dist(&self) -> &'a Self::Var;

    /// Returns a reference to the distributions of the parents·
    fn get_parents_dists(&self) -> Vec<&'a Self::Var>;

    /// Returns a reference to the distributions of the childs·
    fn get_childs_dists(&self) -> Vec<&'a Self::Var>;

    /// Takes an slice reprensenting the realized parent variables values at the current
    /// time **t** and draws a sample based on the corresponding probabilities.
    fn draw_sample(&self, fixed: &[f64]) -> f64;

    /// Sample from the prior distribution, usually called on roots of the tree
    /// to initialize each sampling steep.
    fn init_sample(&self) -> f64;

    fn add_parent(&self, parent: Weak<Self>);

    fn add_child(&self, child: Rc<Self>);
}

pub trait ContVar: Variable {
    type Event: Observation + Deref;

    /// Returns a sample from the original variable, not taking into consideration
    /// the parents in the network (if any).
    fn sample(&self) -> f64;

    /// Returns an slice of known observations for the variable of
    /// this distribution.
    fn get_observations(&self) -> &[Self::Event];

    fn push_observation(&mut self, obs: Self::Event);

    fn float_into_event(f64) -> Self::Event;
}

pub struct ContModel<'a, N: 'a, S>
    where N: ContNode<'a>,
          S: ContinuousSampler + Clone
{
    vars: ContDAG<'a, N>,
    sampler: S,
}

pub type DefContModel<'a> = ContModel<'a, DefContNode<'a, DefContVar>, DefContSampler>;

impl<'a, N, S> ContModel<'a, N, S>
    where N: ContNode<'a>,
          S: ContinuousSampler + Clone
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
                             parent_d: &'a <N as ContNode<'a>>::Var)
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
        node.add_parent(Rc::downgrade(&parent.clone()));
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
}

dag_constructor!(ContDAG, ContNode);

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
    pos: RefCell<usize>, // position in the bayes net vec of self
}

node_constructor!(DefContNode, ContVar, DefContVar, Continuous);
