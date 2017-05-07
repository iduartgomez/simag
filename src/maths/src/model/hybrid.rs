use std::collections::{VecDeque, HashSet};
use std::iter::{FromIterator, Iterator};
use std::sync::{Arc, Weak, RwLock};
use std::marker::PhantomData;

use RGSLRng;
use super::{Node, ContVar, DiscreteVar, ContNode};
use super::{DType, DefContVar, DefDiscreteVar};
use dists::{Normalization, AsContinuous};

use itertools::Itertools;

pub trait HybridNode<'a>: ContNode<'a> + Sized {
    type Discrete: DiscreteVar + AsContinuous;

    /// Makes an hybrid model node from a discrete variable.
    fn new_with_discrete(var: &'a Self::Discrete, pos: usize) -> Result<Self, ()>;
    fn was_discrete(&self) -> bool;
    fn get_disc_dist(&self) -> Option<&'a Self::Discrete>;
    fn inverse_cdf(&self, p: f64) -> u8;
    fn remove_disc_parent(&self, var: &Self::Discrete);
}

#[derive(Debug)]
pub struct HybridModel<'a, N>
    where N: HybridNode<'a>
{
    vars: HybridDAG<'a, N>,
}

pub type DefHybridModel<'a> = HybridModel<'a, DefHybridNode<'a, DefContVar, DefDiscreteVar>>;

impl<'a, N> HybridModel<'a, N>
    where N: HybridNode<'a>
{
    pub fn new() -> HybridModel<'a, N> {
        HybridModel { vars: HybridDAG::new() }
    }

    /// Add a new variable to the model.
    pub fn add_var(&mut self, var: &'a <N as ContNode<'a>>::Var) -> Result<(), ()> {
        let pos = self.vars.nodes.len();
        let node = N::new(var, pos)?;
        self.vars.nodes.push(Arc::new(node));
        Ok(())
    }

    /// Adds a parent `dist` to a child `dist`, connecting both nodes directionally
    /// with an arc. Accepts an optional weight argument for the arc.
    ///
    /// Takes the distribution of a variable, and the parent variable distribution
    /// as arguments and returns a result indicating if the parent was added properly.
    /// Both variables have to be added previously to the model.
    pub fn add_parent(&mut self,
                      node: &'a <N as ContNode<'a>>::Var,
                      parent: &'a <N as ContNode<'a>>::Var,
                      weight: Option<f64>)
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
        node.add_parent(parent.clone(), weight);
        parent.add_child(node.clone());
        // check if it's a DAG and topologically sort the graph
        self.vars.topological_sort()
    }

    /// Remove a continuous variable from the model, the childs will be disjoint if they don't
    /// have an other parent.
    pub fn remove_cont_var(&mut self, var: &'a <N as ContNode<'a>>::Var) {
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

    /// Remove a discrete variable from the model, the childs will be disjoint if they don't
    /// have an other parent.
    pub fn remove_disc_var(&mut self, var: &'a <N as HybridNode<'a>>::Discrete) {
        if let Some(pos) = self.vars
               .nodes
               .iter()
               .position(|n| if let Some(d) = n.get_disc_dist() {
                             d == var
                         } else {
                             false
                         }) {
            if pos < self.vars.nodes.len() - 1 {
                let parent = &self.vars.nodes[pos];
                for node in &self.vars.nodes[pos + 1..] {
                    node.set_position(node.position() - 1);
                    node.remove_disc_parent(var);
                    parent.remove_child(node.get_dist());
                }
            }
            self.vars.nodes.remove(pos);
        };
    }

    /// Returns the total number of variables in the model.
    pub fn var_num(&self) -> usize {
        self.vars.nodes.len()
    }

    /// Get the node in the graph at position *i* unchecked.
    pub fn get_var(&self, i: usize) -> Arc<N> {
        self.vars.get_node(i)
    }
}

impl<'a, N> super::IterModel for HybridModel<'a, N>
    where N: HybridNode<'a>
{
    type Iter = BayesNetIter<'a, N>;
    fn iter_vars(&self) -> Self::Iter {
        BayesNetIter::new(&self.vars.nodes)
    }

    fn var_num(&self) -> usize {
        self.vars.nodes.len()
    }

    fn var_neighbours(&self, idx: usize) -> Vec<usize> {
        self.vars.nodes[idx].get_neighbours()
    }
}

/// A node in the network representing a random variable.
///
/// This type shouldn't be instantiated directly, instead add the random variable
/// distribution to the network.
#[derive(Debug)]
pub struct DefHybridNode<'a, C: 'a, D: 'a>
    where C: ContVar,
          D: DiscreteVar + AsContinuous
{
    cont_dist: Option<&'a C>,
    disc_dist: Option<&'a D>,
    as_cont: Option<C>,
    childs: RwLock<Vec<Weak<DefHybridNode<'a, C, D>>>>,
    parents: RwLock<Vec<Arc<DefHybridNode<'a, C, D>>>>,
    edges: RwLock<Vec<Option<f64>>>, // weight assigned to edges, if any
    pos: RwLock<usize>,
    was_discrete: bool,
}

impl<'a, C: 'a, D: 'a> Node for DefHybridNode<'a, C, D>
    where C: ContVar,
          D: DiscreteVar + AsContinuous
{
    fn get_child_unchecked(&self, pos: usize) -> Arc<Self> {
        let childs = &*self.childs.read().unwrap();
        childs[pos].clone().upgrade().unwrap()
    }

    fn get_childs(&self) -> Vec<Arc<Self>> {
        let childs = &*self.childs.read().unwrap();
        Vec::from_iter(childs.iter().cloned().map(|x| x.upgrade().unwrap()))
    }

    fn is_root(&self) -> bool {
        self.parents_num() == 0
    }

    fn position(&self) -> usize {
        *self.pos.read().unwrap()
    }

    fn get_parents_positions(&self) -> Vec<usize> {
        let parents = &*self.parents.read().unwrap();
        let mut positions = Vec::with_capacity(parents.len());
        for p in parents {
            positions.push(*p.pos.read().unwrap());
        }
        positions
    }

    fn get_all_ancestors(&self) -> Vec<usize> {
        let parents = &*self.parents.read().unwrap();
        let mut anc = Vec::with_capacity(parents.len());
        for p in parents {
            let mut ancestors = p.get_all_ancestors();
            anc.append(&mut ancestors);
            anc.push(*p.pos.read().unwrap());
        }
        anc.sort_by(|a, b| b.cmp(a));
        anc.dedup();
        anc
    }

    fn get_childs_positions(&self) -> Vec<usize> {
        let childs = &*self.childs.read().unwrap();
        let mut positions = Vec::with_capacity(childs.len());
        for c in childs {
            let c = c.upgrade().unwrap();
            positions.push(*c.pos.read().unwrap());
        }
        positions
    }

    fn get_neighbours(&self) -> Vec<usize> {
        let mut p = self.get_parents_positions();
        let mut c = self.get_childs_positions();
        p.append(&mut c);
        p
    }

    fn parents_num(&self) -> usize {
        let parents = &*self.parents.read().unwrap();
        parents.len()
    }

    fn childs_num(&self) -> usize {
        let childs = &*self.childs.read().unwrap();
        childs.len()
    }

    fn set_position(&self, pos: usize) {
        *self.pos.write().unwrap() = pos;
    }
}

impl<'a, C: 'a, D: 'a> HybridNode<'a> for DefHybridNode<'a, C, D>
    where C: ContVar + Normalization,
          D: DiscreteVar + AsContinuous
{
    type Discrete = D;
    fn new_with_discrete(var: &'a Self::Discrete, pos: usize) -> Result<Self, ()> {
        let dist = var.as_continuous()?;
        Ok(DefHybridNode {
               cont_dist: None,
               disc_dist: Some(var),
               as_cont: Some(dist),
               childs: RwLock::new(vec![]),
               parents: RwLock::new(vec![]),
               edges: RwLock::new(vec![]),
               pos: RwLock::new(pos),
               was_discrete: true,
           })
    }

    fn was_discrete(&self) -> bool {
        self.was_discrete
    }

    fn get_disc_dist(&self) -> Option<&'a D> {
        self.disc_dist
    }

    fn inverse_cdf(&self, p: f64) -> u8 {
        match *self.disc_dist.as_ref().unwrap().dist_type() {
            DType::Bernoulli(ref dist) => dist.inverse_cdf(p),
            _ => panic!(),
        }
    }

    fn remove_disc_parent(&self, var: &D) {
        let parents = &mut *self.parents.write().unwrap();
        if let Some(pos) = parents
               .iter()
               .position(|ref x| if let Some(disc) = x.get_disc_dist() {
                             var == disc
                         } else {
                             false
                         }) {
            parents.remove(pos);
            let edges = &mut *self.edges.write().unwrap();
            edges.remove(pos);
        }
    }

    /*
    fn remove_disc_child(&self, var: &D) {
        let childs = &mut *self.childs.write().unwrap();
        if let Some(pos) = childs
               .iter()
               .position(|ref x| if let Some(disc) = x.upgrade().unwrap().get_disc_dist() {
                             var == disc
                         } else {
                             false
                         }) {
            childs.remove(pos);
        }
    }
    */
}

impl<'a, C: 'a, D: 'a> ContNode<'a> for DefHybridNode<'a, C, D>
    where C: ContVar + Normalization,
          D: DiscreteVar + AsContinuous
{
    type Var = C;

    fn new(dist: &'a C, pos: usize) -> Result<Self, ()> {
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
        Ok(DefHybridNode {
               cont_dist: Some(dist),
               disc_dist: None,
               as_cont: None,
               childs: RwLock::new(vec![]),
               parents: RwLock::new(vec![]),
               edges: RwLock::new(vec![]),
               pos: RwLock::new(pos),
               was_discrete: false,
           })
    }

    fn get_dist(&self) -> &C {
        if !self.was_discrete {
            self.cont_dist.unwrap()
        } else {
            self.as_cont.as_ref().unwrap()
        }
    }

    fn init_sample(&self, rng: &mut RGSLRng) -> f64 {
        if !self.was_discrete {
            self.cont_dist.unwrap().sample(rng)
        } else {
            self.as_cont.as_ref().unwrap().sample(rng)
        }
    }

    fn get_parents_dists(&self) -> Vec<&'a C> {
        let parents = &*self.parents.read().unwrap();
        let mut dists = Vec::with_capacity(parents.len());
        for p in parents {
            dists.push(p.cont_dist.unwrap());
        }
        dists
    }

    fn get_childs_dists(&self) -> Vec<&'a C> {
        let childs = &*self.childs.read().unwrap();
        let mut dists = Vec::with_capacity(childs.len());
        for c in childs {
            let c = c.upgrade().unwrap();
            dists.push(c.cont_dist.unwrap());
        }
        dists
    }

    fn add_parent(&self, parent: Arc<Self>, weight: Option<f64>) {
        let parents = &mut *self.parents.write().unwrap();
        let edges = &mut *self.edges.write().unwrap();
        parents.push(parent);
        edges.push(weight);
    }

    fn remove_parent(&self, parent: &C) {
        let parents = &mut *self.parents.write().unwrap();
        if let Some(pos) = parents.iter().position(|ref x| &*x.get_dist() == parent) {
            parents.remove(pos);
            let edges = &mut *self.edges.write().unwrap();
            edges.remove(pos);
        }
    }

    fn add_child(&self, child: Arc<Self>) {
        let parent_childs = &mut *self.childs.write().unwrap();
        parent_childs.push(Arc::downgrade(&child));
    }

    fn remove_child(&self, child: &C) {
        let childs = &mut *self.childs.write().unwrap();
        if let Some(pos) = childs
               .iter()
               .position(|ref x| x.upgrade().unwrap().get_dist() == child) {
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
struct HybridDAG<'a, N>
    where N: HybridNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    nodes: Vec<Arc<N>>,
}

dag_impl!(HybridDAG, HybridNode; [ContVar + Normalization]);


pub type DefMarkovRndField<'a> = MarkovRndField<'a, DefHybridNode<'a, DefContVar, DefDiscreteVar>>;

/// A chain graph is composed by a set of nodes, a set of acyclic directed edges (or arcs) and/or
/// a set of undirected edges.
///
/// Bayesian networks and markov fields are both special cases of chain graphs (which can
/// contain both). This implementation will support hybrid nodes (continuous and discrete variables
/// treated as continuous variables).
///
/// Sampling of this kind of graph is done thought Markov chain Monte Carlo methods, and the
/// underlying graph is 'moralized' first (to an undirected graph), hence any edge weighting
/// information would be lost (so it's unnecessary). To avoid this problem when necessary, the use
/// of one of the other directed models is encouraged.
pub struct MarkovRndField<'a, N>
    where N: HybridNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    edges: Vec<(Arc<N>, Arc<N>, Option<f64>)>,
    underlying: Vec<Arc<N>>,
}

impl<'a, N> MarkovRndField<'a, N>
    where N: HybridNode<'a>
{
    pub fn new() -> MarkovRndField<'a, N> {
        MarkovRndField {
            _nlt: PhantomData,
            edges: vec![],
            underlying: vec![],
        }
    }

    /// Add a new variable to the model.
    pub fn add_var(&mut self, var: &'a <N as ContNode<'a>>::Var) -> Result<(), ()> {
        let pos = self.underlying.len();
        let node = N::new(var, pos)?;
        self.underlying.push(Arc::new(node));
        Ok(())
    }

    /// Adds an (undirected) edge between two variables.
    ///
    /// Both variables have to be added previously to the model.
    pub fn add_edge(&mut self,
                    a: &'a <N as ContNode<'a>>::Var,
                    b: &'a <N as ContNode<'a>>::Var)
                    -> Result<(), ()> {
        let a: Arc<N> = self.underlying
            .iter()
            .find(|n| (&**n).get_dist() == a)
            .cloned()
            .ok_or(())?;
        let b: Arc<N> = self.underlying
            .iter()
            .find(|n| (&**n).get_dist() == b)
            .cloned()
            .ok_or(())?;
        self.edges.push((a, b, None));
        Ok(())
    }

    /// Insert a Bayesian network into the model, it will be "moralized" in the process,
    /// losing all the implied causal information.
    pub fn insert_dag(&mut self, other: HybridModel<'a, N>) {
        let HybridModel { vars: HybridDAG { mut nodes, .. }, .. } = other;
        let cnt = self.underlying.len();
        for node in &mut nodes {
            node.set_position(node.position() + cnt);
        }
        self.underlying.append(&mut nodes);
        // moralize the appended graph, converting directed edges to undirected:
        for node in &self.underlying[cnt..] {
            let mut parents = node.get_parents_positions();
            parents.push(node.position());
            let mut edges = parents
                .iter()
                .tuple_combinations()
                .map(|(a, b)| (self.underlying[*a].clone(), self.underlying[*b].clone(), None))
                .collect();
            self.edges.append(&mut edges);
        }
    }

    /// Remove a variable from the model, the childs will be disjoint if they don't
    /// have an other parent.
    pub fn remove_var(&mut self, node: &'a <N as ContNode<'a>>::Var) {
        if let Some(pos) = self.underlying
               .iter()
               .position(|n| (&**n).get_dist() == node) {
            if pos < self.underlying.len() - 1 {
                for node in &self.underlying[pos + 1..] {
                    node.set_position(node.position() - 1);
                }
            }
            self.underlying.remove(pos);
        };
    }

    /// Get the node in the graph at position *i* unchecked.
    pub fn get_var(&self, i: usize) -> Arc<N> {
        self.underlying[i].clone()
    }
}

impl<'a, N> super::IterModel for MarkovRndField<'a, N>
    where N: HybridNode<'a>
{
    type Iter = MarkovRndFieldIter<'a, N>;
    fn iter_vars(&self) -> MarkovRndFieldIter<'a, N> {
        MarkovRndFieldIter {
            _nlt: PhantomData,
            nodes: self.underlying.clone(),
            cnt: 0,
        }
    }

    fn var_num(&self) -> usize {
        self.underlying.len()
    }

    fn var_neighbours(&self, idx: usize) -> Vec<usize> {
        self.edges
            .iter()
            .filter_map(|&(ref a, ref b, _)| if a.position() == idx {
                            Some(b.position())
                        } else if b.position() == idx {
                Some(a.position())
            } else {
                None
            })
            .collect::<Vec<_>>()
    }
}

pub struct MarkovRndFieldIter<'a, N>
    where N: HybridNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    nodes: Vec<Arc<N>>,
    cnt: usize,
}

impl<'a, N> ::std::iter::Iterator for MarkovRndFieldIter<'a, N>
    where N: HybridNode<'a>
{
    type Item = Arc<N>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cnt < self.nodes.len() {
            let r = self.nodes[self.cnt].clone();
            self.cnt += 1;
            Some(r)
        } else {
            None
        }
    }
}
