use std::cell::RefCell;
use std::collections::{VecDeque, HashSet};
use std::iter::{FromIterator, Iterator};
use std::rc::{Rc, Weak};
use std::marker::PhantomData;

use RGSLRng;
use super::{Node, ContVar, DiscreteVar, ContNode};
use super::{DType, DefContVar, DefDiscreteVar};
use sampling::{HybridSampler, DefHybridSampler, HybridSamplerResult};
use dists::{Normalization, AsContinuous};

use itertools::Itertools;

pub trait HybridNode<'a>: ContNode<'a>
    where Self: Sized
{
    type Discrete: DiscreteVar + AsContinuous;

    /// Makes an hybrid model node from a discrete variable.
    fn new_with_discrete(var: &'a Self::Discrete, pos: usize) -> Result<Self, ()>;
}

#[derive(Debug)]
pub struct HybridModel<'a, N, S>
    where N: HybridNode<'a>,
          S: HybridSampler<'a> + Clone
{
    vars: HybridDAG<'a, N>,
    sampler: S,
}

pub type DefHybridModel<'a> = HybridModel<'a,
                                          DefHybridNode<'a, DefContVar, DefDiscreteVar>,
                                          DefHybridSampler<'a>>;

impl<'a, N, S> HybridModel<'a, N, S>
    where N: HybridNode<'a>,
          S: HybridSampler<'a> + Clone
{
    /// Get a new instance of the default implementation of a continuous model.
    pub fn default() -> DefHybridModel<'a> {
        use sampling::Sampler;
        HybridModel {
            vars: HybridDAG::new(),
            sampler: DefHybridSampler::new(None, None),
        }
    }

    pub fn new(sampler: S) -> HybridModel<'a, N, S> {
        HybridModel {
            vars: HybridDAG::new(),
            sampler: sampler,
        }
    }

    /// Add a new variable to the model.
    pub fn add_var(&mut self, var: &'a <N as ContNode<'a>>::Var) -> Result<(), ()> {
        let pos = self.vars.nodes.len();
        let node = N::new(var, pos)?;
        self.vars.nodes.push(Rc::new(node));
        Ok(())
    }

    /// Adds a parent `dist` to a child `dist`, connecting both nodes directionally
    /// with an arc.
    ///
    /// Takes the distribution of a variable, and the parent variable distribution
    /// as arguments and returns a result indicating if the parent was added properly.
    /// Both variables have to be added previously to the model.
    pub fn add_parent(&mut self,
                      node: &'a <N as ContNode<'a>>::Var,
                      parent: &'a <N as ContNode<'a>>::Var,
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
            .find(|n| (&**n).get_dist() == parent)
            .cloned()
            .ok_or(())?;
        node.add_parent(Rc::downgrade(&parent.clone()), rank_cr);
        parent.add_child(node.clone());
        // check if it's a DAG and topologically sort the graph
        self.vars.topological_sort()
    }

    /// Remove a variable from the model, the childs will be disjoint if they don't
    /// have an other parent.
    pub fn remove_var(&mut self, node: &'a <N as ContNode<'a>>::Var) {
        unimplemented!()
    }

    /// Sample the model in
    pub fn sample(&self) -> Vec<Vec<HybridSamplerResult>> {
        self.sampler.clone().get_samples(self)
    }

    /// Returns the total number of variables in the model.
    pub fn var_num(&self) -> usize {
        self.vars.nodes.len()
    }

    /// Get the node in the graph at position **i** unchecked.
    pub fn get_var(&self, i: usize) -> Rc<N> {
        self.vars.get_node(i)
    }
}

impl<'a, N, S> super::IterModel for HybridModel<'a, N, S>
    where N: HybridNode<'a>,
          S: HybridSampler<'a> + Clone
{
    type Iter = NetIter<'a, N>;
    fn iter_vars(&self) -> Self::Iter {
        NetIter::new(&self.vars.nodes)
    }

    fn var_num(&self) -> usize {
        self.vars.nodes.len()
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
    _disc_dist: Option<&'a D>,
    as_cont: Option<C>,
    childs: RefCell<Vec<Rc<DefHybridNode<'a, C, D>>>>,
    parents: RefCell<Vec<Weak<DefHybridNode<'a, C, D>>>>,
    edges: RefCell<Vec<f64>>, // rank correlations assigned to edges
    pos: RefCell<usize>,
    was_discrete: bool,
}

impl<'a, C: 'a, D: 'a> Node for DefHybridNode<'a, C, D>
    where C: ContVar,
          D: DiscreteVar + AsContinuous
{
    fn get_child_unchecked(&self, pos: usize) -> Rc<Self> {
        let childs = &*self.childs.borrow();
        childs[pos].clone()
    }

    fn get_childs(&self) -> Vec<Rc<Self>> {
        let childs = &*self.childs.borrow();
        Vec::from_iter(childs.iter().cloned())
    }

    fn is_root(&self) -> bool {
        self.parents_num() == 0
    }

    fn position(&self) -> usize {
        *self.pos.borrow()
    }

    fn get_parents_positions(&self) -> Vec<usize> {
        let parents = &*self.parents.borrow();
        let mut positions = Vec::with_capacity(parents.len());
        for p in parents {
            let p = p.upgrade().unwrap();
            positions.push(*p.pos.borrow());
        }
        positions
    }

    fn get_all_ancestors(&self) -> Vec<usize> {
        let parents = &*self.parents.borrow();
        let mut anc = Vec::with_capacity(parents.len());
        for p in parents {
            let p = p.upgrade().unwrap();
            let mut ancestors = p.get_all_ancestors();
            anc.append(&mut ancestors);
            anc.push(*p.pos.borrow());
        }
        anc.sort_by(|a, b| b.cmp(a));
        anc.dedup();
        anc
    }

    fn get_childs_positions(&self) -> Vec<usize> {
        let childs = &*self.childs.borrow();
        let mut positions = Vec::with_capacity(childs.len());
        for c in childs {
            positions.push(*c.pos.borrow());
        }
        positions
    }

    fn parents_num(&self) -> usize {
        let parents = &*self.parents.borrow();
        parents.len()
    }

    fn childs_num(&self) -> usize {
        let childs = &*self.childs.borrow();
        childs.len()
    }

    fn set_position(&self, pos: usize) {
        *self.pos.borrow_mut() = pos;
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
            _disc_dist: Some(var),
            as_cont: Some(dist),
            childs: RefCell::new(vec![]),
            parents: RefCell::new(vec![]),
            edges: RefCell::new(vec![]),
            pos: RefCell::new(pos),
            was_discrete: true,
        })
    }
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
            _disc_dist: None,
            as_cont: None,
            childs: RefCell::new(vec![]),
            parents: RefCell::new(vec![]),
            edges: RefCell::new(vec![]),
            pos: RefCell::new(pos),
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
        let parents = &*self.parents.borrow();
        let mut dists = Vec::with_capacity(parents.len());
        for p in parents {
            let p = p.upgrade().unwrap();
            dists.push(p.cont_dist.unwrap());
        }
        dists
    }

    fn get_childs_dists(&self) -> Vec<&'a C> {
        let childs = &*self.childs.borrow();
        let mut dists = Vec::with_capacity(childs.len());
        for c in childs {
            dists.push(c.cont_dist.unwrap());
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

// HybridDAG:

#[derive(Debug)]
struct HybridDAG<'a, N>
    where N: HybridNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    nodes: Vec<Rc<N>>,
}

dag_impl!(HybridDAG, HybridNode; [ContVar + Normalization]);


type DefChainGraph<'a> = ChainGraph<'a,
                                    DefHybridNode<'a, DefContVar, DefDiscreteVar>,
                                    DefHybridSampler<'a>>;

/// A chain graph is composed by a set of nodes, a set of acyclic directed edges (or arcs) and/or
/// a set of undirected edges.
///
/// Bayesian networks and markov fields are both special cases of chain graphs (which can
/// contain both). This implementation will support hybrid nodes (continuous and discrete variables
/// treated as continuous variables).
///
/// Sampling of this kind of graph is done thought Markov chain Monte Carlo methods, and the
/// underlying graph is 'moralized' first (to an undirected graph), hence any edge weighting
/// information would be lost (so it's unnecessary). To avoid this problem, the use of one of
/// the other provided models is encouraged.
pub struct ChainGraph<'a, N, S>
    where N: HybridNode<'a>,
          S: HybridSampler<'a>
{
    _nlt: PhantomData<&'a ()>,
    sampler: S,
    _edges: Vec<(Rc<N>, Rc<N>)>,
    underlying: Vec<Rc<N>>,
}

impl<'a, N, S> ChainGraph<'a, N, S>
    where N: HybridNode<'a>,
          S: HybridSampler<'a>
{
    pub fn new(sampler: S) -> ChainGraph<'a, N, S> {
        ChainGraph {
            _nlt: PhantomData,
            sampler: sampler,
            _edges: vec![],
            underlying: vec![],
        }
    }

    pub fn default() -> DefChainGraph<'a> {
        use sampling::Sampler;
        ChainGraph {
            _nlt: PhantomData,
            sampler: DefHybridSampler::new(None, None),
            _edges: vec![],
            underlying: vec![],
        }
    }

    /// Add a new variable to the model.
    pub fn add_var(&mut self, var: &'a <N as ContNode<'a>>::Var) -> Result<(), ()> {
        let pos = self.underlying.len();
        let node = N::new(var, pos)?;
        self.underlying.push(Rc::new(node));
        Ok(())
    }

    /// Adds an (undirected) edge between two variables.
    ///
    /// Both variables have to be added previously to the model.
    pub fn add_edge(&mut self,
                    a: &'a <N as ContNode<'a>>::Var,
                    b: &'a <N as ContNode<'a>>::Var)
                    -> Result<(), ()> {
        let a: Rc<N> = self.underlying
            .iter()
            .find(|n| (&**n).get_dist() == a)
            .cloned()
            .ok_or(())?;
        let b: Rc<N> = self.underlying
            .iter()
            .find(|n| (&**n).get_dist() == b)
            .cloned()
            .ok_or(())?;
        self._edges.push((a, b));
        Ok(())
    }

    /// Append a Bayesian network to the model, it will be "moralized" in the process,
    /// losing all the implied causal information.
    pub fn append_dag<So: HybridSampler<'a>>(&mut self, other: HybridModel<'a, N, So>) {
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
            let mut edges: Vec<(Rc<N>, Rc<N>)> = parents.iter()
                .tuple_combinations()
                .map(|(a, b)| (self.underlying[*a].clone(), self.underlying[*b].clone()))
                .collect();
            self._edges.append(&mut edges);
        }
    }

    /// Remove a variable from the model, the childs will be disjoint if they don't
    /// have an other parent.
    pub fn remove_var(&mut self, node: &'a <N as ContNode<'a>>::Var) {
        unimplemented!()
    }

    /// Sample the model in
    pub fn sample(&self) -> Vec<Vec<HybridSamplerResult>> {
        self.sampler.clone().get_samples(self)
    }

    /// Get the node in the graph at position **i** unchecked.
    pub fn get_var(&self, i: usize) -> Rc<N> {
        self.underlying[i].clone()
    }
}

impl<'a, N, S> super::IterModel for ChainGraph<'a, N, S>
    where N: HybridNode<'a>,
          S: HybridSampler<'a> + Clone
{
    type Iter = IterCG<'a, N>;
    fn iter_vars(&self) -> IterCG<'a, N> {
        IterCG {
            _nlt: PhantomData,
            nodes: self.underlying.clone(),
            cnt: 0,
        }
    }

    fn var_num(&self) -> usize {
        self.underlying.len()
    }
}

pub struct IterCG<'a, N>
    where N: HybridNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    nodes: Vec<Rc<N>>,
    cnt: usize,
}

impl<'a, N> ::std::iter::Iterator for IterCG<'a, N>
    where N: HybridNode<'a>
{
    type Item = Rc<N>;

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
