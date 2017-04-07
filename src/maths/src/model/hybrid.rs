use std::cell::RefCell;
use std::collections::{VecDeque, HashSet};
use std::iter::{FromIterator, Iterator};
use std::rc::{Rc, Weak};
use std::marker::PhantomData;

use RGSLRng;
use super::{Node, ContVar, DiscreteVar, ContNode};
use super::{DType, DefContVar, DefDiscreteVar};
use sampling::{HybridSampler, DefHybridSampler, HybridSamplerResult};
use dists::{Gaussianization, AsContinuous};

pub trait HybridNode<'a>: ContNode<'a>
    where Self: Sized
{
    type Discrete: DiscreteVar + AsContinuous;
    fn new_with_discrete(var: &'a Self::Discrete, pos: usize) -> Result<Self, ()>;
}

pub struct HybridModel<'a, N: 'a, S>
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
    pub fn new(init: &'a <N as ContNode<'a>>::Var,
               sampler: S)
               -> Result<HybridModel<'a, N, S>, ()> {
        let init = N::new(init, 0)?;
        let dag = HybridDAG::new(init);
        Ok(HybridModel {
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
    fn with_var<Nd: 'a>(dist: &'a <Nd as ContNode<'a>>::Var, pos: usize) -> Result<Nd, ()>
        where Nd: ContNode<'a>
    {
        let node = Nd::new(dist, pos)?;
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
    pub fn sample(&self) -> Vec<Vec<HybridSamplerResult>> {
        self.sampler.clone().get_samples(self)
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

/// A node in the network representing a random variable.
///
/// This type shouldn't be instantiated directly, instead add the random variable
/// distribution to the network.
pub struct DefHybridNode<'a, C: 'a, D: 'a>
    where C: ContVar,
          D: DiscreteVar + AsContinuous
{
    cont_dist: Option<&'a C>,
    disc_dist: Option<&'a D>,
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

    fn get_childs_positions(&self) -> Vec<usize> {
        let childs = &*self.childs.borrow();
        let mut positions = Vec::with_capacity(childs.len());
        for c in childs {
            positions.push(*c.pos.borrow());
        }
        positions
    }

    #[inline]
    fn parents_num(&self) -> usize {
        let parents = &*self.parents.borrow();
        parents.len()
    }

    #[inline]
    fn childs_num(&self) -> usize {
        let childs = &*self.childs.borrow();
        childs.len()
    }

    fn set_position(&self, pos: usize) {
        *self.pos.borrow_mut() = pos;
    }
}

impl<'a, C: 'a, D: 'a> HybridNode<'a> for DefHybridNode<'a, C, D>
    where C: ContVar + Gaussianization,
          D: DiscreteVar + AsContinuous
{
    type Discrete = D;
    fn new_with_discrete(var: &'a Self::Discrete, pos: usize) -> Result<Self, ()> {
        let dist = var.as_continuous()?;
        Ok(DefHybridNode {
            cont_dist: None,
            disc_dist: Some(var),
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
    where C: ContVar + Gaussianization,
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

struct HybridDAG<'a, N>
    where N: HybridNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    nodes: Vec<Rc<N>>,
}

impl<'a, N> HybridDAG<'a, N>
    where N: HybridNode<'a>
{
    fn new(init: N) -> HybridDAG<'a, N> {
        HybridDAG {
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

    #[inline]
    fn get_node(&self, pos: usize) -> Rc<N> {
        self.nodes[pos].clone()
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
    fn new<'a, N>(graph: &HybridDAG<'a, N>) -> DirectedCycle
        where N: HybridNode<'a>
    {
        DirectedCycle {
            marked: vec![false; graph.nodes.len()],
            on_stack: vec![false; graph.nodes.len()],
            edge_to: Vec::from_iter(0..graph.nodes.len()),
            cycle: Vec::with_capacity(graph.nodes.len()),
            sorted: Vec::with_capacity(graph.nodes.len()),
        }
    }

    fn dfs<'a, N>(&mut self, graph: &HybridDAG<'a, N>, v: usize)
        where N: HybridNode<'a>
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
        }
        self.on_stack[v] = false;
        self.sorted.push(v);
    }

    fn has_cycle(&self) -> bool {
        !self.cycle.is_empty()
    }
}

/// Bayesian Network iterator, visits all nodes from parents to childs
pub struct NetIter<'a, N>
    where N: HybridNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    unvisitted: VecDeque<Rc<N>>,
    processed: HashSet<usize>,
    queued: Rc<N>,
    childs_visitted: usize,
}

impl<'a, N> NetIter<'a, N>
    where N: HybridNode<'a>
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

impl<'a, N> Iterator for NetIter<'a, N>
    where N: HybridNode<'a>
{
    type Item = Rc<N>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            while self.childs_visitted < self.queued.get_childs().len() {
                let next = self.queued.get_child_unchecked(self.childs_visitted);
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
