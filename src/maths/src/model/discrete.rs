use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::{Rc, Weak};

use ndarray::{Array2, Array1};

use super::{Node, BayesNet, Distribution, DiscreteDist, Observation, NetIter};
use sampling::{DiscreteSampler, DefaultSampler};
use P;

pub struct DiscreteModel<'a, D: 'a, O: 'a, S>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation,
          S: DiscreteSampler<D, O> + Clone
{
    vars: BayesNet<D, O, DiscreteNode<'a, D, O>>,
    sampler: S,
    _obtype: PhantomData<O>,
}

impl<'a, D: 'a, O: 'a, S> DiscreteModel<'a, D, O, S>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation,
          S: DiscreteSampler<D, O> + Clone
{
    pub fn new() -> DiscreteModel<'a, D, O, DefaultSampler<D, O>> {
        DiscreteModel {
            _obtype: PhantomData,
            vars: BayesNet::new(),
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

    pub fn iter_vars(&self) -> NetIter<D, DiscreteNode<D, O>> {
        self.vars.iter_vars()
    }

    pub fn var_numb(&self) -> usize {
        self.vars.nodes.len()
    }

    pub fn add_node(&mut self, dist: &'a D) {
        let pos = self.vars.nodes.len();
        let node = DiscreteNode::new(dist, pos);
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

pub struct DiscreteNode<'a, D: 'a, O: 'a>
    where D: Distribution + Hash + Eq,
          O: Observation
{
    _obtype: PhantomData<O>,
    pub dist: &'a D,
    childs: Vec<Rc<DiscreteNode<'a, D, O>>>,
    parents: Vec<(Array1<P>, Weak<DiscreteNode<'a, D, O>>)>,
    pub pos: usize, // position in the bayes net vec of self
    pub cpt: Array2<P>,
}

impl<'a, D: 'a, O: 'a> Node<D, Self> for DiscreteNode<'a, D, O>
    where D: Distribution + Hash + Eq,
          O: Observation
{
    fn get_dist(&self) -> &D {
        unimplemented!()
    }

    fn get_child(&self, pos: usize) -> &Self {
        unimplemented!()
    }

    fn get_childs(&self) -> &[Self] {
        unimplemented!()
    }
}

impl<'a, D: 'a, O: 'a> DiscreteNode<'a, D, O>
    where D: Distribution + Hash + Eq,
          O: Observation
{
    pub fn new(dist: &'a D, pos: usize) -> DiscreteNode<'a, D, O> {
        DiscreteNode {
            _obtype: PhantomData,
            dist: dist,
            childs: vec![],
            parents: vec![],
            pos: pos,
            cpt: Array2::zeros((1, 1)),
        }
    }

    pub fn get_parents_positions(&self) -> Vec<usize> {
        let mut positions = Vec::with_capacity(self.parents.len());
        for &(_, ref p) in &self.parents {
            let p = p.upgrade().unwrap();
            positions.push(p.pos);
        }
        positions
    }

    pub fn get_childs_positions(&self) -> Vec<usize> {
        let mut positions = Vec::with_capacity(self.childs.len());
        for c in &self.childs {
            positions.push(c.pos);
        }
        positions
    }

    #[inline]
    pub fn parents_len(&self) -> usize {
        self.parents.len()
    }

    pub fn get_parents_dists(&self) -> Vec<&D> {
        let mut dists = Vec::with_capacity(self.parents.len());
        for &(_, ref p) in &self.parents {
            let p = p.upgrade().unwrap();
            dists.push(p.dist);
        }
        dists
    }

    /// Takes an slice reprensenting the parents category choices at the current
    /// time **t** and returns a category choice based on the probabilities.
    pub fn get_categ(&self, values: &[u8]) -> u8 {
        use ndarray::Axis;
        for (i, v) in values.iter().enumerate() {
            let p_choices = self.parents[i].0.len_of(Axis(0));
        }
        0
    }
}
