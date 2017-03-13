use std::collections::{VecDeque, HashSet};
use std::hash::Hash;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::rc::{Rc, Weak};

use ndarray::{Array2, Array1};

use super::{Distribution, Observation};
use P;

pub struct BayesNet<'a, D: 'a, O: 'a>
    where D: Distribution + Hash + Eq,
          O: Observation
{
    _disttype: PhantomData<D>,
    _obtype: PhantomData<O>,
    pub nodes: Vec<Node<'a, D, O>>,
}

impl<'a, D, O> BayesNet<'a, D, O>
    where D: Distribution + Hash + Eq,
          O: Observation
{
    pub fn new(vars: &'a [D]) -> BayesNet<'a, D, O> {
        BayesNet {
            _disttype: PhantomData,
            _obtype: PhantomData,
            nodes: vec![],
        }
    }

    pub fn iter_vars<'b>(&'b self) -> NetIter<'a, 'b, D, O> {
        NetIter::new(&self.nodes)
    }
}

pub struct Node<'a, D: 'a, O: 'a>
    where D: Distribution + Hash + Eq,
          O: Observation
{
    _obtype: PhantomData<O>,
    pub dist: &'a D,
    childs: Vec<Rc<Node<'a, D, O>>>,
    parents: Vec<(Array1<P>, Weak<Node<'a, D, O>>)>,
    pub pos: usize, // position in the bayes net vec of self
    pub cpt: Array2<P>,
}

impl<'a, D: 'a, O: 'a> Node<'a, D, O>
    where D: Distribution + Hash + Eq,
          O: Observation
{
    pub fn new(dist: &'a D, pos: usize) -> Node<'a, D, O> {
        Node {
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

/// Bayesian Network iterator, visits all nodes from parents to childs
pub struct NetIter<'a: 'b, 'b, D: 'a, O: 'a>
    where D: Distribution + Hash + Eq,
          O: Observation
{
    unvisitted: VecDeque<&'b Node<'a, D, O>>,
    processed: HashSet<&'a D>,
    queued: &'b Node<'a, D, O>,
    childs_visitted: usize,
}

impl<'a, 'b, D, O> NetIter<'a, 'b, D, O>
    where D: Distribution + Hash + Eq,
          O: Observation
{
    fn new(nodes: &'b [Node<'a, D, O>]) -> NetIter<'a, 'b, D, O> {
        let mut unvisitted = VecDeque::from_iter(nodes);
        let mut processed = HashSet::new();
        let first = unvisitted.pop_front().unwrap();
        processed.insert(first.dist);
        NetIter {
            unvisitted: unvisitted,
            processed: processed,
            queued: first,
            childs_visitted: 0,
        }
    }
}

impl<'a, 'b, D, O> ::std::iter::Iterator for NetIter<'a, 'b, D, O>
    where O: Observation,
          D: Distribution + Hash + Eq
{
    type Item = &'b Node<'a, D, O>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            while self.childs_visitted < self.queued.childs.len() {
                let next = self.queued.childs.get(self.childs_visitted).unwrap();
                self.childs_visitted += 1;
                if !self.processed.contains(next.dist) {
                    return Some(next);
                }
            }

            if self.unvisitted.is_empty() {
                return None;
            } else {
                // add all previously visitted to the list of processed
                for e in &self.queued.childs {
                    self.processed.insert(e.dist);
                }
                let next = self.unvisitted.pop_front().unwrap();
                self.queued = next;
            }
        }
    }
}
