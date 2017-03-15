use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::rc::{Rc, Weak};

use super::{Node, BayesNet, DiscreteDist, Observation, NetIter, DType};
use sampling::{DiscreteSampler, DefaultSampler};
use dists::{Categorical, Binomial};
use P;

pub struct DiscreteModel<'a, D: 'a, O: 'a, S>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation,
          S: DiscreteSampler<D, O> + Clone
{
    vars: BayesNet<'a, D, O, DiscreteNode<'a, D, O>>,
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
            sampler: DefaultSampler::new(None, None),
        }
    }

    pub fn with_sampler(&mut self, sampler: S) {
        self.sampler = sampler;
    }

    /// Sample the model in
    pub fn sample(&self) -> Vec<Vec<u8>> {
        let sampler = self.sampler.clone();
        sampler.get_samples(self)
    }

    /// Iterate the model variables in topographical order.
    pub fn iter_vars<'b>(&'b self) -> NetIter<'a, 'b, D, DiscreteNode<'a, D, O>> {
        self.vars.iter_vars()
    }

    /// Returns the total number of variables in the model.
    pub fn var_num(&self) -> usize {
        self.vars.nodes.len()
    }

    /// Add a new variable to the model.
    pub fn add_var(&mut self, var: &'a D) -> Result<(), ()> {
        let pos = self.vars.nodes.len();
        let node = DiscreteNode::new(var, pos)?;
        self.vars.nodes.push(Rc::new(node));
        Ok(())
    }

    /// Adds a parent `dist` to a child `dist`, connecting both nodes directionally.
    ///
    /// Takes the distribution of a variable, and the parent variable distribution
    /// as arguments and returns a result indicating if the parent was added properly.
    /// Both variables have to be added previously to the model.
    ///
    /// A third argument is also necessary, which is the conditional probability table
    /// for the the child given the parent value.
    ///
    /// This has N x K dimensions, being K the number of caregories of the discrete
    /// variable. N can take two values:
    /// -   the number of categories of the parent.
    /// -   the combination of categories of the different parents.
    ///
    /// This is an important point, because if all the necessary values are not provided
    /// the rest will be guessed using a simple proportional linear model. This is often
    /// not the case in any realistic or complex model and is desirable to provide all
    /// the necessary values when constructing the net.
    ///
    /// ## Example
    /// Assume variable C(k=2) has an existing parent A(k=3) in the model and we add a second,
    /// B(k=3), so we need to pass the CPT corresponding to the joint distribution P(C|A,B)
    /// which would have dimensions 9*2 (Ka*Kb = 9 which are all the possible combinations of
    /// values taken by parents at a given time).
    ///
    /// In case the provided CPT was of 3*2 dimensions, those values will be taken and combined
    /// proportionally with the existing CPT (of 3*2 dimensions) to derive all the necessary
    /// values.
    ///
    /// In both cases each row has to sum up to 1.
    ///
    /// More information on how to build in the [CPT]() type page.
    pub fn add_parent_to_var(&mut self, node: &D, parent: &D, prob: CPT) -> Result<(), ()> {
        // checks to perform:
        //  - both exist in the model
        //  - the theoretical child cannot be a parent of the theoretical parent
        //    as the network is a DAG
        // find node and parents in the net
        let node: Rc<DiscreteNode<_, _>> = self.vars
            .nodes
            .iter()
            .find(|n| (&**n).dist == node)
            .cloned()
            .ok_or(())?;
        let parent: Rc<DiscreteNode<_, _>> = self.vars
            .nodes
            .iter()
            .find(|n| (&**n).dist == parent)
            .cloned()
            .ok_or(())?;
        // check child is not parent of parent already:

        let parent_k = parent.dist.k_num();
        let parents = &mut *node.parents.borrow_mut();
        parents.push((parent_k, Rc::downgrade(&parent.clone())));
        let parent_childs = &mut *parent.childs.borrow_mut();
        parent_childs.push(parent.clone());
        // make CPT for child
        if node.build_cpt(prob, parent_k as usize).is_err() {
            Err(())
        } else {
            Ok(())
        }
    }
}

type Choices = Vec<u8>;

/// Conditional probability table for a child event in the network.
#[derive(Debug, Clone)]
pub struct CPT {
    data: Vec<Vec<P>>,
    indexes: Vec<Choices>,
}

impl CPT {
    pub fn dim_num(&self) -> [usize; 2] {
        let rows = self.data.len();
        let cols = self.data[0].len();
        [rows, cols]
    }

    /// Takes an slice of vectors with dimensions n-rows x k-columns,
    /// and its corresponding zero-indexed indexes vector.
    ///
    /// N = combination of categories for the parents of the variable.
    /// K = number of categories for the variable.
    ///
    /// ## Example
    /// With n=3, k=2:
    /// elements:[[0.5, 0.5], [0.7, 0.3], [0.2, 0.8]]
    /// indexes: [[0], [1], [2]]
    ///
    /// This would be for a child with a binomial distribution (k=2) with
    /// a single parent with 3 categories (k=n=3).
    ///
    /// A child(k=2) with 2 parents with two categories each would have n=4 (k1=2 x k2=2),
    /// and a CPT matrix of 4x2 dimensions.
    pub fn new(elements: Vec<Vec<P>>, indexes: Vec<Choices>) -> Result<CPT, ()> {
        let row_n = elements.len();
        if row_n < 2 {
            return Err(());
        }

        let column_n = elements[0].len();
        if column_n < 2 {
            return Err(());
        }

        if indexes.len() != elements.len() {
            return Err(());
        }

        for (i, row) in elements.iter().enumerate() {
            let r_len = row.len();
            if r_len != column_n {
                return Err(());
            }
            if indexes[i].len() != r_len {
                return Err(())
            }
        }

        Ok(CPT {
            data: elements,
            indexes: indexes,
        })
    }
}

type KSizedParent<'a, D, O> = (u8, Weak<DiscreteNode<'a, D, O>>);

/// A node in the network representing a random variable.
///
/// This type cannot be instantiated directly, instead add the random variable
/// distribution to the network.
pub struct DiscreteNode<'a, D: 'a, O: 'a>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation
{
    _obtype: PhantomData<O>,
    pub dist: &'a D,
    childs: RefCell<Vec<Rc<DiscreteNode<'a, D, O>>>>,
    cpt: RefCell<HashMap<Choices, DType>>,
    parents: RefCell<Vec<KSizedParent<'a, D, O>>>, // (categ, parent_ptr)
    pub pos: usize, // position in the bayes net vec of self
}

impl<'a, D: 'a, O: 'a> DiscreteNode<'a, D, O>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation
{
    fn new(dist: &'a D, pos: usize) -> Result<DiscreteNode<'a, D, O>, ()> {
        match *dist.dist_type() {
            DType::Categorical(_) |
            DType::Binomial(_) => {}
            _ => return Err(()),
        }
        // get the probabilities from the dist and insert as default cpt
        Ok(DiscreteNode {
            _obtype: PhantomData,
            dist: dist,
            childs: RefCell::new(vec![]),
            parents: RefCell::new(vec![]),
            pos: pos,
            cpt: RefCell::new(HashMap::new()),
        })
    }

    /// Returns the node parents positions in the network.
    pub fn get_parents_positions(&self) -> Vec<usize> {
        let parents = &*self.parents.borrow();
        let mut positions = Vec::with_capacity(parents.len());
        for &(_, ref p) in parents {
            let p = p.upgrade().unwrap();
            positions.push(p.pos);
        }
        positions
    }

    /// Returns the node childs positions in the network.
    pub fn get_childs_positions(&self) -> Vec<usize> {
        let childs = &*self.childs.borrow();
        let mut positions = Vec::with_capacity(childs.len());
        for c in childs {
            positions.push(c.pos);
        }
        positions
    }

    /// Takes an slice reprensenting the parents categories at the current
    /// time **t** and draws a sample based on the corresponding probabilities.
    pub fn draw_sample(&self, values: &[u8]) -> u8 {
        let cpt = &*self.cpt.borrow();
        let probs = cpt.get(values).unwrap();
        match *probs {
            DType::Binomial(ref dist) => dist.sample(),
            DType::Categorical(ref dist) => dist.sample(),
            _ => panic!(),
        }
    }

    /// Returns the number of parents this node has.
    #[inline]
    pub fn parents_num(&self) -> usize {
        let parents = &*self.parents.borrow();
        parents.len()
    }

    /// Returns a reference to the distributions of the parents·
    pub fn get_parents_dists(&self) -> Vec<&D> {
        let parents = &*self.parents.borrow();
        let mut dists = Vec::with_capacity(parents.len());
        for &(_, ref p) in parents {
            let p = p.upgrade().unwrap();
            dists.push(p.dist);
        }
        dists
    }

    fn build_cpt(&self, probs: CPT, parent_k: usize) -> Result<(), String> {
        use itertools::zip;

        let parents = &*self.parents.borrow();
        let rows1 = probs.dim_num()[0];
        let rows0 = parents.iter().fold(1, |t, &(i, _)| t * (i as usize));
        if (rows1 != rows0) && rows1 != parent_k {
            return Err("insufficient number of probability rows in the CPT".to_string());
        } else if rows1 == parent_k {
            unimplemented!()
        }

        let mut cpt = HashMap::new();
        let CPT { indexes, data } = probs;
        for (idx, prob) in zip(indexes.into_iter(), data.into_iter()) {
            let dist = if prob.len() == 2 {
                // binomial
                DType::Binomial(Binomial::new(prob[0])?)
            } else {
                // n-categorical
                DType::Categorical(Categorical::new(prob.to_vec())?)
            };
            cpt.insert(idx, dist);
        }

        let mut self_cpt = self.cpt.borrow_mut();
        *self_cpt = cpt;
        Ok(())
    }
}

impl<'a, D: 'a, O: 'a> Node<D, Self> for DiscreteNode<'a, D, O>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation
{
    fn get_dist(&self) -> &D {
        self.dist
    }

    fn get_child(&self, pos: usize) -> &Self {
        let childs = &*self.childs.borrow();
        unsafe { &*(&*childs[pos] as *const Self) }
    }

    fn get_childs(&self) -> Vec<&Self> {
        let childs = &*self.childs.borrow();
        Vec::from_iter(childs.iter().map(|x| unsafe { &*(&**x as *const Self) }))
    }

    fn is_root(&self) -> bool {
        self.parents_num() == 0
    }
}
