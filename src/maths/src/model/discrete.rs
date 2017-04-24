use std::collections::{VecDeque, HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::iter::{FromIterator, Iterator};
use std::sync::{Arc, Weak, RwLock};
use std::marker::PhantomData;

use uuid::Uuid;

use RGSLRng;
use super::{Node, Variable, Observation};
use super::{DType, Discrete};
use sampling::DiscMargSampler;
use dists::{Categorical, Bernoulli};
use err::ErrMsg;

// public traits for models:

/// Node trait for a discrete model.
pub trait DiscreteNode<'a>: Node + Sized {
    type Var: 'a + DiscreteVar;

    fn new(dist: &'a Self::Var, pos: usize) -> Result<Self, ()>;

    /// Return the distribution of a node.
    fn get_dist(&self) -> &'a Self::Var;

    /// Returns a reference to the distributions of the parents·
    fn get_parents_dists(&self) -> Vec<&'a Self::Var>;

    /// Returns a reference to the distributions of the childs·
    fn get_childs_dists(&self) -> Vec<&'a Self::Var>;

    /// Takes an slice reprensenting the parents categories at the current
    /// state *t* and draws a sample based on the corresponding probabilities.
    fn draw_sample(&self, rng: &mut RGSLRng, values: &[u8]) -> u8;

    /// Sample from the prior distribution, usually called on roots of the tree
    /// to initialize each sampling steep.
    fn init_sample(&self, rng: &mut RGSLRng) -> u8;

    /// Add a new parent to this node. Does not add self as child implicitly!
    fn add_parent(&self, parent: Arc<Self>);

    /// Add a child to this node. Does not add self as parent implicitly!
    fn add_child(&self, child: Arc<Self>);

    fn build_cpt(&self, probabilities: CPT, k: usize) -> Result<(), String>;
}

pub trait DiscreteVar: Variable {
    type Event: Observation;

    /// Returns the number of categories for this discrete event
    fn k_num(&self) -> u8;

    /// Returns a sample from the original variable, not talking into account
    /// the parents in the network.
    fn sample(&self, rng: &mut RGSLRng) -> u8;

    /// Returns an slice of known observations for the variable of
    /// this distribution.
    fn get_observations(&self) -> &[Self::Event];

    /// Push a new observation of the measured event/variable to the stack of obserbations.
    fn push_observation(&mut self, obs: Self::Event);
}

pub type DefDiscreteModel<'a> = DiscreteModel<'a, DefDiscreteNode<'a, DefDiscreteVar>>;

#[derive(Debug)]
pub struct DiscreteModel<'a, N>
    where N: DiscreteNode<'a>
{
    vars: DiscreteDAG<'a, N>,
}

impl<'a, N> DiscreteModel<'a, N>
    where N: DiscreteNode<'a>
{
    pub fn new() -> DiscreteModel<'a, N> {
        DiscreteModel { vars: DiscreteDAG::new() }
    }

    /// Add a new variable to the model.
    pub fn add_var(&mut self, var: &'a <N as DiscreteNode<'a>>::Var) -> Result<(), ()> {
        let pos = self.vars.nodes.len();
        let node = N::new(var, pos)?;
        self.vars.nodes.push(Arc::new(node));
        Ok(())
    }

    /// Adds a parent `dist` to a child `dist`, connecting both nodes directionally
    /// with an arc.
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
    /// ## Example
    /// Assume variable C(k=2) has an existing parent A(k=3) in the model and we add a second,
    /// B(k=3), so we need to pass the CPT corresponding to the joint distribution P(C|A,B)
    /// which would have dimensions 9*2 (Ka*Kb = 9 which are all the possible combinations of
    /// values taken by parents at a given time). Each row has to sum up to 1.
    ///
    /// More information on how to build in the [CPT]() type page.
    pub fn add_parent(&mut self,
                      node: &'a <N as DiscreteNode<'a>>::Var,
                      parent: &'a <N as DiscreteNode<'a>>::Var,
                      prob: CPT)
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

        let parent_k = parent.get_dist().k_num();
        // make CPT for child
        if node.build_cpt(prob, parent_k as usize).is_err() {
            return Err(());
        }
        node.add_parent(parent.clone());
        parent.add_child(node.clone());
        // check if it's a DAG and topologically sort the graph
        self.vars.topological_sort()
    }

    /// Remove a variable from the model, the childs will be disjoint if they don't
    /// have an other parent.
    pub fn remove_var(&mut self, node: &'a <N as DiscreteNode<'a>>::Var) {
        unimplemented!()
    }

    /// Sample for the model marginal probabilities with the current elicited probabilities.
    pub fn sample_marginals<S: DiscMargSampler>(&self, sampler: S) -> Vec<Vec<u8>> {
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
struct DiscreteDAG<'a, N>
    where N: DiscreteNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    nodes: Vec<Arc<N>>,
}

dag_impl!(DiscreteDAG, DiscreteNode; [DiscreteVar]);

type Choices = Vec<u8>;

/// Conditional probability table for a child event in the network.
#[derive(Debug, Clone)]
pub struct CPT {
    data: Vec<Vec<f64>>,
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
    pub fn new(elements: Vec<Vec<f64>>, indexes: Vec<Choices>) -> Result<CPT, ()> {
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

        for row in &elements {
            let r_len = row.len();
            if r_len != column_n {
                return Err(());
            }
        }

        Ok(CPT {
               data: elements,
               indexes: indexes,
           })
    }
}

/// A node in the network representing a discrete random variable.
///
/// This type shouldn't be instantiated directly, instead add the random variable
/// distribution to the network.
pub struct DefDiscreteNode<'a, V: 'a>
    where V: DiscreteVar
{
    pub dist: &'a V,
    childs: RwLock<Vec<Weak<DefDiscreteNode<'a, V>>>>,
    cpt: RwLock<HashMap<Choices, DType>>,
    parents: RwLock<Vec<Arc<DefDiscreteNode<'a, V>>>>, // (categ, parent_ptr)
    pos: RwLock<usize>, // position in the bayes net vec of self
}

impl<'a, V: 'a + DiscreteVar> ::std::fmt::Debug for DefDiscreteNode<'a, V> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f,
               "DefDiscreteNode {{ dist: {d:?}, childs: {c}, parents: {p}, pos: {pos} }}",
               c = self.childs.read().unwrap().len(),
               p = self.parents.read().unwrap().len(),
               pos = *self.pos.read().unwrap(),
               d = self.dist)
    }
}

node_impl!(DefDiscreteNode, DiscreteVar);

impl<'a, V: 'a> DiscreteNode<'a> for DefDiscreteNode<'a, V>
    where V: DiscreteVar
{
    type Var = V;

    fn new(dist: &'a V, pos: usize) -> Result<Self, ()> {
        match *dist.dist_type() {
            DType::Categorical(_) |
            DType::Bernoulli(_) => {}
            _ => return Err(()),
        }

        // get the probabilities from the dist and insert as default cpt
        Ok(DefDiscreteNode {
               dist: dist,
               childs: RwLock::new(vec![]),
               parents: RwLock::new(vec![]),
               pos: RwLock::new(pos),
               cpt: RwLock::new(HashMap::new()),
           })
    }

    fn get_dist(&self) -> &'a V {
        self.dist
    }

    fn draw_sample(&self, rng: &mut RGSLRng, values: &[u8]) -> u8 {
        let cpt = &*self.cpt.read().unwrap();
        let probs = cpt.get(values).unwrap();
        match *probs {
            DType::Bernoulli(ref dist) => dist.sample(rng),
            DType::Categorical(ref dist) => dist.sample(rng),
            ref d => panic!(ErrMsg::ContDistDiscNode.panic_msg_with_arg(d)),
        }
    }

    fn init_sample(&self, rng: &mut RGSLRng) -> u8 {
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

    fn add_parent(&self, parent: Arc<Self>) {
        let parents = &mut *self.parents.write().unwrap();
        // check for duplicates:
        let pos = parents
            .iter()
            .enumerate()
            .find(|&(_, x)| &*x.get_dist() == parent.get_dist())
            .map(|(i, _)| i);
        if let None = pos {
            parents.push(parent);
        };
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

    fn build_cpt(&self, probs: CPT, parent_k: usize) -> Result<(), String> {
        use itertools::zip;

        let parents = &*self.parents.read().unwrap();
        let rows1 = probs.dim_num()[0];
        let rows0 = parents
            .iter()
            .fold(1, |t, p| {
                let i = {
                    p.dist.k_num()
                };
                t * (i as usize)
            }) * parent_k;
        if ((rows0 > parent_k) && (rows1 != rows0)) && (parent_k != rows1) {
            return Err("insufficient number of probability rows in the CPT".to_string());
        }

        let mut cpt = HashMap::new();
        let mut sum = 0.;
        let CPT { indexes, data } = probs;
        for (idx, prob) in zip(indexes.into_iter(), data.into_iter()) {
            sum += prob.iter().fold(0_f64, |t, &p| t + p);
            let dist = if prob.len() == 2 {
                // 2 categories
                DType::Bernoulli(Bernoulli::new(prob[1])?)
            } else {
                // n-categorical
                DType::Categorical(Categorical::new(prob.to_vec())?)
            };
            cpt.insert(idx, dist);
        }
        if sum < 1.0 {
            return Err("Conditional probability table must sum to 1".to_string());
        }
        let mut self_cpt = self.cpt.write().unwrap();
        *self_cpt = cpt;
        Ok(())
    }
}

#[derive(Debug)]
pub struct DefDiscreteVar {
    dist: DType,
    observations: Vec<Discrete>,
    id: Uuid,
}

fn validate_dist(dist: &DType) -> Result<(), ()> {
    match *dist {
        DType::Bernoulli(_) |
        DType::Categorical(_) |
        DType::Poisson => Ok(()),
        _ => Err(()),
    }
}

var_impl!(DefDiscreteVar);

impl DiscreteVar for DefDiscreteVar {
    type Event = Discrete;

    fn k_num(&self) -> u8 {
        match self.dist {
            DType::Categorical(ref dist) => dist.k_num(),
            DType::Bernoulli(_) => 2,
            ref d => panic!(ErrMsg::ContDistDiscNode.panic_msg_with_arg(d)),
        }
    }

    fn sample(&self, rng: &mut RGSLRng) -> u8 {
        match self.dist {
            DType::Categorical(ref dist) => dist.sample(rng),
            DType::Bernoulli(ref dist) => dist.sample(rng),
            ref d => panic!(ErrMsg::ContDistDiscNode.panic_msg_with_arg(d)),
        }
    }

    fn get_observations(&self) -> &[<Self as DiscreteVar>::Event] {
        &self.observations
    }

    fn push_observation(&mut self, obs: Self::Event) {
        self.observations.push(obs)
    }
}

use dists::AsContinuous;

impl AsContinuous for DefDiscreteVar {}

