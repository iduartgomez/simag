use std::cell::RefCell;
use std::collections::{VecDeque, HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::iter::{FromIterator, Iterator};
use std::rc::{Rc, Weak};
use std::marker::PhantomData;

use uuid::Uuid;

use RGSLRng;
use super::{Node, Variable, Observation};
use super::{DType, Discrete};
use sampling::{DiscreteSampler, DefDiscreteSampler};
use dists::{Categorical, Binomial};

// public traits for models:

/// Node trait for a discrete model.
pub trait DiscreteNode<'a>: Node
    where Self: Sized
{
    type Var: 'a + DiscreteVar;

    fn new(dist: &'a Self::Var, pos: usize) -> Result<Self, ()>;

    /// Return the distribution of a node.
    fn get_dist(&self) -> &'a Self::Var;

    /// Returns a reference to the distributions of the parents·
    fn get_parents_dists(&self) -> Vec<&'a Self::Var>;

    /// Returns a reference to the distributions of the childs·
    fn get_childs_dists(&self) -> Vec<&'a Self::Var>;

    /// Takes an slice reprensenting the parents categories at the current
    /// time **t** and draws a sample based on the corresponding probabilities.
    fn draw_sample(&self, rng: &mut RGSLRng, values: &[u8]) -> u8;

    /// Sample from the prior distribution, usually called on roots of the tree
    /// to initialize each sampling steep.
    fn init_sample(&self, rng: &mut RGSLRng) -> u8;

    /// Add a new parent to this node. Does not add self as child implicitly!
    fn add_parent(&self, parent: Weak<Self>);

    /// Add a child to this node. Does not add self as parent implicitly!
    fn add_child(&self, child: Rc<Self>);

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

pub struct DiscreteModel<'a, N: 'a, S>
    where N: DiscreteNode<'a>,
          S: DiscreteSampler + Clone
{
    vars: DiscreteDAG<'a, N>,
    sampler: S,
}

pub type DefDiscreteModel<'a> = DiscreteModel<'a,
                                              DefDiscreteNode<'a, DefDiscreteVar>,
                                              DefDiscreteSampler>;

impl<'a, N, S> DiscreteModel<'a, N, S>
    where N: DiscreteNode<'a>,
          S: DiscreteSampler + Clone
{
    pub fn new(init: &'a <N as DiscreteNode<'a>>::Var,
               sampler: S)
               -> Result<DiscreteModel<'a, N, S>, ()> {
        let init = N::new(init, 0)?;
        let dag = DiscreteDAG::new(init);
        Ok(DiscreteModel {
            vars: dag,
            sampler: sampler,
        })
    }

    /// Add a new variable to the model.
    pub fn add_var(&mut self, var: &'a <N as DiscreteNode<'a>>::Var) -> Result<(), ()> {
        let pos = self.vars.nodes.len();
        let node: N = Self::with_var(var, pos)?;
        self.vars.nodes.push(Rc::new(node));
        Ok(())
    }

    /// Make a new orphan node from a discrete random variable (not added to the model
    /// automatically).
    fn with_var<No: 'a>(dist: &'a <No as DiscreteNode<'a>>::Var, pos: usize) -> Result<No, ()>
        where No: DiscreteNode<'a>
    {
        let node = No::new(dist, pos)?;
        Ok(node)
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
    /// ## Example
    /// Assume variable C(k=2) has an existing parent A(k=3) in the model and we add a second,
    /// B(k=3), so we need to pass the CPT corresponding to the joint distribution P(C|A,B)
    /// which would have dimensions 9*2 (Ka*Kb = 9 which are all the possible combinations of
    /// values taken by parents at a given time). Each row has to sum up to 1.
    ///
    /// More information on how to build in the [CPT]() type page.
    pub fn add_parent_to_var(&mut self,
                             node: &'a <N as DiscreteNode<'a>>::Var,
                             parent_d: &'a <N as DiscreteNode<'a>>::Var,
                             prob: CPT)
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

        let parent_k = parent.get_dist().k_num();
        // make CPT for child
        if node.build_cpt(prob, parent_k as usize).is_err() {
            return Err(());
        }
        node.add_parent(Rc::downgrade(&parent.clone()));
        parent.add_child(node.clone());
        // check if it's a DAG and topologically sort the graph
        self.vars.topological_sort()
    }

    /// Remove a variable from the model, the childs will be disjoint if they don't
    /// have an other parent.
    pub fn remove_var(&mut self, _node: &'a <N as DiscreteNode<'a>>::Var) {
        unimplemented!()
    }

    /// Sample the model in
    pub fn sample(&self) -> Vec<Vec<u8>> {
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

struct DiscreteDAG<'a, N>
    where N: DiscreteNode<'a>
{
    _nlt: PhantomData<&'a ()>,
    nodes: Vec<Rc<N>>,
}

dag_impl!(DiscreteDAG, DiscreteNode);

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
/// This type cannot be instantiated directly, instead add the random variable
/// distribution to the network.
pub struct DefDiscreteNode<'a, V: 'a>
    where V: DiscreteVar
{
    pub dist: &'a V,
    childs: RefCell<Vec<Rc<DefDiscreteNode<'a, V>>>>,
    cpt: RefCell<HashMap<Choices, DType>>,
    parents: RefCell<Vec<Weak<DefDiscreteNode<'a, V>>>>, // (categ, parent_ptr)
    pos: RefCell<usize>, // position in the bayes net vec of self
}

node_impl!(DefDiscreteNode, DiscreteVar);

impl<'a, V: 'a> DiscreteNode<'a> for DefDiscreteNode<'a, V>
    where V: DiscreteVar
{
    type Var = V;

    fn new(dist: &'a V, pos: usize) -> Result<Self, ()> {
        match *dist.dist_type() {
            DType::Categorical(_) |
            DType::Binomial(_) => {}
            _ => return Err(()),
        }

        // get the probabilities from the dist and insert as default cpt
        Ok(DefDiscreteNode {
            dist: dist,
            childs: RefCell::new(vec![]),
            parents: RefCell::new(vec![]),
            pos: RefCell::new(pos),
            cpt: RefCell::new(HashMap::new()),
        })
    }

    fn get_dist(&self) -> &'a V {
        self.dist
    }

    fn draw_sample(&self, rng: &mut RGSLRng, values: &[u8]) -> u8 {
        let cpt = &*self.cpt.borrow();
        let probs = cpt.get(values).unwrap();
        match *probs {
            DType::Binomial(ref dist) => dist.sample(rng),
            DType::Categorical(ref dist) => dist.sample(rng),
            _ => panic!(),
        }
    }

    fn init_sample(&self, rng: &mut RGSLRng) -> u8 {
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

    fn add_parent(&self, parent: Weak<Self>) {
        let parents = &mut *self.parents.borrow_mut();
        parents.push(parent);
    }

    fn add_child(&self, child: Rc<Self>) {
        let parent_childs = &mut *self.childs.borrow_mut();
        parent_childs.push(child);
    }

    fn build_cpt(&self, probs: CPT, parent_k: usize) -> Result<(), String> {
        use itertools::zip;

        let parents = &*self.parents.borrow();
        let rows1 = probs.dim_num()[0];
        let rows0 = parents.iter().fold(1, |t, p| {
            let i = {
                let p = p.upgrade().unwrap();
                p.dist.k_num()
            };
            t * (i as usize)
        }) * parent_k;
        if ((rows0 > parent_k) && (rows1 != rows0)) || (parent_k != rows1) {
            return Err("insufficient number of probability rows in the CPT".to_string());
        }

        let mut cpt = HashMap::new();
        let mut sum = 0.;
        let CPT { indexes, data } = probs;
        for (idx, prob) in zip(indexes.into_iter(), data.into_iter()) {
            sum += prob.iter().fold(0_f64, |t, &p| t + p);
            let dist = if prob.len() == 2 {
                // binomial
                DType::Binomial(Binomial::new(prob[1])?)
            } else {
                // n-categorical
                DType::Categorical(Categorical::new(prob.to_vec())?)
            };
            cpt.insert(idx, dist);
        }
        if sum < 1.0 {
            return Err("Conditional probability table must sum to 1".to_string());
        }
        let mut self_cpt = self.cpt.borrow_mut();
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

var_impl!(DefDiscreteVar);

impl DiscreteVar for DefDiscreteVar {
    type Event = Discrete;

    fn k_num(&self) -> u8 {
        match self.dist {
            DType::Categorical(ref dist) => dist.k_num(),
            DType::Binomial(_) => 2,
            _ => panic!(),
        }
    }

    fn sample(&self, rng: &mut RGSLRng) -> u8 {
        match self.dist {
            DType::Categorical(ref dist) => dist.sample(rng),
            DType::Binomial(ref dist) => dist.sample(rng),
            _ => panic!(),
        }
    }
    fn get_observations(&self) -> &[<Self as DiscreteVar>::Event] {
        &self.observations
    }

    fn push_observation(&mut self, obs: Self::Event) {
        self.observations.push(obs)
    }
}

pub fn default_discrete_model(init: &DefDiscreteVar) -> Result<DefDiscreteModel, ()> {
    use sampling::Sampler;
    let sampler = DefDiscreteSampler::new(None, None);
    DiscreteModel::new(init, sampler)
}

#[cfg(test)]
mod test {
    use super::*;
    use model::DType;
    use dists::Binomial;

    #[test]
    fn build() {
        let cp = DType::Binomial(Binomial::new(0.6).unwrap());
        let cloudy = DefDiscreteVar::with_dist(cp).unwrap();

        let cp = DType::Binomial(Binomial::new(0.1).unwrap());
        let sprinkler = DefDiscreteVar::with_dist(cp).unwrap();

        let cp = DType::Binomial(Binomial::new(0.4).unwrap());
        let rain = DefDiscreteVar::with_dist(cp).unwrap();

        let cp = DType::Binomial(Binomial::new(0.7).unwrap());
        let wet_grass = DefDiscreteVar::with_dist(cp).unwrap();

        let mut model = default_discrete_model(&wet_grass).unwrap();
        model.add_var(&sprinkler).unwrap();
        model.add_var(&rain).unwrap();
        model.add_var(&cloudy).unwrap();

        let choices = vec![vec![0_u8], vec![1]];
        let elements = vec![vec![0.25_f64, 0.25], vec![0.4, 0.1]];
        let cpt = CPT::new(elements, choices).unwrap();
        model.add_parent_to_var(&sprinkler, &cloudy, cpt).unwrap();

        let choices = vec![vec![0_u8], vec![1]];
        let elements = vec![vec![0.45_f64, 0.05], vec![0.05, 0.45]];
        let cpt = CPT::new(elements, choices).unwrap();
        model.add_parent_to_var(&rain, &cloudy, cpt).unwrap();
    }
}
