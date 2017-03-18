use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::rc::{Rc, Weak};

use uuid::Uuid;

use super::{Node, Variable};
use super::{DAG, NetIter, DType, EventObs};
use sampling::{DiscreteSampler, DefSampler};
use dists::{Categorical, Binomial};
use P;

// public traits for models:

/// Discrete implementation for a discrete model.
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
    fn draw_sample(&self, values: &[u8]) -> u8;

    /// Sample from the prior distribution, usually called on roots of the tree
    /// to initialize each sampling steep.
    fn init_sample(&self) -> u8;

    fn add_parent(&self, parent: Weak<Self>, k: u8);

    fn add_child(&self, child: Rc<Self>);

    fn build_cpt(&self, probabilities: CPT, k: usize) -> Result<(), String>;
}

pub trait DiscreteVar: Variable {
    /// Returns an slice of known observations for the variable of
    /// this distribution.
    fn get_observations(&self) -> &[Self::O];

    /// Returns the exact form of the distribution, where the distribution
    /// type should be a discrete type.
    fn dist_type(&self) -> &DType;

    /// Returns the number of categories for this discrete event
    fn k_num(&self) -> u8;

    /// Returns a sample from the original variable, not talking into account
    /// the parents in the network.
    fn sample(&self) -> u8;
}

pub struct DiscreteModel<'a, N: 'a, S>
    where N: DiscreteNode<'a>,
          S: DiscreteSampler + Clone
{
    vars: DAG<'a, N>,
    sampler: S,
}

use std::marker::PhantomData;

type DefDiscreteModel<'a> = DiscreteModel<'a, DefDiscreteNode<'a, DefDiscreteVar>, DefSampler>;

impl<'a, N, S> DiscreteModel<'a, N, S>
    where N: DiscreteNode<'a>,
          S: DiscreteSampler + Clone
{
    pub fn new(init: &'a <N as DiscreteNode<'a>>::Var,
               sampler: S)
               -> Result<DiscreteModel<'a, N, S>, ()> {
        let init = N::new(init, 0)?;
        let dag = DAG::new(init);
        Ok(DiscreteModel {
            vars: dag,
            sampler: S::new(None, None),
        })
    }

    /* 
    pub fn default_impl(init: &'a DefDiscreteVar) -> Result<DefDiscreteModel<'a>, ()> {
        let init: DefDiscreteNode<DefDiscreteVar> = DefDiscreteNode::with_var(init, 0)?;
        let dag: DAG<DefDiscreteNode<DefDiscreteVar>> = DAG {
            _node_lifetime: PhantomData,
            nodes: vec![Rc::new(init)],
        };
        Ok(DiscreteModel {
            vars: dag,
            sampler: DefSampler::new(None, None),
        })
    }
    */

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
        node.add_parent(Rc::downgrade(&parent.clone()), parent_k);
        parent.add_child(parent.clone());
        // check if it's a DAG and topologically sort the graph
        self.vars.topological_sort()
    }

    /// Remove a variable from the model, the childs will be disjoint if they don't
    /// have an other parent.
    pub fn remove_var(&mut self, node: &'a <N as DiscreteNode<'a>>::Var) {
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
                return Err(());
            }
        }

        Ok(CPT {
            data: elements,
            indexes: indexes,
        })
    }
}

type KSizedParent<'a, V> = (u8, Weak<DefDiscreteNode<'a, V>>);

/// A node in the network representing a random variable.
///
/// This type cannot be instantiated directly, instead add the random variable
/// distribution to the network.
pub struct DefDiscreteNode<'a, V: 'a>
    where V: DiscreteVar
{
    pub dist: &'a V,
    childs: RefCell<Vec<Rc<DefDiscreteNode<'a, V>>>>,
    cpt: RefCell<HashMap<Choices, DType>>,
    parents: RefCell<Vec<KSizedParent<'a, V>>>, // (categ, parent_ptr)
    pos: RefCell<usize>, // position in the bayes net vec of self
}

impl<'a, V: 'a + DiscreteVar> Node for DefDiscreteNode<'a, V> {
    fn get_child(&self, pos: usize) -> Rc<Self> {
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
        for &(_, ref p) in parents {
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

    fn set_position(&self, pos: usize) {
        *self.pos.borrow_mut() = pos;
    }
}

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

    fn draw_sample(&self, values: &[u8]) -> u8 {
        let cpt = &*self.cpt.borrow();
        let probs = cpt.get(values).unwrap();
        match *probs {
            DType::Binomial(ref dist) => dist.sample(),
            DType::Categorical(ref dist) => dist.sample(),
            _ => panic!(),
        }
    }

    fn init_sample(&self) -> u8 {
        self.dist.sample()
    }

    fn get_parents_dists(&self) -> Vec<&'a V> {
        let parents = &*self.parents.borrow();
        let mut dists = Vec::with_capacity(parents.len());
        for &(_, ref p) in parents {
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

    fn add_parent(&self, parent: Weak<Self>, k: u8) {
        let parents = &mut *self.parents.borrow_mut();
        parents.push((k, parent));
    }

    fn add_child(&self, child: Rc<Self>) {
        let parent_childs = &mut *self.childs.borrow_mut();
        parent_childs.push(child);
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
                DType::Binomial(Binomial::new(prob[1])?)
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

pub struct DefDiscreteVar {
    dist: DType,
    observations: Vec<EventObs>,
    id: Uuid,
}

impl DefDiscreteVar {
    pub fn new() -> Result<DefDiscreteVar, ()> {
        Ok(DefDiscreteVar {
            dist: DType::UnknownDisc,
            observations: Vec::new(),
            id: Uuid::new_v4(),
        })
    }

    pub fn with_dist(dist: DType) -> Result<DefDiscreteVar, ()> {
        match dist {
            DType::Binomial(_) |
            DType::Categorical(_) |
            DType::Poisson |
            DType::UnknownDisc => {}
            _ => return Err(()),
        }
        Ok(DefDiscreteVar {
            dist: dist,
            observations: Vec::new(),
            id: Uuid::new_v4(),
        })
    }

    pub fn as_dist(&mut self, dist: DType) -> Result<(), ()> {
        match dist {
            DType::Binomial(_) |
            DType::Categorical(_) |
            DType::Poisson |
            DType::UnknownDisc => {}
            _ => return Err(()),
        }
        self.dist = dist;
        Ok(())
    }
}

impl Hash for DefDiscreteVar {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for DefDiscreteVar {
    fn eq(&self, other: &DefDiscreteVar) -> bool {
        self.id == other.id
    }
}

impl Eq for DefDiscreteVar {}

impl Variable for DefDiscreteVar {
    type O = EventObs;
}

impl DiscreteVar for DefDiscreteVar {
    fn get_observations(&self) -> &[<Self as Variable>::O] {
        &self.observations
    }

    fn dist_type(&self) -> &DType {
        &self.dist
    }

    fn k_num(&self) -> u8 {
        match self.dist {
            DType::Categorical(ref dist) => dist.sample(),
            DType::Binomial(_) => 2,
            _ => panic!(),
        }
    }

    fn sample(&self) -> u8 {
        match self.dist {
            DType::Categorical(ref dist) => dist.sample(),
            DType::Binomial(ref dist) => dist.sample(),
            _ => panic!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use model::DType;
    use dists::Binomial;
    use sampling::Sampler;

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

        let mut model: DiscreteModel<DefDiscreteNode<_>, _> =
            DiscreteModel::new(&wet_grass, DefSampler::new(None, None)).unwrap();
        model.add_var(&sprinkler).unwrap();
        model.add_var(&rain).unwrap();
        model.add_var(&cloudy).unwrap();

        //let choices = vec![vec![0_u8, 0], vec![1, 0], vec![0, 1], vec![1, 1]];
        let choices = vec![vec![0_u8], vec![1]];
        let elements = vec![vec![0.5_f64, 0.5], vec![0.8, 0.2]];
        let cpt = CPT::new(elements, choices).unwrap();
        model.add_parent_to_var(&sprinkler, &cloudy, cpt);
        //model.add_parent_to_var(&rain, &cloudy);
    }
}
