use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::rc::{Rc, Weak};

use ndarray::{Array, Dim};

use super::{Node, BayesNet, DiscreteDist, Observation, NetIter, DType};
use sampling::{DiscreteSampler, DefaultSampler};
use dists::Categorical;
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
            sampler: DefaultSampler::new(),
        }
    }

    pub fn with_sampler(&mut self, sampler: S) {
        self.sampler = sampler;
    }

    /// Sample the model in
    pub fn sample(&self) -> Vec<Vec<u8>> {
        let mut sampler = self.sampler.clone();
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

        let parents = &mut *node.parents.borrow_mut();
        parents.push(Rc::downgrade(&parent.clone()));
        let parent_childs = &mut *parent.childs.borrow_mut();
        parent_childs.push(parent.clone());
        // make CPT for child
        node.build_cpt(prob)?;
        Ok(())
    }
}

/// Conditional probability table for a child event in the network.
///
/// I
#[derive(Debug, Clone)]
pub struct CPT(Array<P, Dim<[usize; 2]>>);

impl CPT {
    fn dim_num(&self) -> [usize; 2] {
        use ndarray::Axis;
        [self.0.len_of(Axis(0)), self.0.len_of(Axis(1))]
    }

    fn new(elements: &[Vec<P>]) -> Result<CPT, ()> {
        let row_n = elements.len();
        if row_n < 2 {
            return Err(());
        }

        let column_n = elements[0].len();
        if column_n < 2 {
            return Err(());
        }

        let mut arr = Array::<P, Dim<[usize; 2]>>::zeros(Dim([row_n, column_n]));
        for (i, row) in elements.iter().enumerate() {
            if row.len() != column_n {
                return Err(());
            }
            for (j, element) in row.iter().enumerate() {
                arr[[i, j]] = *element;
            }
        }

        Ok(CPT(arr))
    }
}

type Choices = Vec<u8>;

pub struct DiscreteNode<'a, D: 'a, O: 'a>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation
{
    _obtype: PhantomData<O>,
    pub dist: &'a D,
    childs: RefCell<Vec<Rc<DiscreteNode<'a, D, O>>>>,
    cpt: RefCell<HashMap<Choices, Categorical>>,
    parents: RefCell<Vec<Weak<DiscreteNode<'a, D, O>>>>,
    pub pos: usize, // position in the bayes net vec of self
}

impl<'a, D: 'a, O: 'a> DiscreteNode<'a, D, O>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation
{
    fn new(dist: &'a D, pos: usize) -> Result<DiscreteNode<'a, D, O>, ()> {
        match dist.dist_type() {
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

    pub fn get_parents_positions(&self) -> Vec<usize> {
        let parents = &*self.parents.borrow();
        let mut positions = Vec::with_capacity(parents.len());
        for p in parents {
            let p = p.upgrade().unwrap();
            positions.push(p.pos);
        }
        positions
    }

    pub fn get_childs_positions(&self) -> Vec<usize> {
        let childs = &*self.childs.borrow();
        let mut positions = Vec::with_capacity(childs.len());
        for c in childs {
            positions.push(c.pos);
        }
        positions
    }

    /// Takes an slice reprensenting the parents categories at the current
    /// time **t** and draws a sample based on the probabilities.
    pub fn draw_sample(&self, values: &[u8]) -> u8 {
        let cpt = &*self.cpt.borrow();
        let probs = cpt.get(values).unwrap();
        probs.sample()
    }

    #[inline]
    pub fn parents_len(&self) -> usize {
        let parents = &*self.parents.borrow();
        parents.len()
    }

    fn get_parents_dists(&self) -> Vec<&D> {
        let parents = &*self.parents.borrow();
        let mut dists = Vec::with_capacity(parents.len());
        for p in parents {
            let p = p.upgrade().unwrap();
            dists.push(p.dist);
        }
        dists
    }

    fn build_cpt(&self, probs: CPT) -> Result<(), ()> {
        unimplemented!()
    }
}

impl<'a, D: 'a, O: 'a> Node<D, Self> for DiscreteNode<'a, D, O>
    where D: DiscreteDist<O> + Hash + Eq,
          O: Observation
{
    fn get_dist(&self) -> &D {
        &self.dist
    }

    fn get_child(&self, pos: usize) -> &Self {
        let childs = &*self.childs.borrow();
        unsafe { &*(&*childs[pos] as *const Self) }
    }

    fn get_childs(&self) -> Vec<&Self> {
        let childs = &*self.childs.borrow();
        Vec::from_iter(childs.iter().map(|x| unsafe { &*(&**x as *const Self) }))
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn cpt() {
        let p = vec![vec![0.3_f64, 0.3, 0.4], vec![0.2_f64, 0.5, 0.3]];
        let cpt = CPT::new(&p).unwrap();
        assert_eq!(cpt.dim_num(), [2, 3]);
        let CPT(arr) = cpt;
        assert_eq!(arr[[0, 2]], 0.4_f64);

        let p = vec![vec![0.7_f64, 0.3], vec![0.5_f64, 0.5], vec![0.3_f64, 0.7]];
        let cpt = CPT::new(&p).unwrap();
        assert_eq!(cpt.dim_num(), [3, 2]);
        let CPT(arr) = cpt;
        assert_eq!(arr[[2, 1]], 0.7_f64);
    }
}
