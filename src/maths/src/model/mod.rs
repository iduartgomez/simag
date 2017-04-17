//! Infrastructure to instantiate an statistical model with a given set of parameters.

use std::hash::Hash;
use std::rc::Rc;

pub use self::discrete::{DiscreteModel, DefDiscreteModel, DefDiscreteNode, DefDiscreteVar, CPT};
pub use self::discrete::{DiscreteNode, DiscreteVar};
pub use self::continuous::{ContModel, DefContModel, DefContNode, DefContVar};
pub use self::continuous::{ContNode, ContVar};
pub use self::hybrid::{HybridModel, DefHybridModel, DefHybridNode, ChainGraph};
pub use self::hybrid::HybridNode;

macro_rules! dag_impl {
    ($name:ident, $node:ident; [$($var_type:tt)+]) =>  {
        impl<'a, N> $name<'a, N>
            where N: $node<'a>
        {
            fn new() -> $name<'a, N> {
                $name {
                    _nlt: PhantomData,
                    nodes: vec![],
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
        
        dag_impl!(ITER: $name, $node; [$($var_type)+,]);
    };

    (ITER: $name:ident, $node:ident; [$($var_type:tt)+]) => {
        struct DirectedCycle {
            marked: Vec<bool>,
            edge_to: Vec<usize>,
            cycle: Vec<usize>,
            on_stack: Vec<bool>,
            sorted: Vec<usize>,
        }

        impl DirectedCycle {
            fn new<'a, N>(graph: &$name<'a, N>) -> DirectedCycle
                where N: $node<'a>
            {
                DirectedCycle {
                    marked: vec![false; graph.nodes.len()],
                    on_stack: vec![false; graph.nodes.len()],
                    edge_to: Vec::from_iter(0..graph.nodes.len()),
                    cycle: Vec::with_capacity(graph.nodes.len()),
                    sorted: Vec::with_capacity(graph.nodes.len()),
                }
            }

            fn dfs<'a, N>(&mut self, graph: &$name<'a, N>, v: usize)
                where N: $node<'a>
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
            where N: $node<'a>
        {
            _nlt: PhantomData<&'a ()>,
            unvisitted: VecDeque<Rc<N>>,
            processed: HashSet<usize>,
            queued: Rc<N>,
            childs_visitted: usize,
        }

        impl<'a, N> NetIter<'a, N>
            where N: $node<'a>
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
            where N: $node<'a>
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
    };
}

macro_rules! node_impl {
    ($name:ident, $var_trait:ident) => {
        impl<'a, V: 'a + $var_trait> Node for $name<'a, V> {
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
    }
}

macro_rules! var_impl {
    ($var_name:ident) => {
        impl $var_name {
            pub fn with_dist(dist: DType) -> Result<$var_name, ()> {
                validate_dist(&dist)?;
                Ok($var_name {
                    dist: dist,
                    observations: Vec::new(),
                    id: Uuid::new_v4(),
                })
            }

            pub fn as_dist(&mut self, dist: DType) -> Result<(), ()> {
                validate_dist(&dist)?;
                self.dist = dist;
                Ok(())
            }
        }

        impl Variable for $var_name {
            fn new() -> $var_name {
                $var_name {
                    dist: DType::UnknownDisc,
                    observations: Vec::new(),
                    id: Uuid::new_v4(),
                }
            }

            fn dist_type(&self) -> &DType {
                &self.dist
            }

            fn set_dist(&mut self, dist: DType) -> Result<(), ()> {
                validate_dist(&dist)?;
                self.dist = dist;
                Ok(())
            }
        }

        impl Hash for $var_name {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.id.hash(state);
            }
        }

        impl PartialEq for $var_name {
            fn eq(&self, other: &$var_name) -> bool {
                self.id == other.id
            }
        }

        impl Eq for $var_name {}

        impl Default for $var_name {
            fn default() -> Self {
                Self::new()
            }
        }
    }
}

mod discrete;
mod continuous;
mod hybrid;

// public traits for models:

pub trait Observation {
    fn is_kind(&self) -> VariableKind;
}

pub trait Variable: Hash + PartialEq + Eq + ::std::fmt::Debug {
    fn new() -> Self;

    /// Returns the exact form of the distribution, where the distribution
    /// type should be a discrete type.
    fn dist_type(&self) -> &DType;

    /// Set the distribution type for the random variable.
    fn set_dist(&mut self, dist: DType) -> Result<(), ()>;
}

/// A node in the the DAG.
pub trait Node: ::std::fmt::Debug {
    fn get_child_unchecked(&self, pos: usize) -> Rc<Self>;
    fn get_childs(&self) -> Vec<Rc<Self>>;
    fn is_root(&self) -> bool;
    fn position(&self) -> usize;

    /// Returns the node parents positions in the network.
    fn get_parents_positions(&self) -> Vec<usize>;

    /// Returns the positions in the network of the ancestors of this node.
    fn get_all_ancestors(&self) -> Vec<usize>;

    /// Returns the node childs positions in the network.
    fn get_childs_positions(&self) -> Vec<usize>;

    /// Node implementors are behind a reference counted pointer in the network,
    /// so they must have an interior mutable field to keep track of their position
    /// in the network which can be update calling this method.
    fn set_position(&self, pos: usize);

    /// Returns the number of parents this node has.
    fn parents_num(&self) -> usize;

    /// Returns the number of childs this node has.
    fn childs_num(&self) -> usize;
}

pub trait IterModel {
    type Iter: Iterator;

    /// Iterate the model variables in topographical order.
    fn iter_vars(&self) -> Self::Iter;

    /// Returns the total number of variables in the model.
    fn var_num(&self) -> usize;
}

// helper types:

pub type Discrete = usize;
pub type Continuous = f64;
pub type Boolean = bool;

impl Observation for Discrete {
    fn is_kind(&self) -> VariableKind {
        VariableKind::Discrete
    }
}

impl Observation for Continuous {
    fn is_kind(&self) -> VariableKind {
        VariableKind::Continuous
    }
}

impl Observation for Boolean {
    fn is_kind(&self) -> VariableKind {
        VariableKind::Boolean
    }
}

pub enum VariableKind {
    Continuous,
    Discrete,
    Boolean,
}

use dists::*;

#[derive(Debug, Clone)]
pub enum DType {
    // continuous types:
    Normal(Normal),
    Beta(Beta),
    Exponential(Exponential),
    Gamma(Gamma),
    ChiSquared(ChiSquared),
    TDist(TDist),
    FDist(FDist),
    Cauchy(Cauchy),
    LogNormal(LogNormal),
    Logistic(Logistic),
    Pareto(Pareto),
    RelaxedBernoulli(RelaxedBernoulli),
    NonParametric,
    // discrete types:
    Categorical(Categorical),
    Bernoulli(Bernoulli),
    Poisson,
    Binomial,
    Multinomial,
    UnknownDisc,
}


/// A helper type for computing properties of a model composed of several submodels.
///
/// You can construct complex models from other smaller submodels always that
/// the submodels have the following properties:
///
/// - The submodels are composed of continuous variables, or hybrid (pure discrete
///   is unsuported).
/// - The submodels have a designated single "output" node for their respective graphs.
/// - The submodels have a single designated root for their respective graphs.
///
/// When operated, the submodels will output a single value which may function as an input
/// to one other submodel of the joint model.
#[derive(Default)]
pub struct JointModel<'a: 'b, 'b> {
    nodes: Vec<NodeKind<'a, 'b>>,
    _edges: Vec<(usize, usize)>,
}

impl<'a: 'b, 'b> JointModel<'a, 'b> {
    pub fn new() -> JointModel<'a, 'b> {
        JointModel {
            nodes: vec![],
            _edges: vec![],
        }
    }

    pub fn add_submodel(&mut self, node: NodeKind<'a, 'b>) -> Result<(), ()> {
        self.nodes.push(node);
        Ok(())
    }

    /*
    pub fn add_edge(&mut self,
                    a: NodeKind<'a, 'b>,
                    b: NodeKind<'a, 'b>,
                    weight: Option<f64>)
                    -> Result<(), ()> {
        unimplemented!()
    }

    pub fn remove_edge(&mut self, a: NodeKind<'a, 'b>, b: NodeKind<'a, 'b>) {
        unimplemented!()
    }

    pub fn sample(&self) -> f64 {}
    */
}

pub enum NodeKind<'a: 'b, 'b> {
    Continuous(&'b DefContModel<'a>),
    Hybrid(&'b DefHybridModel<'a>),
}
