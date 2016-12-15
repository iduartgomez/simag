use std::str;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::RwLock;

use chrono::{Duration, UTC};
use float_cmp::ApproxEqUlps;

use lang::parser::*;
use lang::logsent::*;
use lang::errors::ParseErrF;
use agent;
use super::Date;

pub use self::errors::TimeFnErr;
const TIME_EQ_DIFF: i64 = 1;

// Predicate types:

#[derive(Debug, Clone)]
pub enum Predicate {
    FreeClsMemb(FreeClsMemb),
    GroundedClsMemb(GroundedClsMemb),
    FreeClsOwner(FreeClsOwner),
}

impl<'a> Predicate {
    fn from(a: &'a ArgBorrowed<'a>,
            context: &'a mut Context,
            name: &'a Terminal,
            is_func: bool)
            -> Result<Predicate, ParseErrF> {
        if name.is_grounded() {
            match Terminal::from(&a.term, context) {
                Ok(Terminal::FreeTerm(ft)) => {
                    let t = FreeClsMemb::new(ft, a.uval, name)?;
                    Ok(Predicate::FreeClsMemb(t))
                }
                Ok(Terminal::GroundedTerm(gt)) => {
                    let t = GroundedClsMemb::new(gt, a.uval, name.get_name(), None, context)?;
                    Ok(Predicate::GroundedClsMemb(t))
                }
                Ok(Terminal::Keyword(kw)) => Err(ParseErrF::ReservedKW(String::from(kw))),
                Err(err) => Err(err),
            }
        } else {
            if context.is_tell {
                return Err(ParseErrF::ClassIsVar);
            }
            match Terminal::from(&a.term, context) {
                Ok(Terminal::FreeTerm(_)) if !is_func => Err(ParseErrF::BothAreVars),
                Ok(Terminal::FreeTerm(ft)) => {
                    let t = FreeClsMemb::new(ft, a.uval, name)?;
                    Ok(Predicate::FreeClsMemb(t))
                }
                Ok(Terminal::GroundedTerm(gt)) => {
                    let t = FreeClsOwner::new(gt, a.uval, name)?;
                    Ok(Predicate::FreeClsOwner(t))
                }
                Ok(Terminal::Keyword(kw)) => Err(ParseErrF::ReservedKW(String::from(kw))),
                Err(err) => Err(err),
            }
        }
    }

    #[inline]
    pub fn is_var(&self) -> bool {
        match *self {
            Predicate::FreeClsMemb(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn get_uval(&self) -> (Option<CompOperator>, Option<f32>) {
        match *self {
            Predicate::GroundedClsMemb(ref t) => {
                if t.value.is_some() {
                    let val = t.value.as_ref().unwrap().read().unwrap();
                    let op = *t.operator.as_ref().unwrap();
                    (Some(op), Some(*val))
                } else {
                    (None, None)
                }
            }
            Predicate::FreeClsMemb(ref t) => {
                if t.value.is_some() {
                    let val = *t.value.as_ref().unwrap();
                    let op = *t.operator.as_ref().unwrap();
                    (Some(op), Some(val))
                } else {
                    (None, None)
                }
            }
            Predicate::FreeClsOwner(ref t) => {
                if t.value.is_some() {
                    let val = *t.value.as_ref().unwrap();
                    let op = *t.operator.as_ref().unwrap();
                    (Some(op), Some(val))
                } else {
                    (None, None)
                }
            }
        }
    }

    #[inline]
    pub fn get_name(&self) -> Rc<String> {
        match *self {
            Predicate::GroundedClsMemb(ref t) => t.get_name(),
            Predicate::FreeClsOwner(ref t) => t.term.clone(),
            _ => panic!(),
        }
    }

    #[inline]
    fn get_id(&self) -> Vec<u8> {
        match *self {
            Predicate::FreeClsMemb(ref t) => t.get_id(),
            Predicate::GroundedClsMemb(ref t) => t.get_id(),
            Predicate::FreeClsOwner(ref t) => t.get_id(),
        }
    }

    pub fn has_uval(&self) -> bool {
        match *self {
            Predicate::GroundedClsMemb(ref t) => t.value.is_some(),
            Predicate::FreeClsMemb(ref t) => t.value.is_some(),
            Predicate::FreeClsOwner(ref t) => t.value.is_some(),
        }
    }
}

// Grounded types:

#[derive(Debug)]
pub enum Grounded {
    Function(Rc<GroundedFunc>),
    Terminal(Rc<GroundedClsMemb>),
}

#[derive(Debug)]
pub struct GroundedClsMemb {
    term: Rc<String>,
    value: Option<RwLock<f32>>,
    operator: Option<CompOperator>,
    parent: Rc<String>,
    dates: RwLock<Vec<Rc<Date>>>,
}

impl GroundedClsMemb {
    //! Internally the mutable parts are wrapped in `RwLock` types, as they can be accessed
    //! from a multithreaded environment. This provides enough atomicity so the most
    //! time it won't be blocking other reads.
    fn new(term: Rc<String>,
           uval: Option<UVal>,
           parent: Rc<String>,
           dates: Option<Vec<Rc<Date>>>,
           context: &Context)
           -> Result<GroundedClsMemb, ParseErrF> {
        let val;
        let op;
        if let Some(uval) = uval {
            let UVal { val: val0, op: op0 } = uval;
            val = Some(RwLock::new(match val0 {
                Number::UnsignedInteger(val) => {
                    if val == 0 || val == 1 {
                        val as f32
                    } else {
                        return Err(ParseErrF::IUVal(val as f32));
                    }
                }
                Number::UnsignedFloat(val) => {
                    if val >= 0. && val <= 1. {
                        val
                    } else {
                        return Err(ParseErrF::IUVal(val as f32));
                    }
                }
                Number::SignedFloat(val) => return Err(ParseErrF::IUVal(val as f32)),
                Number::SignedInteger(val) => return Err(ParseErrF::IUVal(val as f32)),
            }));
            if context.in_assertion && context.is_tell {
                op = match op0 {
                    CompOperator::Equal => Some(CompOperator::Equal),
                    _ => return Err(ParseErrF::IUValComp),
                };
            } else {
                op = Some(op0);
            }
        } else {
            val = None;
            op = None;
        }
        let dates = if let Some(dates) = dates {
            dates
        } else {
            vec![]
        };
        Ok(GroundedClsMemb {
            term: term,
            value: val,
            operator: op,
            parent: parent,
            dates: RwLock::new(dates),
        })
    }

    fn get_id(&self) -> Vec<u8> {
        let mut id: Vec<u8> = vec![];
        let mut id_1 = Vec::from(self.term.as_bytes());
        id.append(&mut id_1);
        match self.operator {
            None => id.push(0),
            Some(CompOperator::Equal) => id.push(1),
            Some(CompOperator::Less) => id.push(2),
            Some(CompOperator::More) => id.push(3),
        }
        if let Some(ref val) = self.value {
            let mut id_2 = format!("{}", *val.read().unwrap()).into_bytes();
            id.append(&mut id_2);
        }
        id
    }

    #[inline]
    pub fn get_name(&self) -> Rc<String> {
        self.term.clone()
    }

    #[inline]
    pub fn get_parent(&self) -> Rc<String> {
        self.parent.clone()
    }

    pub fn update(&self, data: Rc<GroundedClsMemb>) {
        let data: &GroundedClsMemb = &*data;
        *self.value.as_ref().unwrap().write().unwrap() =
            *data.value.as_ref().unwrap().read().unwrap();
    }

    pub fn from_free(free: &FreeClsMemb, assignment: Rc<String>) -> GroundedClsMemb {
        let val = if free.value.is_some() {
            Some(RwLock::new(free.value.unwrap()))
        } else {
            None
        };
        let op = if free.value.is_some() {
            Some(free.operator.unwrap())
        } else {
            None
        };
        GroundedClsMemb {
            term: assignment,
            value: val,
            operator: op,
            parent: free.parent.get_name(),
            dates: RwLock::new(vec![Rc::new(UTC::now())]),
        }
    }

    #[inline]
    pub fn comparable(&self, other: &GroundedClsMemb) -> bool {
        if self.term != other.get_name() {
            return false;
        }
        if self.parent != other.parent {
            return false;
        }
        if self.operator.is_some() && other.operator.is_none() {
            return false;
        }
        if other.operator.is_some() && self.operator.is_none() {
            return false;
        }
        true
    }

    pub fn time_equality(&self, data: &Option<Vec<Rc<Date>>>) -> bool {
        let own_dates = self.dates.read().unwrap();
        let own_is_true = if own_dates.len() % 2 == 1 {
            true
        } else {
            false
        };
        if data.is_none() && own_is_true {
            return true;
        } else if data.is_none() && !own_is_true {
            return false;
        }
        let other_dates = data.as_ref().unwrap();
        time_equality(&*own_dates, other_dates)
    }

    pub fn override_time_data(&self, data: &Vec<Rc<Date>>) {
        let current_dates = &mut *self.dates.write().unwrap();
        current_dates.truncate(0);
        for entry in data {
            current_dates.push(entry.clone());
        }
    }
}

impl ::std::cmp::PartialEq for GroundedClsMemb {
    fn eq(&self, other: &GroundedClsMemb) -> bool {
        if self.term != other.term {
            panic!()
        }
        if self.parent != other.parent {
            panic!()
        }
        let op_lhs;
        let op_rhs;
        if let Some(op) = other.operator {
            op_rhs = op;
            op_lhs = self.operator.unwrap();
        } else {
            return true;
        }
        let val_lhs = &*self.value.as_ref().unwrap().read().unwrap();
        let val_rhs = &*other.value.as_ref().unwrap().read().unwrap();
        match op_lhs {
            CompOperator::Equal => {
                if op_rhs.is_equal() {
                    val_lhs == val_rhs
                } else if op_rhs.is_more() {
                    val_lhs > val_rhs
                } else {
                    val_lhs < val_rhs
                }
            }
            CompOperator::More => {
                if op_rhs.is_equal() {
                    val_lhs < val_rhs
                } else {
                    panic!()
                }
            }
            CompOperator::Less => {
                if op_rhs.is_equal() {
                    val_lhs > val_rhs
                } else {
                    panic!()
                }
            }
        }
    }
}

impl ::std::clone::Clone for GroundedClsMemb {
    fn clone(&self) -> GroundedClsMemb {
        let value;
        if let Some(ref lock) = self.value {
            value = Some(RwLock::new(*lock.read().unwrap()));
        } else {
            value = None;
        }
        GroundedClsMemb {
            term: self.term.clone(),
            value: value,
            operator: self.operator,
            parent: self.parent.clone(),
            dates: RwLock::new(self.dates.read().unwrap().clone()),
        }
    }
}

#[derive(Debug)]
pub struct GroundedFunc {
    pub name: Rc<String>,
    pub args: [GroundedClsMemb; 2],
    pub third: Option<GroundedClsMemb>,
    pub dates: RwLock<Vec<Rc<Date>>>,
}

impl ::std::cmp::PartialEq for GroundedFunc {
    fn eq(&self, other: &GroundedFunc) -> bool {
        if self.name != other.name || self.args != other.args || self.third != other.third {
            false
        } else {
            true
        }
    }
}

impl ::std::cmp::Eq for GroundedFunc {}

impl GroundedFunc {
    pub fn from_free(free: &FuncDecl,
                     assignments: &HashMap<Rc<Var>, &agent::VarAssignment>,
                     time_assign: &HashMap<Rc<Var>, Vec<Rc<Date>>>)
                     -> Result<GroundedFunc, ()> {
        if !free.variant.is_relational() || free.args.as_ref().unwrap().len() < 2 {
            return Err(());
        }
        let name = match free.name {
            Terminal::GroundedTerm(ref name) => name.clone(),
            _ => panic!(),
        };
        let time_data = free.get_own_time_data(time_assign);
        let mut first = None;
        let mut second = None;
        let mut third = None;
        for (i, a) in free.args.as_ref().unwrap().iter().enumerate() {
            let n_a = match *a {
                Predicate::FreeClsMemb(ref free) => {
                    if let Some(entity) = assignments.get(&free.term) {
                        GroundedClsMemb::from_free(free, entity.name.clone())
                    } else {
                        return Err(());
                    }
                }
                Predicate::GroundedClsMemb(ref term) => term.clone(),
                _ => return Err(()),
            };
            if time_data.is_some() {
                n_a.override_time_data(time_data.as_ref().unwrap());
            }
            if i == 0 {
                first = Some(n_a)
            } else if i == 1 {
                second = Some(n_a)
            } else {
                third = Some(n_a)
            }
        }
        Ok(GroundedFunc {
            name: name,
            args: [first.unwrap(), second.unwrap()],
            third: third,
            dates: RwLock::new(Vec::new()),
        })
    }

    fn time_equality(&self, data: &Vec<Rc<Date>>) -> bool {
        let own_dates = self.dates.read().unwrap();
        time_equality(&*own_dates, data)
    }

    #[inline]
    pub fn get_name(&self) -> Rc<String> {
        self.name.clone()
    }

    #[inline]
    pub fn get_value(&self) -> f32 {
        *self.args[0].value.as_ref().unwrap().read().unwrap()
    }

    #[inline]
    pub fn get_args_names(&self) -> Vec<Rc<String>> {
        let mut v = Vec::with_capacity(3);
        v.push(self.args[0].get_name());
        v.push(self.args[1].get_name());
        if let Some(ref arg) = self.third {
            v.push(arg.get_name());
        }
        v
    }

    #[inline]
    pub fn name_in_pos(&self, name: &str, pos: &usize) -> bool {
        if (*pos < 2) && (&**self.args[*pos].get_name() == name) {
            return true;
        }
        if self.third.is_some() && &**self.third.as_ref().unwrap().get_name() == name {
            return true;
        }
        false
    }

    pub fn comparable(&self, other: &GroundedFunc) -> bool {
        if other.name != self.name {
            return false;
        }
        if !self.args[0].comparable(&other.args[0]) {
            return false;
        }
        if !self.args[1].comparable(&other.args[1]) {
            return false;
        }
        if self.third.is_some() && other.third.is_some() {
            let st = self.third.as_ref().unwrap();
            let ot = other.third.as_ref().unwrap();
            st.comparable(ot)
        } else {
            self.third.is_none() && other.third.is_none()
        }
    }

    pub fn update(&self, data: Rc<GroundedFunc>) {
        let data: &GroundedFunc = &*data;
        *self.args[0].value.as_ref().unwrap().write().unwrap() =
            *data.args[0].value.as_ref().unwrap().read().unwrap();
    }
}

impl ::std::clone::Clone for GroundedFunc {
    fn clone(&self) -> GroundedFunc {
        GroundedFunc {
            name: self.name.clone(),
            args: [self.args[0].clone(), self.args[1].clone()],
            third: self.third.clone(),
            dates: RwLock::new(self.dates.read().unwrap().clone()),
        }
    }
}

// Free types:

#[derive(Debug, Clone)]
pub struct FreeClsMemb {
    term: Rc<Var>,
    value: Option<f32>,
    operator: Option<CompOperator>,
    parent: Terminal,
}
impl FreeClsMemb {
    fn new(term: Rc<Var>, uval: Option<UVal>, parent: &Terminal) -> Result<FreeClsMemb, ParseErrF> {
        let (val, op) = match_uval(uval)?;
        Ok(FreeClsMemb {
            term: term,
            value: val,
            operator: op,
            parent: parent.clone(),
        })
    }

    fn get_id(&self) -> Vec<u8> {
        let mut id: Vec<u8> = vec![];
        if let Some(op) = self.operator {
            match op {
                CompOperator::Equal => id.push(0),
                CompOperator::Less => id.push(1),
                CompOperator::More => id.push(2),
            }
        }
        if let Some(value) = self.value {
            let mut id_2 = format!("{}", value).into_bytes();
            id.append(&mut id_2);
        }
        id
    }

    #[inline]
    pub fn get_parent(&self) -> Rc<String> {
        self.parent.get_name()
    }

    #[inline]
    pub fn get_var(&self) -> Rc<Var> {
        self.term.clone()
    }

    /// Compares a free term with a grounded term, assumes they are comparable
    /// (panics otherwise).
    pub fn equal_to_grounded(&self, other: &GroundedClsMemb) -> bool {
        if self.parent.get_name() != other.parent {
            panic!()
        }
        if self.value.is_some() {
            let val_free = self.value.unwrap();
            let val_grounded = *other.value.as_ref().unwrap().read().unwrap();
            match other.operator.unwrap() {
                CompOperator::Equal => {
                    if self.operator.as_ref().unwrap().is_equal() {
                        val_free.approx_eq_ulps(&val_grounded, 2)
                    } else if self.operator.as_ref().unwrap().is_more() {
                        val_grounded > val_free
                    } else {
                        val_grounded < val_free
                    }
                }
                _ => panic!(),
            }
        } else {
            true
        }
    }
}

#[derive(Debug, Clone)]
pub struct FreeClsOwner {
    pub term: Rc<String>,
    value: Option<f32>,
    operator: Option<CompOperator>,
    pub parent: Rc<Var>,
}

impl FreeClsOwner {
    fn new(term: Rc<String>,
           uval: Option<UVal>,
           parent: &Terminal)
           -> Result<FreeClsOwner, ParseErrF> {
        let (val, op) = match_uval(uval)?;
        Ok(FreeClsOwner {
            term: term,
            value: val,
            operator: op,
            parent: parent.get_var(),
        })
    }

    fn get_id(&self) -> Vec<u8> {
        let mut id: Vec<u8> = vec![];
        let mut id_1 = Vec::from(self.term.as_bytes());
        id.append(&mut id_1);
        match self.operator {
            None => id.push(0),
            Some(CompOperator::Equal) => id.push(1),
            Some(CompOperator::Less) => id.push(2),
            Some(CompOperator::More) => id.push(3),
        }
        if let Some(ref val) = self.value {
            let mut id_2 = format!("{}", *val).into_bytes();
            id.append(&mut id_2);
        }
        id
    }

    #[inline]
    pub fn filter_grounded(&self, other: &GroundedClsMemb) -> bool {
        if self.operator.is_some() {
            let val = self.value.as_ref().unwrap();
            match *self.operator.as_ref().unwrap() {
                CompOperator::Equal => {
                    val.approx_eq_ulps(&*other.value.as_ref().unwrap().read().unwrap(), 2)
                }
                CompOperator::Less => *other.value.as_ref().unwrap().read().unwrap() < *val,
                CompOperator::More => *other.value.as_ref().unwrap().read().unwrap() > *val,
            }
        } else {
            true
        }
    }
}

type UValDestruct = (Option<f32>, Option<CompOperator>);
fn match_uval(uval: Option<UVal>) -> Result<UValDestruct, ParseErrF> {
    if let Some(uval) = uval {
        let val = match uval.val {
            Number::UnsignedInteger(val) => {
                if val == 0 || val == 1 {
                    Some(val as f32)
                } else {
                    return Err(ParseErrF::IUVal(val as f32));
                }
            }
            Number::UnsignedFloat(val) => {
                if val >= 0. && val <= 1. {
                    Some(val)
                } else {
                    return Err(ParseErrF::IUVal(val as f32));
                }
            }
            Number::SignedFloat(val) => return Err(ParseErrF::IUVal(val as f32)),
            Number::SignedInteger(val) => return Err(ParseErrF::IUVal(val as f32)),
        };
        let op = Some(uval.op);
        Ok((val, op))
    } else {
        Ok((None, None))
    }
}

// Assert types:

#[derive(Debug, Clone)]
pub enum Assert {
    FuncDecl(FuncDecl),
    ClassDecl(ClassDecl),
}

impl Assert {
    #[inline]
    pub fn get_name(&self) -> Rc<String> {
        match *self {
            Assert::FuncDecl(ref f) => f.get_name(),
            Assert::ClassDecl(ref c) => c.get_name(),
        }
    }

    #[inline]
    pub fn get_time_decl(&self, var: &Var) -> bool {
        match *self {
            Assert::FuncDecl(ref f) => f.get_time_decl(var),
            Assert::ClassDecl(ref c) => c.get_time_decl(var),
        }
    }

    #[inline]
    pub fn get_dates(&self,
                     agent: &agent::Representation,
                     var_assign: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>)
                     -> Option<Vec<Rc<Date>>> {
        match *self {
            Assert::FuncDecl(ref f) => f.get_dates(agent, var_assign),
            Assert::ClassDecl(ref c) => c.get_dates(agent, var_assign),
        }
    }

    #[inline]
    pub fn is_class(&self) -> bool {
        match *self {
            Assert::FuncDecl(_) => false,
            Assert::ClassDecl(_) => true,
        }
    }

    #[inline]
    pub fn contains(&self, var: &Var) -> bool {
        match *self {
            Assert::FuncDecl(ref f) => f.contains_var(var),
            Assert::ClassDecl(ref c) => c.contains_var(var),
        }
    }

    #[inline]
    pub fn parent_is_grounded(&self) -> bool {
        match *self {
            Assert::FuncDecl(ref f) => f.parent_is_grounded(),
            Assert::ClassDecl(ref c) => c.parent_is_grounded(),
        }
    }

    #[inline]
    pub fn parent_is_kw(&self) -> bool {
        match *self {
            Assert::FuncDecl(ref f) => f.parent_is_kw(),
            Assert::ClassDecl(_) => false,
        }
    }

    #[inline]
    pub fn unwrap_fn(self) -> FuncDecl {
        match self {
            Assert::FuncDecl(f) => f,
            Assert::ClassDecl(_) => panic!(),
        }
    }

    #[inline]
    pub fn unwrap_cls(self) -> ClassDecl {
        match self {
            Assert::FuncDecl(_) => panic!(),
            Assert::ClassDecl(c) => c,
        }
    }

    #[inline]
    pub fn equal_to_grounded(&self,
                             agent: &agent::Representation,
                             assignments: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>,
                             time_assign: &HashMap<Rc<Var>, Vec<Rc<Date>>>)
                             -> Option<bool> {
        match *self {
            Assert::FuncDecl(ref f) => f.equal_to_grounded(agent, assignments, time_assign),
            Assert::ClassDecl(ref c) => c.equal_to_grounded(agent, assignments, time_assign),
        }
    }

    #[inline]
    pub fn substitute(&self,
                      agent: &agent::Representation,
                      assignments: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>,
                      time_assign: &HashMap<Rc<Var>, Vec<Rc<Date>>>,
                      context: &mut agent::ProofResult) {
        match *self {
            Assert::FuncDecl(ref f) => f.substitute(agent, assignments, time_assign, context),
            Assert::ClassDecl(ref c) => c.substitute(agent, assignments, time_assign, context),
        }
    }

    #[inline]
    pub fn get_id(&self) -> Vec<u8> {
        match *self {
            Assert::FuncDecl(ref f) => f.get_id(),
            Assert::ClassDecl(ref c) => c.get_id(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    name: Terminal,
    args: Option<Vec<Predicate>>,
    pub op_args: Option<Vec<OpArg>>,
    pub variant: FuncVariants,
}

impl<'a> FuncDecl {
    pub fn from(other: &FuncDeclBorrowed<'a>,
                context: &mut Context)
                -> Result<FuncDecl, ParseErrF> {
        let mut variant = other.variant;
        let func_name = match Terminal::from(&other.name, context) {
            Err(ParseErrF::ReservedKW(val)) => {
                if &val == "time_calc" {
                    variant = FuncVariants::TimeCalc;
                    Terminal::Keyword("time_calc")
                } else {
                    return Err(ParseErrF::ReservedKW(val));
                }
            }
            Err(err) => return Err(err),
            Ok(val) => val,
        };
        match variant {
            FuncVariants::TimeCalc => FuncDecl::decl_timecalc_fn(other, context),
            FuncVariants::Relational => FuncDecl::decl_relational_fn(other, context, func_name),
            FuncVariants::NonRelational => {
                FuncDecl::decl_nonrelational_fn(other, context, func_name)
            }
        }
    }

    /// Assumes all arguments are grounded and converts to a GroundedFunc (panics otherwise).
    pub fn into_grounded(self) -> GroundedFunc {
        let FuncDecl { name, args, op_args, .. } = self;
        let name = match name {
            Terminal::GroundedTerm(name) => name,
            _ => panic!(),
        };
        let mut time_data: Option<Vec<Rc<Date>>> = None;
        if let Some(mut oargs) = op_args {
            for arg in oargs.drain(..) {
                match arg {
                    OpArg::TimeDecl(TimeFn::Date(date)) => {
                        time_data = Some(vec![date]);
                    }
                    OpArg::TimeDecl(TimeFn::Now) => {
                        time_data = Some(vec![Rc::new(UTC::now())]);
                    }
                    _ => {}
                }
            }
        }
        let mut first = None;
        let mut second = None;
        let mut third = None;
        let mut args = args.unwrap();
        for (i, a) in args.drain(..).enumerate() {
            let n_a = match a {
                Predicate::GroundedClsMemb(term) => term,
                _ => panic!(),
            };
            if i == 0 {
                first = Some(n_a)
            } else if i == 1 {
                second = Some(n_a)
            } else {
                third = Some(n_a)
            }
        }
        let dates = if time_data.is_some() {
            time_data.unwrap()
        } else {
            vec![]
        };
        GroundedFunc {
            name: name,
            args: [first.unwrap(), second.unwrap()],
            third: third,
            dates: RwLock::new(dates),
        }
    }

    pub fn is_grounded(&self) -> bool {
        if !self.parent_is_grounded() {
            return false;
        }
        for a in self.args.as_ref().unwrap().iter() {
            match *a {
                Predicate::GroundedClsMemb(_) => {}
                _ => return false,
            }
        }
        true
    }

    pub fn is_relational(&self) -> bool {
        if let FuncVariants::Relational = self.variant {
            true
        } else {
            false
        }
    }

    pub fn get_name(&self) -> Rc<String> {
        match self.name {
            Terminal::FreeTerm(ref var) => Rc::new(var.name.clone()),
            Terminal::GroundedTerm(ref name) => name.clone(),
            Terminal::Keyword(kw) => Rc::new(kw.to_string()),
        }
    }

    #[inline]
    pub fn get_uval(&self) -> (CompOperator, f32) {
        let (op, val) = self.args.as_ref().unwrap().get(0).unwrap().get_uval();
        (op.unwrap(), val.unwrap())
    }

    #[inline]
    pub fn get_parent(&self) -> &Terminal {
        &self.name
    }

    pub fn get_args(&self) -> DeclArgsIter {
        DeclArgsIter {
            count: 0,
            data_ref: self.args.as_ref().unwrap(),
        }
    }

    pub fn get_own_time_data(&self,
                             assignments: &HashMap<Rc<Var>, Vec<Rc<Date>>>)
                             -> Option<Vec<Rc<Date>>> {
        if self.op_args.is_none() {
            return None;
        }
        let mut v = Vec::new();
        for arg in self.op_args.as_ref().unwrap() {
            match *arg {
                OpArg::TimeDecl(_) |
                OpArg::TimeVarAssign(_) => {
                    v = arg.get_time_payload(assignments);
                    break;
                }
                _ => {}
            }
        }
        if !v.is_empty() {
            Some(v)
        } else {
            None
        }
    }

    fn get_time_decl(&self, var0: &Var) -> bool {
        if self.op_args.is_none() {
            return false;
        }
        for arg in self.op_args.as_ref().unwrap() {
            match *arg {
                OpArg::TimeVarFrom(ref var1) => {
                    return var1.var_equality(var0);
                }
                _ => {}
            }
        }
        false
    }

    fn get_dates(&self,
                 agent: &agent::Representation,
                 var_assign: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>)
                 -> Option<Vec<Rc<Date>>> {
        if self.is_grounded() {
            let sbj = self.args.as_ref().unwrap();
            let grfunc = self.clone().into_grounded();
            if let Some(relation) = agent.get_relationship(&grfunc, sbj[0].get_name()) {
                return Some(relation.dates.read().unwrap().clone());
            } else {
                return None;
            }
        } else {
            if var_assign.is_none() {
                return None;
            }
            let assignments = var_assign.as_ref().unwrap();
            let f = HashMap::new();
            if let Ok(grfunc) = GroundedFunc::from_free(self, assignments, &f) {
                for arg in self.get_args() {
                    if let Predicate::FreeClsMemb(ref arg) = *arg {
                        if let Some(entity) = assignments.get(&arg.term) {
                            if let Some(current) = entity.get_relationship(&grfunc) {
                                return Some(current.dates.read().unwrap().clone());
                            }
                        }
                    }
                }
            }
            None
        }
    }

    fn get_id(&self) -> Vec<u8> {
        let mut id = vec![];
        if self.name.is_grounded() {
            let mut id_1 = Vec::from(self.name.get_name().as_bytes());
            id.append(&mut id_1);
        }
        if let Some(ref args) = self.args {
            for a in args {
                let mut id_2 = a.get_id();
                id.append(&mut id_2)
            }
        }
        if let Some(ref args) = self.op_args {
            for a in args {
                let mut id_2 = a.get_id();
                id.append(&mut id_2)
            }
        }
        id
    }

    fn decl_timecalc_fn(other: &FuncDeclBorrowed<'a>,
                        context: &mut Context)
                        -> Result<FuncDecl, ParseErrF> {
        if other.args.is_some() || other.op_args.is_none() {
            return Err(ParseErrF::WrongDef);
        }
        let op_args = match other.op_args {
            Some(ref oargs) => {
                let mut v0 = Vec::with_capacity(oargs.len());
                for e in oargs {
                    let a = match OpArg::from(e, context) {
                        Err(err) => return Err(err),
                        Ok(a) => a,
                    };
                    v0.push(a);
                }
                Some(v0)
            }
            None => return Err(ParseErrF::WrongDef),
        };
        if op_args.as_ref().unwrap().is_empty() {
            return Err(ParseErrF::TimeFnErr(TimeFnErr::InsufArgs));
        }
        Ok(FuncDecl {
            name: Terminal::Keyword("time_calc"),
            args: None,
            op_args: op_args,
            variant: FuncVariants::TimeCalc,
        })
    }

    fn decl_relational_fn(other: &FuncDeclBorrowed<'a>,
                          context: &mut Context,
                          name: Terminal)
                          -> Result<FuncDecl, ParseErrF> {
        let op_args = match other.op_args {
            Some(ref oargs) => {
                let mut v0 = Vec::with_capacity(oargs.len());
                for e in oargs {
                    let a = OpArg::from(e, context)?;
                    v0.push(a);
                }
                Some(v0)
            }
            None => None,
        };
        let mut args = Vec::with_capacity(3);
        if let Some(oargs) = other.args.as_ref() {
            if oargs.len() > 3 || oargs.len() < 2 {
                return Err(ParseErrF::WrongArgNumb);
            }
            let mut vars = 0;
            for (i, a) in oargs.iter().enumerate() {
                let pred = Predicate::from(a, context, &name, true)?;
                if pred.has_uval() && (i == 1 || i == 2) {
                    return Err(ParseErrF::RFuncWrongArgs);
                }
                if pred.is_var() {
                    vars += 1;
                }
                args.push(pred);
            }
            if (oargs.len() == vars) && name.is_var() {
                // it's a free fn query, but at least one of the arguments must be grounded
                return Err(ParseErrF::BothAreVars);
            }
        } else {
            return Err(ParseErrF::WrongArgNumb);
        }
        Ok(FuncDecl {
            name: name,
            args: Some(args),
            op_args: op_args,
            variant: FuncVariants::Relational,
        })
    }

    fn decl_nonrelational_fn(other: &FuncDeclBorrowed<'a>,
                             context: &mut Context,
                             name: Terminal)
                             -> Result<FuncDecl, ParseErrF> {
        let op_args = match other.op_args {
            Some(ref oargs) => {
                let mut v0 = Vec::with_capacity(oargs.len());
                for e in oargs {
                    let a = match OpArg::from(e, context) {
                        Err(err) => return Err(err),
                        Ok(a) => a,
                    };
                    v0.push(a);
                }
                Some(v0)
            }
            None => None,
        };
        Ok(FuncDecl {
            name: name,
            args: None,
            op_args: op_args,
            variant: FuncVariants::NonRelational,
        })
    }

    fn contains_var(&self, var: &Var) -> bool {
        if self.args.is_some() {
            for a in self.args.as_ref().unwrap() {
                if let Predicate::FreeClsMemb(ref term) = *a {
                    if &*term.term as *const Var == &*var as *const Var {
                        return true;
                    }
                } else {
                    continue;
                }
            }
        }
        if self.op_args.is_some() {
            for a in self.op_args.as_ref().unwrap() {
                if a.contains_var(var) {
                    return true;
                }
            }
        }
        false
    }

    fn parent_is_grounded(&self) -> bool {
        match self.name {
            Terminal::GroundedTerm(_) => true,
            _ => false,
        }
    }

    fn parent_is_kw(&self) -> bool {
        match self.name {
            Terminal::Keyword(_) => true,
            _ => false,
        }
    }

    /// Compares two relational functions, if they include free terms variable values
    /// assignments must be provided or will return None or panic in worst case.
    fn equal_to_grounded(&self,
                         agent: &agent::Representation,
                         assignments: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>,
                         time_assign: &HashMap<Rc<Var>, Vec<Rc<Date>>>)
                         -> Option<bool> {
        match self.variant {
            FuncVariants::Relational => {}
            FuncVariants::TimeCalc => return self.time_resolution(time_assign),
            _ => panic!(),
        }
        if self.is_grounded() {
            let sbj = self.args.as_ref().unwrap();
            let grfunc = self.clone().into_grounded();
            agent.has_relationship(&grfunc, sbj[0].get_name())
        } else {
            if assignments.is_none() {
                return None;
            }
            let assignments = assignments.as_ref().unwrap();
            if let Ok(grfunc) = GroundedFunc::from_free(self, assignments, time_assign) {
                for arg in self.get_args() {
                    if let Predicate::FreeClsMemb(ref arg) = *arg {
                        if let Some(entity) = assignments.get(&arg.term) {
                            if let Some(current) = entity.get_relationship(&grfunc) {
                                let grdates = &*grfunc.dates.read().unwrap();
                                if **current != grfunc || !current.time_equality(grdates) {
                                    return Some(false);
                                } else {
                                    return Some(true);
                                }
                            }
                        }
                    }
                }
            }
            None
        }
    }

    fn time_resolution(&self, assignments: &HashMap<Rc<Var>, Vec<Rc<Date>>>) -> Option<bool> {
        for arg in self.op_args.as_ref().unwrap() {
            if !arg.compare_time_args(assignments) {
                return Some(false);
            }
        }
        Some(true)
    }

    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>,
                  time_assign: &HashMap<Rc<Var>, Vec<Rc<Date>>>,
                  context: &mut agent::ProofResult) {
        if let Ok(grfunc) = GroundedFunc::from_free(self,
                                                    assignments.as_ref().unwrap(),
                                                    time_assign) {
            let grfunc = Rc::new(grfunc);
            agent.up_relation(grfunc.clone());
            context.grounded.push((Grounded::Function(grfunc.clone()), UTC::now()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    name: Terminal,
    args: Vec<Predicate>,
    pub op_args: Option<Vec<OpArg>>,
}

impl<'a> ClassDecl {
    pub fn from(other: &ClassDeclBorrowed<'a>,
                context: &mut Context)
                -> Result<ClassDecl, ParseErrF> {
        let class_name = Terminal::from(&other.name, context)?;
        let op_args = match other.op_args {
            Some(ref oargs) => {
                let mut v0 = Vec::with_capacity(oargs.len());
                for e in oargs {
                    let a = OpArg::from(e, context)?;
                    v0.push(a);
                }
                Some(v0)
            }
            None => None,
        };
        let args = {
            let mut v0 = Vec::with_capacity(other.args.len());
            for a in &other.args {
                let pred = Predicate::from(a, context, &class_name, false)?;
                v0.push(pred);
            }
            v0
        };
        Ok(ClassDecl {
            name: class_name,
            args: args,
            op_args: op_args,
        })
    }

    pub fn get_args(&self) -> DeclArgsIter {
        DeclArgsIter {
            count: 0,
            data_ref: &self.args,
        }
    }

    pub fn get_name(&self) -> Rc<String> {
        match self.name {
            Terminal::FreeTerm(ref var) => Rc::new(var.name.clone()),
            Terminal::GroundedTerm(ref name) => name.clone(),
            Terminal::Keyword(_) => panic!(),
        }
    }

    pub fn get_parent(&self) -> &Terminal {
        &self.name
    }

    pub fn get_own_time_data(&self,
                             assignments: &HashMap<Rc<Var>, Vec<Rc<Date>>>)
                             -> Option<Vec<Rc<Date>>> {
        if self.op_args.is_none() {
            return None;
        }
        let mut v = Vec::new();
        for arg in self.op_args.as_ref().unwrap() {
            match *arg {
                OpArg::TimeDecl(_) |
                OpArg::TimeVarAssign(_) => {
                    v = arg.get_time_payload(assignments);
                    break;
                }
                _ => {}
            }
        }
        if !v.is_empty() {
            Some(v)
        } else {
            None
        }
    }

    fn get_time_decl(&self, var0: &Var) -> bool {
        if self.op_args.is_none() {
            return false;
        }
        for arg in self.op_args.as_ref().unwrap() {
            match *arg {
                OpArg::TimeVarFrom(ref var1) => {
                    return var1.var_equality(var0);
                }
                _ => {}
            }
        }
        false
    }

    fn get_dates(&self,
                 agent: &agent::Representation,
                 var_assign: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>)
                 -> Option<Vec<Rc<Date>>> {
        let arg = &self.args[0];
        match *arg {
            Predicate::FreeClsMemb(ref free) => {
                if var_assign.is_none() {
                    return None;
                }
                if let Some(entity) = var_assign.as_ref().unwrap().get(&free.term) {
                    if let Some(grounded) = entity.get_class(free.parent.get_name()) {
                        if free.equal_to_grounded(grounded) {
                            return Some(grounded.dates.read().unwrap().clone());
                        }
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            }
            Predicate::GroundedClsMemb(ref compare) => {
                let entity = agent.get_entity_from_class(self.get_name(), compare.term.clone());
                if let Some(grounded) = entity {
                    if *grounded == *compare {
                        return Some(grounded.dates.read().unwrap().clone());
                    }
                } else {
                    return None;
                }
            }
            _ => return None, // this path won't be taken in any program
        }
        None
    }

    pub fn get_time_payload(&self) -> Option<Vec<Rc<Date>>> {
        if self.op_args.is_none() {
            return None;
        }
        for arg in self.op_args.as_ref().unwrap() {
            match *arg {
                OpArg::TimeDecl(ref decl) => {
                    return Some(decl.get_time_payload());
                }
                _ => {}
            }
        }
        None
    }

    fn get_id(&self) -> Vec<u8> {
        let mut id = vec![];
        if self.name.is_grounded() {
            let mut id_1 = Vec::from(self.name.get_name().as_bytes());
            id.append(&mut id_1);
        }
        for a in &self.args {
            let mut id_2 = a.get_id();
            id.append(&mut id_2)
        }
        if let Some(ref args) = self.op_args {
            for a in args {
                let mut id_2 = a.get_id();
                id.append(&mut id_2)
            }
        }
        id
    }

    fn contains_var(&self, var: &Var) -> bool {
        for a in &self.args {
            match *a {
                Predicate::FreeClsMemb(ref term) if &*term.term as *const Var ==
                                                    &*var as *const Var => return true,
                _ => continue,
            }
        }
        if self.op_args.is_some() {
            for a in self.op_args.as_ref().unwrap() {
                if a.contains_var(var) {
                    return true;
                }
            }
        }
        false
    }

    fn parent_is_grounded(&self) -> bool {
        match self.name {
            Terminal::GroundedTerm(_) => true,
            _ => false,
        }
    }

    /// Compare each term of a class declaration if they are comparable, and returns
    /// the result of such comparison (or none in case they are not comparable).
    fn equal_to_grounded(&self,
                         agent: &agent::Representation,
                         assignments: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>,
                         time_assign: &HashMap<Rc<Var>, Vec<Rc<Date>>>)
                         -> Option<bool> {
        let time_data = self.get_own_time_data(time_assign);
        for a in &self.args {
            match *a {
                Predicate::FreeClsMemb(ref free) => {
                    if assignments.is_none() {
                        return None;
                    }
                    if let Some(entity) = assignments.as_ref().unwrap().get(&free.term) {
                        if let Some(grounded) = entity.get_class(free.parent.get_name()) {
                            if !free.equal_to_grounded(grounded) ||
                               !grounded.time_equality(&time_data) {
                                return Some(false);
                            }
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                Predicate::GroundedClsMemb(ref compare) => {
                    let entity = agent.get_entity_from_class(self.get_name(), compare.term.clone());
                    if let Some(grounded) = entity {
                        if *grounded != *compare || !grounded.time_equality(&time_data) {
                            return Some(false);
                        }
                    } else {
                        return None;
                    }
                }
                _ => return None, // this path won't be taken in any program
            }
        }
        Some(true)
    }

    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>,
                  time_assign: &HashMap<Rc<Var>, Vec<Rc<Date>>>,
                  context: &mut agent::ProofResult) {
        let time_data = self.get_own_time_data(time_assign);
        for a in &self.args {
            let grfact = match *a {
                Predicate::FreeClsMemb(ref free) => {
                    if let Some(entity) = assignments.as_ref().unwrap().get(&free.term) {
                        GroundedClsMemb::from_free(free, entity.name.clone())
                    } else {
                        break;
                    }
                }
                Predicate::GroundedClsMemb(ref grounded) => grounded.clone(),
                _ => return, // this path won't be taken in any program
            };
            if let Some(ref data) = time_data {
                grfact.override_time_data(data);
            } else {
                let set_now = vec![Rc::new(UTC::now())];
                grfact.override_time_data(&set_now);
            }
            let grfact = Rc::new(grfact);
            context.grounded.push((Grounded::Terminal(grfact.clone()), UTC::now()));
            agent.up_membership(grfact.clone())
        }
    }
}

impl ::std::iter::IntoIterator for ClassDecl {
    type Item = GroundedClsMemb;
    type IntoIter = ::std::vec::IntoIter<GroundedClsMemb>;
    fn into_iter(mut self) -> Self::IntoIter {
        let mut v = Vec::new();
        for _ in 0..self.args.len() {
            match self.args.pop() {
                Some(Predicate::GroundedClsMemb(grfact)) => v.push(grfact),
                Some(_) => panic!(),
                None => {}
            }
        }
        v.into_iter()
    }
}

pub struct DeclArgsIter<'a> {
    data_ref: &'a Vec<Predicate>,
    count: usize,
}

impl<'a> ::std::iter::Iterator for DeclArgsIter<'a> {
    type Item = &'a Predicate;
    fn next(&mut self) -> Option<&'a Predicate> {
        if self.count < self.data_ref.len() {
            let c = self.count;
            self.count += 1;
            Some(&(self.data_ref[c]))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OpArg {
    Generic(OpArgTerm, Option<(CompOperator, OpArgTerm)>),
    TimeDecl(TimeFn),
    TimeVar,
    TimeVarAssign(Rc<Var>),
    TimeVarFrom(Rc<Var>),
}

impl<'a> OpArg {
    pub fn from(other: &OpArgBorrowed<'a>, context: &mut Context) -> Result<OpArg, ParseErrF> {
        let t0 = match OpArgTerm::from(&other.term, context) {
            Err(ParseErrF::ReservedKW(kw)) => {
                if &kw == "time" {
                    let targ = OpArg::ignore_kw(other, "time", context)?;
                    return Ok(targ);
                } else {
                    return Err(ParseErrF::ReservedKW(kw));
                }
            }
            Err(err) => return Err(err),
            Ok(arg) => arg,
        };
        let comp = match other.comp {
            Some((op, ref tors)) => {
                match OpArgTerm::from(tors, context) {
                    Ok(t) => Some((op, t)),
                    Err(ParseErrF::ReservedKW(kw)) => {
                        if &kw == "time" {
                            if t0.is_var() {
                                return Ok(OpArg::TimeVarFrom(t0.get_var()));
                            } else {
                                return Err(ParseErrF::WrongDef);
                            }
                        } else {
                            return Err(ParseErrF::ReservedKW(kw));
                        }
                    }
                    Err(err) => return Err(err),
                }
            }
            None => None,
        };
        Ok(OpArg::Generic(t0, comp))
    }

    fn ignore_kw(other: &OpArgBorrowed<'a>,
                 kw: &str,
                 context: &mut Context)
                 -> Result<OpArg, ParseErrF> {
        match kw {
            "time" => {
                let load = OpArgTerm::time_payload(other.comp.as_ref(), context)?;
                if load.1.is_var() {
                    Ok(OpArg::TimeVarAssign(load.1.get_var()))
                } else {
                    match load.1 {
                        OpArgTerm::TimePayload(TimeFn::IsVar) => Ok(OpArg::TimeVar),
                        OpArgTerm::TimePayload(load) => Ok(OpArg::TimeDecl(load)),
                        _ => panic!(),
                    }
                }
            }
            val => Err(ParseErrF::ReservedKW(val.to_string())),
        }
    }

    #[inline]
    fn contains_var(&self, var1: &Var) -> bool {
        match *self {
            OpArg::TimeVarAssign(ref var0) => &**var0 == var1,
            OpArg::TimeVarFrom(ref var0) => &**var0 == var1,
            _ => false,
        }
    }

    fn get_id(&self) -> Vec<u8> {
        let mut id = vec![];
        match *self {
            OpArg::Generic(_, _) => id.push(0),
            OpArg::TimeDecl(_) => id.push(1),
            OpArg::TimeVar => id.push(2),
            OpArg::TimeVarAssign(_) => id.push(3),
            OpArg::TimeVarFrom(_) => id.push(4),
        }
        id
    }

    fn get_time_payload(&self, assignments: &HashMap<Rc<Var>, Vec<Rc<Date>>>) -> Vec<Rc<Date>> {
        match *self {
            OpArg::TimeDecl(TimeFn::Date(ref payload)) => vec![payload.clone()],
            OpArg::TimeDecl(TimeFn::Now) => vec![Rc::new(UTC::now())],
            OpArg::TimeVarAssign(ref var) => assignments.get(&*var).unwrap().clone(),
            _ => panic!(),
        }
    }

    fn compare_time_args(&self, assignments: &HashMap<Rc<Var>, Vec<Rc<Date>>>) -> bool {
        let (term, op, comp) = match *self {
            OpArg::Generic(ref term, Some((ref op, ref comp))) => (term, op, comp),
            _ => return false,
        };
        let var0 = term.get_var();
        let arg0 = assignments.get(&var0).as_ref().unwrap().last().unwrap();
        let var1 = comp.get_var();
        let arg1 = assignments.get(&var1).as_ref().unwrap().last().unwrap();
        match *op {
            CompOperator::Equal => {
                let comp_diff = Duration::seconds(TIME_EQ_DIFF);
                let lower_bound = **arg0 - comp_diff;
                let upper_bound = **arg0 + comp_diff;
                if !(**arg1 > lower_bound) || !(**arg1 < upper_bound) {
                    false
                } else {
                    true
                }
            }
            CompOperator::More => arg0 > arg1,
            CompOperator::Less => arg0 < arg1,
        }
    }
}

fn time_equality(lhs: &Vec<Rc<Date>>, rhs: &Vec<Rc<Date>>) -> bool {
    let lhs_is_true = if lhs.len() % 2 == 1 || lhs.is_empty() {
        true
    } else {
        false
    };
    let rhs_is_true = if rhs.len() % 2 == 1 || rhs.is_empty() {
        true
    } else {
        false
    };
    lhs_is_true == rhs_is_true
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TimeFn {
    Now,
    Date(Rc<Date>),
    IsVar,
}

impl TimeFn {
    fn from_str(slice: &[u8]) -> Result<TimeFn, ParseErrF> {
        if slice == b"Now" {
            Ok(TimeFn::Now)
        } else {
            let s = unsafe { str::from_utf8_unchecked(slice) };
            match s.parse::<Date>() {
                Err(_) => Err(ParseErrF::TimeFnErr(TimeFnErr::WrongFormat(s.to_owned()))),
                Ok(date) => Ok(TimeFn::Date(Rc::new(date))),
            }
        }
    }

    fn get_time_payload(&self) -> Vec<Rc<Date>> {
        match *self {
            TimeFn::Date(ref payload) => vec![payload.clone()],
            TimeFn::Now => vec![Rc::new(UTC::now())],
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OpArgTerm {
    Terminal(Terminal),
    String(String),
    TimePayload(TimeFn),
}

impl<'a> OpArgTerm {
    fn from(other: &OpArgTermBorrowed<'a>, context: &mut Context) -> Result<OpArgTerm, ParseErrF> {
        match *other {
            OpArgTermBorrowed::Terminal(slice) => {
                let t = Terminal::from_slice(slice, context)?;
                Ok(OpArgTerm::Terminal(t))
            }
            OpArgTermBorrowed::String(slice) => {
                Ok(OpArgTerm::String(String::from_utf8_lossy(slice).into_owned()))
            }
        }
    }

    fn time_payload(other: Option<&(CompOperator, OpArgTermBorrowed<'a>)>,
                    context: &mut Context)
                    -> Result<(CompOperator, OpArgTerm), ParseErrF> {
        match other {
            None => Ok((CompOperator::Equal, OpArgTerm::TimePayload(TimeFn::IsVar))),
            Some(&(ref op, ref term)) => {
                if !op.is_equal() {
                    return Err(ParseErrF::TimeFnErr(TimeFnErr::NotAssignment));
                }
                match *term {
                    OpArgTermBorrowed::String(slice) => {
                        let date = TimeFn::from_str(slice)?;
                        Ok((CompOperator::Equal, OpArgTerm::TimePayload(date)))
                    }
                    OpArgTermBorrowed::Terminal(_) => {
                        let var = OpArgTerm::from(term, context)?;
                        if var.is_var() {
                            Ok((CompOperator::Equal, var))
                        } else {
                            return Err(ParseErrF::TimeFnErr(TimeFnErr::IsNotVar));
                        }
                    }
                }
            }
        }
    }

    #[inline]
    fn is_var(&self) -> bool {
        match *self {
            OpArgTerm::Terminal(ref t) => t.is_var(),
            _ => false,
        }
    }

    #[inline]
    fn get_var(&self) -> Rc<Var> {
        match *self {
            OpArgTerm::Terminal(ref term) => term.get_var(),
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
    op_arg: Option<OpArg>,
    pub kind: VarKind,
}

#[derive(Debug, Clone, Copy)]
pub enum VarKind {
    Normal,
    Time,
    TimeDecl,
}

impl Var {
    pub fn from<'a>(input: &VarBorrowed<'a>, context: &mut Context) -> Result<Var, ParseErrF> {
        let &VarBorrowed { name: TerminalBorrowed(name), ref op_arg } = input;
        let mut kind = VarKind::Normal;
        let op_arg = match *op_arg {
            Some(ref op_arg) => {
                let t = OpArg::from(op_arg, context)?;
                match t {
                    OpArg::TimeDecl(_) => {
                        kind = VarKind::TimeDecl;
                    }
                    OpArg::TimeVar => {
                        kind = VarKind::Time;
                    }
                    _ => return Err(ParseErrF::WrongDef),
                }
                Some(t)
            }
            None => None,
        };
        let name = unsafe { String::from(str::from_utf8_unchecked(name)) };
        if reserved(&name) {
            return Err(ParseErrF::ReservedKW(name));
        }
        Ok(Var {
            name: name,
            op_arg: op_arg,
            kind: kind,
        })
    }

    pub fn get_dates(&self) -> Vec<Rc<Date>> {
        let h = HashMap::new();
        self.op_arg.as_ref().unwrap().get_time_payload(&h)
    }

    fn var_equality(&self, v1: &Var) -> bool {
        (&*v1 as *const Var) == (self as *const Var)
    }
}

impl ::std::cmp::PartialEq for Var {
    fn eq(&self, other: &Var) -> bool {
        let s_address = &*self as *const Var as usize;
        let o_address = &*other as *const Var as usize;
        s_address == o_address
    }
}

impl ::std::cmp::Eq for Var {}

impl ::std::hash::Hash for Var {
    fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
        let address = &*self as *const Var as usize;
        address.hash(state);
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Skolem {
    pub name: String,
    op_arg: Option<OpArg>,
}

impl Skolem {
    pub fn from<'a>(input: &SkolemBorrowed<'a>,
                    context: &mut Context)
                    -> Result<Skolem, ParseErrF> {
        let &SkolemBorrowed { name: TerminalBorrowed(name), ref op_arg } = input;
        let op_arg = match *op_arg {
            Some(ref op_arg) => {
                let t = OpArg::from(op_arg, context)?;
                Some(t)
            }
            None => None,
        };
        let name = unsafe { String::from(str::from_utf8_unchecked(name)) };
        if reserved(&name) {
            return Err(ParseErrF::ReservedKW(name));
        }
        Ok(Skolem {
            name: name,
            op_arg: op_arg,
        })
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Terminal {
    FreeTerm(Rc<Var>),
    GroundedTerm(Rc<String>),
    Keyword(&'static str),
}

impl<'a> Terminal {
    fn from(other: &TerminalBorrowed<'a>, context: &mut Context) -> Result<Terminal, ParseErrF> {
        let &TerminalBorrowed(slice) = other;
        let name = unsafe { String::from(str::from_utf8_unchecked(slice)) };
        if reserved(&name) {
            return Err(ParseErrF::ReservedKW(name));
        }
        for v in &context.vars {
            if v.name == name {
                return Ok(Terminal::FreeTerm(v.clone()));
            }
        }
        Ok(Terminal::GroundedTerm(Rc::new(name)))
    }

    fn from_slice(slice: &[u8], context: &mut Context) -> Result<Terminal, ParseErrF> {
        let name = unsafe { String::from(str::from_utf8_unchecked(slice)) };
        if reserved(&name) {
            return Err(ParseErrF::ReservedKW(name));
        }
        for v in &context.vars {
            if v.name == name {
                return Ok(Terminal::FreeTerm(v.clone()));
            }
        }
        Ok(Terminal::GroundedTerm(Rc::new(name)))
    }

    fn is_var(&self) -> bool {
        if let Terminal::FreeTerm(_) = *self {
            true
        } else {
            false
        }
    }

    fn get_name(&self) -> Rc<String> {
        if let Terminal::GroundedTerm(ref name) = *self {
            name.clone()
        } else {
            panic!()
        }
    }

    fn is_grounded(&self) -> bool {
        if let Terminal::GroundedTerm(_) = *self {
            true
        } else {
            false
        }
    }

    pub fn get_var(&self) -> Rc<Var> {
        if let Terminal::FreeTerm(ref var) = *self {
            var.clone()
        } else {
            panic!()
        }
    }
}

fn reserved(s: &str) -> bool {
    match s {
        "let" | "time_calc" | "exists" | "fn" | "time" => true,
        _ => false,
    }
}

mod errors {
    #[derive(Debug, PartialEq, Eq)]
    pub enum TimeFnErr {
        MultiAssign,
        NotAssignment,
        WrongFormat(String),
        IsNotVar,
        InsufArgs,
    }
}
