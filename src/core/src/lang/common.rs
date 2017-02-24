pub use self::errors::TimeFnErr;

use FLOAT_EQ_ULPS;
use TIME_EQ_DIFF;

use super::Date;
use agent;
use agent::{BmsWrapper, Representation};
use lang::errors::ParseErrF;
use lang::logsent::*;
use lang::parser::*;

use chrono::{Duration, UTC};
use float_cmp::ApproxEqUlps;

use std::collections::HashMap;
use std::rc::Rc;
use std::str;
use std::sync::RwLock;
use std::sync::atomic::AtomicBool;

// Predicate types:

#[derive(Debug, Clone)]
pub enum Predicate {
    FreeClsMemb(FreeClsMemb),
    GroundedClsMemb(GroundedClsMemb),
    FreeClsOwner(FreeClsOwner),
}

impl<'a> Predicate {
    fn from(a: &'a ArgBorrowed<'a>,
            context: &'a mut ParseContext,
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
                let o_val = t.value.read().unwrap();
                if let Some(val) = *o_val {
                    let op = *t.operator.as_ref().unwrap();
                    (Some(op), Some(val))
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
            Predicate::GroundedClsMemb(ref t) => t.value.read().unwrap().is_some(),
            Predicate::FreeClsMemb(ref t) => t.value.is_some(),
            Predicate::FreeClsOwner(ref t) => t.value.is_some(),
        }
    }
}

// Grounded types:

#[derive(Debug, Clone)]
pub enum Grounded {
    Function(Rc<GroundedFunc>),
    Class(Rc<GroundedClsMemb>),
}

impl Grounded {
    pub fn get_name(&self) -> String {
        match *self {
            Grounded::Function(ref func) => (&*func).get_name().to_string(),
            Grounded::Class(ref cls) => (&*cls).get_parent().to_string(),
        }
    }
}

pub enum GroundedRef<'a> {
    Function(&'a GroundedFunc),
    Class(&'a GroundedClsMemb),
}

impl<'a> GroundedRef<'a> {
    pub fn update_value(&self, val: Option<f32>) {
        match *self {
            GroundedRef::Function(func) => {
                let mut original = func.args[0].value.write().unwrap();
                *original = val;
            }
            GroundedRef::Class(cls) => {
                let mut original = cls.value.write().unwrap();
                *original = val;
            }
        }
    }
}

#[derive(Debug)]
pub struct GroundedClsMemb {
    term: Rc<String>,
    value: RwLock<Option<f32>>,
    operator: Option<CompOperator>,
    parent: Rc<String>,
    pub bms: Option<Rc<BmsWrapper>>,
}

impl GroundedClsMemb {
    //! Internally the mutable parts are wrapped in `RwLock` types, as they can be accessed
    //! from a multithreaded environment. This provides enough atomicity so the most
    //! time it won't be blocking other reads.
    fn new(term: Rc<String>,
           uval: Option<UVal>,
           parent: Rc<String>,
           dates: Option<Vec<(Date, Option<f32>)>>,
           context: &ParseContext)
           -> Result<GroundedClsMemb, ParseErrF> {
        let val;
        let op;
        let bms;
        if let Some(uval) = uval {
            let UVal { val: val0, op: op0 } = uval;
            val = Some(match val0 {
                Number::UnsignedInteger(val) => {
                    if val == 0 || val == 1 {
                        let t_bms = BmsWrapper::new(false);
                        if let Some(dates) = dates {
                            for (date, val) in dates {
                                t_bms.new_record(Some(date), val, None);
                            }
                        } else {
                            t_bms.new_record(None, Some(val as f32), None);
                        }
                        bms = Some(Rc::new(t_bms));
                        val as f32
                    } else {
                        return Err(ParseErrF::IUVal(val as f32));
                    }
                }
                Number::UnsignedFloat(val) => {
                    if val >= 0. && val <= 1. {
                        let t_bms = BmsWrapper::new(false);
                        if let Some(dates) = dates {
                            for (date, val) in dates {
                                t_bms.new_record(Some(date), val, None);
                            }
                        } else {
                            t_bms.new_record(None, Some(val as f32), None);
                        }
                        bms = Some(Rc::new(t_bms));
                        val
                    } else {
                        return Err(ParseErrF::IUVal(val as f32));
                    }
                }
                Number::SignedFloat(val) => return Err(ParseErrF::IUVal(val as f32)),
                Number::SignedInteger(val) => return Err(ParseErrF::IUVal(val as f32)),
            });
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
            bms = None;
        }
        Ok(GroundedClsMemb {
            term: term,
            value: RwLock::new(val),
            operator: op,
            parent: parent,
            bms: bms,
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
            Some(CompOperator::MoreEqual) => id.push(4),
            Some(CompOperator::LessEqual) => id.push(5),
        }
        if let Some(val) = *self.value.read().unwrap() {
            let mut id_2 = format!("{}", val).into_bytes();
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

    #[inline]
    pub fn get_value(&self) -> Option<f32> {
        if let Some(val) = *self.value.read().unwrap() {
            Some(val)
        } else {
            None
        }
    }

    pub fn update(&self, agent: &Representation, data: &GroundedClsMemb, was_produced: bool) {
        let new_val: Option<f32>;
        {
            let mut value_lock = self.value.write().unwrap();
            new_val = *data.value.read().unwrap();
            *value_lock = new_val;
        }
        if let Some(ref bms) = self.bms {
            if data.bms.is_some() {
                let data_bms = data.bms.as_ref().unwrap();
                bms.update(GroundedRef::Class(self), agent, data_bms, was_produced)
            } else {
                let data_bms = BmsWrapper::new(false);
                data_bms.new_record(None, new_val, None);
                bms.update(GroundedRef::Class(self), agent, &data_bms, was_produced)
            }
        }
    }

    pub fn update_value(&self, val: Option<f32>) {
        *self.value.write().unwrap() = val;
    }

    pub fn from_free(free: &FreeClsMemb, assignment: Rc<String>) -> GroundedClsMemb {
        let bms;
        let val = if free.value.is_some() {
            let t_bms = BmsWrapper::new(false);
            t_bms.new_record(None, free.value, None);
            bms = Some(Rc::new(t_bms));
            Some(free.value.unwrap())
        } else {
            bms = None;
            None
        };
        let op = if free.value.is_some() {
            Some(free.operator.unwrap())
        } else {
            None
        };
        GroundedClsMemb {
            term: assignment,
            value: RwLock::new(val),
            operator: op,
            parent: free.parent.get_name(),
            bms: bms,
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

    pub fn overwrite_time_data(&self, data: &BmsWrapper) {
        self.bms.as_ref().unwrap().overwrite_data(data);
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
        let val_lhs: &Option<f32> = &*self.value.read().unwrap();
        let val_rhs: &Option<f32> = &*other.value.read().unwrap();
        if (val_lhs.is_none() && val_rhs.is_some()) || (val_lhs.is_some() && val_rhs.is_none()) {
            return false;
        }
        match op_lhs {
            CompOperator::Equal => {
                if op_rhs.is_equal() {
                    if let Some(ref val_lhs) = *val_lhs {
                        let val_rhs = val_rhs.as_ref().unwrap();
                        val_lhs.approx_eq_ulps(&val_rhs, FLOAT_EQ_ULPS)
                    } else {
                        true
                    }
                } else if op_rhs.is_more() {
                    val_lhs > val_rhs
                } else if op_rhs.is_less() {
                    val_lhs < val_rhs
                } else if op_rhs.is_more_eq() {
                    if let Some(ref val_lhs) = *val_lhs {
                        let val_rhs = val_rhs.as_ref().unwrap();
                        val_lhs.approx_eq_ulps(&val_rhs, FLOAT_EQ_ULPS) || val_lhs > val_rhs
                    } else {
                        true
                    }
                } else {
                    if let Some(ref val_lhs) = *val_lhs {
                        let val_rhs = val_rhs.as_ref().unwrap();
                        val_lhs.approx_eq_ulps(&val_rhs, FLOAT_EQ_ULPS) || val_lhs < val_rhs
                    } else {
                        true
                    }
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
            CompOperator::MoreEqual => {
                if op_rhs.is_equal() {
                    val_lhs <= val_rhs
                } else {
                    panic!()
                }
            }
            CompOperator::LessEqual => {
                if op_rhs.is_equal() {
                    val_lhs >= val_rhs
                } else {
                    panic!()
                }
            }
        }
    }
}

impl ::std::clone::Clone for GroundedClsMemb {
    fn clone(&self) -> GroundedClsMemb {
        GroundedClsMemb {
            term: self.term.clone(),
            value: RwLock::new(*self.value.read().unwrap()),
            operator: self.operator,
            parent: self.parent.clone(),
            bms: self.bms.clone(),
        }
    }
}

#[derive(Debug)]
pub struct GroundedFunc {
    pub name: Rc<String>,
    pub args: [GroundedClsMemb; 2],
    pub third: Option<GroundedClsMemb>,
    pub bms: Rc<BmsWrapper>,
}

impl ::std::cmp::PartialEq for GroundedFunc {
    fn eq(&self, other: &GroundedFunc) -> bool {
        !(self.name != other.name || self.args != other.args || self.third != other.third)
    }
}

impl ::std::cmp::Eq for GroundedFunc {}

impl GroundedFunc {
    pub fn from_free(free: &FuncDecl,
                     assignments: &HashMap<Rc<Var>, &agent::VarAssignment>,
                     time_assign: &HashMap<Rc<Var>, Rc<BmsWrapper>>)
                     -> Result<GroundedFunc, ()> {
        if !free.variant.is_relational() || free.args.as_ref().unwrap().len() < 2 {
            return Err(());
        }
        let name = match free.name {
            Terminal::GroundedTerm(ref name) => name.clone(),
            _ => panic!(),
        };
        let mut first = None;
        let mut second = None;
        let mut third = None;
        let mut value = None;
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
            if i == 0 {
                value = n_a.get_value();
                first = Some(n_a)
            } else if i == 1 {
                second = Some(n_a)
            } else {
                third = Some(n_a)
            }
        }
        let time_data = free.get_own_time_data(time_assign, value);
        Ok(GroundedFunc {
            name: name,
            args: [first.unwrap(), second.unwrap()],
            third: third,
            bms: Rc::new(time_data),
        })
    }

    #[inline]
    pub fn get_name(&self) -> Rc<String> {
        self.name.clone()
    }

    #[inline]
    pub fn get_value(&self) -> Option<f32> {
        *self.args[0].value.read().unwrap()
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

    pub fn update(&self, agent: &Representation, data: &GroundedFunc, was_produced: bool) {
        {
            let mut value_lock = self.args[0].value.write().unwrap();
            *value_lock = *data.args[0].value.read().unwrap();
        }
        let data_bms = &data.bms;
        self.bms.update(GroundedRef::Function(self), agent, data_bms, was_produced);
    }

    pub fn update_value(&self, val: Option<f32>) {
        let mut value_lock = self.args[0].value.write().unwrap();
        *value_lock = val;
    }
}

impl ::std::clone::Clone for GroundedFunc {
    fn clone(&self) -> GroundedFunc {
        GroundedFunc {
            name: self.name.clone(),
            args: [self.args[0].clone(), self.args[1].clone()],
            third: self.third.clone(),
            bms: Rc::new((&*self.bms).clone()),
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
                CompOperator::MoreEqual => id.push(4),
                CompOperator::LessEqual => id.push(5),
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
    pub fn grounded_eq(&self, other: &GroundedClsMemb) -> bool {
        if self.parent.get_name() != other.parent {
            panic!()
        }
        if self.value.is_some() {
            let val_free = self.value.unwrap();
            let val_grounded = {
                if let Some(val) = *other.value.read().unwrap() {
                    val
                } else {
                    return false;
                }
            };
            match other.operator.unwrap() {
                CompOperator::Equal => {
                    if self.operator.as_ref().unwrap().is_equal() {
                        val_free.approx_eq_ulps(&val_grounded, FLOAT_EQ_ULPS)
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
    pub dates: BmsWrapper,
}

impl FreeClsOwner {
    fn new(term: Rc<String>,
           uval: Option<UVal>,
           parent: &Terminal)
           -> Result<FreeClsOwner, ParseErrF> {
        let (val, op) = match_uval(uval)?;
        let t_bms = BmsWrapper::new(false);
        t_bms.new_record(None, val, None);
        Ok(FreeClsOwner {
            term: term,
            value: val,
            operator: op,
            parent: parent.get_var(),
            dates: t_bms,
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
            Some(CompOperator::MoreEqual) => id.push(4),
            Some(CompOperator::LessEqual) => id.push(5),
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
            let o_val = if let Some(o_val) = *other.value.read().unwrap() {
                o_val
            } else {
                return false;
            };
            match *self.operator.as_ref().unwrap() {
                CompOperator::Equal => val.approx_eq_ulps(&o_val, FLOAT_EQ_ULPS),
                CompOperator::Less => o_val < *val,
                CompOperator::More => o_val > *val,
                CompOperator::MoreEqual => {
                    val.approx_eq_ulps(&o_val, FLOAT_EQ_ULPS) || o_val > *val
                }
                CompOperator::LessEqual => {
                    val.approx_eq_ulps(&o_val, FLOAT_EQ_ULPS) || o_val < *val
                }
            }
        } else {
            true
        }
    }

    pub fn overwrite_time_data(&self, data: &BmsWrapper) {
        self.dates.overwrite_data(data);
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
                     -> Option<Rc<BmsWrapper>> {
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
    pub fn grounded_eq(&self,
                       agent: &agent::Representation,
                       assignments: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>,
                       time_assign: &HashMap<Rc<Var>, Rc<BmsWrapper>>,
                       context: &mut agent::ProofResult)
                       -> Option<bool> {
        match *self {
            Assert::FuncDecl(ref f) => f.grounded_eq(agent, assignments, time_assign, context),
            Assert::ClassDecl(ref c) => c.grounded_eq(agent, assignments, context),
        }
    }

    #[inline]
    pub fn substitute(&self,
                      agent: &agent::Representation,
                      assignments: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>,
                      time_assign: &HashMap<Rc<Var>, Rc<BmsWrapper>>,
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
                context: &mut ParseContext)
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
        let mut first = None;
        let mut second = None;
        let mut third = None;
        let mut val = None;
        let mut args = args.unwrap();
        for (i, a) in args.drain(..).enumerate() {
            let n_a = match a {
                Predicate::GroundedClsMemb(term) => term,
                _ => panic!(),
            };
            if i == 0 {
                val = n_a.get_value();
                first = Some(n_a);
            } else if i == 1 {
                second = Some(n_a);
            } else {
                third = Some(n_a);
            }
        }
        let mut time_data = BmsWrapper::new(false);
        let mut ow = false;
        if let Some(mut oargs) = op_args {
            for arg in oargs.drain(..) {
                match arg {
                    OpArg::TimeDecl(TimeFn::Date(ref date)) => {
                        time_data.new_record(Some(*date), val, None);
                    }
                    OpArg::TimeDecl(TimeFn::Now) => {
                        time_data.new_record(Some(UTC::now()), val, None);
                    }
                    OpArg::OverWrite => {
                        ow = true;
                    }
                    _ => {}
                }
            }
        }
        if time_data.record_len() == 0 {
            time_data.new_record(None, val, None);
        }
        time_data.overwrite = AtomicBool::new(ow);
        GroundedFunc {
            name: name,
            args: [first.unwrap(), second.unwrap()],
            third: third,
            bms: Rc::new(time_data),
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
                             assignments: &HashMap<Rc<Var>, Rc<BmsWrapper>>,
                             value: Option<f32>)
                             -> BmsWrapper {
        if self.op_args.is_none() {
            let t_bms = BmsWrapper::new(false);
            t_bms.new_record(None, value, None);
            return t_bms;
        }
        let mut v = None;
        let mut ow = false;
        for arg in self.op_args.as_ref().unwrap() {
            match *arg {
                OpArg::TimeDecl(_) |
                OpArg::TimeVarAssign(_) => {
                    v = Some(arg.get_time_payload(assignments, value));
                }
                OpArg::OverWrite => {
                    ow = true;
                }
                _ => {}
            }
        }
        if v.is_none() {
            let bms = BmsWrapper::new(ow);
            bms.new_record(None, value, None);
            bms
        } else {
            let mut bms = v.unwrap();
            bms.overwrite = AtomicBool::new(ow);
            bms
        }
    }

    fn get_time_decl(&self, var0: &Var) -> bool {
        if self.op_args.is_none() {
            return false;
        }
        for arg in self.op_args.as_ref().unwrap() {
            if let OpArg::TimeVarFrom(ref var1) = *arg {
                return var1.var_equality(var0);
            }
        }
        false
    }

    fn get_dates(&self,
                 agent: &agent::Representation,
                 var_assign: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>)
                 -> Option<Rc<BmsWrapper>> {
        if self.is_grounded() {
            let sbj = self.args.as_ref().unwrap();
            let grfunc = self.clone().into_grounded();
            if let Some(relation) = agent.get_relationship(&grfunc, sbj[0].get_name()) {
                return Some(relation.bms.clone());
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
                                return Some(current.bms.clone());
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
                        context: &mut ParseContext)
                        -> Result<FuncDecl, ParseErrF> {
        if other.args.is_some() || other.op_args.is_none() {
            return Err(ParseErrF::WrongDef);
        }
        let op_args = match other.op_args {
            Some(ref oargs) => {
                let mut v0 = Vec::with_capacity(oargs.len());
                for e in oargs {
                    let arg = match OpArg::from(e, context) {
                        Err(err) => return Err(err),
                        Ok(a) => a,
                    };
                    match arg {
                        // Generic(OpArgTerm, Option<(CompOperator, OpArgTerm)>)
                        OpArg::Generic(ref v0, Some((_, ref v1))) => {
                            if (!v0.is_var() | !v1.is_var()) ||
                               (!v0.get_var().is_time_var() | !v1.get_var().is_time_var()) {
                                return Err(TimeFnErr::IsNotVar.into());
                            }
                        }
                        _ => return Err(TimeFnErr::InsufArgs.into()),
                    }
                    v0.push(arg);
                }
                Some(v0)
            }
            None => return Err(ParseErrF::WrongDef),
        };
        if op_args.as_ref().unwrap().is_empty() {
            return Err(TimeFnErr::InsufArgs.into());
        }
        Ok(FuncDecl {
            name: Terminal::Keyword("time_calc"),
            args: None,
            op_args: op_args,
            variant: FuncVariants::TimeCalc,
        })
    }

    fn decl_relational_fn(other: &FuncDeclBorrowed<'a>,
                          context: &mut ParseContext,
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
                             context: &mut ParseContext,
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
    fn grounded_eq(&self,
                   agent: &agent::Representation,
                   assignments: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>,
                   time_assign: &HashMap<Rc<Var>, Rc<BmsWrapper>>,
                   context: &mut agent::ProofResult)
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
                                context.antecedents.push(Grounded::Function(current.clone()));
                                if let Some(date) = current.bms
                                    .newest_date(&context.newest_grfact) {
                                    context.newest_grfact = date;
                                }
                                if **current != grfunc {
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

    fn time_resolution(&self, assignments: &HashMap<Rc<Var>, Rc<BmsWrapper>>) -> Option<bool> {
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
                  time_assign: &HashMap<Rc<Var>, Rc<BmsWrapper>>,
                  context: &mut agent::ProofResult) {
        if let Ok(grfunc) = GroundedFunc::from_free(self,
                                                    assignments.as_ref().unwrap(),
                                                    time_assign) {
            let grfunc = Rc::new(grfunc);
            agent.up_relation(grfunc.clone(), Some(context));
            context.grounded.push((Grounded::Function(grfunc.clone()), grfunc.bms.get_last_date()))
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
                context: &mut ParseContext)
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
                             assignments: &HashMap<Rc<Var>, Rc<BmsWrapper>>,
                             value: Option<f32>)
                             -> BmsWrapper {
        if self.op_args.is_none() {
            let bms = BmsWrapper::new(false);
            bms.new_record(None, value, None);
            return bms;
        }
        let mut v = None;
        let mut ow = false;
        for arg in self.op_args.as_ref().unwrap() {
            match *arg {
                OpArg::TimeDecl(_) |
                OpArg::TimeVarAssign(_) => {
                    v = Some(arg.get_time_payload(assignments, value));
                }
                OpArg::OverWrite => {
                    ow = true;
                }
                _ => {}
            }
        }
        if v.is_none() {
            let bms = BmsWrapper::new(ow);
            bms.new_record(None, value, None);
            bms
        } else {
            let mut bms = v.unwrap();
            bms.overwrite = AtomicBool::new(ow);
            bms
        }
    }

    fn get_time_decl(&self, var0: &Var) -> bool {
        if self.op_args.is_none() {
            return false;
        }
        for arg in self.op_args.as_ref().unwrap() {
            if let OpArg::TimeVarFrom(ref var1) = *arg {
                return var1.var_equality(var0);
            }
        }
        false
    }

    fn get_dates(&self,
                 agent: &agent::Representation,
                 var_assign: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>)
                 -> Option<Rc<BmsWrapper>> {
        let arg = &self.args[0];
        match *arg {
            Predicate::FreeClsMemb(ref free) => {
                if var_assign.is_none() {
                    return None;
                }
                if let Some(entity) = var_assign.as_ref().unwrap().get(&free.term) {
                    if let Some(grounded) = entity.get_class(free.parent.get_name()) {
                        if free.grounded_eq(grounded) {
                            return grounded.bms.clone();
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
                        return grounded.bms.clone();
                    }
                } else {
                    return None;
                }
            }
            _ => return None, // this path won't be taken in any program
        }
        None
    }

    pub fn get_time_payload(&self, value: Option<f32>) -> Option<BmsWrapper> {
        if self.op_args.is_none() {
            return None;
        }
        for arg in self.op_args.as_ref().unwrap() {
            if let OpArg::TimeDecl(ref decl) = *arg {
                return Some(decl.get_time_payload(value));
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
    fn grounded_eq(&self,
                   agent: &agent::Representation,
                   assignments: &Option<HashMap<Rc<Var>, &agent::VarAssignment>>,
                   context: &mut agent::ProofResult)
                   -> Option<bool> {
        for a in &self.args {
            match *a {
                Predicate::FreeClsMemb(ref free) => {
                    if assignments.is_none() {
                        return None;
                    }
                    if let Some(entity) = assignments.as_ref().unwrap().get(&free.term) {
                        if let Some(current) = entity.get_class(free.parent.get_name()) {
                            context.antecedents.push(Grounded::Class(current.clone()));
                            if let Some(date) = current.bms
                                .as_ref()
                                .unwrap()
                                .newest_date(&context.newest_grfact) {
                                context.newest_grfact = date;
                            }
                            if !free.grounded_eq(current) {
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
                    if let Some(current) = entity {
                        context.antecedents.push(Grounded::Class(current.clone()));
                        if let Some(date) = current.bms
                            .as_ref()
                            .unwrap()
                            .newest_date(&context.newest_grfact) {
                            context.newest_grfact = date;
                        }
                        if *current != *compare {
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
                  time_assign: &HashMap<Rc<Var>, Rc<BmsWrapper>>,
                  context: &mut agent::ProofResult) {
        let time_data = self.get_own_time_data(time_assign, None);
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
            let t = time_data.clone();
            t.replace_last_val(grfact.get_value());
            grfact.overwrite_time_data(&t);
            let grfact = Rc::new(grfact);
            context.grounded.push((Grounded::Class(grfact.clone()),
                                   grfact.bms.as_ref().unwrap().get_last_date()));
            agent.up_membership(grfact.clone(), Some(context))
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
    OverWrite,
    TimeDecl(TimeFn),
    TimeVar,
    TimeVarAssign(Rc<Var>),
    TimeVarFrom(Rc<Var>),
}

impl<'a> OpArg {
    pub fn from(other: &OpArgBorrowed<'a>, context: &mut ParseContext) -> Result<OpArg, ParseErrF> {
        let t0 = match OpArgTerm::from(&other.term, context) {
            Err(ParseErrF::ReservedKW(kw)) => {
                match &*kw {
                    "time" => {
                        let targ = OpArg::ignore_kw(other, "time", context)?;
                        return Ok(targ);
                    }
                    "overwrite" => return Ok(OpArg::OverWrite),
                    _ => return Err(ParseErrF::ReservedKW(kw)),
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
                 context: &mut ParseContext)
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
            OpArg::TimeVarAssign(ref var0) |
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
            OpArg::OverWrite => id.push(5),
        }
        id
    }

    fn get_time_payload(&self,
                        assignments: &HashMap<Rc<Var>, Rc<BmsWrapper>>,
                        value: Option<f32>)
                        -> BmsWrapper {
        let bms = BmsWrapper::new(false);
        match *self {
            OpArg::TimeDecl(TimeFn::Date(ref payload)) => {
                bms.new_record(Some(*payload), value, None);
            }
            OpArg::TimeDecl(TimeFn::Now) => {
                bms.new_record(None, value, None);
            }
            OpArg::TimeVarAssign(ref var) => return (&**(assignments.get(var).unwrap())).clone(),
            _ => panic!(),
        }
        bms
    }

    fn compare_time_args(&self, assignments: &HashMap<Rc<Var>, Rc<BmsWrapper>>) -> bool {
        let (term, op, comp) = match *self {
            OpArg::Generic(ref term, Some((ref op, ref comp))) => (term, op, comp),
            _ => return false,
        };
        let var0 = term.get_var();
        let arg0 = assignments.get(&var0).unwrap().get_last_date();
        let var1 = comp.get_var();
        let arg1 = assignments.get(&var1).unwrap().get_last_date();
        match *op {
            CompOperator::Equal => {
                let comp_diff = Duration::seconds(TIME_EQ_DIFF);
                let lower_bound = arg0 - comp_diff;
                let upper_bound = arg0 + comp_diff;
                !((arg1 < lower_bound) || (arg1 > upper_bound))
            }
            CompOperator::More => arg0 > arg1,
            CompOperator::Less => arg0 < arg1,
            CompOperator::MoreEqual => {
                let comp_diff = Duration::seconds(TIME_EQ_DIFF);
                let lower_bound = arg0 - comp_diff;
                let upper_bound = arg0 + comp_diff;
                !((arg1 < lower_bound) || (arg1 > upper_bound)) || arg0 > arg1
            }
            CompOperator::LessEqual => {
                let comp_diff = Duration::seconds(TIME_EQ_DIFF);
                let lower_bound = arg0 - comp_diff;
                let upper_bound = arg0 + comp_diff;
                !((arg1 < lower_bound) || (arg1 > upper_bound)) || arg0 < arg1
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TimeFn {
    Now,
    Date(Date),
    IsVar,
}

impl TimeFn {
    #[allow(should_implement_trait)]
    fn from_str(slice: &[u8]) -> Result<TimeFn, ParseErrF> {
        if slice == b"Now" {
            Ok(TimeFn::Now)
        } else {
            let s = unsafe { str::from_utf8_unchecked(slice) };
            match s.parse::<Date>() {
                Err(_) => Err(TimeFnErr::WrongFormat(s.to_owned()).into()),
                Ok(date) => Ok(TimeFn::Date(date)),
            }
        }
    }

    fn get_time_payload(&self, value: Option<f32>) -> BmsWrapper {
        let bms = BmsWrapper::new(false);
        match *self {
            TimeFn::Date(ref payload) => {
                bms.new_record(Some(*payload), value, None);
            }
            TimeFn::Now => {
                bms.new_record(None, value, None);
            }
            _ => panic!(),
        }
        bms
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OpArgTerm {
    Terminal(Terminal),
    String(String),
    TimePayload(TimeFn),
}

impl<'a> OpArgTerm {
    fn from(other: &OpArgTermBorrowed<'a>,
            context: &mut ParseContext)
            -> Result<OpArgTerm, ParseErrF> {
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
                    context: &mut ParseContext)
                    -> Result<(CompOperator, OpArgTerm), ParseErrF> {
        match other {
            None => Ok((CompOperator::Equal, OpArgTerm::TimePayload(TimeFn::IsVar))),
            Some(&(ref op, ref term)) => {
                if !op.is_equal() {
                    return Err(TimeFnErr::NotAssignment.into());
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
                            Err(TimeFnErr::IsNotVar.into())
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

#[derive(Debug, Clone)]
pub enum VarKind {
    Normal,
    Time,
    TimeDecl,
}

impl Var {
    pub fn from<'a>(input: &VarBorrowed<'a>, context: &mut ParseContext) -> Result<Var, ParseErrF> {
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

    pub fn get_dates(&self) -> BmsWrapper {
        let h = HashMap::new();
        self.op_arg.as_ref().unwrap().get_time_payload(&h, None)
    }

    fn var_equality(&self, v1: &Var) -> bool {
        (&*v1 as *const Var) == (self as *const Var)
    }

    pub fn is_normal(&self) -> bool {
        match self.kind {
            VarKind::Normal => true,
            _ => false,
        }
    }

    fn is_time_var(&self) -> bool {
        match self.kind {
            VarKind::Time | VarKind::TimeDecl => true,
            _ => false,
        }
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
                    context: &mut ParseContext)
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
    fn from(other: &TerminalBorrowed<'a>,
            context: &mut ParseContext)
            -> Result<Terminal, ParseErrF> {
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

    fn from_slice(slice: &[u8], context: &mut ParseContext) -> Result<Terminal, ParseErrF> {
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
        "let" | "time_calc" | "exists" | "fn" | "time" | "overwrite" | "self" | "none" => true,
        _ => false,
    }
}

mod errors {
    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    pub enum TimeFnErr {
        MultiAssign,
        NotAssignment,
        WrongFormat(String),
        IsNotVar,
        InsufArgs,
    }

    impl Into<ParseErrF> for TimeFnErr {
        fn into(self) -> ParseErrF {
            ParseErrF::TimeFnErr(self)
        }
    }
}
