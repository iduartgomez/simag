use std::str;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use chrono::{UTC, DateTime};

use lang::parser::*;
use lang::logsent::*;
use agent;

// Predicate types:

#[derive(Debug, Clone)]
pub enum Predicate {
    FreeClsMemb(FreeClsMemb),
    GroundedClsMemb(GroundedClsMemb),
}

impl<'a> Predicate {
    fn from(a: &'a ArgBorrowed<'a>,
            context: &'a mut Context,
            func_name: &'a Terminal)
            -> Result<Predicate, ParseErrF> {
        match Terminal::from(&a.term, context) {
            Ok(Terminal::FreeTerm(ft)) => {
                let t = FreeClsMemb::new(ft, a.uval, func_name, None);
                if t.is_err() {
                    return Err(t.unwrap_err());
                }
                Ok(Predicate::FreeClsMemb(t.unwrap()))
            }
            Ok(Terminal::GroundedTerm(gt)) => {
                let t =
                    GroundedClsMemb::new(gt, a.uval.clone(), func_name.to_string(), None, context)?;
                Ok(Predicate::GroundedClsMemb(t))
            }
            Ok(Terminal::Keyword(kw)) => return Err(ParseErrF::ReservedKW(String::from(kw))),
            Err(err) => Err(err),
        }
    }

    #[inline]
    pub fn is_var(&self) -> bool {
        match *self {
            Predicate::FreeClsMemb(_) => true,
            Predicate::GroundedClsMemb(_) => false,
        }
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        match *self {
            Predicate::GroundedClsMemb(ref t) => t.get_name(),
            _ => panic!("simag: expected a grounded terminal, found a free terminal"),
        }
    }

    #[inline]
    fn get_id(&self) -> Vec<u8> {
        match *self {
            Predicate::FreeClsMemb(ref t) => t.get_id(),
            Predicate::GroundedClsMemb(ref t) => t.get_id(),
        }
    }

    fn has_uval(&self) -> bool {
        match *self {
            Predicate::GroundedClsMemb(ref t) => {
                if t.value.is_some() {
                    true
                } else {
                    false
                }
            }
            Predicate::FreeClsMemb(ref t) => {
                if t.value.is_some() {
                    true
                } else {
                    false
                }
            }
        }
    }
}

// Grounded types:

#[derive(Debug)]
pub enum Grounded {
    Function(Rc<GroundedFunc>),
    Terminal(Rc<GroundedClsMemb>),
}

#[derive(Debug, Clone)]
pub struct GroundedClsMemb {
    term: String,
    value: Option<RefCell<f32>>,
    operator: Option<CompOperator>,
    parent: String,
    dates: RefCell<Vec<DateTime<UTC>>>,
}

impl GroundedClsMemb {
    fn new(term: String,
           uval: Option<UVal>,
           parent: String,
           dates: Option<Vec<DateTime<UTC>>>,
           context: &Context)
           -> Result<GroundedClsMemb, ParseErrF> {
        let val;
        let op;
        if let Some(uval) = uval {
            let UVal { val: val0, op: op0 } = uval;
            val = Some(RefCell::new(match val0 {
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
        let dates_e;
        if let Some(dates) = dates {
            dates_e = dates;
        } else {
            dates_e = vec![UTC::now()];
        }
        Ok(GroundedClsMemb {
            term: term,
            value: val,
            operator: op,
            parent: parent,
            dates: RefCell::new(dates_e),
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
            let mut id_2 = format!("{}", *val.borrow()).into_bytes();
            id.append(&mut id_2);
        }
        id
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        &self.term
    }

    #[inline]
    pub fn get_parent(&self) -> &str {
        self.parent.as_ref()
    }

    #[inline]
    pub fn get_parent_as_string_ref(&self) -> &String {
        &self.parent
    }

    pub fn update(&self, data: Rc<GroundedClsMemb>) {
        let data: &GroundedClsMemb = &*data;
        *self.value.as_ref().unwrap().borrow_mut() = *data.value.as_ref().unwrap().borrow();
    }

    pub fn from_free(free: &FreeClsMemb, assignment: &str) -> GroundedClsMemb {
        let val;
        let op;
        if free.value.is_some() {
            val = Some(RefCell::new(free.value.unwrap()));
            op = Some(free.operator.unwrap());
        } else {
            val = None;
            op = None;
        }
        GroundedClsMemb {
            term: String::from(assignment),
            value: val,
            operator: op,
            parent: free.parent.to_string(),
            dates: RefCell::new(vec![UTC::now()]),
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
        } else if other.operator.is_some() && self.operator.is_none() {
            return false;
        }
        true
    }
}

impl ::std::cmp::PartialEq for GroundedClsMemb {
    fn eq(&self, other: &GroundedClsMemb) -> bool {
        if self.term != other.term {
            panic!("simag: grounded terms with different names cannot be compared")
        }
        if self.parent != other.parent {
            panic!("simag: grounded terms with different classes cannot be compared")
        }
        let val_lhs = &self.value;
        let val_rhs = &other.value;
        let op_lhs;
        let op_rhs;
        if let Some(op) = other.operator {
            op_rhs = op;
            op_lhs = self.operator.unwrap();
        } else {
            return true;
        }
        match op_lhs {
            CompOperator::Equal => {
                if op_rhs.is_equal() {
                    if val_lhs == val_rhs {
                        return true;
                    } else {
                        return false;
                    }
                } else if op_rhs.is_more() {
                    if val_lhs > val_rhs {
                        return true;
                    } else {
                        return false;
                    }
                } else {
                    if val_lhs < val_rhs {
                        return true;
                    } else {
                        return false;
                    }
                }
            }
            CompOperator::More => {
                if op_rhs.is_equal() {
                    if val_lhs < val_rhs {
                        return true;
                    } else {
                        return false;
                    }
                } else {
                    panic!("simag: grounded terms operators in assertments \
                            must be assignments")
                }
            }
            CompOperator::Less => {
                if op_rhs.is_equal() {
                    if val_lhs > val_rhs {
                        return true;
                    } else {
                        return false;
                    }
                } else {
                    panic!("simag: grounded terms operators in assertments \
                            must be assignments")
                }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct GroundedFunc {
    pub name: String,
    pub args: [GroundedClsMemb; 2],
    pub third: Option<GroundedClsMemb>,
}

impl GroundedFunc {
    pub fn from_free(free: &FuncDecl,
                     assignments: &HashMap<*const Var, &agent::VarAssignment>)
                     -> Result<GroundedFunc, ()> {
        if !free.variant.is_relational() || free.args.as_ref().unwrap().len() < 2 {
            return Err(());
        }
        let name = match free.name {
            Terminal::GroundedTerm(ref name) => name.clone(),
            _ => panic!("simag: expected a grounded terminal, found a free terminal"),
        };
        let mut first = None;
        let mut second = None;
        let mut third = None;
        for (i, a) in free.args.as_ref().unwrap().iter().enumerate() {
            let n_a = match a {
                &Predicate::FreeClsMemb(ref free) => {
                    if let Some(ref entity) = assignments.get(&free.term) {
                        GroundedClsMemb::from_free(free, entity.name)
                    } else {
                        return Err(());
                    }
                }
                &Predicate::GroundedClsMemb(ref term) => term.clone(),
            };
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
        })
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    #[inline]
    pub fn get_parent_as_string_ref(&self) -> &String {
        &self.name
    }

    pub fn comparable_entity(&self,
                             free: &FuncDecl,
                             entity_name: &str,
                             var: Option<*const Var>)
                             -> bool {
        if free.get_name() != self.name {
            return false;
        }
        if var.is_some() {
            let var = var.unwrap();
            if let Some(pos) = free.var_in_pos(var) {
                match pos {
                    0 => {
                        if self.args[0].get_name() == entity_name {
                            return true;
                        }
                    }
                    1 => {
                        if self.args[1].get_name() == entity_name {
                            return true;
                        }
                    }
                    2 => {
                        if let Some(ref term) = self.third {
                            if term.get_name() == entity_name {
                                return true;
                            }
                        }
                    }
                    _ => return false,
                }
            }
        } else {
            if let Some(pos) = free.term_in_pos(entity_name) {
                match pos {
                    0 => {
                        if self.args[0].get_name() == entity_name {
                            return true;
                        }
                    }
                    1 => {
                        if self.args[1].get_name() == entity_name {
                            return true;
                        }
                    }
                    2 => {
                        if let Some(ref term) = self.third {
                            if term.get_name() == entity_name {
                                return true;
                            }
                        }
                    }
                    _ => return false,
                }
            }
        }
        false
    }

    pub fn comparable(&self, other: &GroundedFunc) -> bool {
        if other.get_name() != self.name {
            return false;
        }
        if !self.args[0].comparable(&other.args[0]) {
            return false;
        }
        if !self.args[1].comparable(&other.args[1]) {
            return false;
        }
        if self.third.is_some() && other.third.is_some() {
            if !self.third.as_ref().unwrap().comparable(other.third.as_ref().unwrap()) {
                return false;
            }
        } else if self.third.is_none() && other.third.is_none() {
            return true;
        } else {
            return false;
        }
        true
    }

    pub fn update(&self, data: Rc<GroundedFunc>) {
        let data: &GroundedFunc = &*data;
        *self.args[0].value.as_ref().unwrap().borrow_mut() =
            *data.args[0].value.as_ref().unwrap().borrow();
    }
}

impl ::std::clone::Clone for GroundedFunc {
    fn clone(&self) -> GroundedFunc {
        GroundedFunc {
            name: self.name.clone(),
            args: [self.args[0].clone(), self.args[1].clone()],
            third: self.third.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FreeClsMemb {
    term: *const Var,
    value: Option<f32>,
    operator: Option<CompOperator>,
    parent: Terminal,
    dates: Option<Vec<i32>>,
}

impl FreeClsMemb {
    fn new(term: *const Var,
           uval: Option<UVal>,
           parent: &Terminal,
           _dates: Option<Vec<DateTime<UTC>>>)
           -> Result<FreeClsMemb, ParseErrF> {
        let val;
        let op;
        if uval.is_some() {
            let uval = uval.unwrap();
            val = match uval.val {
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
            op = Some(uval.op);
        } else {
            val = None;
            op = None;
        }
        Ok(FreeClsMemb {
            term: term,
            value: val,
            operator: op,
            parent: parent.clone(),
            dates: None,
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
    pub fn get_var_ref(&self) -> &Var {
        unsafe { &*(self.term) }
    }

    /// Compares a free term with a grounded term, assumes they are comparable
    /// (panics otherwise).
    fn equal_to_grounded(&self, other: &GroundedClsMemb) -> bool {
        if self.parent.get_name() != other.parent.as_str() {
            panic!("simag: grounded terms from different classes cannot be compared")
        }
        if self.value.is_some() {
            let val_free = self.value.unwrap();
            let val_grounded = *other.value.as_ref().unwrap().borrow();
            match other.operator.unwrap() {
                CompOperator::Equal => {
                    if self.operator.as_ref().unwrap().is_equal() {
                        if val_free == val_grounded {
                            return true;
                        } else {
                            return false;
                        }
                    } else if self.operator.as_ref().unwrap().is_more() {
                        if val_grounded > val_free {
                            return true;
                        } else {
                            return false;
                        }
                    } else {
                        if val_grounded < val_free {
                            return true;
                        } else {
                            return false;
                        }
                    }
                }
                _ => {
                    panic!("simag: grounded terminal operators in assertments must be assignments")
                }
            }
        } else {
            true
        }
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
    pub fn get_name(&self) -> &str {
        match self {
            &Assert::FuncDecl(ref f) => f.get_name(),
            &Assert::ClassDecl(ref c) => c.get_name_as_str(),
        }
    }

    #[inline]
    pub fn get_name_as_string_ref(&self) -> &String {
        match self {
            &Assert::FuncDecl(ref f) => f.get_name_as_string_ref(),
            &Assert::ClassDecl(ref c) => c.get_name_as_string_ref(),
        }
    }

    #[inline]
    pub fn parent_is_grounded(&self) -> bool {
        match self {
            &Assert::FuncDecl(ref f) => f.parent_is_grounded(),
            &Assert::ClassDecl(ref c) => c.parent_is_grounded(),
        }
    }

    #[inline]
    pub fn unwrap_fn(self) -> FuncDecl {
        match self {
            Assert::FuncDecl(f) => f,
            Assert::ClassDecl(_) => {
                panic!("simag: expected a function declaration, found class instead")
            }
        }
    }

    #[inline]
    pub fn unwrap_cls(self) -> ClassDecl {
        match self {
            Assert::FuncDecl(_) => {
                panic!("simag: expected a class declaration, found function instead")
            }
            Assert::ClassDecl(c) => c,
        }
    }

    #[inline]
    pub fn equal_to_grounded(&self,
                             agent: &agent::Representation,
                             assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>)
                             -> Option<bool> {
        match self {
            &Assert::FuncDecl(ref f) => f.equal_to_grounded(agent, assignments),
            &Assert::ClassDecl(ref c) => c.equal_to_grounded(agent, assignments),
        }
    }

    #[inline]
    pub fn is_class(&self) -> bool {
        match self {
            &Assert::FuncDecl(_) => false,
            &Assert::ClassDecl(_) => true,
        }
    }

    #[inline]
    pub fn contains(&self, var: &Var) -> bool {
        match self {
            &Assert::FuncDecl(ref f) => f.contains(var),
            &Assert::ClassDecl(ref c) => c.contains(var),
        }
    }

    #[inline]
    pub fn substitute(&self,
                      agent: &agent::Representation,
                      assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>,
                      context: &mut agent::ProofResult) {
        match self {
            &Assert::FuncDecl(ref f) => f.substitute(agent, assignments, context),
            &Assert::ClassDecl(ref c) => c.substitute(agent, assignments, context),
        }
    }

    #[inline]
    pub fn get_id(&self) -> Vec<u8> {
        match self {
            &Assert::FuncDecl(ref f) => f.get_id(),
            &Assert::ClassDecl(ref c) => c.get_id(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    name: Terminal,
    args: Option<Vec<Predicate>>,
    op_args: Option<Vec<OpArg>>,
    variant: FuncVariants,
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
            FuncVariants::TimeCalc => return FuncDecl::decl_timecalc_fn(other, context),
            FuncVariants::Relational => {
                return FuncDecl::decl_relational_fn(other, context, func_name)
            }
            FuncVariants::NonRelational => {
                return FuncDecl::decl_nonrelational_fn(other, context, func_name)
            }
        }
    }

    /// Assumes all arguments are grounded and converts to a GroundedFunc (panics otherwise).
    pub fn into_grounded(self) -> GroundedFunc {
        let name = match self.name {
            Terminal::GroundedTerm(ref name) => name.clone(),
            _ => panic!("simag: expected a grounded terminal, found a free terminal"),
        };
        let mut first = None;
        let mut second = None;
        let mut third = None;
        let mut args = self.args.unwrap();
        for (i, a) in args.drain(..).enumerate() {
            let n_a = match a {
                Predicate::GroundedClsMemb(term) => term,
                _ => {
                    panic!("simag: found a non-grounded terminal while making a grounded \
                            relational function")
                }
            };
            if i == 0 {
                first = Some(n_a)
            } else if i == 1 {
                second = Some(n_a)
            } else {
                third = Some(n_a)
            }
        }
        GroundedFunc {
            name: name,
            args: [first.unwrap(), second.unwrap()],
            third: third,
        }
    }

    pub fn is_grounded(&self) -> bool {
        if !self.parent_is_grounded() {
            return false;
        }
        for a in self.args.as_ref().unwrap().iter() {
            match a {
                &Predicate::GroundedClsMemb(_) => {}
                _ => return false,
            }
        }
        true
    }

    pub fn get_name(&self) -> &str {
        match self.name {
            Terminal::FreeTerm(var) => unsafe { &(&*var).name },
            Terminal::GroundedTerm(ref name) => name,
            Terminal::Keyword(name) => name,
        }
    }

    pub fn get_name_as_string_ref(&self) -> &String {
        match self.name {
            Terminal::FreeTerm(var) => unsafe { &(&*var).name },
            Terminal::GroundedTerm(ref name) => &name,
            Terminal::Keyword(_) => panic!(),
        }
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

    fn get_id(&self) -> Vec<u8> {
        let mut id = vec![];
        let mut id_1 = Vec::from(self.name.get_name().as_bytes());
        id.append(&mut id_1);
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
        if op_args.as_ref().unwrap().len() != 2 {
            return Err(ParseErrF::WrongDef);
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
        let mut args = Vec::with_capacity(3);
        if let Some(oargs) = other.args.as_ref() {
            if oargs.len() > 3 || oargs.len() < 2 {
                return Err(ParseErrF::WrongArgNumb);
            }
            for (i, a) in oargs.iter().enumerate() {
                let pred = Predicate::from(a, context, &name);
                if pred.is_err() {
                    return Err(pred.unwrap_err());
                }
                let pred = pred.unwrap();
                if pred.has_uval() && (i == 1 || i == 2) {
                    return Err(ParseErrF::RFuncWrongArgs);
                }
                args.push(pred);
            }
        } else {
            return Err(ParseErrF::WrongArgNumb);
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
            None => None,
        };
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

    fn contains(&self, var: &Var) -> bool {
        if self.args.is_some() {
            for a in self.args.as_ref().unwrap() {
                match a {
                    &Predicate::FreeClsMemb(ref term) => {
                        if term.term == &*var as *const Var {
                            return true;
                        }
                    }
                    _ => continue,
                }
            }
        }
        if self.op_args.is_some() {
            for a in self.op_args.as_ref().unwrap() {
                if a.contains(var) {
                    return true;
                }
            }
        }
        false
    }

    pub fn var_in_pos(&self, var: *const Var) -> Option<usize> {
        if self.args.is_some() {
            for (i, a) in self.args.as_ref().unwrap().iter().enumerate() {
                match a {
                    &Predicate::FreeClsMemb(ref term) => {
                        if term.term == var {
                            return Some(i);
                        }
                    }
                    _ => continue,
                }
            }
        }
        None
    }

    pub fn term_in_pos(&self, var: &str) -> Option<usize> {
        if self.args.is_some() {
            for (i, a) in self.args.as_ref().unwrap().iter().enumerate() {
                match a {
                    &Predicate::GroundedClsMemb(ref term) => {
                        if term.term == var {
                            return Some(i);
                        }
                    }
                    _ => continue,
                }
            }
        }
        None
    }

    fn parent_is_grounded(&self) -> bool {
        match self.name {
            Terminal::GroundedTerm(_) => true,
            _ => false,
        }
    }

    /// Compares two relational functions, if they include free terms variable values
    /// assignments must be provided or will return None or panic in worst case.
    fn equal_to_grounded(&self,
                         agent: &agent::Representation,
                         assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>)
                         -> Option<bool> {
        match self.variant {
            FuncVariants::Relational => {}
            _ => panic!("simag: cannot compare non-relational functions"),
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
            if let Ok(grfunc) = GroundedFunc::from_free(&self, assignments) {
                for arg in self.get_args() {
                    if let &Predicate::FreeClsMemb(ref arg) = arg {
                        if let Some(entity) = assignments.get(&arg.term) {
                            if let Some(current) = entity.get_relationship(&grfunc) {
                                if current != &grfunc {
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

    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>,
                  context: &mut agent::ProofResult) {
        if let Ok(grfunc) = GroundedFunc::from_free(&self, assignments.as_ref().unwrap()) {
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
    op_args: Option<Vec<OpArg>>,
}

impl<'a> ClassDecl {
    pub fn get_name_as_str(&self) -> &str {
        match self.name {
            Terminal::FreeTerm(var) => unsafe { &(&*var).name },
            Terminal::GroundedTerm(ref name) => name,
            Terminal::Keyword(name) => name,
        }
    }

    pub fn get_name_as_string_ref(&self) -> &String {
        match self.name {
            Terminal::FreeTerm(var) => unsafe { &(&*var).name },
            Terminal::GroundedTerm(ref name) => &name,
            Terminal::Keyword(_) => panic!(),
        }
    }

    pub fn get_parent(&self) -> &Terminal {
        &self.name
    }

    pub fn from(other: &ClassDeclBorrowed<'a>,
                context: &mut Context)
                -> Result<ClassDecl, ParseErrF> {
        let class_name = Terminal::from(&other.name, context)?;
        let args = {
            let mut v0 = Vec::with_capacity(other.args.len());
            for a in &other.args {
                let pred = Predicate::from(a, context, &class_name);
                if pred.is_err() {
                    return Err(pred.unwrap_err());
                }
                v0.push(pred.unwrap());
            }
            v0
        };
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

    fn get_id(&self) -> Vec<u8> {
        let mut id = vec![];
        let mut id_1 = Vec::from(self.name.get_name().as_bytes());
        id.append(&mut id_1);
        for a in self.args.iter() {
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

    fn contains(&self, var: &Var) -> bool {
        for a in &self.args {
            match a {
                &Predicate::FreeClsMemb(ref term) => {
                    if term.term == &*var as *const Var {
                        return true;
                    }
                }
                _ => continue,
            }
        }
        if self.op_args.is_some() {
            for a in self.op_args.as_ref().unwrap() {
                if a.contains(var) {
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
                         assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>)
                         -> Option<bool> {
        for a in &self.args {
            match a {
                &Predicate::FreeClsMemb(ref free) => {
                    if assignments.is_none() {
                        return None;
                    }
                    if let Some(entity) = assignments.as_ref().unwrap().get(&free.term) {
                        let grounded = entity.get_class(&free.parent.get_name());
                        if !free.equal_to_grounded(grounded) {
                            return Some(false);
                        }
                    } else {
                        return None;
                    }
                }
                &Predicate::GroundedClsMemb(ref compare) => {
                    let entity =
                        agent.get_entity_from_class(self.get_name_as_str(), compare.term.as_str());
                    if let Some(current) = entity {
                        if current != compare {
                            return Some(false);
                        }
                    } else {
                        return None;
                    }
                }
            }
        }
        Some(true)
    }

    fn substitute(&self,
                  agent: &agent::Representation,
                  assignments: &Option<HashMap<*const Var, &agent::VarAssignment>>,
                  context: &mut agent::ProofResult) {
        for a in &self.args {
            let grfact = match a {
                &Predicate::FreeClsMemb(ref free) => {
                    if let Some(ref entity) = assignments.as_ref().unwrap().get(&free.term) {
                        GroundedClsMemb::from_free(free, entity.name)
                    } else {
                        break;
                    }
                }
                &Predicate::GroundedClsMemb(ref grounded) => grounded.clone(),
            };
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
                Some(_) => {
                    panic!("simag: expected a grounded predicate, found a free term instead")
                }
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
struct OpArg {
    term: OpArgTerm,
    comp: Option<(CompOperator, OpArgTerm)>,
}

impl<'a> OpArg {
    pub fn from(other: &OpArgBorrowed<'a>, context: &mut Context) -> Result<OpArg, ParseErrF> {
        let comp = match other.comp {
            Some((op, ref tors)) => {
                let t = OpArgTerm::from(&tors, context);
                if t.is_err() {
                    return Err(t.unwrap_err());
                }
                Some((op, t.unwrap()))
            }
            None => None,
        };
        let t = OpArgTerm::from(&other.term, context);
        if t.is_err() {
            return Err(t.unwrap_err());
        }
        Ok(OpArg {
            term: t.unwrap(),
            comp: comp,
        })
    }

    #[inline]
    fn contains(&self, var: &Var) -> bool {
        if self.term.is_var(var) {
            return true;
        }
        if let Some((_, ref term)) = self.comp {
            if term.is_var(var) {
                return true;
            }
        }
        false
    }

    fn get_id(&self) -> Vec<u8> {
        let mut id = vec![];
        let mut id_1 = Vec::from(self.term.get_name().as_bytes());
        id.append(&mut id_1);
        if let Some((ref op, ref term)) = self.comp {
            match op {
                &CompOperator::Equal => id.push(0),
                &CompOperator::Less => id.push(1),
                &CompOperator::More => id.push(2),
            }
            let mut id_2 = Vec::from(term.get_name().as_bytes());
            id.append(&mut id_2);
        }
        id
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum OpArgTerm {
    Terminal(Terminal),
    String(String),
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

    #[inline]
    fn is_var(&self, var: &Var) -> bool {
        match *self {
            OpArgTerm::Terminal(ref term) => term.is_var(var),
            OpArgTerm::String(_) => false,
        }
    }

    fn get_name(&self) -> &str {
        match *self {
            OpArgTerm::Terminal(ref term) => term.get_name(),
            OpArgTerm::String(ref s) => s.as_str(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Var {
    pub name: String,
    op_arg: Option<OpArg>,
}

impl Var {
    pub fn from<'a>(input: &VarBorrowed<'a>, context: &mut Context) -> Result<Var, ParseErrF> {
        let &VarBorrowed { name: TerminalBorrowed(name), ref op_arg } = input;
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
        Ok(Var {
            name: name,
            op_arg: op_arg,
        })
    }
}

#[derive(Debug)]
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
    FreeTerm(*const Var),
    GroundedTerm(String),
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
            let v_r: &Var = unsafe { &**v };
            if v_r.name == name {
                return Ok(Terminal::FreeTerm(*v));
            }
        }
        Ok(Terminal::GroundedTerm(name))
    }

    fn from_slice(slice: &[u8], context: &mut Context) -> Result<Terminal, ParseErrF> {
        let name = unsafe { String::from(str::from_utf8_unchecked(slice)) };
        if reserved(&name) {
            return Err(ParseErrF::ReservedKW(name));
        }
        for v in &context.vars {
            let v: &Var = unsafe { &**v };
            if v.name == name {
                return Ok(Terminal::FreeTerm(v));
            }
        }
        Ok(Terminal::GroundedTerm(name))
    }

    fn is_var(&self, v1: &Var) -> bool {
        match *self {
            Terminal::FreeTerm(v0) => {
                if (&*v1 as *const Var) == v0 {
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn get_name(&self) -> &str {
        match self {
            &Terminal::GroundedTerm(ref name) => name,
            _ => panic!("simag: attempted to get a name from a non-grounded terminal"),
        }
    }

    fn to_string(&self) -> String {
        match self {
            &Terminal::GroundedTerm(ref name) => name.clone(),
            _ => panic!("simag: attempted to get a name from a non-grounded terminal"),
        }
    }
}

fn reserved(s: &str) -> bool {
    match s {
        "let" => true,
        "time_calc" => true,
        "exists" => true,
        "fn" => true,
        "time" => true,
        _ => false,
    }
}
