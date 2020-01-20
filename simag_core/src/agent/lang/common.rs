use super::GroundedFunc;
use super::GroundedMemb;

use chrono::{DateTime, Duration, Utc};
use float_cmp::ApproxEqUlps;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::str;
use std::sync::{Arc, Weak};

use super::{
    cls_decl::ClassDecl,
    errors::ParseErrF,
    fn_decl::FuncDecl,
    logsent::{LogSentResolution, ParseContext, ProofResContext},
    parser::{
        ArgBorrowed, CompOperator, Number, OpArgBorrowed, OpArgTermBorrowed, SkolemBorrowed,
        TerminalBorrowed, UVal, VarBorrowed,
    },
    Time,
};
use crate::agent::{
    kb::bms::BmsWrapper,
    kb::{repr::Representation, VarAssignment},
};
use crate::FLOAT_EQ_ULPS;
use crate::TIME_EQ_DIFF;

pub use self::errors::TimeFnErr;

// Predicate types:

#[derive(Debug, Clone)]
pub(in crate::agent) enum Predicate {
    FreeClsMemb(FreeClsMemb),
    GroundedMemb(GroundedMemb),
    FreeClassMembership(FreeClassMembership),
}

impl<'a> Predicate {
    pub(in crate::agent::lang) fn from(
        arg: &'a ArgBorrowed<'a>,
        context: &'a mut ParseContext,
        name: &'a Terminal,
        is_func: bool,
    ) -> Result<Predicate, ParseErrF> {
        if name.is_grounded() {
            match Terminal::from(&arg.term, context) {
                Ok(Terminal::FreeTerm(ft)) => {
                    let t = FreeClsMemb::try_new(ft, arg.uval, name)?;
                    Ok(Predicate::FreeClsMemb(t))
                }
                Ok(Terminal::GroundedTerm(gt)) => {
                    let t = GroundedMemb::try_new(
                        gt,
                        arg.uval,
                        name.get_name().to_string(),
                        None,
                        context,
                    )?;
                    Ok(Predicate::GroundedMemb(t))
                }
                Ok(Terminal::Keyword(kw)) => Err(ParseErrF::ReservedKW(String::from(kw))),
                Err(err) => Err(err),
            }
        } else {
            if context.is_tell {
                return Err(ParseErrF::ClassIsVar);
            }
            match Terminal::from(&arg.term, context) {
                Ok(Terminal::FreeTerm(_)) if !is_func => Err(ParseErrF::BothAreVars),
                Ok(Terminal::FreeTerm(ft)) => {
                    let t = FreeClsMemb::try_new(ft, arg.uval, name)?;
                    Ok(Predicate::FreeClsMemb(t))
                }
                Ok(Terminal::GroundedTerm(gt)) => {
                    let t = FreeClassMembership::try_new(gt, arg.uval, name)?;
                    Ok(Predicate::FreeClassMembership(t))
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
            Predicate::GroundedMemb(ref t) => {
                let o_val = t.value.read();
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
            Predicate::FreeClassMembership(ref t) => {
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
    pub fn get_name(&self) -> &str {
        match *self {
            Predicate::GroundedMemb(ref t) => t.get_name(),
            Predicate::FreeClassMembership(ref t) => &t.term,
            Predicate::FreeClsMemb(_) => panic!(),
        }
    }

    #[inline]
    pub(in crate::agent::lang) fn generate_uid(&self) -> Vec<u8> {
        match *self {
            Predicate::FreeClsMemb(ref t) => t.generate_uid(),
            Predicate::GroundedMemb(ref t) => t.generate_uid(),
            Predicate::FreeClassMembership(ref t) => t.generate_uid(),
        }
    }

    pub fn has_uval(&self) -> bool {
        match *self {
            Predicate::GroundedMemb(ref t) => t.value.read().is_some(),
            Predicate::FreeClsMemb(ref t) => t.value.is_some(),
            Predicate::FreeClassMembership(ref t) => t.value.is_some(),
        }
    }
}

// Grounded types:

#[derive(Debug, Clone)]
pub(in crate::agent) enum Grounded {
    Function(Weak<GroundedFunc>),
    Class(Weak<GroundedMemb>),
}

#[derive(Debug, Clone)]
pub(in crate::agent) enum GroundedRef<'a> {
    Function(&'a GroundedFunc),
    Class(&'a GroundedMemb),
}

impl<'a> GroundedRef<'a> {
    pub fn update_value(&self, val: Option<f32>) {
        match *self {
            GroundedRef::Function(func) => {
                let mut original = func.args[0].value.write();
                *original = val;
            }
            GroundedRef::Class(cls) => {
                let mut original = cls.value.write();
                *original = val;
            }
        }
    }
}

// Free types:

#[derive(Debug, Clone)]
pub(in crate::agent) struct FreeClsMemb {
    pub(in crate::agent::lang) term: Arc<Var>,
    pub(in crate::agent::lang) value: Option<f32>,
    pub(in crate::agent::lang) operator: Option<CompOperator>,
    pub(in crate::agent::lang) parent: Terminal,
}

impl FreeClsMemb {
    fn try_new(
        term: Arc<Var>,
        uval: Option<UVal>,
        parent: &Terminal,
    ) -> Result<FreeClsMemb, ParseErrF> {
        let (val, op) = match_uval(uval)?;
        Ok(FreeClsMemb {
            term,
            value: val,
            operator: op,
            parent: parent.clone(),
        })
    }

    fn generate_uid(&self) -> Vec<u8> {
        let mut id: Vec<u8> = vec![];
        let mut var = format!("{:?}", &*self.term as *const Var).into_bytes();
        id.append(&mut var);
        if let Some(value) = self.value {
            let mut id_2 = format!("{}", value).into_bytes();
            id.append(&mut id_2);
        }
        if let Some(ref cmp) = self.operator {
            cmp.generate_uid(&mut id);
        }
        id.append(&mut self.parent.generate_uid());
        id
    }

    #[inline]
    pub fn get_parent(&self) -> &str {
        self.parent.get_name()
    }

    #[inline]
    pub fn get_var_ref(&self) -> &Var {
        &*self.term
    }

    #[inline]
    pub fn get_var(&self) -> Arc<Var> {
        self.term.clone()
    }

    /// Compares a free term with a grounded term, assumes they are comparable
    /// (panics otherwise).
    pub fn grounded_eq(&self, other: &GroundedMemb) -> bool {
        if self.parent.get_name() != other.parent {
            return false;
        }
        if self.value.is_some() {
            let val_free = self.value.unwrap();
            let val_grounded = {
                if let Some(val) = *other.value.read() {
                    val
                } else {
                    return false;
                }
            };
            match other.operator.unwrap() {
                CompOperator::Equal => {
                    let self_op = self.operator.as_ref().unwrap();
                    if self_op.is_equal() {
                        val_free.approx_eq_ulps(&val_grounded, FLOAT_EQ_ULPS)
                    } else if self_op.is_more() {
                        val_grounded > val_free
                    } else if self_op.is_less() {
                        val_grounded < val_free
                    } else if self_op.is_less_eq() {
                        (val_grounded < val_free)
                            | val_free.approx_eq_ulps(&val_grounded, FLOAT_EQ_ULPS)
                    } else {
                        (val_grounded > val_free)
                            | val_free.approx_eq_ulps(&val_grounded, FLOAT_EQ_ULPS)
                    }
                }
                CompOperator::Less
                | CompOperator::More
                | CompOperator::MoreEqual
                | CompOperator::LessEqual => panic!(),
                _ => unreachable!(),
            }
        } else {
            true
        }
    }
}

/// Reified object, free class belongship. Ie: x[$Lucy,u>0.5]
#[derive(Debug, Clone)]
pub(in crate::agent) struct FreeClassMembership {
    term: String,
    value: Option<f32>,
    operator: Option<CompOperator>,
    parent: Arc<Var>,
    pub times: BmsWrapper,
}

impl FreeClassMembership {
    fn try_new(
        term: String,
        uval: Option<UVal>,
        parent: &Terminal,
    ) -> Result<FreeClassMembership, ParseErrF> {
        let (val, op) = match_uval(uval)?;
        let t_bms = BmsWrapper::new(false);
        t_bms.new_record(None, val, None);
        Ok(FreeClassMembership {
            term,
            value: val,
            operator: op,
            parent: parent.get_var(),
            times: t_bms,
        })
    }

    fn generate_uid(&self) -> Vec<u8> {
        let mut id: Vec<u8> = vec![];
        id.append(&mut Vec::from(self.term.as_bytes()));
        if let Some(ref val) = self.value {
            let mut id_2 = format!("{}", *val).into_bytes();
            id.append(&mut id_2);
        }
        if let Some(ref cmp) = self.operator {
            cmp.generate_uid(&mut id);
        }
        let mut var = format!("{:?}", &*self.parent as *const Var).into_bytes();
        id.append(&mut var);
        id
    }

    pub fn filter_grounded(&self, other: &GroundedMemb) -> bool {
        if self.operator.is_some() {
            let val = self.value.as_ref().unwrap();
            let o_val = if let Some(o_val) = *other.value.read() {
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
                CompOperator::Until | CompOperator::At | CompOperator::FromUntil => unreachable!(),
            }
        } else {
            true
        }
    }

    pub fn overwrite_time_data(&self, data: &BmsWrapper) {
        self.times.overwrite_data(data);
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        &self.term
    }

    #[inline]
    pub fn get_var_ref(&self) -> &Var {
        &*self.parent
    }

    #[inline]
    pub fn get_var(&self) -> Arc<Var> {
        self.parent.clone()
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
pub(in crate::agent) enum Assert {
    FuncDecl(FuncDecl),
    ClassDecl(ClassDecl),
}

impl Assert {
    #[inline]
    pub fn get_name(&self) -> &str {
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
    pub fn get_times(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> Option<Arc<BmsWrapper>> {
        match *self {
            Assert::FuncDecl(ref f) => f.get_times(agent, var_assign),
            Assert::ClassDecl(ref c) => c.get_times(agent, var_assign),
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
    pub fn unwrap_fn_as_ref(&self) -> &FuncDecl {
        match *self {
            Assert::FuncDecl(ref f) => f,
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
    pub fn unwrap_cls_as_ref(&self) -> &ClassDecl {
        match *self {
            Assert::FuncDecl(_) => panic!(),
            Assert::ClassDecl(ref c) => c,
        }
    }

    #[inline]
    pub fn grounded_eq<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) -> Option<bool> {
        match *self {
            Assert::FuncDecl(ref f) => f.grounded_eq(agent, assignments, time_assign, context),
            Assert::ClassDecl(ref c) => c.grounded_eq(agent, assignments, time_assign, context),
        }
    }

    #[inline]
    pub fn substitute<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper>>,
        context: &mut T,
    ) {
        match *self {
            Assert::FuncDecl(ref f) => f.substitute(agent, assignments, time_assign, context),
            Assert::ClassDecl(ref c) => c.substitute(agent, assignments, time_assign, context),
        }
    }

    #[inline]
    pub fn generate_uid(&self) -> Vec<u8> {
        match *self {
            Assert::FuncDecl(ref f) => f.generate_uid(),
            Assert::ClassDecl(ref c) => c.generate_uid(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum OpArg {
    Generic(OpArgTerm, Option<(CompOperator, OpArgTerm)>),
    TimeVar,
    TimeDecl(TimeFn),
    TimeVarAssign(Arc<Var>),
    TimeVarFrom(Arc<Var>),
    // TimeVarUntil(Arc<Var>),
    TimeVarFromUntil(Arc<Var>, Arc<Var>),
    OverWrite,
}

impl<'a> OpArg {
    pub fn from(other: &OpArgBorrowed<'a>, context: &ParseContext) -> Result<OpArg, ParseErrF> {
        let t0 = match OpArgTerm::from(&other.term, context) {
            Err(ParseErrF::ReservedKW(kw)) => match &*kw {
                "time" => return OpArg::ignore_kw(other, "time", context),
                "overwrite" | "ow" => return Ok(OpArg::OverWrite),
                _ => return Err(ParseErrF::ReservedKW(kw)),
            },
            Err(err) => return Err(err),
            Ok(arg) => arg,
        };
        let comp = match other.comp {
            Some((op, ref tors)) => match OpArgTerm::from(tors, context) {
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
            },
            None => None,
        };
        match comp {
            Some((CompOperator::At, _)) | Some((CompOperator::Until, _)) => {
                OpArg::ignore_kw(other, "time", context)
            }
            Some((CompOperator::FromUntil, _)) => {
                let load0 = if t0.is_var() {
                    t0.get_var()
                } else {
                    return Err(ParseErrF::TimeFnErr(TimeFnErr::IsNotVar));
                };
                let load1 = OpArgTerm::time_payload(other.comp.as_ref(), context)?;
                let load1 = if load1.1.is_var() {
                    load1.1.get_var()
                } else {
                    return Err(ParseErrF::TimeFnErr(TimeFnErr::IsNotVar));
                };
                Ok(OpArg::TimeVarFromUntil(load0, load1))
            }
            _ => Ok(OpArg::Generic(t0, comp)),
        }
    }

    fn ignore_kw(
        other: &OpArgBorrowed<'a>,
        kw: &str,
        context: &ParseContext,
    ) -> Result<OpArg, ParseErrF> {
        match kw {
            "time" => {
                let load = OpArgTerm::time_payload(other.comp.as_ref(), context)?;
                if load.1.is_var() {
                    Ok(OpArg::TimeVarAssign(load.1.get_var()))
                } else {
                    match load.1 {
                        OpArgTerm::TimePayload(TimeFn::IsVar) => Ok(OpArg::TimeVar),
                        OpArgTerm::TimePayload(load) => Ok(OpArg::TimeDecl(load)),
                        _ => Err(ParseErrF::WrongDef),
                    }
                }
            }
            val => Err(ParseErrF::ReservedKW(val.to_string())),
        }
    }

    #[inline]
    pub(in crate::agent::lang) fn contains_var(&self, var1: &Var) -> bool {
        match self {
            OpArg::TimeVarAssign(var0) | OpArg::TimeVarFrom(var0) => var0.as_ref() == var1,
            OpArg::TimeVarFromUntil(v0_0, v0_1) => v0_0.as_ref() == var1 || v0_1.as_ref() == var1,
            _ => false,
        }
    }

    pub(in crate::agent::lang) fn generate_uid(&self) -> Vec<u8> {
        match self {
            OpArg::Generic(a0, a1) => {
                let mut id = vec![];
                id.append(&mut a0.generate_uid());
                if let Some((ref cmp, ref a1)) = *a1 {
                    cmp.generate_uid(&mut id);
                    id.append(&mut a1.generate_uid());
                }
                id
            }
            OpArg::TimeDecl(decl) => decl.generate_uid(),
            OpArg::TimeVarAssign(var) | OpArg::TimeVarFrom(var) => {
                format!("{:?}", var.as_ref() as *const Var).into_bytes()
            }
            OpArg::TimeVarFromUntil(v0, v1) => {
                let mut id = format!("{:?}", v0.as_ref() as *const Var).into_bytes();
                id.append(&mut format!("{:?}", v1.as_ref() as *const Var).into_bytes());
                id
            }
            OpArg::TimeVar => vec![2],
            OpArg::OverWrite => vec![5],
        }
    }

    pub(in crate::agent::lang) fn get_time_payload(
        &self,
        assignments: &HashMap<&Var, Arc<BmsWrapper>>,
        value: Option<f32>,
    ) -> BmsWrapper {
        let bms = BmsWrapper::new(false);
        match self {
            OpArg::TimeDecl(TimeFn::Time(payload)) => {
                bms.new_record(Some(*payload), value, None);
            }
            OpArg::TimeDecl(TimeFn::Now) => {
                bms.new_record(None, value, None);
            }
            OpArg::TimeDecl(TimeFn::Interval(time0, time1)) => {
                bms.new_record(Some(*time0), value, None);
                bms.new_record(Some(*time1), None, None);
            }
            OpArg::TimeVarAssign(var) => {
                let assignment = &**(assignments.get(&**var).unwrap());
                return assignment.clone();
            }
            _ => panic!(),
        }
        bms
    }

    pub(in crate::agent::lang) fn compare_time_args(
        &self,
        assignments: &HashMap<&Var, Arc<BmsWrapper>>,
    ) -> bool {
        let (term, op, comp) = match *self {
            OpArg::Generic(ref term, Some((ref op, ref comp))) => (term, op, comp),
            _ => return false,
        };

        let var0 = term.get_var_ref();
        let var1 = comp.get_var_ref();
        let arg0 = assignments.get(&*var0).unwrap().get_last_date();
        let arg1 = assignments.get(&*var1).unwrap().get_last_date();

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
            CompOperator::Until | CompOperator::At | CompOperator::FromUntil => unreachable!(),
        }
    }

    /// While constructing an assertion in a tell context performs variable
    /// substitution whenever is possible, variables must be declared.
    pub(in crate::agent::lang) fn var_substitution(&mut self) -> Result<(), ParseErrF> {
        match self {
            OpArg::TimeVarFromUntil(var0, var1) => {
                let mut var0_time = var0.get_times();
                let var1_time = var1.get_times();
                var0_time.merge_from_until(&var1_time)?;
                let mut assignment = OpArg::TimeDecl(TimeFn::from_bms(&var0_time)?);
                std::mem::swap(&mut assignment, self);
            }
            OpArg::TimeVarFrom(var0) => {
                let var0_time = var0.get_times();
                let mut assignment = OpArg::TimeDecl(TimeFn::from_bms(&var0_time)?);
                std::mem::swap(&mut assignment, self);
            }
            _ => {}
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum TimeFn {
    Now,
    Time(Time),
    /// Time interval for value decl, in the form of [t0,t1)
    Interval(Time, Time),
    IsVar,
}

impl TimeFn {
    fn from_str(slice: &[u8]) -> Result<TimeFn, ParseErrF> {
        if slice == b"now" {
            Ok(TimeFn::Now)
        } else {
            let s = str::from_utf8(slice).unwrap();
            match DateTime::parse_from_rfc3339(s) {
                Err(_e) => Err(TimeFnErr::WrongFormat(s.to_owned()).into()),
                Ok(time) => Ok(TimeFn::Time(time.with_timezone(&Utc))),
            }
        }
    }

    /// Get a time interval from a bmswrapper, ie. created with the
    /// merge_from_until method.
    fn from_bms(rec: &BmsWrapper) -> Result<TimeFn, ParseErrF> {
        let values: Vec<_> = rec.iter_values().map(|(t, _)| t).collect();
        if values.len() != 2 {
            return Err(ParseErrF::TimeFnErr(TimeFnErr::IllegalSubstitution));
        }
        Ok(TimeFn::Interval(values[0], values[1]))
    }

    pub(in crate::agent::lang) fn get_time_payload(&self, value: Option<f32>) -> BmsWrapper {
        let bms = BmsWrapper::new(false);
        match *self {
            TimeFn::Time(ref payload) => {
                bms.new_record(Some(*payload), value, None);
            }
            TimeFn::Now => {
                bms.new_record(None, value, None);
            }
            _ => panic!(),
        }
        bms
    }

    fn generate_uid(&self) -> Vec<u8> {
        let mut id = vec![];
        match self {
            TimeFn::Time(time) => {
                id.push(0);
                id.append(&mut format!("{}", time).into_bytes());
            }
            TimeFn::Interval(time0, time1) => {
                id.push(1);
                id.append(&mut format!("{}", time0).into_bytes());
                id.append(&mut format!("{}", time1).into_bytes());
            }
            TimeFn::Now => id.push(2),
            TimeFn::IsVar => id.push(3),
        }
        id
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum OpArgTerm {
    Terminal(Terminal),
    String(String),
    TimePayload(TimeFn),
}

impl<'a> OpArgTerm {
    fn from(other: &OpArgTermBorrowed<'a>, context: &ParseContext) -> Result<OpArgTerm, ParseErrF> {
        match *other {
            OpArgTermBorrowed::Terminal(slice) => {
                let t = Terminal::from_slice(slice, context)?;
                Ok(OpArgTerm::Terminal(t))
            }
            OpArgTermBorrowed::String(slice) => Ok(OpArgTerm::String(
                String::from_utf8_lossy(slice).into_owned(),
            )),
        }
    }

    fn time_payload(
        other: Option<&(CompOperator, OpArgTermBorrowed<'a>)>,
        context: &ParseContext,
    ) -> Result<(CompOperator, OpArgTerm), ParseErrF> {
        match other {
            None => Ok((CompOperator::Equal, OpArgTerm::TimePayload(TimeFn::IsVar))),
            Some(&(ref op, ref term)) => {
                if !op.is_time_assignment() {
                    return Err(TimeFnErr::NotAssignment.into());
                }
                match *term {
                    OpArgTermBorrowed::String(slice) => {
                        let time = TimeFn::from_str(slice)?;
                        Ok((CompOperator::Equal, OpArgTerm::TimePayload(time)))
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

    fn generate_uid(&self) -> Vec<u8> {
        match *self {
            OpArgTerm::Terminal(ref t) => t.generate_uid(),
            OpArgTerm::String(ref s) => Vec::from_iter(s.as_bytes().iter().cloned()),
            OpArgTerm::TimePayload(ref t) => t.generate_uid(),
        }
    }

    #[inline]
    pub(in crate::agent::lang) fn is_var(&self) -> bool {
        match *self {
            OpArgTerm::Terminal(ref t) => t.is_var(),
            _ => false,
        }
    }

    #[inline]
    fn get_var(&self) -> Arc<Var> {
        match *self {
            OpArgTerm::Terminal(ref term) => term.get_var(),
            _ => panic!(),
        }
    }

    #[inline]
    pub(in crate::agent::lang) fn get_var_ref(&self) -> &Var {
        match *self {
            OpArgTerm::Terminal(ref term) => term.get_var_ref(),
            _ => panic!(),
        }
    }
}

/// Variable equality is bassed on physical address, to compare term equality use the
/// `name_eq` method.
#[derive(Clone)]
pub(in crate::agent) struct Var {
    pub name: String,
    pub op_arg: Option<OpArg>,
    pub kind: VarKind,
}

#[derive(Clone, PartialEq)]
pub(in crate::agent) enum VarKind {
    Normal,
    Time,
    TimeDecl,
}

impl std::fmt::Debug for VarKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for VarKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VarKind::Normal => write!(f, "Normal"),
            VarKind::Time => write!(f, "Time"),
            VarKind::TimeDecl => write!(f, "TimeDecl"),
        }
    }
}

impl Var {
    pub fn from<'a>(input: &VarBorrowed<'a>, context: &ParseContext) -> Result<Var, ParseErrF> {
        let &VarBorrowed {
            name: TerminalBorrowed(name),
            ref op_arg,
        } = input;
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
        let name = str::from_utf8(name).unwrap().to_owned();
        if reserved(&name) {
            return Err(ParseErrF::ReservedKW(name));
        }
        Ok(Var { name, op_arg, kind })
    }

    pub fn get_times(&self) -> BmsWrapper {
        let h = HashMap::new();
        self.op_arg.as_ref().unwrap().get_time_payload(&h, None)
    }

    pub fn is_time_var(&self) -> bool {
        match self.kind {
            VarKind::Time | VarKind::TimeDecl => true,
            _ => false,
        }
    }

    pub fn name_eq(&self, other: &Var) -> bool {
        self.name == other.name
    }
}

impl std::cmp::PartialEq for Var {
    fn eq(&self, other: &Var) -> bool {
        (self as *const Var) == (other as *const Var)
    }
}

impl std::cmp::Eq for Var {}

impl std::hash::Hash for Var {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let address = &*self as *const Var as usize;
        address.hash(state);
    }
}

impl std::fmt::Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(a) = &self.op_arg {
            let a = format!("{:?}", a);
            write!(f, "{}: {} = {}", self.name, self.kind, a)
        } else {
            write!(f, "{}: {}", self.name, self.kind)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(in crate::agent) struct Skolem {
    pub name: String,
    op_arg: Option<OpArg>,
}

impl Skolem {
    pub fn from<'a>(
        input: &SkolemBorrowed<'a>,
        context: &ParseContext,
    ) -> Result<Skolem, ParseErrF> {
        let &SkolemBorrowed {
            name: TerminalBorrowed(name),
            ref op_arg,
        } = input;
        let op_arg = match *op_arg {
            Some(ref op_arg) => {
                let t = OpArg::from(op_arg, context)?;
                Some(t)
            }
            None => None,
        };
        let name = str::from_utf8(name).unwrap().to_owned();
        if reserved(&name) {
            return Err(ParseErrF::ReservedKW(name));
        }
        Ok(Skolem { name, op_arg })
    }

    pub fn name_eq(&self, other: &Skolem) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub(in crate::agent) enum Terminal {
    FreeTerm(Arc<Var>),
    GroundedTerm(String),
    Keyword(&'static str),
}

impl<'a> Terminal {
    pub(in crate::agent::lang) fn from(
        other: &TerminalBorrowed<'a>,
        context: &mut ParseContext,
    ) -> Result<Terminal, ParseErrF> {
        let &TerminalBorrowed(slice) = other;
        let name = str::from_utf8(slice).unwrap().to_owned();
        if reserved(&name) {
            return Err(ParseErrF::ReservedKW(name));
        }
        for v in &context.vars {
            if v.name == name {
                return Ok(Terminal::FreeTerm(v.clone()));
            }
        }
        Ok(Terminal::GroundedTerm(name))
    }

    fn from_slice(slice: &[u8], context: &ParseContext) -> Result<Terminal, ParseErrF> {
        let name = str::from_utf8(slice).unwrap().to_owned();
        if reserved(&name) {
            return Err(ParseErrF::ReservedKW(name));
        }
        for v in &context.vars {
            if v.name == name {
                return Ok(Terminal::FreeTerm(v.clone()));
            }
        }
        Ok(Terminal::GroundedTerm(name))
    }

    pub(in crate::agent::lang) fn generate_uid(&self) -> Vec<u8> {
        match *self {
            Terminal::FreeTerm(ref var) => format!("{:?}", &**var as *const Var).into_bytes(),
            Terminal::GroundedTerm(ref name) => Vec::from_iter(name.as_bytes().iter().cloned()),
            Terminal::Keyword(name) => Vec::from_iter(name.as_bytes().iter().cloned()),
        }
    }

    pub(in crate::agent::lang) fn is_var(&self) -> bool {
        if let Terminal::FreeTerm(_) = *self {
            true
        } else {
            false
        }
    }

    #[inline]
    pub(in crate::agent::lang) fn get_name(&self) -> &str {
        if let Terminal::GroundedTerm(ref name) = *self {
            name
        } else {
            unreachable!()
        }
    }

    fn is_grounded(&self) -> bool {
        if let Terminal::GroundedTerm(_) = *self {
            true
        } else {
            false
        }
    }

    pub fn get_var_ref(&self) -> &Var {
        if let Terminal::FreeTerm(ref var) = *self {
            &*var
        } else {
            unreachable!()
        }
    }

    pub fn get_var(&self) -> Arc<Var> {
        if let Terminal::FreeTerm(ref var) = *self {
            var.clone()
        } else {
            panic!()
        }
    }
}

fn reserved(s: &str) -> bool {
    match s {
        "let" | "time_calc" | "exists" | "fn" | "time" | "overwrite" | "ow" | "self" | "none" => {
            true
        }
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
        IllegalSubstitution,
    }

    impl Into<ParseErrF> for TimeFnErr {
        fn into(self) -> ParseErrF {
            ParseErrF::TimeFnErr(self)
        }
    }
}
