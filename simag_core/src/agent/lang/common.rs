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
    parser::{ArgBorrowed, CompOperator, Number, OpArgBorrowed, OpArgTermBorrowed, UVal},
    time_semantics::{TimeArg, TimeFn, TimeFnErr, TimeOps},
    var::Var,
    BuiltIns, GroundedFunc, GroundedMemb, Terminal,
};
use crate::agent::{
    kb::bms::BmsWrapper,
    kb::{repr::Representation, VarAssignment},
};
use crate::FLOAT_EQ_ULPS;

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
            Predicate::GroundedMemb(ref t) => t.get_name().into(),
            Predicate::FreeClassMembership(ref t) => &t.term,
            Predicate::FreeClsMemb(_) => unreachable!(),
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
                | CompOperator::LessEqual => unreachable!(),
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
    SpecialFunc(BuiltIns),
}

impl Assert {
    #[inline]
    pub fn get_name(&self) -> &str {
        match self {
            Assert::FuncDecl(f) => f.get_name(),
            Assert::ClassDecl(c) => c.get_name(),
            Assert::SpecialFunc(f) => f.get_name(),
        }
    }

    #[inline]
    pub fn get_time_decl(&self, var: &Var) -> bool {
        match *self {
            Assert::FuncDecl(ref f) => f.get_time_decl(var),
            Assert::ClassDecl(ref c) => c.get_time_decl(var),
            Assert::SpecialFunc(_) => false,
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
            Assert::SpecialFunc(_) => None,
        }
    }

    #[inline]
    pub fn is_class(&self) -> bool {
        match *self {
            Assert::FuncDecl(_) | Assert::SpecialFunc(_) => false,
            Assert::ClassDecl(_) => true,
        }
    }

    #[inline]
    pub fn contains(&self, var: &Var) -> bool {
        match self {
            Assert::FuncDecl(f) => f.contains_var(var),
            Assert::ClassDecl(c) => c.contains_var(var),
            Assert::SpecialFunc(builtins) => match builtins {
                BuiltIns::TimeCalculus(f) => f.contains_var(var),
            },
        }
    }

    #[inline]
    pub fn parent_is_grounded(&self) -> bool {
        match *self {
            Assert::FuncDecl(ref f) => f.parent_is_grounded(),
            Assert::ClassDecl(ref c) => c.parent_is_grounded(),
            Assert::SpecialFunc(_) => false,
        }
    }

    #[inline]
    pub fn parent_is_kw(&self) -> bool {
        match self {
            Assert::ClassDecl(_) | Assert::FuncDecl(_) => false,
            Assert::SpecialFunc(_) => true,
        }
    }

    #[inline]
    pub fn unwrap_fn(self) -> FuncDecl {
        match self {
            Assert::FuncDecl(f) => f,
            Assert::ClassDecl(_) => unreachable!(),
            Assert::SpecialFunc(_) => unreachable!(),
        }
    }

    #[inline]
    pub fn unwrap_fn_as_ref(&self) -> &FuncDecl {
        match *self {
            Assert::FuncDecl(ref f) => f,
            Assert::ClassDecl(_) => unreachable!(),
            Assert::SpecialFunc(_) => unreachable!(),
        }
    }

    #[inline]
    pub fn unwrap_cls(self) -> ClassDecl {
        match self {
            Assert::FuncDecl(_) => unreachable!(),
            Assert::ClassDecl(c) => c,
            Assert::SpecialFunc(_) => unreachable!(),
        }
    }

    #[inline]
    pub fn unwrap_cls_as_ref(&self) -> &ClassDecl {
        match *self {
            Assert::FuncDecl(_) => unreachable!(),
            Assert::ClassDecl(ref c) => c,
            Assert::SpecialFunc(_) => unreachable!(),
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
        match self {
            Assert::FuncDecl(f) => f.grounded_eq(agent, assignments, time_assign, context),
            Assert::ClassDecl(c) => c.grounded_eq(agent, assignments, time_assign, context),
            Assert::SpecialFunc(f) => f.grounded_eq(time_assign),
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
            Assert::SpecialFunc(_) => todo!(),
        }
    }

    #[inline]
    pub fn generate_uid(&self) -> Vec<u8> {
        match self {
            Assert::FuncDecl(f) => f.generate_uid(),
            Assert::ClassDecl(c) => c.generate_uid(),
            Assert::SpecialFunc(f) => f.generate_uid(),
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
                let load1 = TimeArg::time_payload(other.comp.as_ref(), context)?;
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
                let load = TimeArg::time_payload(other.comp.as_ref(), context)?;
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
pub(in crate::agent) enum OpArgTerm {
    Terminal(Terminal),
    String(String),
    TimePayload(TimeFn),
}

pub(in crate::agent) fn op_arg_term_from_borrowed<'a>(
    other: &OpArgTermBorrowed<'a>,
    context: &ParseContext,
) -> Result<OpArgTerm, ParseErrF> {
    OpArgTerm::from(other, context)
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
            _ => unreachable!(),
        }
    }

    #[inline]
    pub(in crate::agent::lang) fn get_var_ref(&self) -> &Var {
        match *self {
            OpArgTerm::Terminal(ref term) => term.get_var_ref(),
            _ => unreachable!(),
        }
    }
}
