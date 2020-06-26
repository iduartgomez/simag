use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::FromIterator;
use std::str;
use std::sync::{Arc, Weak};

use super::{
    cls_decl::ClassDecl,
    errors::ParseErrF,
    fn_decl::FuncDecl,
    logsent::{LogSentResolution, ParseContext, ProofResContext},
    parser::{ArgBorrowed, Number, OpArgBorrowed, Operator, UVal, UnconstraintArg},
    space_semantics::{SpaceArg, SpaceFnErr},
    time_semantics::{TimeArg, TimeFn, TimeFnErr, TimeOps},
    var::Var,
    BuiltIns, GroundedFunc, GroundedMemb, Terminal,
};
use crate::agent::{
    kb::bms::BmsWrapper,
    kb::{repr::Representation, VarAssignment},
};
use crate::FLOAT_EQ_ULPS;
use float_cmp::ApproxEqUlps;
use parking_lot::RwLock;

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
    pub fn get_uval(&self) -> (Option<Operator>, Option<f32>) {
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

    pub(in crate::agent::lang) fn replace_uval(&mut self, val: f32) {
        match self {
            Predicate::FreeClsMemb(ref mut t) => t.value = Some(val),
            Predicate::GroundedMemb(ref mut t) => t.value = RwLock::new(Some(val)),
            Predicate::FreeClassMembership(ref mut t) => t.value = Some(val),
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
    pub(in crate::agent::lang) operator: Option<Operator>,
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
        // FIXME: check time equality!
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
                Operator::Equal => {
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
    pub(in crate::agent::lang) value: Option<f32>,
    operator: Option<Operator>,
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
        // FIXME: check time equality!
        if self.operator.is_some() {
            let val = self.value.as_ref().unwrap();
            let o_val = if let Some(o_val) = *other.value.read() {
                o_val
            } else {
                return false;
            };
            match *self.operator.as_ref().unwrap() {
                Operator::Equal => val.approx_eq_ulps(&o_val, FLOAT_EQ_ULPS),
                Operator::Less => o_val < *val,
                Operator::More => o_val > *val,
                Operator::MoreEqual => val.approx_eq_ulps(&o_val, FLOAT_EQ_ULPS) || o_val > *val,
                Operator::LessEqual => val.approx_eq_ulps(&o_val, FLOAT_EQ_ULPS) || o_val < *val,
                _ => unreachable!(),
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

type UValDestruct = (Option<f32>, Option<Operator>);

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
                BuiltIns::MoveFn(f) => f.contains_var(var),
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
    pub fn unwrap_fn_as_ref(&self) -> &FuncDecl {
        match *self {
            Assert::FuncDecl(ref f) => f,
            Assert::ClassDecl(_) => unreachable!(),
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
    // TODO: split `generic` in two variants
    /// Generic optional argument which includes one binding value and optionally a second operand to compare against
    Generic(ConstraintValue, Option<(Operator, ConstraintValue)>),
    Time(TimeArg),
    Space(SpaceArg),
    OverWrite,
}

impl<'a> TryFrom<(&'a OpArgBorrowed<'a>, &'a ParseContext)> for OpArg {
    type Error = ParseErrF;
    fn try_from(input: (&OpArgBorrowed<'a>, &ParseContext)) -> Result<OpArg, ParseErrF> {
        match TimeArg::try_from(input) {
            Ok(arg) => return Ok(OpArg::Time(arg)),
            Err(ParseErrF::TimeFnErr(TimeFnErr::WrongDef)) => {
                return Err(ParseErrF::TimeFnErr(TimeFnErr::WrongDef))
            }
            _ => {}
        }

        match SpaceArg::try_from(input) {
            Ok(arg) => return Ok(OpArg::Space(arg)),
            Err(ParseErrF::SpaceFnErr(SpaceFnErr::WrongDef)) => {
                return Err(ParseErrF::SpaceFnErr(SpaceFnErr::WrongDef))
            }
            _ => {}
        }

        let (other, context) = input;
        let t0 = match ConstraintValue::try_from((&other.term, context)) {
            Ok(arg) => arg,
            Err(ParseErrF::ReservedKW(kw)) => match &*kw {
                "overwrite" | "ow" => return Ok(OpArg::OverWrite),
                _ => return Err(ParseErrF::ReservedKW(kw)),
            },
            Err(err) => return Err(err),
        };
        let comp = match other.comp {
            Some((op, ref tors)) => match ConstraintValue::try_from((tors, context)) {
                Ok(t) => Some((op, t)),
                Err(ParseErrF::ReservedKW(kw)) => return Err(ParseErrF::ReservedKW(kw)),
                Err(err) => return Err(err),
            },
            None => None,
        };

        Ok(OpArg::Generic(t0, comp))
    }
}

impl<'a> OpArg {
    #[inline]
    pub(in crate::agent::lang) fn contains_var(&self, var: &Var) -> bool {
        match self {
            OpArg::Time(this_val) => this_val.contains_var(var),
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
            OpArg::Time(time_arg) => time_arg.generate_uid(),
            OpArg::Space(space_arg) => space_arg.generate_uid(),
            OpArg::OverWrite => vec![5],
        }
    }

    /// While constructing an assertion in a tell context performs variable
    /// substitution whenever is possible, variables must be declared.
    pub(in crate::agent::lang) fn var_substitution(&mut self) -> Result<(), ParseErrF> {
        if let OpArg::Time(time) = self {
            time.var_substitution()?
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::agent) enum ConstraintValue {
    Terminal(Terminal),
    String(String),
    TimePayload(TimeFn),
    SpacePayload,
}

impl<'a> TryFrom<(&'a UnconstraintArg<'a>, &'a ParseContext)> for ConstraintValue {
    type Error = ParseErrF;
    fn try_from(input: (&'a UnconstraintArg<'a>, &'a ParseContext)) -> Result<Self, Self::Error> {
        let (other, context) = input;
        match other {
            UnconstraintArg::Terminal(slice) => {
                let t = Terminal::from_slice(slice, context)?;
                Ok(ConstraintValue::Terminal(t))
            }
            UnconstraintArg::String(slice) => Ok(ConstraintValue::String(
                String::from_utf8_lossy(slice).into_owned(),
            )),
            UnconstraintArg::Keyword(b"time") => Ok(ConstraintValue::TimePayload(TimeFn::ThisTime)),
            UnconstraintArg::Keyword(b"space") => Ok(ConstraintValue::SpacePayload),
            UnconstraintArg::Keyword(kw) => Err(ParseErrF::ReservedKW(
                str::from_utf8(kw).unwrap().to_owned(),
            )),
        }
    }
}

impl<'a> ConstraintValue {
    pub fn generate_uid(&self) -> Vec<u8> {
        match self {
            ConstraintValue::Terminal(t) => t.generate_uid(),
            ConstraintValue::String(s) => Vec::from_iter(s.as_bytes().iter().cloned()),
            ConstraintValue::TimePayload(t) => t.generate_uid(),
            ConstraintValue::SpacePayload => vec![0], // FIXME
        }
    }

    #[inline]
    pub(in crate::agent::lang) fn is_var(&self) -> bool {
        match self {
            ConstraintValue::Terminal(t) => t.is_var(),
            _ => false,
        }
    }

    #[inline]
    pub fn get_var(&self) -> Arc<Var> {
        match self {
            ConstraintValue::Terminal(term) => term.get_var(),
            _ => unreachable!(),
        }
    }

    #[inline]
    pub(in crate::agent::lang) fn get_var_ref(&self) -> &Var {
        match self {
            ConstraintValue::Terminal(term) => term.get_var_ref(),
            _ => unreachable!(),
        }
    }

    pub fn contains_var(&self, var: &Var) -> bool {
        match self {
            ConstraintValue::Terminal(term) => {
                let this_var = term.get_var_ref();
                this_var == var
            }
            _ => unreachable!(),
        }
    }
}
