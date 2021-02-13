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
    spatial_semantics::{SpatialArg, SpatialFnErr},
    time_semantics::{TimeArg, TimeFn, TimeFnErr, TimeOps},
    var::Var,
    BuiltIns, GroundedFunc, GroundedMemb, Point, SpatialOps, Terminal, Time,
};
use crate::agent::{
    kb::bms::{BmsWrapper, HasBms, IsSpatialData, IsTimeData},
    kb::{repr::Representation, VarAssignment},
};
use crate::FLOAT_EQ_ULPS;
use float_cmp::ApproxEqUlps;

// Predicate types:

#[derive(Debug, Clone)]
pub(in crate::agent) enum Predicate {
    /// (let x in some\[x\])
    FreeMembershipToClass(FreeMembershipToClass),
    /// abc\[$def=1\]
    GroundedMemb(GroundedMemb),
    /// (let x in x\[$Lucy>0.5\])
    FreeClassMembership(FreeClassMembership),
}

impl<'a> Predicate {
    /// Tries to create a predicate from parsed elements.
    ///
    /// ## Args
    /// - is_func: whether this predicate is a func declaration or not; and the argument number in case it is.
    pub(in crate::agent::lang) fn from(
        arg: &'a ArgBorrowed<'a>,
        context: &'a mut ParseContext,
        name: &'a Terminal,
        is_func: Option<usize>,
    ) -> Result<Predicate, ParseErrF> {
        if name.is_grounded() {
            match Terminal::from(&arg.term, context) {
                Ok(Terminal::FreeTerm(ft)) => {
                    let t = FreeMembershipToClass::try_new(&ft, arg.uval, name)?;
                    Ok(Predicate::FreeMembershipToClass(t))
                }
                Ok(Terminal::GroundedTerm(gt)) => {
                    let t = GroundedMemb::try_new(
                        gt,
                        arg.uval,
                        name.get_name().to_string(),
                        None,
                        context,
                        is_func,
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
                Ok(Terminal::FreeTerm(_)) if is_func.is_none() => Err(ParseErrF::BothAreVars),
                Ok(Terminal::FreeTerm(ft)) => {
                    let t = FreeMembershipToClass::try_new(&ft, arg.uval, name)?;
                    Ok(Predicate::FreeMembershipToClass(t))
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
            Predicate::FreeMembershipToClass(_) => true,
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
            Predicate::FreeMembershipToClass(ref t) => {
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
            Predicate::FreeMembershipToClass(_) => unreachable!(),
        }
    }

    #[inline]
    pub(in crate::agent::lang) fn generate_uid(&self) -> Vec<u8> {
        match *self {
            Predicate::FreeMembershipToClass(ref t) => t.generate_uid(),
            Predicate::GroundedMemb(ref t) => t.generate_uid(),
            Predicate::FreeClassMembership(ref t) => t.generate_uid(),
        }
    }

    pub fn has_uval(&self) -> bool {
        match *self {
            Predicate::GroundedMemb(ref t) => t.value.read().is_some(),
            Predicate::FreeMembershipToClass(ref t) => t.value.is_some(),
            Predicate::FreeClassMembership(ref t) => t.value.is_some(),
        }
    }

    pub fn get_last_date(&self) -> Option<Time> {
        match self {
            Predicate::GroundedMemb(t) => t.get_bms().map(|bms| bms.get_last_time()),
            Predicate::FreeMembershipToClass(_) => None,
            Predicate::FreeClassMembership(t) => t.get_bms().map(|bms| bms.get_last_time()),
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

/// Any entities that belong to a class. Does not include support for time values (yet).
/// E.g.: (let x in some\[x\])
#[derive(Debug, Clone)]
pub(in crate::agent) struct FreeMembershipToClass {
    pub(in crate::agent::lang) term: Var,
    pub(in crate::agent::lang) value: Option<f32>,
    pub(in crate::agent::lang) operator: Option<Operator>,
    pub(in crate::agent::lang) parent: Terminal,
}

impl FreeMembershipToClass {
    fn try_new(
        term: &Var,
        uval: Option<UVal>,
        parent: &Terminal,
    ) -> Result<FreeMembershipToClass, ParseErrF> {
        let (val, op) = match_uval(uval)?;
        Ok(FreeMembershipToClass {
            term: term.clone(),
            value: val,
            operator: op,
            parent: parent.clone(),
        })
    }

    fn generate_uid(&self) -> Vec<u8> {
        let mut id = Vec::from_iter(b"free_memb_cls<".iter().cloned());
        let mut var = self.term.generate_uid();
        id.append(&mut var);
        if let Some(value) = self.value {
            let mut id_2 = format!("{}", value).into_bytes();
            id.append(&mut id_2);
        }
        if let Some(ref cmp) = self.operator {
            cmp.generate_uid(&mut id);
        }
        id.append(&mut self.parent.generate_uid());
        id.push(b'>');
        id
    }

    #[inline]
    pub fn get_parent(&self) -> &str {
        self.parent.get_name()
    }

    #[inline]
    pub fn get_var_ref(&self) -> &Var {
        &self.term
    }

    #[inline]
    pub fn get_var(&self) -> Var {
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

/// Reified object, free class belongship. E.g.: x\[$Lucy>0.5\]
#[derive(Debug, Clone)]
pub(in crate::agent) struct FreeClassMembership {
    term: String,
    pub(in crate::agent::lang) value: Option<f32>,
    operator: Option<Operator>,
    parent: Var,
    times: BmsWrapper<IsTimeData>,
}

impl FreeClassMembership {
    fn try_new(
        term: String,
        uval: Option<UVal>,
        parent: &Terminal,
    ) -> Result<FreeClassMembership, ParseErrF> {
        //TODO: should be able to take op_args
        let (val, op) = match_uval(uval)?;
        Ok(FreeClassMembership {
            term,
            value: val,
            operator: op,
            parent: parent.get_var(),
            times: BmsWrapper::<IsTimeData>::new(None, val),
        })
    }

    fn generate_uid(&self) -> Vec<u8> {
        let mut id = Vec::from_iter(b"free_cls_memb<".iter().cloned());
        id.append(&mut Vec::from(self.term.as_bytes()));
        if let Some(ref val) = self.value {
            let mut id_2 = format!("{}", *val).into_bytes();
            id.append(&mut id_2);
        }
        if let Some(ref cmp) = self.operator {
            cmp.generate_uid(&mut id);
        }
        let mut var = format!("{:?}", self.parent).into_bytes();
        id.append(&mut var);
        id.push(b'>');
        id
    }

    pub fn filter_grounded(&self, other: &GroundedMemb) -> bool {
        if self.operator.is_some() {
            let val = self.value.as_ref().unwrap();
            // get the value at the current set time for self to compare
            let o_val = if let Some(o_val) = other
                .bms
                .as_ref()
                .map(|bms| bms.get_record_at_time(self.times.get_last_time()).0)
                .flatten()
                .or_else(|| *other.value.read())
            {
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

    #[inline]
    pub fn get_name(&self) -> &str {
        &self.term
    }

    #[inline]
    pub fn get_var(&self) -> Var {
        self.parent.clone()
    }
}

impl HasBms for FreeClassMembership {
    type BmsType = BmsWrapper<IsTimeData>;

    fn get_bms(&self) -> Option<&Self::BmsType> {
        Some(&self.times)
    }

    fn get_value(&self) -> Option<f32> {
        self.value
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
    ) -> Option<Arc<BmsWrapper<IsTimeData>>> {
        match *self {
            Assert::FuncDecl(ref f) => f.get_times(agent, var_assign),
            Assert::ClassDecl(ref c) => c.get_times(agent, var_assign),
            Assert::SpecialFunc(_) => None,
        }
    }

    #[inline]
    pub fn get_loc_decl(&self, var: &Var) -> bool {
        match *self {
            Assert::FuncDecl(ref f) => f.get_loc_decl(var),
            Assert::ClassDecl(ref c) => c.get_loc_decl(var),
            Assert::SpecialFunc(_) => false,
        }
    }

    #[inline]
    pub fn get_location(
        &self,
        agent: &Representation,
        var_assign: Option<&HashMap<&Var, &VarAssignment>>,
    ) -> Option<Arc<BmsWrapper<IsSpatialData>>> {
        match *self {
            Assert::FuncDecl(ref f) => f.get_location(agent, var_assign),
            Assert::ClassDecl(ref c) => c.get_location(agent, var_assign),
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
                BuiltIns::Move(f) => f.contains_var(var),
                BuiltIns::Location(f) => f.contains_var(var),
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
        time_assign: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
        loc_assign: &HashMap<&Var, Arc<BmsWrapper<IsSpatialData>>>,
        context: &mut T,
    ) -> Option<bool> {
        match self {
            Assert::FuncDecl(f) => f.grounded_eq(agent, assignments, time_assign, context),
            Assert::ClassDecl(c) => c.grounded_eq(agent, assignments, time_assign, context),
            Assert::SpecialFunc(f) => f.grounded_eq(time_assign, loc_assign),
        }
    }

    #[inline]
    pub fn substitute<T: ProofResContext>(
        &self,
        agent: &Representation,
        assignments: Option<&HashMap<&Var, &VarAssignment>>,
        time_assign: &HashMap<&Var, Arc<BmsWrapper<IsTimeData>>>,
        loc_assign: &HashMap<&Var, Arc<BmsWrapper<IsSpatialData>>>,
        context: &mut T,
    ) {
        match self {
            Assert::FuncDecl(f) => f.substitute(agent, assignments, time_assign, context),
            Assert::ClassDecl(c) => c.substitute(agent, assignments, time_assign, context),
            Assert::SpecialFunc(BuiltIns::Move(move_fn)) => {
                move_fn.substitute(agent, assignments, time_assign, loc_assign, context)
            }
            Assert::SpecialFunc(_) => {
                unreachable!("SIMAG - implication cannot have any other than `move` buil-in func")
            }
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
    // TODO: split `generic` in two variants, one with 2nd operand and one w/o
    /// Generic optional argument which includes one binding value and optionally a second operand to compare against
    Generic(ConstraintValue, Option<(Operator, ConstraintValue)>),
    Time(TimeArg),
    Spatial(SpatialArg),
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

        match SpatialArg::try_from(input) {
            Ok(arg) => return Ok(OpArg::Spatial(arg)),
            Err(ParseErrF::SpatialFnErr(SpatialFnErr::WrongDef)) => {
                return Err(ParseErrF::SpatialFnErr(SpatialFnErr::WrongDef))
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
            OpArg::Spatial(spatial_arg) => spatial_arg.generate_uid(),
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
    SpatialPayload(Point),
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
            UnconstraintArg::Keyword(b"location") => todo!(),
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
            ConstraintValue::SpatialPayload(p) => p.generate_uid(),
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
    pub fn get_var(&self) -> Var {
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
}
