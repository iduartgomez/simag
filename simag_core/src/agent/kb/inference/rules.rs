use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use chrono::Utc;

use crate::agent::{
    kb::VarAssignment,
    lang::{
        GrTerminalKind, Grounded, GroundedFunc, GroundedMemb, GroundedRef, LogSentence,
        ProofResContext, SentID, Time, Var,
    },
    Representation,
};

const MAX_ITERATIONS: u8 = 100;

/// Takes a grounded fact and checks out that is consistent with the existing rules
/// in the representation.
///
/// If it's not the case, then false is retuned.
pub(in crate::agent::kb) fn rules_inference_lookahead(
    agent: &Representation,
    rules: Vec<Arc<LogSentence>>,
    grounded: &GroundedRef,
) -> bool {
    // get all the rules that apply to the provided predicates
    for rule in rules {
        // construct a proper context for the sentence resolution
        let context = RuleResContext::new(&*rule, Some(grounded.clone()));
        // if the rule has any special var, get proper assignments
        let assignments: Option<&HashMap<&Var, &VarAssignment>> = {
            if rule.has_time_vars > 0 {
                todo!()
            } else {
                None
            }
        };
        // get the results from the rule and return value
        let solved_proof = rule.solve(agent, assignments, context);
        if let Some(false) = solved_proof.result {
            return true;
        }
        if solved_proof.inconsistent {
            return true;
        }
    }
    false
}

/// Evaluates if all the grounded facts in the representation are consistent with
/// a given rule.
///
/// If it's not the case, then changes are rolled back (from newest to oldest) until
/// they are consistent with the new rule.
pub(in crate::agent::kb) fn rules_inference_rollback(
    agent: &Representation,
    rule: &Arc<LogSentence>,
) {
    // test the rule
    let context = RuleResContext::new(&*rule, None);
    // if the rule has any special var, get proper assignments
    let assignments: Option<&HashMap<&Var, &VarAssignment>> = {
        if rule.has_time_vars > 0 {
            todo!()
        } else {
            None
        }
    };
    // if the result of the rule is false, proceed to rollback
    let mut solved_proof = rule.solve(agent, assignments, context);
    match solved_proof.result {
        None | Some(true) => return,
        _ => {}
    }
    let mut gr_classes: Vec<Arc<GroundedMemb>> = Vec::new();
    let mut gr_funcs: Vec<Arc<GroundedFunc>> = Vec::new();
    let preds = rule.get_all_predicates();
    for p in preds {
        if p.is_class() {
            let cls_name = p.get_name();
            for decl in p.unwrap_cls_as_ref().get_args() {
                let obj_name = decl.get_name();
                if let Some(decl) =
                    agent.get_obj_from_class(cls_name, &GrTerminalKind::from(obj_name))
                {
                    gr_classes.push(decl);
                }
            }
        } else {
            let func = p.unwrap_fn_as_ref();
            let cmp = func.clone().into();
            if let Some(grfunc) = agent.get_relationship(&cmp, cmp.get_arg_name(1)) {
                gr_funcs.push(grfunc);
            }
        }
    }

    // sort grounded funcs and classes by age and rollback once then check again
    // if the rule still returns false, repeat until it returns None or True
    let mut num_iters = 0;
    loop {
        let mut funcs: Vec<_> = gr_funcs
            .iter()
            .filter(|x| solved_proof.was_false_fn(x))
            .collect();
        funcs.sort_by(|a, b| a.bms.cmp_by_time(&*b.bms));
        let mut classes: Vec<_> = gr_classes
            .iter()
            .filter(|x| solved_proof.was_false_cls(x))
            .collect();
        classes.sort_by(|a, b| {
            a.bms
                .as_ref()
                .unwrap()
                .cmp_by_time(&*b.bms.as_ref().unwrap())
        });

        let func = funcs.last().unwrap();
        let cls = classes.last().unwrap();
        func.bms.rollback_one_once(cls.bms.as_ref().unwrap());

        let unresolved_proof = solved_proof.clone();
        solved_proof = rule.solve(agent, assignments, unresolved_proof);
        match solved_proof.result {
            None | Some(true) => break,
            _ => num_iters += 1,
        }
        if num_iters >= MAX_ITERATIONS {
            // Safety break guarantee; this should be unreachable
            unreachable!("SIMAG - inference/rules.rs: Reached max number of iterations")
        }
    }
}

#[derive(Debug)]
struct RuleResContext<'a> {
    sent: &'a LogSentence,
    global_substitution_time: Time,
    result: Option<bool>,
    newest_grfact: Time,
    antecedents: Vec<Grounded>,
    grounded_fn: Vec<(GroundedFunc, Time)>,
    grounded_cls: Vec<(GroundedMemb, Time)>,
    false_fn: HashSet<*const GroundedFunc>,
    false_cls: HashSet<*const GroundedMemb>,
    cmp_pred: Option<GroundedRef<'a>>,
    sub_mode: bool,
    inconsistent: bool,
}

impl<'a> RuleResContext<'a> {
    fn new(proof: &'a LogSentence, cmp: Option<GroundedRef<'a>>) -> RuleResContext<'a> {
        RuleResContext {
            sent: proof,
            global_substitution_time: Utc::now(),
            result: None,
            newest_grfact: chrono::MIN_DATE.and_hms(0, 0, 0),
            antecedents: vec![],
            grounded_fn: vec![],
            grounded_cls: vec![],
            false_fn: HashSet::new(),
            false_cls: HashSet::new(),
            cmp_pred: cmp,
            sub_mode: false,
            inconsistent: false,
        }
    }

    fn was_false_cls(&self, cmp: &GroundedMemb) -> bool {
        self.false_cls.contains(&(cmp as *const GroundedMemb))
    }

    fn was_false_fn(&self, cmp: &GroundedFunc) -> bool {
        self.false_fn.contains(&(cmp as *const GroundedFunc))
    }
}

impl<'a> std::clone::Clone for RuleResContext<'a> {
    fn clone(&self) -> RuleResContext<'a> {
        let RuleResContext {
            sent, ref cmp_pred, ..
        } = *self;
        RuleResContext {
            sent,
            result: None,
            newest_grfact: chrono::MIN_DATE.and_hms(0, 0, 0),
            global_substitution_time: Utc::now(),
            antecedents: vec![],
            grounded_fn: vec![],
            grounded_cls: vec![],
            false_fn: HashSet::new(),
            false_cls: HashSet::new(),
            cmp_pred: cmp_pred.clone(),
            sub_mode: false,
            inconsistent: false,
        }
    }
}

impl<'a> ProofResContext for RuleResContext<'a> {
    fn sent(&self) -> &LogSentence {
        self.sent
    }

    fn substituting(&mut self) {
        self.sub_mode = true;
    }

    fn is_substituting(&self) -> bool {
        self.sub_mode
    }

    fn set_inconsistent(&mut self, val: bool) {
        self.inconsistent = val;
    }

    fn is_inconsistent(&self) -> bool {
        self.inconsistent
    }

    fn set_result(&mut self, res: Option<bool>) {
        self.result = res;
    }

    fn get_id(&self) -> SentID {
        self.sent.id
    }

    fn get_production_time(&self) -> Time {
        self.global_substitution_time
    }

    fn push_grounded_func(&mut self, grounded: GroundedFunc, time: Time) {
        self.grounded_fn.push((grounded, time));
    }

    fn push_grounded_cls(&mut self, grounded: GroundedMemb, time: Time) {
        self.grounded_cls.push((grounded, time));
    }

    fn newest_grfact(&self) -> Time {
        self.newest_grfact
    }

    fn set_newest_grfact(&mut self, time: Time) {
        self.newest_grfact = time;
    }

    fn get_antecedents(&self) -> &[Grounded] {
        &self.antecedents
    }

    fn push_antecedents(&mut self, grounded: Grounded) {
        self.antecedents.push(grounded);
    }

    fn push_false_fn_assert(&mut self, func: Arc<GroundedFunc>) {
        self.false_fn.insert(&*func as *const GroundedFunc);
    }

    fn push_false_cls_assert(&mut self, cls: Arc<GroundedMemb>) {
        self.false_cls.insert(&*cls as *const GroundedMemb);
    }

    fn compare_relation(&self, func: &GroundedFunc) -> bool {
        match self.cmp_pred {
            Some(GroundedRef::Class(_)) | None => false,
            Some(GroundedRef::Function(cmp)) => cmp.comparable(func),
        }
    }

    fn compare_cls(&self, cls: &GroundedMemb) -> bool {
        match self.cmp_pred {
            Some(GroundedRef::Function(_)) | None => false,
            Some(GroundedRef::Class(cmp)) => cmp.comparable(cls),
        }
    }

    fn has_relationship(&self, func: &GroundedFunc) -> Option<bool> {
        match self.cmp_pred {
            Some(GroundedRef::Class(_)) | None => None,
            Some(GroundedRef::Function(cmp)) => Some(cmp.compare_ignoring_times(func)),
        }
    }

    fn has_cls_memb(&self, cls: &GroundedMemb) -> Option<bool> {
        match self.cmp_pred {
            Some(GroundedRef::Function(_)) | None => None,
            Some(GroundedRef::Class(cmp)) => Some(cmp.compare_ignoring_times(cls)),
        }
    }
}
