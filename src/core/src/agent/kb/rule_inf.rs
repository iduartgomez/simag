use super::repr::*;
use lang::*;

use std::collections::HashMap;
use std::sync::Arc;

pub fn rules_inference(agent: &Representation, cls_names: &[&str], func_names: &[&FuncDecl]) {
    let obj_dic = agent.by_class(cls_names);
    let func_dic = agent.by_relationship(func_names);
    for objects in obj_dic.values() {
        for decl in objects {
            
        }
    }
    panic!("not implemented: rule.call(self, e)")
}

#[derive(Debug)]
pub struct RuleResContext {
    result: Option<bool>,
    newest_grfact: Date,
    antecedents: Vec<Grounded>,
    grounded_func: Vec<(GroundedFunc, Date)>,
    grounded_cls: Vec<(GroundedMemb, Date)>,
    sent_id: SentID,
}

impl ProofResContext for RuleResContext {
    fn set_result(&mut self, res: Option<bool>) {
        self.result = res;
    }

    fn get_id(&self) -> SentID {
        self.sent_id
    }

    fn push_grounded_func(&mut self, grounded: GroundedFunc, time: Date) {
        self.grounded_func.push((grounded, time));
    }

    fn push_grounded_cls(&mut self, grounded: GroundedMemb, time: Date) {
        self.grounded_cls.push((grounded, time));
    }

    fn newest_grfact(&self) -> &Date {
        &self.newest_grfact
    }

    fn set_newest_grfact(&mut self, date: Date) {
        self.newest_grfact = date;
    }

    fn get_antecedents(&self) -> &[Grounded] {
        &self.antecedents
    }

    fn push_antecedents(&mut self, grounded: Grounded) {
        self.antecedents.push(grounded);
    }
}

impl RuleResContext {
    fn new(proof: &LogSentence) -> RuleResContext {
        RuleResContext {
            result: None,
            newest_grfact: ::chrono::date::MIN.and_hms(0, 0, 0),
            sent_id: proof.get_id(),
            antecedents: vec![],
            grounded_func: vec![],
            grounded_cls: vec![],
        }
    }
}
