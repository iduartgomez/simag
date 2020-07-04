use std::convert::TryFrom;

use super::{AssertBorrowed, LogicOperator, ParseErrF, Scope};
use crate::agent::lang::{
    cls_decl::ClassDecl,
    common::Assert,
    fn_decl::FuncDecl,
    logsent::{ParseContext, SentKind},
    LogSentence,
};

#[derive(Debug)]
pub(in crate::agent) enum ParseTree {
    Assertion(Vec<Assert>),
    IExpr(LogSentence),
    Expr(LogSentence),
    ParseErr(ParseErrF),
}

impl ParseTree {
    pub(super) fn process_ast(input: &ASTNode, tell: bool) -> Result<ParseTree, ParseErrF> {
        let mut context = ParseContext::new();
        context.in_assertion = true;
        context.is_tell = tell;
        if let Ok(Some(tree)) = input.is_assertion(&mut context) {
            return Ok(tree);
        }
        // it's an expression, make logic sentence from nested expressions
        let mut context = ParseContext::new();
        context.is_tell = tell;
        match LogSentence::try_new(&input, &mut context) {
            Ok(sent) => match context.stype {
                SentKind::IExpr => Ok(ParseTree::IExpr(sent)),
                SentKind::Expr if context.is_tell => {
                    Err(ParseErrF::ExprWithVars(format!("{}", sent)))
                }
                SentKind::Rule | SentKind::Expr => Ok(ParseTree::Expr(sent)),
            },
            Err(err) => Err(ParseErrF::LogSentErr(err)),
        }
    }

    #[allow(dead_code)]
    pub fn is_err(&self) -> bool {
        match *self {
            ParseTree::ParseErr(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub(in crate::agent) enum ASTNode<'a> {
    Assert(AssertBorrowed<'a>),
    Scope(Box<Scope<'a>>),
    Chain(Vec<ASTNode<'a>>),
    None,
}

impl<'a> ASTNode<'a> {
    pub fn get_op(&self) -> LogicOperator {
        match *self {
            ASTNode::Scope(ref node) => *node.logic_op.as_ref().unwrap(),
            ASTNode::Assert(_) | ASTNode::Chain(_) | ASTNode::None => unreachable!(),
        }
    }

    pub fn would_assert(&self) -> bool {
        match *self {
            ASTNode::Assert(_) => true,
            _ => false,
        }
    }

    pub(super) fn is_assertion(
        &self,
        context: &mut ParseContext,
    ) -> Result<Option<ParseTree>, ParseErrF> {
        context.depth += 1;
        let tree_node = match *self {
            ASTNode::Assert(ref decl) => match *decl {
                // perform potential variable substitution
                AssertBorrowed::ClassDecl(ref decl) => {
                    let mut cls = ClassDecl::try_from((decl, &mut *context))?;
                    if context.in_assertion && context.is_tell {
                        cls.var_substitution()?;
                    }
                    Ok(Some(ParseTree::Assertion(vec![Assert::ClassDecl(cls)])))
                }
                AssertBorrowed::FuncDecl(ref decl) => {
                    let mut func = FuncDecl::try_from((decl, &mut *context))?;
                    if context.in_assertion && context.is_tell {
                        func.var_substitution()?;
                    }
                    Ok(Some(ParseTree::Assertion(vec![Assert::FuncDecl(func)])))
                }
            },
            ASTNode::Chain(ref multi_decl) => {
                let mut v0: Vec<Assert> = Vec::with_capacity(multi_decl.len());
                // chek that indeed all elements are indeed assertions
                // avoid creating declarations prematurely
                for decl in multi_decl {
                    let d = decl.is_assertion(context);
                    match d {
                        Err(err) => return Err(err),
                        Ok(Some(ParseTree::Assertion(mut inner))) => {
                            for e in inner.drain(..) {
                                v0.push(e)
                            }
                        }
                        _ => return Ok(None),
                    }
                }
                Ok(Some(ParseTree::Assertion(v0)))
            }
            ASTNode::Scope(ref node) => {
                match node.logic_op {
                    None | Some(LogicOperator::And) => {}
                    _ => return Err(ParseErrF::WrongDef),
                }
                let a: Result<Option<ParseTree>, ParseErrF> = (**node).is_assertion(context);
                match a {
                    Err(err) => Err(err),
                    Ok(Some(ParseTree::Assertion(assert))) => {
                        Ok(Some(ParseTree::Assertion(assert)))
                    }
                    _ => Ok(None),
                }
            }
            ASTNode::None => Ok(None),
        };
        context.depth -= 1;
        tree_node
    }
}

impl<'a> From<AssertBorrowed<'a>> for ASTNode<'a> {
    fn from(assert: AssertBorrowed<'a>) -> Self {
        ASTNode::Assert(assert)
    }
}
