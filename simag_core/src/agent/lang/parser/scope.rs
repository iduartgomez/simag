use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::{map, opt},
    multi::many0,
    sequence::tuple,
};

use super::*;
use crate::agent::lang::logsent::ParseContext;

#[derive(Debug)]
pub(in crate::agent) struct Scope<'a> {
    pub vars: Option<Vec<VarDeclBorrowed<'a>>>,
    pub logic_op: Option<LogicOperator>,
    pub next: ASTNode<'a>,
}

impl<'a> Scope<'a> {
    pub(super) fn parse_scope(input: &[u8]) -> IResult<&[u8], ASTNode> {
        if let Ok((rest, sentence)) = record_decl(input) {
            Ok((rest, sentence))
        } else if let Ok((rest, sentence)) = sentence(input) {
            Ok((rest, sentence))
        } else {
            multiple_asserts(input)
        }
    }

    pub(super) fn is_assertion(
        &self,
        context: &mut ParseContext,
    ) -> Result<Option<ParseTree>, ParseErrF> {
        match self.logic_op {
            Some(LogicOperator::And) | None => {}
            _ => return Ok(None),
        }
        if context.depth == 1 && self.vars.is_some() {
            self.vars
                .as_ref()
                .unwrap()
                .iter()
                .map(|x| context.push_var(x))
                .collect::<Result<Vec<_>, _>>()?;
            self.next.is_assertion(context)
        } else {
            self.next.is_assertion(context)
        }
    }
}

pub(super) fn sentence(input: &[u8]) -> IResult<&[u8], ASTNode> {
    let (i, _) = multispace0(input)?;
    let (i, _) = tag("(")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, vars) = opt(scope_var_decl)(i)?;
    let (i, _) = multispace0(i)?;

    let node;
    let next_input;

    if let Ok((rest, (decl, op, next))) = lhs(i) {
        // is lhs
        if let Ok((_, decl)) = declaration(vars, decl, op, next, true) {
            node = decl;
            next_input = rest;
        } else {
            panic!()
        }
    } else if let Ok((rest, (decl, op, next))) = rhs(i) {
        // is rhs
        match declaration(vars, decl, op, next, false) {
            Ok((_, decl)) => {
                node = decl;
                next_input = rest;
            }
            Err(err) => return Err(err),
        }
    } else if let Ok((rest, (lhs, op, rhs))) = joint_scopes(i) {
        // joint scopes
        match logic_cond(vars, lhs, op, rhs) {
            Ok((_, decl)) => {
                node = decl;
                next_input = rest;
            }
            Err(err) => return Err(err),
        }
    } else if let Ok((rest, next)) = empty_scope(vars, i) {
        node = next;
        next_input = rest;
    } else {
        return Err(nom::Err::Failure(ParseErrB::SyntaxError));
    }

    let (i, _) = multispace0(next_input)?;
    let (i, _) = tag(")")(i)?;
    let (rest, _) = multispace0(i)?;
    Ok((rest, node))
}

#[inline]
fn lhs(i: &[u8]) -> IResult<&[u8], (AssertBorrowed, Option<LogicOperator>, ASTNode)> {
    let (i, decl) = assert_knowledge(i)?;
    let (i, _) = multispace0(i)?;
    let (i, op) = opt(LogicOperator::from_bytes)(i)?;
    let (i, _) = multispace0(i)?;
    let (rest, next) = alt((multiple_asserts, Scope::parse_scope))(i)?;
    Ok((rest, (decl, op, next)))
}

#[inline]
fn rhs(i: &[u8]) -> IResult<&[u8], (AssertBorrowed, Option<LogicOperator>, ASTNode)> {
    let (i, next) = alt((multiple_asserts, Scope::parse_scope))(i)?;
    let (i, _) = multispace0(i)?;
    let (i, op) = opt(LogicOperator::from_bytes)(i)?;
    let (i, _) = multispace0(i)?;
    let (rest, decl) = assert_knowledge(i)?;
    Ok((rest, (decl, op, next)))
}

#[inline]
fn joint_scopes(i: &[u8]) -> IResult<&[u8], (ASTNode, LogicOperator, ASTNode)> {
    let (i, lhs) = alt((multiple_asserts, Scope::parse_scope))(i)?;
    let (i, _) = multispace0(i)?;
    let (i, op) = LogicOperator::from_bytes(i)?;
    let (i, _) = multispace0(i)?;
    let (rest, rhs) = alt((multiple_asserts, Scope::parse_scope))(i)?;
    Ok((rest, (lhs, op, rhs)))
}

/// An scope devoid of any expressions except optionally variable declarations.
/// e.g.: (let x, y (...))
#[inline]
fn empty_scope<'a>(
    vars: Option<DeclVars<'a>>,
    input: &'a [u8],
) -> IResult<'a, &'a [u8], ASTNode<'a>> {
    let (rest, next) = opt(alt((multiple_asserts, Scope::parse_scope)))(input)?;
    if let Some(vars) = vars {
        if let Some(next) = next {
            Ok((
                rest,
                ASTNode::Scope(Box::new(Scope {
                    vars: Some(vars),
                    logic_op: None,
                    next,
                })),
            ))
        } else {
            Ok((rest, ASTNode::None))
        }
    } else if let Some(next) = next {
        Ok((rest, next))
    } else {
        Ok((rest, ASTNode::None))
    }
}

/// Assertion optionally followed by an other assertion:
///     e.g.1: (let x in abc[x=1])
///     e.g.2: (let y in abc[y=2] && ...)
fn declaration<'a>(
    vars: Option<DeclVars<'a>>,
    decl: AssertBorrowed<'a>,
    op: Option<LogicOperator>,
    next: ASTNode<'a>,
    is_lhs: bool,
) -> IResult<'a, &'static [u8], ASTNode<'a>> {
    // let (vars, decl, op, next, is_lhs) = input;
    let assert = ASTNode::Assert(decl);
    let chained = if is_lhs {
        ASTNode::Chain(vec![assert, next])
    } else {
        ASTNode::Chain(vec![next, assert])
    };
    let curr = Scope {
        next: chained,
        vars,
        logic_op: op,
    };
    let curr = ASTNode::Scope(Box::new(curr));
    Ok((EMPTY, curr))
}

/// Logic condition on two expressions, e.g.: (... expr1 || expr2)
fn logic_cond<'a>(
    vars: Option<DeclVars<'a>>,
    lhs: ASTNode<'a>,
    op: LogicOperator,
    rhs: ASTNode<'a>,
) -> IResult<'a, &'static [u8], ASTNode<'a>> {
    // let (vars, lhs, op, rhs) = input;
    let next = Scope {
        vars,
        logic_op: Some(op),
        next: ASTNode::Chain(vec![lhs, rhs]),
    };
    Ok((EMPTY, ASTNode::Scope(Box::new(next))))
}

/// One or multiple concatenated assertions, e.g.: (let x, y in abc[x=1] && def[x=2] && ...)
pub(super) fn multiple_asserts(input: &[u8]) -> IResult<&[u8], ASTNode> {
    let take_decl = map(
        tuple((
            multispace0,
            assert_knowledge,
            multispace0,
            operators::logic_operator,
            multispace0,
        )),
        |(_, decl, _, op, ..)| -> IResult<&[u8], (LogicOperator, AssertBorrowed)> {
            Ok((EMPTY, (LogicOperator::from_bytes(op)?.1, decl)))
        },
    );

    let (i, _) = tuple((multispace0, tag("("), multispace0))(input)?;
    let (i, vars) = opt(scope_var_decl)(i)?;
    let (i, decl) = many0(map(take_decl, |decl| -> IResult<&[u8], ASTNode> {
        assert_one(decl?.1)
    }))(i)?;
    let (i, last) = map(assert_knowledge, ASTNode::from)(i)?;
    let (rest, _) = tuple((multispace0, tag(")"), multispace0))(i)?;
    let asserts = assert_many((vars, decl, last))?.1;
    Ok((rest, asserts))
}

pub(super) type AssertOne<'a> = (LogicOperator, AssertBorrowed<'a>);

pub(super) fn assert_one(input: AssertOne) -> IResult<&[u8], ASTNode> {
    let (op, assertion) = input;
    Ok((
        EMPTY,
        ASTNode::Scope(Box::new(Scope {
            next: ASTNode::Assert(assertion),
            vars: None,
            logic_op: Some(op),
        })),
    ))
}

type AssertMany<'a, T> = (Option<DeclVars<'a>>, T, ASTNode<'a>);

pub(super) fn assert_many<'a, T>(input: AssertMany<'a, T>) -> IResult<&[u8], ASTNode>
where
    T: IntoIterator<Item = IResult<'a, &'a [u8], ASTNode<'a>>>,
{
    let (vars, decl, last) = input;

    let mut fd = vec![];
    for e in decl.into_iter() {
        match e {
            Ok((_, e)) => fd.push(e),
            err => return err,
        };
    }

    let last = ASTNode::Scope(Box::new(Scope {
        vars: None,
        logic_op: None,
        next: last,
    }));

    if !fd.is_empty() {
        fd.push(last);
        let f = if let Some(vars) = vars {
            ASTNode::Scope(Box::new(Scope {
                vars: Some(vars),
                logic_op: None,
                next: ASTNode::Chain(fd),
            }))
        } else {
            ASTNode::Chain(fd)
        };
        Ok((EMPTY, f))
    } else {
        let f = if let Some(vars) = vars {
            ASTNode::Scope(Box::new(Scope {
                vars: Some(vars),
                logic_op: None,
                next: last,
            }))
        } else {
            last
        };
        Ok((EMPTY, f))
    }
}

type DeclVars<'a> = Vec<VarDeclBorrowed<'a>>;

/// only var: let a, b in
/// only existential: exist c, d in
/// both: let a, b and exist c, d in
pub(super) fn scope_var_decl(i: &[u8]) -> IResult<&[u8], DeclVars> {
    fn get_ty(ty: UnconstraintArg) -> Result<&[u8], nom::Err<ParseErrB>> {
        match ty {
            UnconstraintArg::Keyword(kw) => {
                if is_type(kw) {
                    Ok(kw)
                } else {
                    Err(nom::Err::Error(ParseErrB::SyntaxError))
                }
            }
            UnconstraintArg::Terminal(def) => Ok(def),
            UnconstraintArg::String(_) => Err(nom::Err::Error(ParseErrB::SyntaxError)),
        }
    }

    fn get_vars(mut input: &[u8], var: bool) -> IResult<&[u8], Vec<VarDeclBorrowed>> {
        let mut vars = vec![];
        let mut seps = 0;
        loop {
            let (i, _) = multispace0(input)?;
            let (i, name) = alt((terminal, alt((tag("in"), tag("and")))))(i)?;
            if name == b"in" || name == b"and" {
                input = i;
                break;
            }
            let (i, ty, val) = {
                match opt(tuple((
                    tag(":"),
                    multispace0,
                    UnconstraintArg::get,
                    multispace0,
                    opt(tuple((tag("="), multispace0, UnconstraintArg::get))),
                )))(i)?
                {
                    (rest, Some((.., ty, _, Some((.., val))))) => {
                        (rest, TypeDefBorrowed(get_ty(ty)?), Some(val))
                    }
                    (rest, Some((_, _, ty, .., None))) => {
                        (rest, TypeDefBorrowed(get_ty(ty)?), None)
                    }
                    (rest, None) => (rest, TypeDefBorrowed(EMPTY), None),
                }
            };
            let (i, _) = multispace0(i)?;
            let (i, s) = opt(tag(","))(i)?;
            if s.is_some() {
                seps += 1;
            }
            if var {
                vars.push(VarDeclBorrowed::Var(VarBorrowed {
                    name: TerminalBorrowed::from(name),
                    ty,
                    val,
                }));
            } else {
                vars.push(VarDeclBorrowed::Skolem(SkolemBorrowed {
                    name: TerminalBorrowed::from(name),
                    ty,
                    val,
                }));
            }
            input = i;
        }

        if vars.len() - 1 == seps {
            Ok((input, vars))
        } else {
            Err(nom::Err::Error(ParseErrB::SyntaxError))
        }
    }

    let mut scope_vars = vec![];
    let (i, _) = multispace0(i)?;

    let (mut i, let_kw) = opt(tag("let "))(i)?;
    if let_kw.is_some() {
        let (rest, vars) = get_vars(i, true)?;
        i = rest;
        scope_vars.extend(vars);
    }
    let (i, _) = multispace0(i)?;
    let (i, and_kw) = opt(tag("and"))(i)?;
    let (i, _) = multispace0(i)?;

    let (mut i, exists_kw) = opt(tag("exists "))(i)?;
    // check that the combination of keywords is correct:
    match (let_kw, and_kw, exists_kw) {
        (Some(_), None, None) => {}
        (None, None, Some(_)) => {}
        (Some(_), Some(_), Some(_)) => {}
        _ => return Err(nom::Err::Error(ParseErrB::SyntaxError)),
    };

    if exists_kw.is_some() {
        let (rest, vars) = get_vars(i, false)?;
        i = rest;
        scope_vars.extend(vars);
    }

    Ok((i, scope_vars))
}
