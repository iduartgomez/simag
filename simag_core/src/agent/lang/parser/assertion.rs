use super::args::{arg, args, op_arg, op_args};
use super::scope::{assert_many, assert_one};
use super::*;
use nom::branch::alt;
use nom::character::complete::{char, multispace0};
use nom::combinator::map;
use nom::combinator::opt;
use nom::multi::separated_list1;
use nom::sequence::tuple;

pub(super) fn assert_knowledge(input: &[u8]) -> IResult<&[u8], AssertBorrowed> {
    alt!(
        input,
        map!(class_decl, AssertBorrowed::from) | map!(func_decl, AssertBorrowed::from)
    )
}

pub(super) fn class_decl(input: &[u8]) -> IResult<&[u8], ClassDeclBorrowed> {
    do_parse!(
        input,
        multispace0
            >> name: map!(terminal, TerminalBorrowed::from_slice)
            >> op_args: opt!(op_args)
            >> a1: args
            >> (ClassDeclBorrowed {
                name,
                op_args,
                args: a1
            })
    )
}

pub(super) fn func_decl(input: &[u8]) -> IResult<&[u8], FuncDeclBorrowed> {
    if let Ok(relational) = do_parse!(
        input,
        multispace0
            >> tag!("fn::")
            >> name: map!(terminal, TerminalBorrowed::from_slice)
            >> op1: opt!(op_args)
            >> a1: args
            >> (FuncDeclBorrowed {
                name,
                args: Some(a1),
                op_args: op1,
                variant: FuncVariants::Relational
            })
    ) {
        Ok(relational)
    } else {
        do_parse!(
            input,
            multispace0
                >> tag!("fn::")
                >> name: map!(terminal, TerminalBorrowed::from_slice)
                >> op1: op_args
                >> (FuncDeclBorrowed {
                    name,
                    args: None,
                    op_args: Some(op1),
                    variant: FuncVariants::NonRelational
                })
        )
    }
}

enum Args<'a> {
    OpArg(OpArgBorrowed<'a>),
    Arg(ArgBorrowed<'a>),
}

pub(super) fn record_decl(input: &[u8]) -> IResult<&[u8], ASTNode> {
    let (i, _) = multispace0(input)?;
    let (i, name) = map(terminal, TerminalBorrowed::from_slice)(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = char('=')(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = char('{')(i)?;

    let sep = tuple((multispace0, char(','), multispace0)); // optional trailing comma
    let elements = alt((map(arg, Args::Arg), map(op_arg, Args::OpArg)));
    let (i, args) = separated_list1(sep, elements)(i)?;

    let (i, _) = tuple((multispace0, opt(char(',')), multispace0))(i)?;
    let (rest, _) = char('}')(i)?;

    let mut normal_args = Vec::with_capacity(args.len());
    let mut op_args = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Args::Arg(a) => normal_args.push(a),
            Args::OpArg(a) => op_args.push(a),
        }
    }

    let mut declared = Vec::with_capacity(normal_args.len());
    for decl in normal_args {
        let assert = AssertBorrowed::from(ClassDeclBorrowed {
            name,
            op_args: {
                if !op_args.is_empty() {
                    Some(op_args.clone())
                } else {
                    None
                }
            },
            args: vec![decl],
        });
        let (_, assert_scope) = assert_one((LogicOperator::And, assert))?;
        declared.push(Ok((EMPTY, assert_scope)));
    }
    let (_, mut last) = declared.pop().unwrap()?;
    if let ASTNode::Scope(last_scope) = &mut last {
        last_scope.logic_op = None;
    }
    if declared.len() > 0 {
        assert_many((None, declared, last))
    } else {
        Ok((rest, last))
    }
}

#[derive(Debug)]
pub(in crate::agent) enum AssertBorrowed<'a> {
    FuncDecl(FuncDeclBorrowed<'a>),
    ClassDecl(ClassDeclBorrowed<'a>),
}

impl<'a> From<ClassDeclBorrowed<'a>> for AssertBorrowed<'a> {
    fn from(decl: ClassDeclBorrowed<'a>) -> AssertBorrowed<'a> {
        AssertBorrowed::ClassDecl(decl)
    }
}

impl<'a> From<FuncDeclBorrowed<'a>> for AssertBorrowed<'a> {
    fn from(decl: FuncDeclBorrowed<'a>) -> AssertBorrowed<'a> {
        AssertBorrowed::FuncDecl(decl)
    }
}

#[derive(Debug, Clone)]
pub(in crate::agent) enum VarDeclBorrowed<'a> {
    Var(VarBorrowed<'a>),
    Skolem(SkolemBorrowed<'a>),
}

// skol_decl = '(' 'exists' $(term[':'op_arg]),+ ')' ;
#[derive(Debug, Clone)]
pub(in crate::agent) struct SkolemBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub ty: TypeDefBorrowed<'a>,
    pub val: Option<UnconstraintArg<'a>>,
}

// var_decl = '(' 'let' $(term[':'op_arg]),+ ')' ;
#[derive(Debug, PartialEq, Clone)]
pub(in crate::agent) struct VarBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub ty: TypeDefBorrowed<'a>,
    pub val: Option<UnconstraintArg<'a>>,
}

// func_decl = 'fn::' term ['(' op_args ')'] args
// 			 | 'fn::' term '(' op_args ')' ;
#[derive(Debug, PartialEq)]
pub(in crate::agent) struct FuncDeclBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub args: Option<Vec<ArgBorrowed<'a>>>,
    pub op_args: Option<Vec<OpArgBorrowed<'a>>>,
    pub variant: FuncVariants,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(in crate::agent) enum FuncVariants {
    Relational,
    NonRelational,
}

impl FuncVariants {
    pub fn is_relational(self) -> bool {
        match self {
            FuncVariants::Relational => true,
            _ => false,
        }
    }
}

// class_decl = term ['(' op_args ')'] args ;
#[derive(Debug, PartialEq)]
pub(in crate::agent) struct ClassDeclBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub args: Vec<ArgBorrowed<'a>>,
    pub op_args: Option<Vec<OpArgBorrowed<'a>>>,
}
