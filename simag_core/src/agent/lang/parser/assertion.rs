use super::args::{arg, args, op_arg, op_args};
use super::*;
use crate::agent::lang::built_ins::{MOVE_FN, TIME_CALC_FN};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, multispace0};
use nom::combinator::{map, opt};
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
            >> name: map!(terminal, TerminalBorrowed::from)
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
    #[inline(always)]
    fn special_func(input: &[u8]) -> IResult<&[u8], FuncDeclBorrowed> {
        let (i, _) = tuple((multispace0, tag("fn::")))(input)?;
        let (i, name) = match is_keyword(i) {
            Ok((rest, TIME_CALC_FN)) => (rest, TIME_CALC_FN),
            Ok((rest, MOVE_FN)) => (rest, MOVE_FN),
            _ => return Err(nom::Err::Error(ParseErrB::SyntaxError)),
        };
        let (mut rest, op1) = op_args(i)?;
        let args = if !rest.is_empty() {
            let (r, args) = opt(args)(rest)?;
            rest = r;
            args
        } else {
            None
        };
        if args.is_some() && name != MOVE_FN {
            return Err(nom::Err::Error(ParseErrB::SyntaxError));
        }
        Ok((
            rest,
            FuncDeclBorrowed {
                name: TerminalBorrowed(name),
                args,
                op_args: Some(op1),
                variant: FuncVariants::NonRelational,
            },
        ))
    }

    #[inline(always)]
    fn normal_pred(input: &[u8]) -> IResult<&[u8], FuncDeclBorrowed> {
        let i = tuple((multispace0, tag("fn::")))(input)?.0;
        let (i, name) = {
            let (i, name) = terminal(i)?;
            (i, TerminalBorrowed::from(name))
        };
        let (i, oa) = opt(op_args)(i)?;
        let (i, a) = args(i)?;

        do_parse!(
            input,
            multispace0
                >> tag!("fn::")
                >> name: map!(terminal, TerminalBorrowed::from)
                >> op1: opt!(op_args)
                >> a1: args
                >> (FuncDeclBorrowed {
                    name,
                    args: Some(a1),
                    op_args: op1,
                    variant: FuncVariants::Relational
                })
        )
    }

    if let Ok((rest, non_relation)) = special_func(input) {
        Ok((rest, non_relation))
    } else {
        normal_pred(input)
    }
}

enum Args<'a> {
    OpArg(OpArgBorrowed<'a>),
    Arg(ArgBorrowed<'a>),
}

pub(super) fn record_decl(input: &[u8]) -> IResult<&[u8], ASTNode> {
    let sep = tuple((multispace0, char(','), multispace0));
    let elements = alt((map(arg, Args::Arg), map(op_arg, Args::OpArg)));

    let (i, _) = tuple((multispace0, char('('), multispace0))(input)?;
    let (i, open_ls_delim) = {
        let (i, l0) = opt(char('['))(i)?;
        (i, l0.is_some())
    };
    let (i, names) = separated_list1(
        char(','),
        map(
            tuple((
                multispace0,
                map(terminal, TerminalBorrowed::from),
                multispace0,
            )),
            |(_, name, _)| name,
        ),
    )(i)?;
    let (i, close_ls_delim) = {
        let (i, (_, l1, ..)) = tuple((multispace0, opt(char(']')), multispace0))(i)?;
        (i, l1.is_some())
    };

    if names.len() > 1 && (!open_ls_delim || !close_ls_delim) {
        return Err(nom::Err::Error(ParseErrB::SyntaxError));
    }

    let (i, _) = tuple((multispace0, char('='), multispace0, char('{')))(i)?;
    let (i, args) = separated_list1(sep, elements)(i)?;
    let (i, _) = tuple((multispace0, opt(char(',')), multispace0))(i)?; // opt trailing comma
    let (rest, _) = tuple((char('}'), multispace0, char(')'), multispace0))(i)?;

    let mut normal_args = Vec::with_capacity(args.len());
    let mut op_args = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            Args::Arg(a) => normal_args.push(a),
            Args::OpArg(a) => op_args.push(a),
        }
    }

    // let name: TerminalBorrowed = names[0];

    let mut declared = Vec::with_capacity(normal_args.len());
    for decl in normal_args {
        for name in &names {
            let mut decl = decl.clone();
            // flip argument and name
            let class_name = decl.term;
            decl.term = *name;
            let assert = ASTNode::Assert(AssertBorrowed::from(ClassDeclBorrowed {
                name: class_name,
                op_args: {
                    if !op_args.is_empty() {
                        Some(op_args.clone())
                    } else {
                        None
                    }
                },
                args: vec![decl],
            }));
            declared.push(assert);
        }
    }
    Ok((rest, ASTNode::Chain(declared)))
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
