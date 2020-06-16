use super::{
    terminal, ArgBorrowed, IResult, OpArgBorrowed, TerminalBorrowed, TypeDefBorrowed,
    UnconstraintArg,
};
use nom::character::complete::multispace0;

#[derive(Debug)]
pub(in crate::agent) enum AssertBorrowed<'a> {
    FuncDecl(FuncDeclBorrowed<'a>),
    ClassDecl(ClassDeclBorrowed<'a>),
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

impl<'a> FuncDeclBorrowed<'a> {
    #[inline]
    pub(super) fn convert_to_assert(decl: FuncDeclBorrowed<'a>) -> AssertBorrowed<'a> {
        AssertBorrowed::FuncDecl(decl)
    }
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

#[inline]
pub(super) fn func_decl(input: &[u8]) -> IResult<&[u8], FuncDeclBorrowed> {
    use super::args::{args, op_args};

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

// class_decl = term ['(' op_args ')'] args ;
#[derive(Debug, PartialEq)]
pub(in crate::agent) struct ClassDeclBorrowed<'a> {
    pub name: TerminalBorrowed<'a>,
    pub args: Vec<ArgBorrowed<'a>>,
    pub op_args: Option<Vec<OpArgBorrowed<'a>>>,
}

impl<'a> ClassDeclBorrowed<'a> {
    #[inline]
    pub(super) fn convert_to_assert(decl: ClassDeclBorrowed<'a>) -> AssertBorrowed<'a> {
        AssertBorrowed::ClassDecl(decl)
    }
}

#[inline]
pub(super) fn class_decl(input: &[u8]) -> IResult<&[u8], ClassDeclBorrowed> {
    use super::args::{args, op_args};

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
