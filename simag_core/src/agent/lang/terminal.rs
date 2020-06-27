use std::iter::FromIterator;
use std::str;
use std::sync::Arc;

use super::{errors::ParseErrF, logsent::ParseContext, parser::TerminalBorrowed, var::Var};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub(in crate::agent) enum Terminal {
    FreeTerm(Arc<Var>),
    GroundedTerm(String),
}

impl<'a> Terminal {
    pub(in crate::agent::lang) fn from(
        other: &TerminalBorrowed<'a>,
        context: &mut ParseContext,
    ) -> Result<Terminal, ParseErrF> {
        let &TerminalBorrowed(slice) = other;
        let name = str::from_utf8(slice).unwrap().to_owned();
        if super::reserved(name.as_bytes()) {
            return Err(ParseErrF::ReservedKW(name));
        }
        for v in &context.vars {
            if v.name == name {
                return Ok(Terminal::FreeTerm(v.clone()));
            }
        }
        Ok(Terminal::GroundedTerm(name))
    }

    pub(super) fn from_slice(slice: &[u8], context: &ParseContext) -> Result<Terminal, ParseErrF> {
        let name = str::from_utf8(slice).unwrap().to_owned();
        if super::reserved(slice) {
            return Err(ParseErrF::ReservedKW(name));
        }
        for v in &context.vars {
            if v.name == name {
                return Ok(Terminal::FreeTerm(v.clone()));
            }
        }
        Ok(Terminal::GroundedTerm(name))
    }

    pub(in crate::agent::lang) fn generate_uid(&self) -> Vec<u8> {
        match *self {
            Terminal::FreeTerm(ref var) => var.generate_uid(),
            Terminal::GroundedTerm(ref name) => Vec::from_iter(name.as_bytes().iter().cloned()),
        }
    }

    pub(in crate::agent::lang) fn is_var(&self) -> bool {
        if let Terminal::FreeTerm(_) = *self {
            true
        } else {
            false
        }
    }

    #[inline]
    pub(in crate::agent::lang) fn get_name(&self) -> &str {
        if let Terminal::GroundedTerm(ref name) = *self {
            name
        } else {
            unreachable!()
        }
    }

    pub(super) fn is_grounded(&self) -> bool {
        if let Terminal::GroundedTerm(_) = *self {
            true
        } else {
            false
        }
    }

    pub fn get_var_ref(&self) -> &Var {
        if let Terminal::FreeTerm(ref var) = *self {
            &*var
        } else {
            unreachable!()
        }
    }

    pub fn get_var(&self) -> Arc<Var> {
        if let Terminal::FreeTerm(ref var) = *self {
            var.clone()
        } else {
            panic!(format!(
                "SIMAG - {}:{} - called `get_var` on a non-var term",
                file!(),
                line!()
            ))
        }
    }
}

/// Represents a grounded, existing, object which can be a single
/// individual entity or a class of entities.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub(in crate::agent) enum GrTerminalKind<T>
where
    T: AsRef<str>,
{
    Entity(T),
    Class(T),
}

impl<'a, T> Into<&'a str> for &'a GrTerminalKind<T>
where
    T: AsRef<str>,
{
    fn into(self) -> &'a str {
        match self {
            GrTerminalKind::Entity(s) => s.as_ref(),
            GrTerminalKind::Class(s) => s.as_ref(),
        }
    }
}

impl<'a, T> From<T> for GrTerminalKind<T>
where
    T: AsRef<str>,
{
    fn from(s: T) -> GrTerminalKind<T> {
        if s.as_ref().starts_with('$') {
            GrTerminalKind::Entity(s)
        } else {
            GrTerminalKind::Class(s)
        }
    }
}

impl<T> std::fmt::Display for GrTerminalKind<T>
where
    T: AsRef<str>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let term = match self {
            GrTerminalKind::Entity(val) => val,
            GrTerminalKind::Class(val) => val,
        };
        write!(f, "{}", term.as_ref())
    }
}
