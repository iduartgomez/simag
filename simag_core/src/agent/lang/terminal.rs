use super::{errors::ParseErrF, logsent::ParseContext, parser::TerminalBorrowed, var::Var};
use std::iter::FromIterator;
use std::ops::Deref;
use std::str;

#[cfg(feature = "persistence")]
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
pub(in crate::agent) enum Terminal {
    FreeTerm(Box<Var>),
    GroundedTerm(String),
}

impl<'a> Terminal {
    pub(in crate::agent::lang) fn from(
        other: &TerminalBorrowed<'a>,
        context: &mut ParseContext,
    ) -> Result<Terminal, ParseErrF> {
        let &TerminalBorrowed(slice) = other;
        let name = str::from_utf8(slice).unwrap();
        if super::reserved(name.as_bytes()) {
            return Err(ParseErrF::ReservedKW(name.to_owned()));
        }
        for v in &context.vars {
            if v.name_eq(name) {
                return Ok(Terminal::FreeTerm(Box::new(v.clone())));
            }
        }
        Ok(Terminal::GroundedTerm(name.to_owned()))
    }

    pub(super) fn from_slice(slice: &[u8], context: &ParseContext) -> Result<Terminal, ParseErrF> {
        let name = str::from_utf8(slice).unwrap();
        if super::reserved(slice) {
            return Err(ParseErrF::ReservedKW(name.to_owned()));
        }
        for v in &context.vars {
            if v.name_eq(name) {
                return Ok(Terminal::FreeTerm(Box::new(v.clone())));
            }
        }
        Ok(Terminal::GroundedTerm(name.to_owned()))
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

    pub fn get_var(&self) -> Var {
        if let Terminal::FreeTerm(var) = self {
            (&**var).clone()
        } else {
            unreachable!("SIMAG - called `get_var` on a non-var term")
        }
    }
}

/// Represents a grounded, existing, object which can be a single
/// individual entity or a class of entities.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
#[cfg_attr(feature = "persistence", derive(Serialize, Deserialize))]
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

impl<'a> Deref for GrTerminalKind<&'a str> {
    type Target = &'a str;

    fn deref(&self) -> &Self::Target {
        match self {
            GrTerminalKind::Entity(s) => s,
            GrTerminalKind::Class(s) => s,
        }
    }
}

impl<T> From<T> for GrTerminalKind<T>
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

impl From<&[u8]> for GrTerminalKind<String> {
    fn from(s: &[u8]) -> GrTerminalKind<String> {
        unsafe {
            // safety: this is always safe as parser only receives valid utf-8 str
            if s.starts_with(b"$") {
                GrTerminalKind::Entity(str::from_utf8_unchecked(s).to_owned())
            } else {
                GrTerminalKind::Class(str::from_utf8_unchecked(s).to_owned())
            }
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
