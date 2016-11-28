//! Main knowledge-base logic module, in this module reside the different
//! types that transform and store the data for the individual agents and
//! serve as representations of the different objects and the relationships
//! between them.
//!
//! ## Safety
//! There is some unsafe code on this module, the unsafe code performs to taks:
//!     * Assure the compiler that data being referenced will live long enough,
//!       this is, increasing the lifetime bounds.
//!     * Bypass the ```Send``` trait type check when multithreading.
//!
//! Both of those are safe because a Representation owns, uniquely, all its
//! knowledge by the duration of it's own lifetime (data is never dropped, while
//! the representation is alive), thereby is safe to point to the data being
//! referenced from the representation or the query (for the duration of the query).
//!
//! There is an intensive use of reference counted data throught the module,
//! to avoid unnecesary copies. The problem is that Rc is not ```Send``` compliant
//! so we need to 'hack' the type checking system through unsafe code to cheat
//! the compiler.

mod inf;
mod repr;

pub use self::inf::*;
pub use self::repr::*;
