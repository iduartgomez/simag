//! Main knowledge-base logic module, in this module reside the different
//! types that transform and store the data for the individual agents and
//! serve as representations of the different objects and the relationships
//! between them.
//!
//! Main
//! ----
//! `Representation`: Main type, stores all the representations and
//! relationships for a given agent in a concrete time.
//!
//! `Individual`: Represents a singular entity, which is the unique
//! member of it's own set.
//!
//! `Classes`: The sets in which the agent can classify objects.
//! Also stores the types of relations an object can have.
//!
//! Support types, methods and functions
//! -------------------------------------------
//! `Inference`: Encapsulates the whole inference process, from making
//! a temporal substitution representation where the inference is operated to
//! solving the query (including query parsing, data fetching and unification).
