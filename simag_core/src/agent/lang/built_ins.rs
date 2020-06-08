use super::time_semantics::TimeCalc;

/// Special built-in functions callable in logical sentences.
#[derive(Debug, Clone)]
pub(in crate::agent) enum BuiltIns {
    TimeCalculus(TimeCalc),
}
