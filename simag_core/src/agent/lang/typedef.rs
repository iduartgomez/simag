#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(super) enum TypeDef {
    Erased,
    Class,
    Entity,
    Time,
}
