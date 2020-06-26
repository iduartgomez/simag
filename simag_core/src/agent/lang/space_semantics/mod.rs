mod move_fn;
mod space_arg;

pub(super) use errors::SpaceFnErr;
pub(super) use move_fn::MoveFn;
pub(super) use space_arg::{Point, SpaceArg};

mod errors {
    use crate::agent::ParseErrF;

    #[derive(Debug, PartialEq, Eq)]
    pub enum SpaceFnErr {
        WrongDef,
    }

    impl Into<ParseErrF> for SpaceFnErr {
        fn into(self) -> ParseErrF {
            ParseErrF::SpaceFnErr(self)
        }
    }
}
