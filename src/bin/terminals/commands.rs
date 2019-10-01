/*
pub struct CommandManager<'a, T>
    where T: CommandMatcher<'a>
{
    command: HashMap<String, T>,
}

pub trait CommandMatcher<'a>  {
    type OutputCommand: Command<'a>;

    fn make_cmd(command: &'a str) -> Self::OutputCommand;
}


pub trait Command<'a>: From<&'a str> {

}
*/
