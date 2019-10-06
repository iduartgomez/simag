mod interpreter;

pub use interpreter::SimagInterpreter;

const HELP_COMMAND: &str = "\
Welcome to the interactive Simag 0.0.1 interpreter!

You can start feeding information for the interpreter by writing syntactically valid expressions.
For querying the interpreter just preceed your expression query with a ?. For more info on 
the query expression operator write \"help queries\".

To quit this utility just write \"quit\" or \"exit\". For a complete list of commands write 
\"help commands\".
";

const HELP_COMMANDS: &str = "\
List of valid commands:
* help > the help command
* help commands > this command, prints info about commands
* help queries > how to query the engine
* quit | exit > close down the program
";

const HELP_QUERYING: &str = "\
For querying just preceed your query with ?. If the query is valid you can explore the results 
using ??<expr>, substitue <expr> for a valid expression for the ?? operator:

> ??single > return the global result for the query
> ??multi <expr> > return the result for this part of the query
";
