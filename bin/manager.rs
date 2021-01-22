//! SimAg Manager
//!
//! Creation and management for for SimAg simulations.

use std::collections::HashMap;

use simag_term_utils::{Action, ReplInterpreter, Terminal};

extern crate ansi_term;

use ansi_term::Colour::{Black, Blue, Green, Red, White};
use ansi_term::{ANSIString, ANSIStrings, Style};

use once_cell::sync::Lazy;

// Env management

struct Manager {
    messages: ConsoleMsg,
    buffer: String,
    on_going_cmd: Option<Args>,
}

impl Manager {
    fn new() -> Manager {
        Manager {
            messages: ConsoleMsg::new(),
            buffer: String::new(),
            on_going_cmd: None,
        }
    }

    fn parse_args(&self, cmd: String) -> Args {
        let mut args: Vec<&str> = cmd.trim().split("--").collect();
        let cmd = args.remove(0).trim().to_owned();

        let mut parsed: HashMap<String, Vec<String>> = HashMap::new();
        for orig_arg in &args {
            let mut opt_args: Vec<String> = Vec::new();
            let mut new = true;
            let mut arg: Vec<char> = vec![];
            for c in (*orig_arg).to_string().chars() {
                if !c.is_whitespace() && new {
                    arg.push(c);
                    new = false;
                } else if !c.is_whitespace() {
                    arg.push(c);
                } else if !new && c.is_whitespace() {
                    opt_args.push(arg.iter().cloned().collect::<String>());
                    arg = vec![];
                    new = true;
                }
            }
            let add_arg = arg.iter().cloned().collect::<String>().trim().to_string();
            if add_arg != "" {
                opt_args.push(add_arg)
            };
            parsed.insert(opt_args.remove(0).clone(), opt_args);
        }

        Args { cmd, args: parsed }
    }

    fn make(&self) -> Action<'static> {
        let cmd = self.on_going_cmd.as_ref().unwrap();
        if cmd.args.is_empty() {
            let mut args = HashMap::new();
            args.insert("num", "0");
            args.insert("cmd", "make");
            args.insert("helper", "make --help");
            return self.messages.cmd_errors("arg_num", args);
        }
        for a in cmd.args.keys() {
            if a == "help" {
                return self.messages.help_for_cmd(&cmd);
            }
        }
        Action::WriteStr((self.messages.done(), true))
    }

    fn clean_up(&self) -> Action<'static> {
        let clean_up_pipe = vec![
            Action::WriteStr(("Closing gracefully, please wait ...", false)),
            Action::Command("closing".to_string()),
        ];
        Action::Chain(clean_up_pipe)
    }
}

impl ReplInterpreter for Manager {
    fn digest<'b, 'a: 'b>(&'b mut self, input: char) -> Action<'a> {
        match input {
            '\n' => self.newline_eval(),
            _ => {
                self.buffer.push(input);
                Action::Continue
            }
        }
    }

    fn cmd_executor<'b, 'a: 'b>(&'b mut self, command: String) -> Option<Action<'a>> {
        match command.as_str() {
            "make" => Some(self.make()),
            "help" => Some(Action::WriteMultiStr((self.messages.help(), true))),
            "quit" => Some(self.clean_up()),
            "closing" => Some(Action::Chain(vec![
                Action::Sleep(2000),
                Action::WriteStr((self.messages.done(), false)),
                Action::Sleep(1000),
                Action::Exit,
            ])),
            _ => Some(Action::WriteMultiStr((self.messages.unknown(), true))),
        }
    }

    fn evaluate<'b, 'a: 'b>(&'b mut self) -> Result<Action<'a>, String> {
        unreachable!()
    }

    fn is_reading(&self) -> bool {
        false
    }

    fn set_reading(&mut self, _currently_reading: bool) {}

    fn last_source_input(&self) -> Option<char> {
        self.buffer.chars().last()
    }

    fn queued_command(&mut self) -> Option<String> {
        if self.buffer.is_empty() {
            return None;
        }

        let new_cmd = String::new();
        let exec_cmd = std::mem::replace(&mut self.buffer, new_cmd);
        let args = self.parse_args(exec_cmd);
        let cmd = args.cmd.clone();
        self.on_going_cmd = Some(args);
        Some(cmd)
    }

    fn delete_last<'b, 'a: 'b>(&'b mut self) -> Option<Action<'a>> {
        self.buffer.pop();
        None
    }

    fn drop_command(&mut self) {
        self.buffer.truncate(0);
    }
}

struct Args {
    cmd: String,
    args: HashMap<String, Vec<String>>,
}

// Commands

struct ConsoleMsg {
    messages: HashMap<&'static str, &'static str>,
}

const HELP: &str = "\
Help commands:
* help > the help command
* <COMMAND> --help > prints the help for the command
* make > creates a new network 
* quit > gracefully shut down the program and all the nodes
";

impl ConsoleMsg {
    fn new() -> ConsoleMsg {
        let mut messages = HashMap::new();
        messages.insert("help", HELP);

        static UNKNOWN_COMMAND: Lazy<String> = Lazy::new(|| {
            ANSIStrings(&[
                Red.paint("Unknown command, write "),
                Black.bold().on(White).paint("help"),
                Red.paint(" for a list of commands"),
            ])
            .to_string()
        });
        messages.insert("unknown", &**UNKNOWN_COMMAND);

        static DONE: Lazy<String> = Lazy::new(|| format!("{}", Green.bold().paint("Done!")));
        messages.insert("done", &**DONE);

        static ARG_NUM: Lazy<String> = Lazy::new(|| {
            "Wrong number of arguments ({}), for {} command, write {} for help".to_string()
        });
        messages.insert("arg_num", &**ARG_NUM);

        ConsoleMsg { messages }
    }

    fn unknown(&self) -> &'static str {
        self.messages.get("unknown").unwrap()
    }

    fn help(&self) -> &'static str {
        self.messages.get("help").unwrap()
    }

    fn done(&self) -> &'static str {
        self.messages.get("done").unwrap()
    }

    fn cmd_errors(&self, err: &str, args: HashMap<&str, &str>) -> Action<'static> {
        match err {
            "arg_num" => {
                let msg_ori = self.messages.get("arg_num").unwrap();
                let mut msg_ori: Vec<&str> = msg_ori.split("{}").collect();
                let mut msg_new: Vec<ANSIString> = Vec::with_capacity(7);

                let &num = args.get("num").unwrap();
                let &cmd = args.get("cmd").unwrap();
                let &helper = args.get("helper").unwrap();

                msg_new.push(Style::default().paint(num));
                msg_new.push(Black.bold().on(White).paint(cmd));
                msg_new.push(Black.bold().on(White).paint(helper));

                let mut i = 0;
                while !msg_ori.is_empty() {
                    if (i + 1) % 2 != 0 {
                        msg_new.insert(i, Red.paint(msg_ori.remove(0)));
                    }
                    i += 1;
                }
                let msg = format!("{}", ANSIStrings(&msg_new));
                Action::WriteMulti((msg, true))
            }
            _ => Action::WriteStr(("Wrong arguments...", true)),
        }
    }

    fn help_for_cmd(&self, cmd: &Args) -> Action<'static> {
        let cmd_help = format!(
            "Arguments for {}:\n --num <NODES>",
            Black.bold().on(White).paint(&cmd.cmd)
        );
        Action::WriteMulti((cmd_help, true))
    }
}

static INFO: Lazy<String> = Lazy::new(|| {
    let msg: &[ANSIString] = &[
        Black.on(White).paint("\n    Welcome to the "),
        Red.bold().underline().on(White).paint("SimAg"),
        Style::default().on(White).paint(" "),
        Blue.bold()
            .underline()
            .on(White)
            .paint("Management Command Line"),
        Style::default().on(White).paint("    \n"),
    ];
    ANSIStrings(msg).to_string()
});

fn main() {
    let manager = Manager::new();
    let mut terminal = Terminal::new(manager);
    terminal.print_multiline(&INFO, true);
    terminal.start_event_loop();
}
