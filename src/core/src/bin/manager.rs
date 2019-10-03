//! SimAg Manager
//!
//! Creation and management for for SimAg simulations.

use std::collections::HashMap;

use simag_term::{Action, Interpreter, Terminal};

extern crate ansi_term;

use ansi_term::Colour::{Black, Blue, Green, Red, White};
use ansi_term::{ANSIString, ANSIStrings, Style};

use once_cell::sync::Lazy;

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

// Env management

struct Manager {
    messages: ConsoleMsg,
    buffer: String,
    reading: bool,
}

impl Manager {
    fn new() -> Manager {
        Manager {
            messages: ConsoleMsg::new(),
            buffer: String::new(),
            reading: false,
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
            for c in orig_arg.to_string().chars() {
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

    fn make(&self, cmd: &Args) {
        if cmd.args.is_empty() {
            let mut args = HashMap::new();
            args.insert("num", "0");
            args.insert("cmd", "make");
            args.insert("helper", "make --help");
            self.messages.cmd_errors("arg_num", args);
            return;
        }
        for a in cmd.args.keys() {
            if a == "help" {
                self.messages.help_for_cmd(&cmd);
                return;
            }
        }
        self.messages.done();
    }

    fn clean_up(&self) {
        println!("Closing gracefully, please wait ...");
        self.messages.done();
    }
}

impl Interpreter for Manager {
    fn digest(&mut self, input: char) -> Action {
        match input {
            '\n' => self.newline_eval(),
            _ => {
                self.buffer.push(input);
                Action::Continue
            }
        }
    }

    fn cmd_executor(&mut self, command: String) -> Option<Action> {
        let parsed: Args = self.parse_args(command);

        if parsed.cmd == "quit" {
            return Some(Action::Exit);
        }

        match parsed.cmd.as_ref() {
            //"make" => self.make(&parsed),
            "help" => Some(Action::WriteMulti(self.messages.help().to_owned())),
            //"quit" => self.clean_up(),
            _ => Some(Action::WriteMulti(self.messages.unknown().to_owned())),
        }
    }

    fn evaluate(&mut self) -> Result<Action, String> {
        Ok(Action::Continue)
    }

    fn is_reading(&self) -> bool {
        self.reading
    }

    fn set_reading(&mut self, currently_reading: bool) {
        self.reading = currently_reading;
    }

    fn last_source_input(&self) -> Option<char> {
        self.buffer.chars().last()
    }

    fn queued_command(&mut self) -> Option<String> {
        unimplemented!();
    }

    fn delete_last(&mut self) -> Option<Action> {
        None
    }
}

struct Args {
    cmd: String,
    args: HashMap<String, Vec<String>>,
}

// Commands and std output

struct ConsoleMsg {
    messages: HashMap<&'static str, String>,
}

impl ConsoleMsg {
    fn new() -> ConsoleMsg {
        let mut messages = HashMap::new();

        let unknown_command: &[ANSIString] = &[
            Red.paint("Unknown command, write "),
            Black.bold().on(White).paint("help"),
            Red.paint(" for a list of commands"),
        ];
        messages.insert("unknown", format!("{}", ANSIStrings(&unknown_command)));

        messages.insert(
            "help",
            format!("{}", Style::default().paint("Help commands:")),
        );

        messages.insert("done", format!("{}", Green.bold().paint("Done!")));

        messages.insert(
            "arg_num",
            "Wrong number of arguments ({}), for {} command, write {} for help".to_string(),
        );

        ConsoleMsg { messages }
    }

    fn unknown(&self) -> &str {
        self.messages.get("unknown").unwrap()
    }

    fn help(&self) -> &str {
        self.messages.get("help").unwrap()
    }

    fn done(&self) -> &str {
        self.messages.get("done").unwrap()
    }

    fn cmd_errors(&self, err: &str, args: HashMap<&str, &str>) {
        if err == "arg_num" {
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
            self.print(&msg);
        } else if err == "wrong_args" {

        }
    }

    fn print(&self, line: &str) -> String {
        format!("{}", &line)
    }

    fn help_for_cmd(&self, cmd: &Args) -> String {
        format!(
            "Commands for {}:\n --no make",
            Black.bold().on(White).paint(&cmd.cmd)
        )
    }
}

fn main() {
    let mut terminal = Terminal::new(Manager::new());
    terminal.print_multiline(&INFO);
    terminal.newline();
    terminal.start_event_loop();
}
