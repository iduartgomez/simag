//! SimAg Manager
//!
//! Creation and management for for SimAg simulations.
use simag_term_utils::{Action, Application, ReplInterpreter};
use std::collections::HashMap;
use tui::{
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
};

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

    fn make_cmd(&self) -> Action<'static> {
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
        Action::WriteInfoText(self.messages.done().clone())
    }

    fn clean_up(&self) -> Action<'static> {
        let clean_up_pipe = vec![
            Action::WriteInfoText(Text::from("Closing gracefully, please wait ...")),
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
            "make" => Some(self.make_cmd()),
            "help" => Some(Action::WriteInfoText(self.messages.help().clone())),
            "quit" => Some(self.clean_up()),
            "closing" => Some(Action::Chain(vec![
                Action::Sleep(2000),
                Action::WriteInfoText(self.messages.done().clone()),
                Action::Sleep(1000),
                Action::Exit,
            ])),
            _ => Some(Action::WriteInfoText(self.messages.unknown().clone())),
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
    messages: HashMap<&'static str, Text<'static>>,
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
        messages.insert("help", Text::from(HELP));

        let unknown_command = Text::from(Spans::from(vec![
            Span::styled("Unknown command, write ", Style::default().fg(Color::Red)),
            Span::styled(
                "help",
                Style::default()
                    .bg(Color::White)
                    .fg(Color::Black)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::styled(" for a list of commands", Style::default().fg(Color::Red)),
        ]));
        messages.insert("unknown", unknown_command);

        let done = Text::styled(
            "Done!",
            Style::default()
                .add_modifier(Modifier::BOLD)
                .fg(Color::Green),
        );
        messages.insert("done", done);

        let arg_num = Text::from(Spans::from(vec![
            Span::styled(
                "Wrong number of arguments (",
                Style::default().fg(Color::Red),
            ),
            Span::from("{}"),
            Span::styled("), for ", Style::default().fg(Color::Red)),
            Span::from("{}"),
            Span::styled("command, write ", Style::default().fg(Color::Red)),
            Span::from("{}"),
            Span::styled(" for help", Style::default().fg(Color::Red)),
        ]));
        messages.insert("arg_num", arg_num);

        ConsoleMsg { messages }
    }

    fn unknown(&self) -> &Text<'static> {
        self.messages.get("unknown").unwrap()
    }

    fn help(&self) -> &Text<'static> {
        self.messages.get("help").unwrap()
    }

    fn done(&self) -> &Text<'static> {
        self.messages.get("done").unwrap()
    }

    fn cmd_errors(&self, err: &str, args: HashMap<&str, &'static str>) -> Action<'static> {
        match err {
            "arg_num" => {
                let mut msg_new = self.messages.get("arg_num").unwrap().clone();
                let line = &mut msg_new.lines[0].0;

                let num = Span::from(*args.get("num").unwrap());
                line.get_mut(1).map(|t| *t = num);

                let cmd = Span::styled(
                    *args.get("cmd").unwrap(),
                    Style::default()
                        .bg(Color::White)
                        .fg(Color::Black)
                        .add_modifier(Modifier::BOLD),
                );
                line.get_mut(1).map(|t| *t = cmd);

                let helper = Span::styled(
                    *args.get("helper").unwrap(),
                    Style::default()
                        .bg(Color::White)
                        .fg(Color::Black)
                        .add_modifier(Modifier::BOLD),
                );
                line.get_mut(1).map(|t| *t = helper);

                Action::WriteInfoText(msg_new)
            }
            _ => Action::WriteInfoText(Text::from("Wrong arguments...")),
        }
    }

    fn help_for_cmd(&self, cmd: &Args) -> Action<'static> {
        let cmd_help = Spans::from(vec![
            Span::from("Arguments for "),
            Span::styled(
                format!("{}", cmd.cmd),
                Style::default()
                    .bg(Color::White)
                    .fg(Color::Black)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::from(":\n --num <NODES>"),
        ]);
        Action::WriteInfoText(Text::from(cmd_help))
    }
}

pub fn init_app() -> std::io::Result<()> {
    let info = Text::from(Spans::from(vec![
        Span::styled(
            " Welcome to the ",
            Style::default().fg(Color::Black).bg(Color::White),
        ),
        Span::styled(
            "SimAg",
            Style::default()
                .bg(Color::White)
                .fg(Color::Red)
                .add_modifier(Modifier::BOLD)
                .add_modifier(Modifier::UNDERLINED),
        ),
        Span::styled(" ", Style::default().bg(Color::White)),
        Span::styled(
            "Management Command Line",
            Style::default()
                .bg(Color::White)
                .fg(Color::Red)
                .add_modifier(Modifier::BOLD)
                .add_modifier(Modifier::UNDERLINED),
        ),
        Span::styled(" ", Style::default().fg(Color::Black).bg(Color::White)),
    ]));

    let mut app = Application::new(Manager::new());
    app.print_text(info);
    app.start_event_loop()
}

fn main() -> std::io::Result<()> {
    init_app()?;
    Ok(())
}
