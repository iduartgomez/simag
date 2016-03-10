//! SimAg Manager
//!
//! Creation and management for for SimAg simulations.

use std::io;
use std::io::{stdin, stdout, Write};
use std::collections::{HashMap};
//use std::sync::{Arc,Mutex};
//use std::thread;

extern crate ansi_term;

use ansi_term::Colour::{Red, White, Black, Blue, Green};
use ansi_term::{ANSIString, ANSIStrings, Style};

// Env management

struct Manager<'a> { 
	messages: Box<ConsoleMsg<'a>>
}

struct Args<'a> {
	cmd: &'a str,
	args: HashMap<String, Vec<String>>
}

impl<'a> Manager<'a> {
	
	fn new() -> Manager<'a> {
		Manager {
			messages: Box::new( ConsoleMsg::new() ),
		}
	}
	
	fn command_line<'b>(&self) -> Option<bool> {
		print!(">>> ");
		io::stdout().flush().unwrap();
		let mut cmd = String::new();
		stdin().read_line(&mut cmd).unwrap();
		
		let parsed: Args = self.parse_args(&cmd);
		//println!("command: {}, args: {:?}", parsed.cmd, parsed.args);
		match parsed.cmd {
			"make" => self.make(&parsed),
			"help" => self.messages.help(),
			"quit" => self.clean_up(),
			_ => self.messages.unknown(),
		};
		
		// if the cmd is "quit" send Some(true) to the event loop
		if parsed.cmd == "quit" { Some(true) }
		else { None }
	}
	
	fn parse_args<'b>(&'b self, cmd: &'b str) -> Args {
		let mut args: Vec<&str> = cmd.trim().split("--").collect();
		let cmd = args.remove(0).trim();
		
		let mut parsed: HashMap<String, Vec<String>> = HashMap::new();
		for orig_arg in &args {
			let mut opt_args: Vec<String> = Vec::new();
			let mut new = true;
			let mut arg: Vec<char> = vec![];
			for c in orig_arg.to_string().chars() {
				if !c.is_whitespace() && new == true {
					arg.push(c);
					new = false;
				} else if !c.is_whitespace() {
					arg.push(c);
				} else if new == false && c.is_whitespace() {
					opt_args.push(
						arg.iter().cloned().collect::<String>()
					);
					arg = vec![];
					new = true;
				}
			}
			let add_arg = arg.iter().cloned()
				.collect::<String>().trim().to_string();
			if add_arg != "" { opt_args.push(add_arg) };
			parsed.insert(opt_args.remove(0).clone(), opt_args);
		};
		
		Args {
			cmd: cmd,
			args: parsed,
		}
	}
	
	fn make(&self, cmd: &Args) {
		if cmd.args.len() == 0 {
			let mut args = HashMap::new();
			args.insert("num", "0");
			args.insert("cmd", "make");
			args.insert("helper", "make --help");
			self.messages.cmd_errors("arg_num", args);
			return
		}
		for a in cmd.args.keys() {
			if a == "help" { 
				self.messages.help_for_cmd(&cmd);
				return
			}
		}
		self.messages.done();
	}
	
	fn clean_up(&self) {
		println!("Closing gracefully, please wait ...");
		self.messages.done();
	}
}

// Commands and std output

struct ConsoleMsg<'a> {
	messages: HashMap<&'a str, String>,
}

impl<'a> ConsoleMsg<'a> {
	
	fn new() -> ConsoleMsg<'a> {
		let mut messages = HashMap::new();
		
		let msg: &[ANSIString] = &[
			Red.paint("Unknown command, write "),
			Black.bold().on(White).paint("help"),
			Red.paint(" for a list of commands"),
		];
		messages.insert("unknown", format!("{}", ANSIStrings(&msg)));
		
		messages.insert("help", 
			format!("{}", Style::default().paint("Help commands:")));
		
		messages.insert("done", format!("{}", Green.bold().paint("Done!")));
		
		messages.insert(
			"arg_num",
			"Wrong number of arguments ({}), for {} command, \
				write {} for help".to_string()
		);
		
		ConsoleMsg {
			messages: messages,
		}
	}
	
	fn unknown(&self) {
		let &msg = &self.messages.get("unknown").unwrap();
		self.print(&msg);
	}
	
	fn help(&self) {
		let &msg = &self.messages.get("help").unwrap();
		self.print(&msg);
	}
	
	fn done(&self) {
		let &msg = &self.messages.get("done").unwrap();
		self.print(&msg);
	}
	
	fn cmd_errors(&self, err: &str, args: HashMap<&str, &str>) {
		if err == "arg_num" {
			let msg_ori = self.messages.get("arg_num").unwrap();
			let mut msg_ori: Vec<&str> = msg_ori.split("{}").collect();
			let mut msg_new: Vec<ANSIString> = Vec::with_capacity(7);
			
			let &num = args.get("num").unwrap();
			let &cmd = args.get("cmd").unwrap();
			let &helper = args.get("helper").unwrap();
			
			msg_new.push( Style::default().paint(num) );
			msg_new.push( Black.bold().on(White).paint(cmd) );
			msg_new.push( Black.bold().on(White).paint(helper) );
			
			let mut i = 0;
			while msg_ori.len() > 0 {
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
	
	fn print(&self, line: &str) {
		println!("{}", &line);
	}
	
	fn help_for_cmd(&self, cmd: &Args) {
		println!(
			"Commands for {}:\n \
				--no make", 
			Black.bold().on(White).paint(cmd.cmd)
		);
	}
}

fn main() {
	{
		let msg: &[ANSIString] = &[
			Black.on(White).paint("\n    Welcome to the "),
			Red.bold().underline().on(White).paint("SimAg"),
			Style::default().on(White).paint(" "),
			Blue.bold().underline().on(White).paint("Management Command Line"),
			Style::default().on(White).paint("    \n"),
		];
		println!("{}", ANSIStrings(msg));
	}
	
	let manager: Manager = Manager::new();
	loop {
		let cml_break = manager.command_line();
		if cml_break.is_some() { break }
	}
	
}

/*
struct Agent{

}

/// Expose an interface for agent management it includes the calls 
/// and callbacks from Python.
impl Agent {
	
	/// Routine to create a new agent from Python returns a PyObject
	pub fn new() {
		unsafe {
		}
	}
	
	/// Tells the agent a string of FOLext flavour and
	/// returns True if it succeeds or {custom error} otherwise
	pub fn tell() {
		unsafe {
		}
	}
	
	/// Asks the agent a string of FOLext flavour returns 
	/// a boolean or a string
	pub fn ask() {
		unsafe {
		}
	}
}
*/