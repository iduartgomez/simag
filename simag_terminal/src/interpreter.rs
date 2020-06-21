//! A type that implements the behaviour of the input stream interpreter.
//!
//! There are two kind of evaluation dispositions for the interpreter:
//! * Command interpreter
//! * Source reading mode
//!
//! The second mode is optional and is set manually by the interpreter itself.
//! Eventually during the evaluation of newline context the default behaviour will check
//! its own disposition and evaluate the source if in reading mode. This allows to feed
//! the interpreter new source input as long as it wants (ie. until it finds some scape sequence)
//! and only then evalute the result of the source code.
//!
//! Command line interpreter must be implemented mandatorily, in the default implementation
//! the interpreter will be queued for new commands and then will require the interpreter
//! to execute the command.

use crate::Action;

pub trait ReplInterpreter {
    /// Last source code input char that was received by the interpreter.
    fn last_source_input(&self) -> Option<char>;

    /// Returns if the interpreter is currently receiving source stream
    /// or is parsing commmand.
    fn is_reading(&self) -> bool;

    /// Feed new input characters to the interpreter and return
    /// a new action after digestion.
    fn digest<'b, 'a: 'b>(&'b mut self, input: char) -> Action<'a>;

    /// Execute a command and return the continuation action if there
    /// is one.
    fn cmd_executor<'b, 'a: 'b>(&'b mut self, command: String) -> Option<Action<'a>>;

    /// Drop last change of the input stream and return an optional action
    /// to be performed after rollingback last change.
    fn delete_last<'b, 'a: 'b>(&'b mut self) -> Option<Action<'a>>;

    /// Drops any undergoing write on the interpreter command buffer.
    fn drop_command(&mut self);

    /// Set current reading source disposition.
    fn set_reading(&mut self, currently_reading: bool);

    /// Evaluate the currently ingested source.
    fn evaluate<'b, 'a: 'b>(&'b mut self) -> Result<Action<'a>, String>;

    /// Return any queued command if there is one.
    fn queued_command(&mut self) -> Option<String>;

    /// Evaluate the special newline eval character.
    ///
    /// The default implementation will check the reading dispossition of the interpreter
    /// and issue an evaluation of the source if corresponding. Check the module details
    /// for more information.
    fn newline_eval<'b, 'a: 'b>(&'b mut self) -> Action<'a> {
        let currently_reading = self.is_reading();
        let last_input = self.last_source_input();
        if currently_reading && last_input.is_some() && last_input.unwrap() != '\n' {
            Action::Read
        } else if currently_reading {
            self.set_reading(false);
            match self.evaluate() {
                Ok(Action::Write(p)) => Action::Write(p),
                Ok(Action::WriteStr(p)) => Action::WriteStr(p),
                Ok(Action::WriteMulti(p)) => Action::WriteMulti(p),
                Ok(Action::WriteMultiStr(p)) => Action::WriteMultiStr(p),
                Err(msg) => Action::WriteMulti((msg, true)),
                _ => Action::StopReading,
            }
        } else if let Some(command) = self.queued_command() {
            Action::Command(command)
        } else if last_input.is_some() && last_input.unwrap() == '\n' {
            Action::StopReading
        } else {
            Action::Newline
        }
    }
}
