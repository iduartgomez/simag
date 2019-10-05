#[derive(PartialEq)]
pub enum Action<'a> {
    /// Signal to the terminal that the interpreter is currently reading
    /// source interpretable instructions.
    Read,
    /// Signal to the terminal that the interpreter is done ingesting
    /// source of interpretable instructions.
    StopReading,
    /// Output a single line message to the terminal.  
    Write((String, bool)),
    /// Output a single line message to the terminal.
    WriteStr((&'a str, bool)),
    /// Output multiple lines message to the terminal.
    WriteMulti((String, bool)),
    /// Output multiple lines message to the terminal.
    WriteMultiStr((&'static str, bool)),
    /// Signal an interpretable command to the terminal main event loop
    /// for the interpreter.
    Command(String),
    /// Continue digesting input
    Continue,
    /// Exit the program
    Exit,
    /// Signal cancelation of any ongoing evaluation
    Discard,
    /// Move to newline
    Newline,
    /// Make the thread sleep for this ammount of ms
    Sleep(u64),
    /// Chain several actions together after processing an event
    Chain(Vec<Action<'a>>),
    None,
}

impl<'a> Action<'a> {
    pub fn exit(&self) -> bool {
        self == &Action::Exit
    }
}
