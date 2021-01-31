use copypasta::ClipboardContext;
use std::{
    collections::VecDeque,
    io::Stdout,
    time::{Duration, Instant},
};
use tui::{
    backend::CrosstermBackend,
    layout::Rect,
    style::{Color, Style},
    text::{Span, Spans, Text},
    Frame,
};

pub(crate) struct AppState<'a> {
    /// in reading mode flag
    pub reading: bool,
    /// terminate the application flag
    pub terminate: bool,
    pub cmd_history: CommandHistory,
    pub cursor: Cursor,
    /// each string belongs to a single line in the box
    input_box_content: Vec<String>,
    input_box: Rect,
    /// each group of spans belongs to a single line in the box
    info_box_content: Vec<Spans<'a>>,
    pub info_box: Rect,
    pub clipboard: ClipboardContext,
}

impl<'a> AppState<'a> {
    pub fn new() -> Self {
        let mut input_box = Rect::default();
        input_box.height = 10; // default heigh
        AppState {
            reading: false,
            terminate: false,
            cmd_history: CommandHistory::default(),
            cursor: Cursor::new(),
            input_box_content: Vec::new(),
            input_box,
            info_box_content: Vec::new(),
            info_box: Rect::default(),
            clipboard: ClipboardContext::new().unwrap(),
        }
    }

    pub fn input_box_frame_content(&self) -> Text {
        Text::from(
            self.input_box_content
                .iter()
                .map(|s| Spans::from(s.as_str()))
                .into_iter()
                .collect::<Vec<_>>(),
        )
    }

    pub fn info_box_frame_content(&mut self) -> Text {
        let spans_to_print: Vec<_> = {
            let diff = self
                .info_box_content
                .len()
                .saturating_sub(self.info_box.height as usize - 2);
            if diff > 0 {
                self.info_box_content
                    .iter()
                    .skip(diff as usize)
                    .cloned()
                    .collect()
            } else {
                self.info_box_content.clone()
            }
        };
        Text::from(spans_to_print)
    }

    pub fn set_input_box(&mut self, ib: Rect) {
        // ignore margin row and column by adding 1
        self.cursor.x_offset = ib.x + 1;
        self.cursor.y_offset = ib.y + 1;
        self.input_box = ib;
    }

    /// Append a character to the current input box line.
    pub fn input_char(&mut self, input: char) {
        self.cursor.x += 1;
        if let Some(line) = self.input_box_content.last_mut() {
            line.push(input);
        } else {
            self.input_box_content.push(String::from(input));
        };
    }

    pub fn print_text<'b>(&'b mut self, text: Text<'a>) {
        self.info_box_content.extend(text.into_iter())
    }

    pub fn print_error(&mut self, err: &dyn std::error::Error) {
        self.print_text(Text::styled(
            format!("Failed with: {}", err),
            Style::default().fg(Color::Red),
        ));
    }

    // Report an error in the output box.
    pub fn report_error(&mut self, msg: &'a str) {
        self.info_box_content.push(Spans::from(Span::styled(
            msg,
            Style::default().fg(Color::Red),
        )))
    }

    pub fn initial_line(&mut self) {
        self.newline();
        self.cursor.y -= 1;
    }

    /// Make a new line in the input box.
    pub fn newline(&mut self) {
        self.cursor.effect_on = true;
        self.cursor.x = 4;
        let input_area = self.input_box.height - 2; // ignore both margin rows
        if self.cursor.y + 1 < input_area {
            self.cursor.y += 1;
        }
        if self.input_box_content.len() as u16 >= (self.input_box.height - 2) {
            self.input_box_content.remove(0);
        }
        self.input_box_content.push(String::from(">>> "));
    }

    pub fn clear_input_box(&mut self) {
        self.input_box_content.clear();
        self.cursor.y = 0;
    }

    pub fn clear_info_box(&mut self) {
        self.info_box_content.clear();
    }

    pub fn clear_input_line(&mut self) {
        self.input_box_content.pop();
        self.newline();
        self.cursor.y -= 1;
    }

    // Delete previous character from the input box.
    pub fn delete(&mut self) {
        if let Some(line) = self.input_box_content.last_mut() {
            if line.pop().is_some() {
                self.cursor.x -= 1;
            }
        }
    }

    pub fn side_effects(&mut self, terminal: &mut Frame<CrosstermBackend<Stdout>>) {
        self.cursor.side_effects(terminal);
    }
}

#[derive(Default)]
pub(crate) struct CommandHistory {
    call_stack: VecDeque<String>,
    shifted_backward: usize,
    shifted_forward: usize,
}

impl CommandHistory {
    pub fn push_in_call(&mut self, call: String) {
        if self.call_stack.len() == 100 {
            self.call_stack.pop_front();
        }
        self.call_stack.push_back(call);
    }

    pub fn get_previous_call(&mut self) -> Option<String> {
        if self.call_stack.is_empty() || self.shifted_backward == self.call_stack.len() {
            return None;
        }
        self.call_stack.rotate_right(1);
        self.shifted_backward += 1;
        if self.shifted_forward > 0 {
            self.shifted_forward -= 1;
        }
        self.call_stack.front().cloned()
    }

    pub fn get_following_call(&mut self) -> Option<String> {
        if self.call_stack.is_empty()
            || self.shifted_forward == self.call_stack.len() - 1
            || self.shifted_backward == 0
        {
            return None;
        }
        self.call_stack.rotate_left(1);
        self.shifted_forward += 1;
        self.shifted_backward -= 1;
        self.call_stack.front().cloned()
    }

    pub fn reset_call_stack(&mut self) {
        if self.shifted_backward > 0 {
            self.call_stack.rotate_left(self.shifted_backward);
            if (self.shifted_forward + 1) > 2 {
                self.shifted_forward -= self.shifted_backward;
            }
        }
        if (self.shifted_forward + 1) > 2 {
            self.call_stack.rotate_right(self.shifted_forward + 2);
        }
        self.shifted_backward = 0;
        self.shifted_forward = 0;
    }
}

pub(crate) struct Cursor {
    x: u16,
    x_offset: u16,
    y: u16,
    y_offset: u16,
    time: Instant,
    show: bool,
    pub effect_on: bool,
}

impl Cursor {
    fn new() -> Cursor {
        Cursor {
            y: 0,
            x_offset: 0,
            x: 0,
            y_offset: 0,
            time: Instant::now(),
            show: true,
            effect_on: true,
        }
    }

    fn side_effects(&mut self, terminal: &mut Frame<CrosstermBackend<Stdout>>) {
        // cursor effect
        if !self.effect_on {
            return;
        }
        //FIXME: should be on/off depending on duration
        terminal.set_cursor(self.x + self.x_offset, self.y + self.y_offset);
        let nt = Instant::now();
        if nt.duration_since(self.time) >= Duration::new(0, 500_000_000) {
            if self.show {
                self.show = false;
            } else {
                self.show = true;
            }
            self.time = nt;
        }
    }

    pub fn at_start_of_the_line(&self) -> bool {
        self.x <= 4
    }
}

#[test]
fn call_stack_rotation() {
    use std::iter::FromIterator;

    let front_sample = "baz".to_owned();
    let back_sample = "bar".to_owned();
    let mut cmd_history = CommandHistory::default();
    cmd_history.call_stack = VecDeque::from_iter(vec!["foo".to_owned(); 2]);
    cmd_history.call_stack.push_front(front_sample);
    cmd_history.push_in_call(back_sample);

    for i in 0..5 {
        let r = cmd_history.get_previous_call();
        if i == 4 {
            assert!(r.is_none());
        } else {
            assert!(r.is_some());
            if i == 3 {
                assert_eq!("baz", r.unwrap());
            }
        }
    }

    for i in 0..4 {
        let r = cmd_history.get_following_call();
        if i == 3 {
            assert!(r.is_none());
        } else {
            assert!(r.is_some());
            if i == 2 {
                assert_eq!("bar", r.unwrap());
            }
        }
    }

    cmd_history.reset_call_stack();
    assert_eq!(
        Vec::from(cmd_history.call_stack.clone()),
        vec!["baz", "foo", "foo", "bar"]
    );

    cmd_history.push_in_call("stuff".to_owned());
    assert_eq!(5, cmd_history.call_stack.len());
    assert_eq!("stuff", cmd_history.get_previous_call().unwrap().as_str());
}
