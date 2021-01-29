use std::{
    collections::VecDeque,
    time::{Duration, Instant},
};

use copypasta::ClipboardContext;
use tui::{
    layout::Rect,
    style::{Color, Style},
    text::{Span, Spans, Text},
};

const CURSOR_ON: char = '\u{2588}';

pub(crate) struct AppState<'a> {
    /// in reading mode flag
    pub reading: bool,
    /// terminate the application flag
    pub terminate: bool,
    call_stack: VecDeque<String>,
    shifted_backward: usize,
    shifted_forward: usize,
    pub cursor: Cursor,
    /// each string belongs to a single line in the box
    input_box: Vec<String>,
    pub input_box_size: Rect,
    /// each group of spans belongs to a single line in the box
    info_box: Vec<Spans<'a>>,
    pub info_box_size: Rect,
    pub clipboard: ClipboardContext,
}

impl<'a> AppState<'a> {
    pub fn new() -> Self {
        AppState {
            reading: false,
            terminate: false,
            call_stack: VecDeque::with_capacity(100),
            shifted_backward: 0,
            shifted_forward: 0,
            cursor: Cursor::new(),
            input_box: Vec::new(),
            input_box_size: Rect::default(),
            info_box: Vec::new(),
            info_box_size: Rect::default(),
            clipboard: ClipboardContext::new().unwrap(),
        }
    }

    pub fn get_current_input_box(&self) -> Text {
        Text::from(
            self.input_box
                .iter()
                .map(|s| Spans::from(s.as_str()))
                .into_iter()
                .collect::<Vec<_>>(),
        )
    }

    pub fn get_current_output_box(&mut self) -> Text {
        let spans_to_print: Vec<_> = if self.info_box.len() > self.info_box_size.height as usize {
            self.info_box[self.info_box_size.height as usize..]
                .iter()
                .cloned()
                .collect()
        } else {
            self.info_box.clone()
        };
        Text::from(spans_to_print)
    }

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

    /// Append a character to the current input box line.
    pub fn input_char(&mut self, input: char) {
        if let Some(line) = self.input_box.last_mut() {
            line.push(input);
        } else {
            self.input_box.push(String::from(input));
        };
    }

    pub fn print_text<'b>(&'b mut self, text: Text<'a>) {
        self.info_box.extend(text.into_iter())
    }

    pub fn print_error(&mut self, err: &dyn std::error::Error) {
        self.print_text(Text::styled(
            format!("Failed with: {}", err),
            Style::default().fg(Color::Red),
        ));
    }

    // Report an error in the output box.
    pub fn report_error(&mut self, msg: &'a str) {
        self.info_box.push(Spans::from(Span::styled(
            msg,
            Style::default().fg(Color::Red),
        )))
    }

    pub fn clear_input_box(&mut self) {
        self.input_box.clear();
    }

    pub fn clear_info_box(&mut self) {
        self.info_box.clear();
    }

    pub fn clear_input_line(&mut self) {
        self.input_box.pop();
        self.newline()
    }

    /// Make a new line in the input box.
    pub fn newline(&mut self) {
        self.input_box.push(String::from(">>> "));
    }

    // Delete previous character from the input box.
    pub fn delete(&mut self) {
        if let Some(line) = self.input_box.last_mut() {
            line.pop();
        }
    }

    pub fn side_effects(&mut self) {
        if self.cursor.effect_on {
            if let Some(line) = self.input_box.last_mut() {
                self.cursor.side_effects(line);
            }
        }
    }
}

pub(crate) struct Cursor {
    column: u16,
    row: u16,
    time: Instant,
    show: bool,
    pub effect_on: bool,
}

impl Default for Cursor {
    fn default() -> Self {
        Self::new()
    }
}

impl Cursor {
    pub fn new() -> Cursor {
        Cursor {
            row: 1,
            column: 1,
            time: Instant::now(),
            show: true,
            effect_on: true,
        }
    }

    pub fn side_effects(&mut self, writer: &mut String) {
        // cursor effect
        if !self.effect_on {
            return;
        }
        let nt = Instant::now();
        if nt.duration_since(self.time) >= Duration::new(0, 500_000_000) {
            if self.show {
                self.show = false;
                match writer.pop() {
                    Some(CURSOR_ON) => {}
                    Some(other) => writer.push(other),
                    None => {}
                }
            } else {
                self.show = true;
                writer.push(CURSOR_ON);
            }
            self.time = nt;
        }
    }

    pub fn at_start_of_the_line(&self) -> bool {
        self.column <= 5
    }
}

#[test]
fn call_stack_rotation() {
    use std::iter::FromIterator;

    let front_sample = "baz".to_owned();
    let back_sample = "bar".to_owned();
    let mut state = AppState::new();
    state.call_stack = VecDeque::from_iter(vec!["foo".to_owned(); 2]);
    state.call_stack.push_front(front_sample);
    state.push_in_call(back_sample);

    for i in 0..5 {
        let r = state.get_previous_call();
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
        let r = state.get_following_call();
        if i == 3 {
            assert!(r.is_none());
        } else {
            assert!(r.is_some());
            if i == 2 {
                assert_eq!("bar", r.unwrap());
            }
        }
    }

    state.reset_call_stack();
    assert_eq!(
        Vec::from(state.call_stack.clone()),
        vec!["baz", "foo", "foo", "bar"]
    );

    state.push_in_call("stuff".to_owned());
    assert_eq!(5, state.call_stack.len());
    assert_eq!("stuff", state.get_previous_call().unwrap().as_str());
}
