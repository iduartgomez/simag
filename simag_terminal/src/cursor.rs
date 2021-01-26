use std::time::{Duration, Instant};

const CURSOR_ON: char = '\u{2588}';

pub(crate) struct Cursor {
    pub column: u16,
    pub row: u16,
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
