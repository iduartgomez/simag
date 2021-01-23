use std::io::Write;
use std::time::{Duration, Instant};

pub(crate) struct Cursor<W> {
    _writter: std::marker::PhantomData<W>,
    pub column: u16,
    pub row: u16,
    space: (u16, u16),
    time: Instant,
    show: bool,
    pub effect_on: bool,
}

impl<W: Write> Default for Cursor<W> {
    fn default() -> Self {
        Self::new()
    }
}

impl<W: Write> Cursor<W> {
    pub fn new() -> Cursor<W> {
        Cursor {
            _writter: std::marker::PhantomData,
            row: 1,
            column: 1,
            space: (0, 0),
            time: Instant::now(),
            show: true,
            effect_on: true,
        }
    }

    pub fn action(&mut self, writer: &mut W, action: CursorMovement) -> CursorMovement {
        match action {
            CursorMovement::MoveDown(pos) => {
                self.move_down(writer, pos);
                CursorMovement::None
            }
            CursorMovement::MoveRight(pos) => self.move_right(writer, pos),
            CursorMovement::MoveLeft(pos) => self.move_left(writer, pos),
            CursorMovement::Newline => {
                self.move_down(writer, 1);
                CursorMovement::None
            }
            CursorMovement::None => CursorMovement::None,
        }
    }

    pub fn side_effects(&mut self, writer: &mut W) {
        // re-evaluate terminal size for dynamic changes in size
        // self.space = termion::terminal_size().unwrap();

        // cursor effect
        if !self.effect_on {
            return;
        }
        let nt = Instant::now();
        if nt.duration_since(self.time) >= Duration::new(0, 500_000_000) {
            if self.show {
                self.show = false;
                self.hide(writer);
            } else {
                self.show = true;
                self.show(writer);
            }
            self.time = nt;
        }
    }

    pub fn show(&self, writer: &mut W) {
        // write!(writer, "{}", termion::cursor::Show).unwrap();
        // writer.flush().unwrap();
    }

    pub fn hide(&self, writer: &mut W) {
        // write!(writer, "{}", termion::cursor::Hide).unwrap();
        // writer.flush().unwrap();
    }

    pub fn command_line_start(&mut self) {
        self.column = 5;
    }

    pub fn at_start_of_the_line(&self) -> bool {
        self.column <= 5
    }

    fn move_down(&mut self, writer: &mut W, pos: u16) {
        if self.row + pos > self.space.1 {
            // write!(writer, "{}", termion::scroll::Up(pos)).unwrap();
        }
        self.row += pos;
        // write!(writer, "{}", termion::cursor::Goto(1, self.row)).unwrap();
        writer.flush().unwrap();
    }

    fn move_right(&mut self, writer: &mut W, pos: u16) -> CursorMovement {
        if self.column + pos > self.space.0 {
            return CursorMovement::Newline;
        } else {
            self.column += pos;
        }
        // write!(writer, "{}", termion::cursor::Goto(self.column, self.row)).unwrap();
        writer.flush().unwrap();
        CursorMovement::None
    }

    fn move_left(&mut self, writer: &mut W, pos: u16) -> CursorMovement {
        if self.column > 2 {
            self.column -= pos;
        }
        // write!(writer, "{}", termion::cursor::Goto(self.column, self.row)).unwrap();
        writer.flush().unwrap();
        CursorMovement::None
    }
}

pub(crate) enum CursorMovement {
    /// Move to newline
    Newline,
    MoveDown(u16),
    MoveRight(u16),
    MoveLeft(u16),
    None,
}
