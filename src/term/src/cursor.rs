use std::io::Write;
use std::time::{Duration, Instant};

pub struct Cursor<O> {
    _writter: std::marker::PhantomData<O>,
    pub column: u16,
    pub row: u16,
    space: (u16, u16),
    time: Instant,
    show: bool,
    pub effect_on: bool,
}

impl<O: Write> Default for Cursor<O> {
    fn default() -> Self {
        Self::new()
    }
}

impl<O: Write> Cursor<O> {
    pub fn new() -> Cursor<O> {
        Cursor {
            _writter: std::marker::PhantomData,
            row: 1,
            column: 1,
            space: termion::terminal_size().unwrap(),
            time: Instant::now(),
            show: true,
            effect_on: true,
        }
    }

    pub(crate) fn action(&mut self, stdout: &mut O, action: CursorMovement) -> CursorMovement {
        match action {
            CursorMovement::MoveDown(pos) => {
                self.move_down(stdout, pos);
                CursorMovement::None
            }
            CursorMovement::MoveRight(pos) => self.move_right(stdout, pos),
            CursorMovement::MoveLeft(pos) => self.move_left(stdout, pos),
            CursorMovement::Newline => {
                self.move_down(stdout, 1);
                CursorMovement::None
            }
            CursorMovement::None => CursorMovement::None,
        }
    }

    pub fn cursor_effect(&mut self, stdout: &mut O) {
        if !self.effect_on {
            return;
        }
        let nt = Instant::now();
        if nt.duration_since(self.time) >= Duration::new(0, 500_000_000) {
            if self.show {
                self.show = false;
                self.hide(stdout);
            } else {
                self.show = true;
                self.show(stdout);
            }
            self.time = nt;
        }
    }

    pub fn show(&self, stdout: &mut O) {
        write!(stdout, "{}", termion::cursor::Show).unwrap();
        stdout.flush().unwrap();
    }

    pub fn hide(&self, stdout: &mut O) {
        write!(stdout, "{}", termion::cursor::Hide).unwrap();
        stdout.flush().unwrap();
    }

    pub fn command_line_start(&mut self) {
        self.column = 5;
    }

    pub fn at_start_of_the_line(&self) -> bool {
        self.column > 5
    }

    fn move_down(&mut self, stdout: &mut O, pos: u16) {
        if self.row + pos > self.space.1 {
            write!(stdout, "{}", termion::scroll::Up(pos)).unwrap();
        }
        self.row += pos;
        write!(stdout, "{}", termion::cursor::Goto(1, self.row)).unwrap();
        stdout.flush().unwrap();
    }

    fn move_right(&mut self, stdout: &mut O, pos: u16) -> CursorMovement {
        if self.column + pos > self.space.0 {
            return CursorMovement::Newline;
        } else {
            self.column += pos;
        }
        write!(stdout, "{}", termion::cursor::Goto(self.column, self.row)).unwrap();
        stdout.flush().unwrap();
        CursorMovement::None
    }

    fn move_left(&mut self, stdout: &mut O, pos: u16) -> CursorMovement {
        //FIXME: this breaks out if the move to the left goes beyond the start of the line
        self.column -= pos;
        write!(
            stdout,
            "{} {}",
            termion::cursor::Goto(self.column, self.row),
            termion::cursor::Goto(self.column, self.row)
        )
        .unwrap();
        stdout.flush().unwrap();
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
