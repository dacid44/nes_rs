use enum_map::{Enum, EnumMap};

#[derive(Debug, Clone)]
pub struct Joypad {
    pub buttons: EnumMap<Button, bool>,
    read_cycle: Option<Button>,
    strobe_mode: bool,
}

impl Joypad {
    pub fn new() -> Self {
        Self {
            buttons: EnumMap::default(),
            read_cycle: None,
            strobe_mode: false,
        }
    }

    pub fn read(&mut self) -> u8 {
        let data = match (self.read_cycle, self.strobe_mode) {
            (_, true) => self.buttons[Button::A] as u8,
            (Some(button), false) => {
                let data = self.buttons[button] as u8;
                self.read_cycle = match button {
                    Button::A => Some(Button::B),
                    Button::B => Some(Button::Select),
                    Button::Select => Some(Button::Start),
                    Button::Start => Some(Button::Up),
                    Button::Up => Some(Button::Down),
                    Button::Down => Some(Button::Left),
                    Button::Left => Some(Button::Right),
                    Button::Right => None,
                };
                data
            }
            (None, false) => 1
        };
        data
    }

    pub fn write(&mut self, data: u8) {
        if data & 1 == 0 {
            self.strobe_mode = false;
        } else {
            self.strobe_mode = true;
            self.read_cycle = Some(Button::A);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Enum)]
pub enum Button {
    A,
    B,
    Select,
    Start,
    Up,
    Down,
    Left,
    Right,
}
