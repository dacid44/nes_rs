use std::{
    sync::mpsc::{self, Sender},
    thread,
    time::{Duration, Instant},
};

use bus::CpuBus;
use cpu::Cpu;
use joypad::{Button, Joypad};
use ppu::Frame;
use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
    EventPump,
};

use crate::{bus::NesBus, rom::Rom};

mod bus;
mod cpu;
mod joypad;
mod ppu;
mod rom;

// 1.79 MHz
const CLOCK_SPEED: f64 = 1.79E6;

const GAME_CODE: &[u8] = &[
    0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9, 0x02, 0x85,
    0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85, 0x12, 0xa9, 0x0f, 0x85,
    0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60, 0xa5, 0xfe, 0x85, 0x00, 0xa5, 0xfe,
    0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60, 0x20, 0x4d, 0x06, 0x20, 0x8d, 0x06, 0x20, 0xc3,
    0x06, 0x20, 0x19, 0x07, 0x20, 0x20, 0x07, 0x20, 0x2d, 0x07, 0x4c, 0x38, 0x06, 0xa5, 0xff, 0xc9,
    0x77, 0xf0, 0x0d, 0xc9, 0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0, 0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60,
    0xa9, 0x04, 0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85, 0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0,
    0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01, 0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04, 0x85, 0x02,
    0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05, 0xa9, 0x08, 0x85, 0x02, 0x60, 0x60, 0x20, 0x94, 0x06,
    0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00, 0xc5, 0x10, 0xd0, 0x0d, 0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07,
    0xe6, 0x03, 0xe6, 0x03, 0x20, 0x2a, 0x06, 0x60, 0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06,
    0xb5, 0x11, 0xc5, 0x11, 0xf0, 0x09, 0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c,
    0x35, 0x07, 0x60, 0xa6, 0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02,
    0x4a, 0xb0, 0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9,
    0x20, 0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28, 0x60, 0xe6,
    0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69, 0x20, 0x85, 0x10, 0xb0,
    0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c, 0x60, 0xc6, 0x10, 0xa5, 0x10, 0x29,
    0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35, 0x07, 0xa0, 0x00, 0xa5, 0xfe, 0x91, 0x00, 0x60,
    0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10, 0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10, 0x60, 0xa2, 0x00, 0xea,
    0xea, 0xca, 0xd0, 0xfb, 0x60,
];

const TEST_CODE: &[u8] = include_bytes!("../roms/6502_functional_test.bin");

fn main() {
    // let mut cpu = Cpu::new();
    // cpu.load(GAME_CODE);
    // cpu.memory.copy_from_slice(TEST_CODE);
    // cpu.reset();
    // cpu.program_counter = 0x400;

    let mut cpu = Cpu::new(NesBus::new(
        Rom::new(include_bytes!("../roms/Pac-Man (U) [!].nes")).unwrap(),
    ));
    cpu.reset();
    // cpu.program_counter = 0xC000;
    // cpu.stack_pointer = 0xFD;

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window(
            "NES Emulator",
            Frame::WIDTH as u32 * 3,
            Frame::HEIGHT as u32 * 3,
        )
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(3.0, 3.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(
            PixelFormatEnum::RGB24,
            Frame::WIDTH as u32,
            Frame::HEIGHT as u32,
        )
        .unwrap();

    let mut is_vblank = false;

    let mut ts = Instant::now();

    let mut last_frame = Instant::now();
    let mut times = [0.0; 256];
    let mut times_i = 0u8;

    let (frame_tx, frame_rx) = mpsc::channel();
    let (input_tx, input_rx) = mpsc::channel();
    thread::spawn(move || loop {
        ts += Duration::from_secs_f64(1.0 / CLOCK_SPEED);
        // let duration = (ts - Instant::now()).max(Duration::ZERO);
        // spin_sleep::sleep(duration);
        while Instant::now() < ts {}

        if !is_vblank && cpu.bus.ppu.is_vblank() {
            frame_tx.send(cpu.bus.ppu.frame.data.clone()).unwrap();
            for (joypad, button, state) in input_rx.try_iter() {
                (&mut cpu.bus.joypads[joypad] as &mut Joypad).buttons[button] = state;
            }
            is_vblank = true;
        } else if is_vblank && !cpu.bus.ppu.is_vblank() {
            is_vblank = false;
        }

        cpu.run_cycle();
        cpu.bus.ppu.run_cycle();
        cpu.bus.ppu.run_cycle();
        cpu.bus.ppu.run_cycle();
    });

    loop {
        handle_user_input(&input_tx, &mut event_pump);

        for frame in frame_rx.try_iter() {
            texture
                .update(None, frame.as_slice(), Frame::WIDTH * 3)
                .unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();

            let t = Instant::now();
            times[times_i as usize] = (t - last_frame).as_secs_f64();
            times_i = times_i.wrapping_add(1);
            if times_i == 0 {
                println!(
                    "average framerate: {}",
                    1.0 / (times.iter().sum::<f64>() / 256.0)
                );
            }
            last_frame = t;
        }
    }
}

fn handle_user_input(channel: &Sender<(usize, Button, bool)>, event_pump: &mut EventPump) {
    let joypad_button = |keycode: Keycode| {
        Some(match keycode {
            Keycode::Num1 => (0, Button::A),
            Keycode::Num2 => (0, Button::B),
            Keycode::Return => (0, Button::Select),
            Keycode::Space => (0, Button::Start),
            Keycode::W => (0, Button::Up),
            Keycode::S => (0, Button::Down),
            Keycode::A => (0, Button::Left),
            Keycode::D => (0, Button::Right),
            Keycode::Semicolon => (1, Button::A),
            Keycode::Quote => (1, Button::B),
            Keycode::Comma => (1, Button::Select),
            Keycode::Period => (1, Button::Start),
            Keycode::Up => (1, Button::Up),
            Keycode::Down => (1, Button::Down),
            Keycode::Left => (1, Button::Left),
            Keycode::Right => (1, Button::Right),
            _ => return None,
        })
    };

    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => std::process::exit(0),
            Event::KeyDown {
                keycode: Some(keycode),
                ..
            } => {
                if let Some((joypad, button)) = joypad_button(keycode) {
                    channel.send((joypad, button, true)).unwrap();
                }
            }
            Event::KeyUp {
                keycode: Some(keycode),
                ..
            } => {
                if let Some((joypad, button)) = joypad_button(keycode) {
                    channel.send((joypad, button, false)).unwrap();
                }
            }
            _ => {}
        }
    }
}

fn transform_color(byte: u8) -> Color {
    match byte {
        0 => Color::BLACK,
        1 => Color::WHITE,
        2 | 9 => Color::GREY,
        3 | 10 => Color::RED,
        4 | 11 => Color::GREEN,
        5 | 12 => Color::BLUE,
        6 | 13 => Color::MAGENTA,
        7 | 14 => Color::YELLOW,
        _ => Color::CYAN,
    }
}

fn read_screen_state(cpu: &mut Cpu<impl CpuBus>, frame: &mut [u8; 32 * 32 * 3]) -> bool {
    let mut update = false;
    for i in 0x00..0x400 {
        let color: [u8; 3] = transform_color(cpu.mem_read(i as u16 + 0x200)).rgb().into();
        let pixel = &mut frame[i * 3..i * 3 + 3];
        if pixel != &color {
            pixel.copy_from_slice(&color);
            update = true;
        }
    }
    update
}

// fn print_snake_state(cpu: &Cpu<impl Bus>) {
//     println!("apple: {:#06X}", cpu.mem_read_u16(0x00));
//     println!("direction: {:04b}", cpu.mem_read(0x02));
//     println!("snake length: {:#04X}", cpu.mem_read(0x03));
//     println!("snake head: {:#06X}", cpu.mem_read_u16(0x10));
//     println!("snake body: {:02X?}", &cpu.memory[0x12..0x12 + cpu.mem_read(0x03) as usize]);
// }
