use enum_dispatch::enum_dispatch;

use crate::{
    cpu::{Interrupts, PROGRAM_START, PROGRAM_START_LOC},
    joypad::Joypad,
    ppu::{Ppu, PpuBus},
    rom::{Rom, Mapper},
};

pub trait CpuBus: Bus {
    fn check_interrupts(&mut self) -> Interrupts {
        Interrupts::empty()
    }

    fn run_cycle(&mut self) {}
}

#[enum_dispatch]
pub trait Bus {
    fn read(&mut self, addr: u16) -> u8;

    fn write(&mut self, addr: u16, data: u8);

    fn peek(&self, addr: u16) -> Option<u8>;
}

#[derive(Clone, PartialEq, Eq)]
pub struct FlatRam {
    pub memory: [u8; u16::MAX as usize + 1],
}

impl FlatRam {
    pub fn new() -> Self {
        Self {
            memory: [0; u16::MAX as usize + 1],
        }
    }

    pub fn load(&mut self, program: &[u8]) {
        self.memory[PROGRAM_START as usize..(PROGRAM_START as usize + program.len())]
            .copy_from_slice(program);
        let [low, high] = PROGRAM_START_LOC.to_le_bytes();
        self.write(PROGRAM_START, low);
        self.write(PROGRAM_START.wrapping_add(1), high);
    }
}

impl Bus for FlatRam {
    fn read(&mut self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn peek(&self, addr: u16) -> Option<u8> {
        Some(self.memory[addr as usize])
    }
}

impl CpuBus for FlatRam {}

pub struct NesBus {
    cpu_ram: [u8; 0x800],
    rom: Rom,
    pub ppu: Ppu,
    last_nmi_state: bool,
    pub joypads: [Joypad; 2],
}

impl NesBus {
    const RAM: u16 = 0x0000;
    const RAM_MIRRORS_END: u16 = 0x1FFF;
    const PPU_REGISTERS: u16 = 0x2000;
    const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

    pub fn new(rom: Rom) -> Self {
        let ppu_mapper = rom.ppu_mapper();
        Self {
            cpu_ram: [0; 2048],
            rom,
            ppu: Ppu::new(PpuBus::new(ppu_mapper)),
            last_nmi_state: false,
            joypads: [Joypad::new(), Joypad::new()],
        }
    }
}

impl Bus for NesBus {
    fn read(&mut self, addr: u16) -> u8 {
        // println!("read at {addr:04X}");
        match addr {
            Self::RAM..=Self::RAM_MIRRORS_END => {
                self.cpu_ram[(addr & 0b00000111_11111111) as usize]
            }
            Self::PPU_REGISTERS..=Self::PPU_REGISTERS_MIRRORS_END => self.ppu.read(addr),
            0x4016..=0x4017 => self.joypads[(addr - 0x4016) as usize].read(),
            0x4020..=0xFFFF => self.rom.read(addr),
            _ => {
                // println!("Ignoring mem read access at {addr:#06X}");
                0
            }
        }
    }

    fn write(&mut self, addr: u16, data: u8) {
        match addr {
            Self::RAM..=Self::RAM_MIRRORS_END => {
                self.cpu_ram[(addr & 0b00000111_11111111) as usize] = data;
            }
            Self::PPU_REGISTERS..=Self::PPU_REGISTERS_MIRRORS_END => {
                self.ppu.write(addr, data);
            }
            0x4014 => {
                // TODO: Cycle accuracy: https://www.nesdev.org/wiki/PPU_registers#OAM_DMA_($4014)_%3E_write
                let page = (data as u16) << 8;
                for i in 0..=0xFF {
                    self.ppu.oam_data[self.ppu.oam_addr.wrapping_add(i) as usize] =
                        self.read(page | i as u16);
                }
            }
            0x4016 => {
                self.joypads[0].write(data);
                self.joypads[1].write(data);
            }
            0x4020..=0xFFFF => self.rom.write(addr, data),
            _ => {
                // println!("Ignoring mem write access at {addr:#06X}");
            }
        }
    }

    fn peek(&self, addr: u16) -> Option<u8> {
        match addr {
            Self::RAM..=Self::RAM_MIRRORS_END => {
                Some(self.cpu_ram[(addr & 0b00000111_11111111) as usize])
            }
            0x4020..=0xFFFF => self.rom.peek(addr),
            _ => None,
        }
    }
}

impl CpuBus for NesBus {
    fn check_interrupts(&mut self) -> Interrupts {
        let mut interrupts = Interrupts::empty();
        let nmi_state = self.ppu.check_nmi();
        if !self.last_nmi_state && nmi_state {
            interrupts.insert(Interrupts::NMI);
        }

        self.last_nmi_state = nmi_state;
        // if interrupts != Interrupts::empty() {
        //     println!("bus interrupts: {:?}", interrupts);
        // }
        interrupts
    }

    fn run_cycle(&mut self) {}
}
