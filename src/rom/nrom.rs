use std::rc::Rc;

use crate::{bus::Bus, rom::Mirroring};

use super::{mirror_vram_addr, Mapper, PpuMapper};

pub struct NRom {
    prg_rom: Box<[u8]>,
    chr_rom: Rc<[u8]>,
    mirroring: Mirroring,
}

impl NRom {
    pub fn new(
        prg_rom: impl Into<Box<[u8]>>,
        chr_rom: impl Into<Rc<[u8]>>,
        mirroring: Mirroring,
    ) -> Self {
        Self {
            prg_rom: prg_rom.into(),
            chr_rom: chr_rom.into(),
            mirroring,
        }
    }
}

impl Mapper for NRom {
    fn ppu_mapper(&self) -> PpuMapper {
        NRomPpu::new(self.chr_rom.clone(), self.mirroring).into()
    }
}

impl Bus for NRom {
    fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => panic!("addr {addr:#06X} is outside of MMC0 mapped memory"),
            0x8000..=0xFFFF => {
                let mut rom_addr = addr - 0x8000;
                if rom_addr as usize >= self.prg_rom.len() {
                    rom_addr -= 0x4000;
                }
                self.prg_rom[rom_addr as usize]
            }
        }
    }

    fn write(&mut self, addr: u16, _data: u8) {
        match addr {
            0x0000..=0x7FFF => panic!("addr {addr:#06X} is outside of MMC0 mapped memory"),
            0x8000..=0xFFFF => {
                panic!("attempt to write to cartridge PRG ROM space");
            }
        }
    }

    fn peek(&self, addr: u16) -> Option<u8> {
        match addr {
            0x0000..=0x7FFF => None,
            0x8000..=0xFFFF => {
                let mut rom_addr = addr - 0x8000;
                if rom_addr as usize >= self.prg_rom.len() {
                    rom_addr -= 0x4000;
                }
                Some(self.prg_rom[rom_addr as usize])
            }
        }
    }
}

pub struct NRomPpu {
    chr_rom: Rc<[u8]>,
    vram: [u8; 0x800],
    mirroring: Mirroring,
}

impl NRomPpu {
    pub fn new(chr_rom: impl Into<Rc<[u8]>>, mirroring: Mirroring) -> Self {
        Self {
            chr_rom: chr_rom.into(),
            vram: [0; 0x800],
            mirroring,
        }
    }
}

impl Bus for NRomPpu {
    fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.chr_rom[addr as usize],
            0x2000..=0x2FFF | 0x3000..=0x3EFF => {
                // Some sources say 0x3000..=0x3EFF are unused, others say they mirror
                // VRAM. I'm guessing it probably depends on the mapper.
                let vram_addr = mirror_vram_addr(addr, self.mirroring);
                self.vram[vram_addr as usize]
            }
            0x3F00.. => panic!("addr {addr:#06X} is outside of MMC0 PPU mapped memory"),
        }
    }

    fn write(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000..=0x1FFF => {
                panic!("attempt to write to cartridge CHR ROM space: addr {addr:#06X}");
            }
            0x2000..=0x2FFF | 0x3000..=0x3EFF => {
                // Some sources say 0x3000..=0x3EFF are unused, others say they mirror
                // VRAM. I'm guessing it probably depends on the mapper.
                self.vram[mirror_vram_addr(addr, self.mirroring) as usize] = data;
            }
            0x3F00.. => panic!("addr {addr:#06X} is outside of MMC0 PPU mapped memory"),
        }
    }

    fn peek(&self, _addr: u16) -> Option<u8> {
        // TODO: Implement this for better tracing and debuggability
        None
    }
}
