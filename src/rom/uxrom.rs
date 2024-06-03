use crate::bus::Bus;

use super::{mirror_vram_addr, Mapper, Mirroring, PpuMapper};

pub struct UxRom {
    prg_rom: Box<[u8]>,
    num_banks: usize,
    bank_offset: usize,
    mirroring: Mirroring,
}

impl UxRom {
    pub fn new(prg_rom: impl Into<Box<[u8]>>, mirroring: Mirroring) -> Self {
        let prg_rom = prg_rom.into();
        let num_banks = prg_rom.len() / 0x4000;
        Self {
            prg_rom,
            num_banks,
            bank_offset: 0,
            mirroring,
        }
    }
}

impl Mapper for UxRom {
    fn ppu_mapper(&self) -> PpuMapper {
        UxRomPpu::new(self.mirroring).into()
    }
}

impl Bus for UxRom {
    fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => panic!("addr {addr:#06X} is outside of MMC2 mapped memory"),
            0x8000..=0xBFFF => {
                let data = self.prg_rom[addr as usize - 0x8000 + self.bank_offset];
                // println!("bank {} access: {addr:#06X}, data: {data:#04X}", self.bank_offset / 0x4000);
                data
            }
            0xC000..=0xFFFF => {
                let data = self.prg_rom[addr as usize - 0xC000 + (self.num_banks - 1) * 0x4000];
                let hardcoded_addr = addr as usize - 0xC000 + (7 * 0x4000);
                // println!("num_banks: {}, addr at hardcoded last bank: {hardcoded_addr:#08X}, data: {:#04X}", self.num_banks, self.prg_rom[hardcoded_addr]);
                // println!("last bank access: {addr:#06X}, data: {data:#04X}");
                data
            },
        }
    }

    fn write(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000..=0x7FFF => panic!("addr {addr:#06X} is outside of MMC2 mapped memory"),
            0x8000..=0xFFFF => {
                let mask = self.num_banks as u8 - 1;
                self.bank_offset = (data & mask) as usize * 0x4000;
                println!("bank switch: data {data:#04X}, new bank offset: {:#08X}", self.bank_offset);
            },
        }
    }

    fn peek(&self, addr: u16) -> Option<u8> {
        match addr {
            0x0000..=0x7FFF => None,
            0x8000..=0xBFFF => {
                Some(self.prg_rom[addr as usize - 0x8000 + self.bank_offset])
            }
            0xC000..=0xFFFF => Some(self.prg_rom[addr as usize - 0xC000 + (self.prg_rom.len() - 0x4000)]),
        }
    }
}

pub struct UxRomPpu {
    chr_ram: [u8; 0x2000],
    vram: [u8; 0x800],
    mirroring: Mirroring,
}

impl UxRomPpu {
    pub fn new(mirroring: Mirroring) -> Self {
        Self {
            chr_ram: [0; 0x2000],
            mirroring,
            vram: [0; 0x800],
        }
    }
}

impl Bus for UxRomPpu {
    fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.chr_ram[addr as usize],
            0x2000..=0x2FFF | 0x3000..=0x3EFF => {
                // Some sources say 0x3000..=0x3EFF are unused, others say they mirror
                // VRAM. I'm guessing it probably depends on the mapper.
                let vram_addr = mirror_vram_addr(addr, self.mirroring);
                self.vram[vram_addr as usize]
            }
            0x3F00.. => panic!("addr {addr:#06X} is outside of MMC2 PPU mapped memory"),
        }
    }

    fn write(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000..=0x1FFF => {
                self.chr_ram[addr as usize] = data;
            }
            0x2000..=0x2FFF | 0x3000..=0x3EFF => {
                // Some sources say 0x3000..=0x3EFF are unused, others say they mirror
                // VRAM. I'm guessing it probably depends on the mapper.
                self.vram[mirror_vram_addr(addr, self.mirroring) as usize] = data;
            }
            0x3F00.. => panic!("addr {addr:#06X} is outside of MMC2 PPU mapped memory"),
        }
    }

    fn peek(&self, addr: u16) -> Option<u8> {
        match addr {
            0x0000..=0x1FFF => Some(self.chr_ram[addr as usize]),
            0x2000..=0x2FFF | 0x3000..=0x3EFF => {
                // Some sources say 0x3000..=0x3EFF are unused, others say they mirror
                // VRAM. I'm guessing it probably depends on the mapper.
                let vram_addr = mirror_vram_addr(addr, self.mirroring);
                Some(self.vram[vram_addr as usize])
            }
            0x3F00.. => None,
        }
    }
}
