use enum_dispatch::enum_dispatch;

use crate::bus::Bus;

use self::{
    nrom::{NRom, NRomPpu},
    uxrom::{UxRom, UxRomPpu},
};

mod nrom;
mod uxrom;

const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const PRG_ROM_PAGE_SIZE: usize = 0x4000;
const CHR_ROM_PAGE_SIZE: usize = 0x2000;

#[enum_dispatch]
pub trait Mapper: Bus {
    fn ppu_mapper(&self) -> PpuMapper;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

pub fn mirror_vram_addr(addr: u16, mirroring: Mirroring) -> u16 {
    let vram_index = addr & 0xFFF;
    let offset = match mirroring {
        Mirroring::Vertical => [0, 0, 0x800, 0x800],
        Mirroring::Horizontal => [0, 0x400, 0x400, 0x800],
        _ => [0; 4],
    };
    vram_index - offset[(vram_index / 0x400) as usize]
}

#[enum_dispatch(Mapper, Bus)]
pub enum Rom {
    NRom,
    UxRom,
}

#[enum_dispatch(Bus)]
pub enum PpuMapper {
    NRomPpu,
    UxRomPpu,
}

impl Rom {
    pub fn new(raw: &[u8]) -> Result<Self, String> {
        if &raw[0..4] != &NES_TAG {
            return Err("File is not in iNES file format".to_string());
        }

        let mapper = (raw[7] & 0b1111_0000) | (raw[6] >> 4);

        let i_nes_ver = (raw[7] >> 2) & 0b11;
        if i_nes_ver != 0 {
            return Err("iNES 2.0 format is not supported".to_string());
        }

        let four_screen = raw[6] & 0b1000 != 0;
        let vertical_mirroring = raw[6] & 0b1 != 0;
        let screen_mirroring = match (four_screen, vertical_mirroring) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        let prg_rom_size = raw[4] as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size = raw[5] as usize * CHR_ROM_PAGE_SIZE;

        let skip_trainer = raw[6] & 0b100 != 0;

        let prg_rom_start = 16 + if skip_trainer { 0x200 } else { 0x0 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        // let mut raw = raw.to_vec();
        // raw[chr_rom_start..].copy_within(0x1210..0x1220, 0x1240);
        // let chr1 = 0b0000_1110;
        // let chr2 = 0b0111_1110;
        // raw[chr_rom_start..][0x1240..0x1250].copy_from_slice(&[
        //     0x00, chr1, chr1, chr1, chr1, chr1, chr1, 0x00, 0x00, 0x00, 0x00, 0x00, chr2, chr2,
        //     chr2, 0x00,
        // ]);

        eprintln!("mapper: {mapper}, prg_rom_start: {prg_rom_start}, prg_rom_size: {prg_rom_size}");
        match mapper {
            0 => Ok(Self::NRom(NRom::new(
                &raw[prg_rom_start..prg_rom_start + prg_rom_size],
                &raw[chr_rom_start..chr_rom_start + chr_rom_size],
                screen_mirroring,
            ))),
            2 => Ok(Self::UxRom(UxRom::new(
                &raw[prg_rom_start..prg_rom_start + prg_rom_size],
                screen_mirroring,
            ))),
            _ => Err("Unknown mapper".to_string()),
        }
    }
}
