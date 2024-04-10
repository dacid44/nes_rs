const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const PRG_ROM_PAGE_SIZE: usize = 0x4000;
const CHR_ROM_PAGE_SIZE: usize = 0x2000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

pub struct Rom {
    pub prg_rom: Box<[u8]>,
    pub chr_rom: Box<[u8]>,
    pub mapper: u8,
    pub screen_mirroring: Mirroring,
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

        Ok(Self {
            prg_rom: raw[prg_rom_start..prg_rom_start + prg_rom_size].into(),
            chr_rom: raw[chr_rom_start..chr_rom_start + chr_rom_size].into(),
            mapper,
            screen_mirroring,
        })
    }

    pub fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if addr as usize >= self.prg_rom.len() {
            addr -= 0x4000;
        }
        self.prg_rom[addr as usize]
    }
}
