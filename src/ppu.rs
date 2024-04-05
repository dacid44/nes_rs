use bitflags::{bitflags, Flags};

use crate::{bus::Bus, rom::Mirroring};
use std::{array, mem};

#[rustfmt::skip]
pub static SYSTEM_PALETTE: [(u8, u8, u8); 64] = [
   (0x80, 0x80, 0x80), (0x00, 0x3D, 0xA6), (0x00, 0x12, 0xB0), (0x44, 0x00, 0x96), (0xA1, 0x00, 0x5E),
   (0xC7, 0x00, 0x28), (0xBA, 0x06, 0x00), (0x8C, 0x17, 0x00), (0x5C, 0x2F, 0x00), (0x10, 0x45, 0x00),
   (0x05, 0x4A, 0x00), (0x00, 0x47, 0x2E), (0x00, 0x41, 0x66), (0x00, 0x00, 0x00), (0x05, 0x05, 0x05),
   (0x05, 0x05, 0x05), (0xC7, 0xC7, 0xC7), (0x00, 0x77, 0xFF), (0x21, 0x55, 0xFF), (0x82, 0x37, 0xFA),
   (0xEB, 0x2F, 0xB5), (0xFF, 0x29, 0x50), (0xFF, 0x22, 0x00), (0xD6, 0x32, 0x00), (0xC4, 0x62, 0x00),
   (0x35, 0x80, 0x00), (0x05, 0x8F, 0x00), (0x00, 0x8A, 0x55), (0x00, 0x99, 0xCC), (0x21, 0x21, 0x21),
   (0x09, 0x09, 0x09), (0x09, 0x09, 0x09), (0xFF, 0xFF, 0xFF), (0x0F, 0xD7, 0xFF), (0x69, 0xA2, 0xFF),
   (0xD4, 0x80, 0xFF), (0xFF, 0x45, 0xF3), (0xFF, 0x61, 0x8B), (0xFF, 0x88, 0x33), (0xFF, 0x9C, 0x12),
   (0xFA, 0xBC, 0x20), (0x9F, 0xE3, 0x0E), (0x2B, 0xF0, 0x35), (0x0C, 0xF0, 0xA4), (0x05, 0xFB, 0xFF),
   (0x5E, 0x5E, 0x5E), (0x0D, 0x0D, 0x0D), (0x0D, 0x0D, 0x0D), (0xFF, 0xFF, 0xFF), (0xA6, 0xFC, 0xFF),
   (0xB3, 0xEC, 0xFF), (0xDA, 0xAB, 0xEB), (0xFF, 0xA8, 0xF9), (0xFF, 0xAB, 0xB3), (0xFF, 0xD2, 0xB0),
   (0xFF, 0xEF, 0xA6), (0xFF, 0xF7, 0x9C), (0xD7, 0xE8, 0x95), (0xA6, 0xED, 0xAF), (0xA2, 0xF2, 0xDA),
   (0x99, 0xFF, 0xFC), (0xDD, 0xDD, 0xDD), (0x11, 0x11, 0x11), (0x11, 0x11, 0x11),
];

pub struct Ppu {
    chr_rom: Box<[u8]>,
    palette_table: [u8; 32],
    vram: [u8; 2048],
    pub oam_data: [u8; 256],
    mirroring: Mirroring,
    ctrl_register: ControlRegister,
    mask_register: MaskRegister,
    status_register: StatusRegister,
    pub oam_addr: u8,
    addr_scroll_registers: AddrScrollRegisters,
    data_buffer: u8,
    line: usize,
    dot: usize,
    odd_frame: bool,
    pub frame: Frame,
    line_sprites: [Option<u8>; 8],
}

impl Ppu {
    pub fn new(chr_rom: Box<[u8]>, mirroring: Mirroring) -> Self {
        Self {
            chr_rom,
            palette_table: [0; 32],
            vram: [0; 2048],
            oam_data: [0; 256],
            mirroring,
            ctrl_register: ControlRegister::empty(),
            mask_register: MaskRegister::empty(),
            status_register: StatusRegister::empty(),
            oam_addr: 0,
            addr_scroll_registers: AddrScrollRegisters::new(),
            data_buffer: 0,
            // Start on pre-render scanline
            line: 261,
            dot: 0,
            odd_frame: false,
            frame: Frame::new(),
            line_sprites: [None; 8],
        }
    }

    fn get_address(&mut self) -> u16 {
        let addr = self.addr_scroll_registers.get_addr();
        self.addr_scroll_registers
            .increment_addr(self.ctrl_register.vram_addr_increment());
        addr
    }

    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let vram_index = addr & 0xFFF;
        let offset = match self.mirroring {
            Mirroring::Vertical => [0, 0, 0x800, 0x800],
            Mirroring::Horizontal => [0, 0x400, 0x400, 0x800],
            _ => [0; 4],
        };
        vram_index - offset[(vram_index / 0x400) as usize]
    }

    pub fn run_cycle(&mut self) {
        // TODO: Sprite overflow and sprite 0 hit flags
        match (self.line, self.dot) {
            (0..=239, 0) => {
                self.get_line_sprites();
            }
            (241, 1) => {
                self.status_register.insert(StatusRegister::VBLANK_STARTED);
            }
            (261, 1) => {
                self.status_register.remove(StatusRegister::VBLANK_STARTED);
            }
            _ => {}
        }

        // Draw line of 8 pixels
        if (0..=239).contains(&self.line) && (1..=256).contains(&self.dot) && (self.dot % 8 == 0) {
            let x = self.dot - 8;
            let mut pixels = self.get_background_line((x / 8) as u8, self.line as u8);
            (|| {
                if let Some(sprite_data) = self.get_sprite_data(x as u8) {
                    for i in 0..8 {
                        pixels[i] = if sprite_data[i].1 {
                            pixels[i].or(sprite_data[i].0)
                        } else {
                            sprite_data[i].0.or(pixels[i])
                        };
                    }
                }
            })();

            (|| {
                for (x, pixel) in (self.dot - 8..self.dot).zip(pixels.into_iter()) {
                    self.frame.set_pixel(
                        x,
                        self.line,
                        pixel.unwrap_or_else(|| SYSTEM_PALETTE[self.palette_table[0] as usize]),
                    );
                }
            })();
        }

        // Increment
        (|| {
            self.dot = (self.dot + 1) % 341;
            if self.dot == 0 {
                self.line = (self.line + 1) % 262;
                if self.line == 0 {
                    self.odd_frame = !self.odd_frame;
                    if self.odd_frame {
                        self.dot += 1;
                    }
                }
            }
        })();
    }

    fn get_background_line(&self, tile_x: u8, y: u8) -> [Option<(u8, u8, u8)>; 8] {
        let tile_y = y / 8;
        let nametable_offset =
            self.mirror_vram_addr(self.ctrl_register.nametable_offset()) as usize;

        // Fetch tile number from nametable
        // println!("{tile_x}, {tile_y}, {:#06X}", (tile_y as usize) * 32 + tile_x as usize + nametable_offset);
        let tile_number = self.vram[(tile_y as usize) * 32 + tile_x as usize + nametable_offset];

        // Fetch palette number from nametable attribute data
        let attribute_table_index = (tile_y / 4 * 8) + (tile_x / 4);
        let mut palette_number =
            self.vram[attribute_table_index as usize + 0x03C0 + nametable_offset];
        if tile_y & 0b10 == 0 {
            // Top
            palette_number &= 0x0F;
        } else {
            // Bottom
            palette_number >>= 4;
        }
        if tile_x & 0b10 == 0 {
            // Left
            palette_number &= 0b11;
        } else {
            // Right
            palette_number >>= 2;
        }

        // Fetch color indices from pattern table
        let color_indices = self.get_pattern_table_entry(true, tile_number, y % 8);

        // Fetch colors
        color_indices.map(|color_index| self.get_color(true, palette_number, color_index))
        // [(255, 0, 0); 8]
    }

    fn get_line_sprites(&mut self) {
        self.line_sprites = [None; 8];
        let mut num_line_sprites = 0;
        for i in 0..64 {
            let sprite_y = self.oam_data[i * 4].saturating_add(1);
            if (sprite_y..=sprite_y.saturating_add(7)).contains(&(self.line as u8)) {
                self.line_sprites[num_line_sprites] = Some(i as u8);
                num_line_sprites += 1;
                if num_line_sprites == 8 {
                    break;
                }
            }
        }
    }

    fn get_sprite_data(&self, x: u8) -> Option<[(Option<(u8, u8, u8)>, bool); 8]> {
        self.line_sprites
            .into_iter()
            .flatten()
            .filter_map(|i| {
                let i = i as usize;
                // I think this should be saturating, NESDev Wiki says a sprite is offscreen if y =
                // 0xFF
                let sprite_y = self.oam_data[i * 4].saturating_add(1);

                // TODO: Tall sprites
                let tile_number = self.oam_data[i * 4 + 1];

                let byte_2 = self.oam_data[i * 4 + 2];
                let palette_number = byte_2 & 0b11;
                let priority = byte_2 & 0b0010_0000 != 0;
                let h_flip = byte_2 & 0b0100_0000 != 0;
                let v_flip = byte_2 & 0b1000_0000 != 0;

                let sprite_x = self.oam_data[i * 4 + 3];

                let overlap_x =
                    x.max(sprite_x)..=x.saturating_add(7).min(sprite_x.saturating_add(7));

                if overlap_x.is_empty() {
                    return None;
                }

                let color_indices = self.get_pattern_table_entry(
                    false,
                    tile_number,
                    if v_flip {
                        7 - (self.line as u8 - sprite_y)
                    } else {
                        self.line as u8 - sprite_y
                    },
                );

                let mut sprite_data = [(None, false); 8];

                for j in overlap_x {
                    let sprite_index = if h_flip {
                        7 - (j - sprite_x)
                    } else {
                        j - sprite_x
                    };

                    sprite_data[(j - x) as usize] = (
                        self.get_color(
                            false,
                            palette_number,
                            color_indices[(sprite_index) as usize],
                        ),
                        priority,
                    );
                }
                Some(sprite_data)
            })
            .reduce(|front, back| stack_sprites(back, front))
    }

    fn get_pattern_table_entry(
        &self,
        background: bool,
        tile_number: u8,
        row_number: u8,
    ) -> [u8; 8] {
        let mut index = 0usize;
        let right_page = self.ctrl_register.contains(if background {
            ControlRegister::BACKGROUND_PATTERN_ADDR
        } else {
            ControlRegister::SPRITE_PATTERN_ADDR
        });
        index |= if right_page { 1 << 12 } else { 0 };
        index |= (tile_number as usize) << 4;

        let low = self.chr_rom[index + row_number as usize];
        let high = self.chr_rom[index + 8 + row_number as usize];

        // Specifying the array instead of a range gets us a fixed-length array back
        [0, 1, 2, 3, 4, 5, 6, 7].map(|bit| {
            let bit = 7 - bit;
            let low_bit = (low & (1 << bit)) >> bit;
            let high_bit = (high & (1 << bit)) >> bit;
            (high_bit << 1) | low_bit
        })
    }

    fn get_color(
        &self,
        background: bool,
        palette_number: u8,
        color_index: u8,
    ) -> Option<(u8, u8, u8)> {
        if color_index == 0 {
            return None;
        }
        let mut index = color_index;
        index |= palette_number << 2;
        index |= if background { 0 } else { 1 << 4 };
        let system_palette_index = self.palette_table[index as usize];
        Some(SYSTEM_PALETTE[system_palette_index as usize])
    }

    pub fn is_vblank(&self) -> bool {
        self.status_register
            .contains(StatusRegister::VBLANK_STARTED)
    }

    pub fn check_nmi(&mut self) -> bool {
        self.is_vblank() && self.ctrl_register.contains(ControlRegister::GENERATE_NMI)
    }
}

impl Bus for Ppu {
    fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("attempt to read from write-only PPU register: {addr:#06X}")
            }
            0x2002 => self.status_register.bits(),
            0x2004 => self.oam_data[self.oam_addr as usize],
            0x2007 => {
                let ppu_addr = self.get_address();
                match ppu_addr {
                    0x0000..=0x1FFF => {
                        mem::replace(&mut self.data_buffer, self.chr_rom[ppu_addr as usize])
                    }
                    0x2000..=0x2FFF | 0x3000..=0x3EFF => {
                        let vram_addr = self.mirror_vram_addr(ppu_addr);
                        mem::replace(&mut self.data_buffer, self.vram[vram_addr as usize])
                    }
                    // 0x3000..=0x3EFF => panic!(
                    //     "addr {ppu_addr:#06X} is in 0x3000..=0x3EFF which should have been mirrored"
                    // ),
                    0x3F00..=0x3FFF => self.palette_table[(addr % 0x20) as usize],
                    _ => panic!("unexpected access to PPU addr {ppu_addr:#06X}"),
                }
            }
            0x2008..=0x3FFF => self.read(addr % 0x8 + 0x2000),
            _ => panic!("address {addr:#06X} is out of PPU range"),
        }
    }

    fn write(&mut self, addr: u16, data: u8) {
        match addr {
            0x2000 => self.ctrl_register = ControlRegister::from_bits_truncate(data),
            0x2001 => self.mask_register = MaskRegister::from_bits_truncate(data),
            0x2002 => panic!("attempt to write to read-only PPU status register (0x2002)"),
            0x2003 => self.oam_addr = data,
            0x2004 => self.oam_data[self.oam_addr as usize] = data,
            0x2005 => self.addr_scroll_registers.update_scroll(data),
            0x2006 => self.addr_scroll_registers.update_addr(data),
            0x2007 => {
                let ppu_addr = self.get_address();
                match ppu_addr {
                    0x0000..=0x1FFF => {
                        panic!("attempt to write to cartridge CHR ROM space");
                    }
                    0x2000..=0x2FFF | 0x3000..=0x3EFF => {
                        self.vram[self.mirror_vram_addr(ppu_addr) as usize] = data;
                    }
                    // 0x3000..=0x3EFF => panic!(
                    //     "addr {ppu_addr:#06X} is in 0x3000..=0x3EFF which is unused by the PPU"
                    // ),
                    0x3F00..=0x3FFF => self.palette_table[(ppu_addr % 0x20) as usize] = data,
                    _ => panic!("unexpected access to PPU addr {ppu_addr:#06X}"),
                }
            }
            0x2008..=0x3FFF => self.write(addr % 0x8 + 0x2000, data),
            _ => panic!("address {addr:#06X} is out of PPU range"),
        }
    }

    fn peek(&self, _addr: u16) -> Option<u8> {
        // TODO: Are any of the PPU registers readable without side effects? maybe add those here
        None
    }
}

fn stack_sprites<T: Copy, const N: usize>(
    back: [(Option<T>, bool); N],
    front: [(Option<T>, bool); N],
) -> [(Option<T>, bool); N] {
    array::from_fn(|i| match (front[i].0, back[i].0) {
        (Some(_), _) => front[i],
        (None, Some(_)) => back[i],
        (None, None) => (None, false),
    })
}

fn print_if_not_same<T: PartialEq + std::fmt::Debug>(array: [T; 8]) -> [T; 8] {
    if array
        .iter()
        .try_fold(&array[0], |a, b| (a == b).then_some(a))
        .is_none()
    {
        println!("{array:?}");
    }
    array
}

struct AddrScrollRegisters {
    // Big-endian
    address: [u8; 2],
    scroll_x: u8,
    scroll_y: u8,
    high_x_byte: bool,
}

impl AddrScrollRegisters {
    fn new() -> Self {
        Self {
            address: [0; 2],
            scroll_x: 0,
            scroll_y: 0,
            high_x_byte: true,
        }
    }

    fn set_addr(&mut self, addr: u16) {
        self.address = (addr % 0x4000).to_be_bytes();
    }

    fn get_addr(&self) -> u16 {
        u16::from_be_bytes(self.address)
    }

    fn update_addr(&mut self, value: u8) {
        if self.high_x_byte {
            // Mirror addresses above 0x4000
            self.address[0] = value % 0x40;
        } else {
            self.address[1] = value;
        }
        self.high_x_byte = !self.high_x_byte;
    }

    fn increment_addr(&mut self, inc: u8) {
        let (result, carry) = self.address[1].overflowing_add(inc);
        self.address[1] = result;
        if carry {
            self.address[0] = self.address[0].wrapping_add(1) % 0x40
        }
    }

    fn get_scroll(&self) -> (u8, u8) {
        (self.scroll_x, self.scroll_x)
    }

    fn update_scroll(&mut self, value: u8) {
        if self.high_x_byte {
            self.scroll_x = value;
        } else {
            self.scroll_y = value;
        }
        self.high_x_byte = !self.high_x_byte;
    }

    fn reset_latch(&mut self) {
        self.high_x_byte = true;
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct ControlRegister: u8 {
       const NAMETABLE1               = 0b00000001;
       const NAMETABLE2               = 0b00000010;
       const VRAM_ADD_INCREMENT       = 0b00000100;
       const SPRITE_PATTERN_ADDR      = 0b00001000;
       const BACKGROUND_PATTERN_ADDR  = 0b00010000;
       const SPRITE_SIZE              = 0b00100000;
       const MASTER_SLAVE_SELECT      = 0b01000000;
       const GENERATE_NMI             = 0b10000000;
    }
}

impl ControlRegister {
    fn vram_addr_increment(&self) -> u8 {
        if self.contains(ControlRegister::VRAM_ADD_INCREMENT) {
            32
        } else {
            1
        }
    }

    fn nametable_offset(&self) -> u16 {
        (self.bits() & 0b11) as u16 * 0x400
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct MaskRegister: u8 {
        const GRAYSCALE               = 0b00000001;
        const BG_LEFT_COL_ENABLE      = 0b00000010;
        const SPRITE_LEFT_COL_ENABLE  = 0b00000100;
        const BG_ENABLE               = 0b00001000;
        const SPRITE_ENABLE           = 0b00010000;
        const EMPHASIZE_RED           = 0b00100000;
        const EMPHASIZE_GREEN         = 0b01000000;
        const EMPHASIZE_BLUE          = 0b10000000;
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct StatusRegister: u8 {
        const SPRITE_OVERFLOW  = 0b00100000;
        const SPRITE_0_HIT     = 0b01000000;
        const VBLANK_STARTED   = 0b10000000;
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Frame {
    pub data: Box<[u8; Self::HEIGHT * Self::WIDTH * 3]>,
}

impl Frame {
    pub const WIDTH: usize = 256;
    pub const HEIGHT: usize = 240;

    pub fn new() -> Self {
        Self {
            data: Box::new([0; Self::HEIGHT * Self::WIDTH * 3]),
        }
    }

    fn set_pixel(&mut self, x: usize, y: usize, color: (u8, u8, u8)) {
        let index = ((y * Self::WIDTH) + x) * 3;
        // println!("set pixel at {x}, {y}: index {index}, color {color:?}");
        self.data[index] = color.0;
        self.data[index + 1] = color.1;
        self.data[index + 2] = color.2;
    }
}
