use bitfield::bitfield;
use bitflags::{bitflags, Flags};

use crate::{bus::Bus, rom::Mirroring};
use std::{array, mem, ops::RangeInclusive};

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
    bus: PpuBus,
    pub oam_data: [u8; 256],
    secondary_oam: [u8; 32],
    num_found_sprites: usize,
    found_sprite_zero: bool,
    fetched_sprites: [Option<FetchedSprite>; 8],
    status_register: StatusRegister,
    pub oam_addr: u8,
    data_buffer: u8,
    line: usize,
    dot: usize,
    odd_frame: bool,
    pub frame: Frame,
    registers: Registers,
    shift_registers: ShiftRegisters,
}

impl Ppu {
    pub fn new(bus: PpuBus) -> Self {
        const NONE: Option<(usize, RangeInclusive<u8>)> = None;
        Self {
            bus,
            oam_data: [0; 256],
            secondary_oam: [0; 32],
            num_found_sprites: 0,
            found_sprite_zero: false,
            fetched_sprites: [None; 8],
            status_register: StatusRegister::empty(),
            oam_addr: 0,
            data_buffer: 0,
            // Start on pre-render scanline
            line: 261,
            dot: 0,
            odd_frame: false,
            frame: Frame::new(),
            registers: Registers::new(),
            shift_registers: ShiftRegisters::new(),
        }
    }

    fn get_address(&mut self, increment: bool) -> u16 {
        let addr = self.registers.v.addr();
        if increment {
            self.registers
                .v
                .set_addr((addr + self.registers.ctrl.vram_addr_increment() as u16) % 0x4000);
        }
        addr
    }

    pub fn run_cycle(&mut self) {
        // println!(
        //     "PPU CYCLE START: line {:>3}, dot {:>3}",
        //     self.line, self.dot
        // );
        // TODO: Sprite overflow flag
        match (self.line, self.dot) {
            (241, 1) => {
                // println!("started vblank");
                self.status_register.insert(StatusRegister::VBLANK_STARTED);
            }
            (261, 1) => {
                // println!("ended vblank");
                self.status_register.remove(
                    StatusRegister::VBLANK_STARTED
                        | StatusRegister::SPRITE_OVERFLOW
                        | StatusRegister::SPRITE_0_HIT,
                );
            }
            _ => {}
        }

        // if (0..=239).contains(&self.line) && (1..=256).contains(&self.dot) {
        //     let mut pixel = self
        //         .mask_register
        //         .contains(MaskRegister::BG_ENABLE)
        //         .then(|| self.get_background())
        //         .flatten();
        //     (|| {
        //         if self.mask_register.contains(MaskRegister::SPRITE_ENABLE) {
        //             if let Some(sprite_data) = self.get_sprite_data() {
        //                 if sprite_data.2 && pixel.is_some() && sprite_data.0.is_some() {
        //                     self.status_register.insert(StatusRegister::SPRITE_0_HIT);
        //                 }
        //                 pixel = if sprite_data.1 {
        //                     pixel.or(sprite_data.0)
        //                 } else {
        //                     sprite_data.0.or(pixel)
        //                 }
        //             }
        //         }
        //     })();
        //
        //     self.frame.set_pixel(
        //         self.dot - 1,
        //         self.line,
        //         pixel.unwrap_or_else(|| SYSTEM_PALETTE[self.bus.read(0x3F00) as usize]),
        //     );
        // }

        let background_pixel =
            self.shift_registers
                .cycle(self.line, self.dot, &mut self.registers, &mut self.bus);
        let sprite_pixel = self.run_sprite_cycle();
        self.registers.cycle(self.line, self.dot);

        if let Some((palette_index, color_index)) = background_pixel {
            let background_color = self.get_color(true, palette_index, color_index);
            let color = match (background_color, sprite_pixel) {
                (Some(background_color), Some((sprite, sprite_color))) => {
                    if sprite.is_zero {
                        self.status_register.insert(StatusRegister::SPRITE_0_HIT);
                    }
                    if sprite.attributes.contains(SpriteAttributes::PRIORITY) {
                        Some(background_color)
                    } else {
                        Some(sprite_color)
                    }
                }
                (Some(background_color), None) => Some(background_color),
                (None, Some((_, sprite_color))) => Some(sprite_color),
                (None, None) => None,
            };

            self.frame.set_pixel(
                self.dot - 1,
                self.line,
                color.unwrap_or_else(|| SYSTEM_PALETTE[self.bus.read(0x3F00) as usize]),
            );
        }

        // Increment
        self.dot += 1;
        if self.dot == 341 {
            self.dot = 0;
            self.line += 1;
            if self.line == 262 {
                self.line = 0;
                self.odd_frame = !self.odd_frame;
                if self.odd_frame {
                    self.dot += 1;
                }
            }
        }
    }

    fn run_sprite_cycle(&mut self) -> Option<(FetchedSprite, (u8, u8, u8))> {
        // Only run sprite evaluation during visible scanlines
        if !matches!((self.line, self.dot), (0..=239, _)) {
            return None;
        }

        match self.dot {
            1 => {
                self.num_found_sprites = 0;
                self.found_sprite_zero = false;
            }
            // Clear secondary OAM
            2..=64 if self.dot % 2 == 0 => {
                self.secondary_oam[self.dot / 2 - 1] = 0xFF;
            }
            // Sprite evaluation (this happens over dots 65..=256 but it should be accurate enough
            // to just run it on dot 65)
            65 => self.get_line_sprites(),
            257 => {
                self.fetched_sprites = [None; 8];
            }
            258..=320 => {
                let sprite_num = (self.dot - 257) / 8;
                if sprite_num < self.num_found_sprites {
                    let sprite_cycle = (self.dot - 257) % 8;
                    // println!("sprite_num: {sprite_num}, sprite_cycle: {sprite_cycle}");
                    match sprite_cycle {
                        1 => {
                            // Garbage nametable fetch
                        }
                        2 => {
                            self.fetched_sprites[sprite_num] = Some(FetchedSprite {
                                is_zero: sprite_num == 0 && self.found_sprite_zero,
                                attributes: SpriteAttributes::from_bits_truncate(
                                    self.secondary_oam[sprite_num * 4 + 2],
                                ),
                                x: 0,
                                pattern: [0; 2],
                            });
                        }
                        3 => {
                            if let Some(sprite) = self.fetched_sprites[sprite_num].as_mut() {
                                sprite.x = self.secondary_oam[sprite_num * 4 + 3];
                            }
                        }
                        5 | 7 => {
                            if let Some(sprite) = self.fetched_sprites[sprite_num].as_mut() {
                                let is_high = sprite_cycle == 7;
                                let v_flip = sprite.attributes.contains(SpriteAttributes::V_FLIP);
                                let mut y =
                                    (self.line as u8 - self.secondary_oam[sprite_num * 4]) % 8;
                                let mut tile_number = self.secondary_oam[sprite_num * 4 + 1];
                                let right_page =
                                    if self.registers.ctrl.contains(ControlRegister::SPRITE_SIZE) {
                                        if (y > 7) ^ v_flip {
                                            tile_number |= 1;
                                            true
                                        } else {
                                            tile_number &= !1;
                                            false
                                        }
                                    } else {
                                        self.registers
                                            .ctrl
                                            .contains(ControlRegister::SPRITE_PATTERN_ADDR)
                                    };
                                if v_flip {
                                    y = 7 - y;
                                }
                                let address = y as u16
                                    | (is_high as u16) << 3
                                    | (tile_number as u16) << 4
                                    | (right_page as u16) << 12;
                                let mut pattern = self.bus.read(address);
                                if sprite.attributes.contains(SpriteAttributes::H_FLIP) {
                                    pattern = pattern.reverse_bits();
                                }
                                sprite.pattern[is_high as usize] = pattern;
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }

        if (1..=256).contains(&self.dot) {
            let x = (self.dot - 1) as u8;
            self.fetched_sprites.clone()
                .into_iter()
                .flatten()
                .filter(|sprite| (sprite.x..=sprite.x.saturating_add(7)).contains(&x))
                .find_map(|sprite| {
                    let bit_index = 7 - ((self.dot - 1) as u8 - sprite.x);
                    let bit = 1 << bit_index;
                    self.get_color(
                        false,
                        sprite.attributes.palette_number(),
                        ((sprite.pattern[0] & bit) >> bit_index)
                            | ((sprite.pattern[1] & bit) >> bit_index) << 1,
                        // 3,
                    )
                    .map(|color| (sprite, color))
                })
        } else {
            None
        }
    }

    // fn get_background(&self) -> Option<(u8, u8, u8)> {
    //     let x = self.dot - 1;
    //     let tile_x = (x / 8) as u8;
    //     let tile_y = (self.line / 8) as u8;
    //     let nametable_offset =
    //         self.mirror_vram_addr(self.ctrl_register.nametable_offset()) as usize;
    //
    //     // Fetch tile number from nametable
    //     // println!("{tile_x}, {tile_y}, {:#06X}", (tile_y as usize) * 32 + tile_x as usize + nametable_offset);
    //     let tile_number = self.vram[(tile_y as usize) * 32 + tile_x as usize + nametable_offset];
    //
    //     // Fetch palette number from nametable attribute data
    //     let attribute_table_index = (tile_y / 4 * 8) + (tile_x / 4);
    //     let mut palette_number =
    //         self.vram[attribute_table_index as usize + 0x03C0 + nametable_offset];
    //     if tile_y & 0b10 == 0 {
    //         // Top
    //         palette_number &= 0x0F;
    //     } else {
    //         // Bottom
    //         palette_number >>= 4;
    //     }
    //     if tile_x & 0b10 == 0 {
    //         // Left
    //         palette_number &= 0b11;
    //     } else {
    //         // Right
    //         palette_number >>= 2;
    //     }
    //
    //     // Fetch color index from pattern table
    //     let color_index = self.get_pattern_table_entry(
    //         self.ctrl_register
    //             .contains(ControlRegister::BACKGROUND_PATTERN_ADDR),
    //         tile_number,
    //         (self.line % 8) as u8,
    //         (x % 8) as u8,
    //     );
    //
    //     // Fetch color
    //     self.get_color(true, palette_number, color_index)
    // }

    fn get_line_sprites(&mut self) {
        for i in 0..64 {
            let sprite_y = self.oam_data[i * 4];
            // actually 16 and 8, but we actually need the bottom y, not the size
            let sprite_height = if self.registers.ctrl.contains(ControlRegister::SPRITE_SIZE) {
                15
            } else {
                7
            };

            let range_y = sprite_y..=sprite_y.saturating_add(sprite_height);
            if range_y.contains(&(self.line as u8)) {
                self.secondary_oam[self.num_found_sprites * 4..][..4]
                    .copy_from_slice(&self.oam_data[i * 4..][..4]);
                if i == 0 {
                    self.found_sprite_zero = true;
                }
                self.num_found_sprites += 1;
                if self.num_found_sprites == 8 {
                    break;
                }
            }
        }
    }

    // fn get_sprite_data(&self) -> Option<(Option<(u8, u8, u8)>, bool, bool)> {
    //     let x = (self.dot - 1) as u8;
    //     self.line_sprites
    //         .iter()
    //         .flatten()
    //         .filter(|(_, range_x)| range_x.contains(&x))
    //         .map(|(i, range_x)| {
    //             // I think this should be saturating, NESDev Wiki says a sprite is offscreen if y =
    //             // 0xFF
    //             let sprite_y = self.oam_data[i * 4].saturating_add(1);
    //
    //             // TODO: Tall sprites
    //             let mut tile_number = self.oam_data[i * 4 + 1];
    //
    //             let byte_2 = self.oam_data[i * 4 + 2];
    //             let palette_number = byte_2 & 0b11;
    //             let priority = byte_2 & 0b0010_0000 != 0;
    //             let h_flip = byte_2 & 0b0100_0000 != 0;
    //             let v_flip = byte_2 & 0b1000_0000 != 0;
    //
    //             let right_page = if self.ctrl_register.contains(ControlRegister::SPRITE_SIZE) {
    //                 let right_page = tile_number & 1 != 0;
    //                 if (self.line as u8 - sprite_y > 7) ^ v_flip {
    //                     tile_number |= 1;
    //                 } else {
    //                     tile_number &= !1;
    //                 }
    //                 right_page
    //             } else {
    //                 self.ctrl_register
    //                     .contains(ControlRegister::SPRITE_PATTERN_ADDR)
    //             };
    //
    //             let color_index = self.get_pattern_table_entry(
    //                 right_page,
    //                 tile_number,
    //                 if v_flip {
    //                     7 - (self.line as u8 - sprite_y)
    //                 } else {
    //                     self.line as u8 - sprite_y
    //                 },
    //                 if h_flip {
    //                     7 - (x - range_x.start())
    //                 } else {
    //                     x - range_x.start()
    //                 },
    //             );
    //
    //             (
    //                 self.get_color(false, palette_number, color_index),
    //                 priority,
    //                 *i == 0,
    //             )
    //         })
    //         .reduce(|front, back| stack_colors(back, front))
    // }

    fn get_color(
        &mut self,
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
        let system_palette_index = self.bus.read(0x3F00 + index as u16);
        Some(SYSTEM_PALETTE[system_palette_index as usize])
    }

    pub fn is_vblank(&self) -> bool {
        self.status_register
            .contains(StatusRegister::VBLANK_STARTED)
    }

    pub fn check_nmi(&mut self) -> bool {
        self.is_vblank() && self.registers.ctrl.contains(ControlRegister::GENERATE_NMI)
    }
}

impl Bus for Ppu {
    fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("attempt to read from write-only PPU register: {addr:#06X}")
            }
            0x2002 => {
                self.registers.w = false;
                self.status_register.bits()
            }
            0x2004 => self.oam_data[self.oam_addr as usize],
            0x2007 => {
                let ppu_addr = self.get_address(true);
                match ppu_addr {
                    0x0000..=0x3EFF => mem::replace(&mut self.data_buffer, self.bus.read(ppu_addr)),
                    _ => self.bus.read(ppu_addr),
                }
            }
            0x2008..=0x3FFF => self.read(addr % 0x8 + 0x2000),
            _ => panic!("address {addr:#06X} is out of PPU range"),
        }
    }

    fn write(&mut self, addr: u16, data: u8) {
        match addr {
            0x2000 => {
                self.registers.write_ctrl(data);
            }
            0x2001 => self.registers.mask = MaskRegister::from_bits_truncate(data),
            0x2002 => panic!("attempt to write to read-only PPU status register (0x2002)"),
            0x2003 => self.oam_addr = data,
            0x2004 => self.oam_data[self.oam_addr as usize] = data,
            0x2005 => {
                self.registers.write_scroll(data);
            }
            0x2006 => {
                self.registers.write_addr(data);
            }
            0x2007 => {
                let ppu_addr = self.get_address(true);
                self.bus.write(ppu_addr, data);
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

pub struct PpuBus {
    chr_rom: Box<[u8]>,
    vram: [u8; 2048],
    palette_table: [u8; 32],
    mirroring: Mirroring,
}

impl PpuBus {
    pub fn new(chr_rom: Box<[u8]>, mirroring: Mirroring) -> Self {
        Self {
            chr_rom,
            vram: [0; 2048],
            palette_table: [0; 32],
            mirroring,
        }
    }
}

impl Bus for PpuBus {
    fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.chr_rom[addr as usize],
            0x2000..=0x2FFF | 0x3000..=0x3EFF => {
                // Some sources say 0x3000..=0x3EFF are unused, others say they mirror
                // VRAM. I'm guessing it probably depends on the mapper.
                let vram_addr = mirror_vram_addr(addr, self.mirroring);
                self.vram[vram_addr as usize]
            }
            0x3F00..=0x3FFF => self.palette_table[(addr % 0x20) as usize],
            _ => panic!("unexpected access to PPU address {addr:#06X}"),
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
            0x3F00..=0x3FFF => self.palette_table[(addr % 0x20) as usize] = data,
            _ => panic!("unexpected access to PPU addr {addr:#06X}"),
        }
    }

    fn peek(&self, _addr: u16) -> Option<u8> {
        // TODO: Implement this for better tracing and debuggability
        None
    }
}

// TODO: possibly do these with the bitfield crate
struct Registers {
    ctrl: ControlRegister,
    mask: MaskRegister,
    v: VTRegister,
    t: VTRegister,
    x: u8,
    w: bool,
}

impl Registers {
    const HORIZONTAL_BITS: u16 = 0b0000100_00011111;

    fn new() -> Self {
        Self {
            ctrl: ControlRegister::empty(),
            mask: MaskRegister::empty(),
            v: VTRegister(0),
            t: VTRegister(0),
            x: 0,
            w: false,
        }
    }

    fn write_ctrl(&mut self, data: u8) {
        self.t.set_nt_select(data as u16 & 0b11);
        self.ctrl = ControlRegister::from_bits_truncate(data);
        // println!(
        //     "write ctrl,             data: {data:#010b}, new t: {:?}",
        //     self.t
        // );
    }

    fn write_scroll(&mut self, data: u8) {
        if self.w {
            self.t.set_fine_y(data as u16 & 0b0000_0111);
            self.t.set_coarse_y(data as u16 & 0b1111_1000);
            self.w = false;
        } else {
            self.t.set_coarse_x(data as u16 >> 3);
            self.x = data & 0b111;
            self.w = true;
        }
        // println!(
        //     "write scroll, y: {:<5}, data: {data:#010b}, new t: {:?}",
        //     self.w, self.t
        // );
    }

    fn write_addr(&mut self, data: u8) {
        // println!("write addr, low: {}, data: {data:#04X}", self.w);
        if self.w {
            self.t.set_addr_low(data as u16);
            self.v = self.t;
            self.w = false;
        } else {
            self.t.set_addr_high(data as u16 & 0b0011_1111);
            self.t.0 &= 0b10111111_11111111;
            self.w = true;
        }
        // println!(
        //     "write addr, low: {:<5}, data: {data:#010b}, new t: {:?}\n                                    new v: {:?}",
        //     self.w, self.t, self.v
        // );
    }

    fn inc_coarse_x(&mut self) {
        // println!("inc coarse x");
        // If coarse x would overflow, swap the horizontal nametable
        if self.v.coarse_x() == 31 {
            // println!("coarse x overflow");
            self.v.set_coarse_x(0);
            self.v.set_nt_select(self.v.nt_select() ^ 0b01);
        } else {
            self.v.set_coarse_x(self.v.coarse_x() + 1);
        }
        // println!(
        //     "inc coarse x,                             new v: {:?}",
        //     self.v
        // );
    }

    fn inc_y(&mut self) {
        // println!("increment fine y");
        // If fine y would overflow
        if self.v.fine_y() == 7 {
            // println!("increment coarse y");
            // Clear fine y
            self.v.set_fine_y(0);
            let coarse_y = self.v.coarse_y();
            // println!("coarse y: {coarse_y}");
            if coarse_y == 29 {
                // println!("coarse y overflow");
                // Clear coarse y and swap vertical nametable
                self.v.set_coarse_y(0);
                self.v.set_nt_select(self.v.nt_select() ^ 0b10);
            } else if coarse_y == 31 {
                // println!("coarse y extra overflow");
                // Coarse y can be out of bounds. In that case, it will wrap to 0, but not swap the
                // vertical nametable
                self.v.set_coarse_y(0);
            } else {
                // Increment coarse y as normal
                self.v.set_coarse_y(self.v.coarse_y() + 1);
            }
            // println!("new coarse y: {:#010b}", (self.v >> 5) & 0b11111);
        } else {
            // Increment fine y
            self.v.set_fine_y(self.v.fine_y() + 1);
        }
    }

    fn cycle(&mut self, line: usize, dot: usize) {
        if !self
            .mask
            .intersects(MaskRegister::BG_ENABLE | MaskRegister::SPRITE_ENABLE)
        {
            return;
        }
        match (line, dot) {
            (261, 280..=304) => {
                // Copy vertical components of t to v
                self.v.set_fine_y(self.t.fine_y());
                self.v.set_coarse_y(self.t.coarse_y());
                self.v.set_nt_y_select(self.t.nt_y_select());
                return;
            }
            (0..=239 | 261, 1..=257 | 321..=336) => {}
            _ => return,
        }

        if dot % 8 == 0 {
            self.inc_coarse_x();
        }

        match dot {
            256 => {
                self.inc_y();
            }
            257 => {
                self.v.set_coarse_x(self.t.coarse_x());
                self.v.set_nt_x_select(self.t.nt_x_select());
            }
            _ => {}
        }
    }
}

bitfield! {
    #[derive(Clone, Copy, PartialEq, Eq)]
    struct VTRegister(u16);
    // impl Debug;
    coarse_x, set_coarse_x: 4, 0;
    coarse_y, set_coarse_y: 9, 5;
    coarse_pos, set_coarse_pos: 9, 0;
    nt_x_select, set_nt_x_select: 10, 10;
    nt_y_select, set_nt_y_select: 11, 11;
    nt_select, set_nt_select: 11, 10;
    fine_y, set_fine_y: 14, 12;
    addr, set_addr: 13, 0;
    addr_low, set_addr_low: 7, 0;
    addr_high, set_addr_high: 13, 8;
}

impl std::fmt::Debug for VTRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VTRegister")
            .field("coarse_x", &format_args!("{:>2}", self.coarse_x()))
            .field("coarse_y", &format_args!("{:>2}", self.coarse_y()))
            .field("fine_y", &format_args!("{}", self.fine_y()))
            .field("nt_select", &format_args!("{:#04b}", self.nt_select()))
            .field("addr", &format_args!("{:#06X}", self.addr()))
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq)]
struct ShiftRegisters {
    tile_number: u8,
    pattern_data: [u8; 2],
    pattern_shift_registers: [u16; 2],
    attribute_data: [bool; 2],
    attribute_latches: [bool; 2],
    attribute_shift_registers: [u8; 2],
}

impl std::fmt::Debug for ShiftRegisters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ShiftRegisters")
            .field("tile_number", &format_args!("{:#04X}", self.tile_number))
            .field(
                "pattern_data",
                &format_args!(
                    "[{:#04X}, {:#04X}]",
                    self.pattern_data[0], self.pattern_data[1]
                ),
            )
            .field(
                "pattern_shift_registers",
                &format_args!(
                    "[{:#018b}, {:#018b}]",
                    self.pattern_shift_registers[0], self.pattern_shift_registers[1]
                ),
            )
            .field(
                "attribute_shift_registers",
                &format_args!(
                    "[{:#010b}, {:#010b}]",
                    self.attribute_shift_registers[0], self.attribute_shift_registers[1]
                ),
            )
            .field(
                "attribute_latches",
                &format_args!(
                    "[{:01b}, {:01b}]",
                    self.attribute_latches[0] as u8, self.attribute_latches[1] as u8
                ),
            )
            .field("attribute_data", &format_args!("{:?}", self.attribute_data))
            .finish()
    }
}

impl ShiftRegisters {
    fn new() -> Self {
        Self {
            tile_number: 0,
            pattern_data: [0; 2],
            pattern_shift_registers: [0; 2],
            attribute_data: [false; 2],
            attribute_latches: [false; 2],
            attribute_shift_registers: [0; 2],
        }
    }

    fn cycle(
        &mut self,
        line: usize,
        dot: usize,
        registers: &mut Registers,
        bus: &mut PpuBus,
    ) -> Option<(u8, u8)> {
        if (240..=260).contains(&line) || !matches!(dot, 1..=256 | 321..=336) {
            return None;
        }

        // Load tile data into buffers
        match dot % 8 {
            2 => {
                self.tile_number = bus.read(0x2000 | registers.v.coarse_pos());
            }
            4 => {
                let address = 0x23C0
                    | (registers.v.nt_select() << 10)
                    | ((registers.v.coarse_y() >> 2) << 3)
                    | (registers.v.coarse_x() >> 2);
                let attribute_byte = bus.read(address);
                let attribute_index = (((registers.v.coarse_y() & 0b10) >> 0)
                    | ((registers.v.coarse_x() & 0b10) >> 1))
                    * 2;
                for i in 0..=1 {
                    self.attribute_data[i] =
                        attribute_byte & (1 << (attribute_index + i as u16)) != 0;
                }
            }
            6 | 0 => {
                let is_high_byte = dot % 8 == 0;
                let is_right_page = registers
                    .ctrl
                    .contains(ControlRegister::BACKGROUND_PATTERN_ADDR);
                let pattern_table_address = 0
                    | (is_right_page as u16) << 12
                    | (self.tile_number as u16) << 4
                    | (is_high_byte as u16) << 3
                    | (registers.v.fine_y());
                self.pattern_data[is_high_byte as usize] = bus.read(pattern_table_address);
            }
            _ => {}
        }

        for i in 0..=1 {
            self.attribute_shift_registers[i] <<= 1;
            self.attribute_shift_registers[i] |= self.attribute_latches[i] as u8;

            self.pattern_shift_registers[i] <<= 1;
        }

        if dot % 8 == 1 {
            self.attribute_latches = self.attribute_data;
            for i in 0..=1 {
                self.pattern_shift_registers[i] &= 0xFF00;
                self.pattern_shift_registers[i] |= self.pattern_data[i] as u16;
            }
        }

        let bit_index = 7 - registers.x;
        let bit = 1 << bit_index;
        let palette_index = ((self.attribute_shift_registers[0] & bit) >> bit_index)
            | ((self.attribute_shift_registers[1] & bit) >> bit_index) << 1;
        let bit = (bit as u16) << 8;
        let color_index = (((self.pattern_shift_registers[0] & bit) >> (bit_index + 8))
            | ((self.pattern_shift_registers[1] & bit) >> (bit_index + 7)))
            as u8;

        matches!((line, dot), (0..=239, 1..=256)).then_some((palette_index, color_index))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FetchedSprite {
    is_zero: bool,
    attributes: SpriteAttributes,
    x: u8,
    pattern: [u8; 2],
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct SpriteAttributes: u8 {
        const PALETTE_LOW   = 0b00000001;
        const PALETTE_HIGH  = 0b00000010;
        const PRIORITY      = 0b00100000;
        const H_FLIP        = 0b01000000;
        const V_FLIP        = 0b10000000;
    }
}

impl SpriteAttributes {
    fn palette_number(&self) -> u8 {
        self.bits() & 0b11
    }
}

fn mirror_vram_addr(addr: u16, mirroring: Mirroring) -> u16 {
    let vram_index = addr & 0xFFF;
    let offset = match mirroring {
        Mirroring::Vertical => [0, 0, 0x800, 0x800],
        Mirroring::Horizontal => [0, 0x400, 0x400, 0x800],
        _ => [0; 4],
    };
    vram_index - offset[(vram_index / 0x400) as usize]
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct ControlRegister: u8 {
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
