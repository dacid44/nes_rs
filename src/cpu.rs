#[cfg(test)]
mod tom_harte_cpu_tests;

use std::{fmt::Debug, ops::ControlFlow};

use bitflags::bitflags;
use enum_map::{enum_map, EnumMap};
use paste::paste;

use crate::bus::{Bus, CpuBus, FlatRam};

const MEMORY_SIZE: usize = 0x10000;
// pub const PROGRAM_START: u16 = 0x8000;
// pub const PROGRAM_START: u16 = 0x0600;
pub const PROGRAM_START: u16 = 0xC000;
pub const PROGRAM_START_LOC: u16 = 0xFFFC;
const STACK_OFFSET: u16 = 0x0100;
const STACK_INIT: u8 = 0xFF;

macro_rules! opcodes {
    ( $( $opcode:literal => $name:ident, $mode:ident $(,, $handling:path )? $(, $break:ident )?; )+ ) => {
        {
            let mut opcodes = [None; 256];
            $(
                opcodes[$opcode] = Some(Opcode {
                    instruction: paste! { Cpu::[<$name:lower>] },
                    name: stringify!($name),
                    mode: paste! { AddressingMode::[<$mode>] },
                    handling: opcodes! { [<handling $($handling)?>] },
                    control_flow: opcodes! { [<control_flow $($break)?>] },
                });
            )+

            EnumMap::from_array(opcodes)
        }
    };
    ( [<control_flow>] ) => { ControlFlow::Continue(()) };
    ( [<control_flow break>] ) => { ControlFlow::Break(()) };
    ( [<handling>] ) => { None };
    ( [<handling $handling:path>] ) => { Some($handling) };
}

#[derive(Clone, PartialEq)]
pub struct Cpu<B> {
    pub accumulator: u8,
    pub index_x: u8,
    pub index_y: u8,
    pub stack_pointer: u8,
    pub status: StatusFlags,
    pub program_counter: u16,
    pub bus: B,
    current_instruction: Option<(Opcode<B>, u8)>,
    fetched_addr: Option<u16>,
    addr_cache: [u8; 3],
    data_cache: u8,
    buffered_interrupts: Interrupts,
    interrupt: bool,
    reset_state: ResetState,
}

impl<B> Debug for Cpu<B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cpu")
            .field("accumulator", &format_args!("{:#4X}", self.accumulator))
            .field("index_x", &format_args!("{:#4X}", self.index_x))
            .field("index_y", &format_args!("{:#4X}", self.index_y))
            .field("stack_pointer", &format_args!("{:#4X}", self.stack_pointer))
            .field("status", &self.status)
            .field(
                "program_counter",
                &format_args!("{:#6X}", self.program_counter),
            )
            .finish()
    }
}

impl Cpu<FlatRam> {
    pub fn load_and_run(&mut self, program: &[u8]) {
        self.load(program);
        self.reset();
        self.run()
    }

    pub fn load(&mut self, program: &[u8]) {
        self.bus.load(program)
    }
}

impl<B: CpuBus> Cpu<B> {
    const OPCODES: EnumMap<u8, Option<Opcode<B>>> = opcodes! {
        0x69 => ADC, Immediate;
        0x65 => ADC, ZeroPage;
        0x75 => ADC, ZeroPageX;
        0x6D => ADC, Absolute;
        0x7D => ADC, AbsoluteX;
        0x79 => ADC, AbsoluteY;
        0x61 => ADC, IndexedIndirectX;
        0x71 => ADC, IndirectIndexedY;

        0x29 => AND, Immediate;
        0x25 => AND, ZeroPage;
        0x35 => AND, ZeroPageX;
        0x2D => AND, Absolute;
        0x3D => AND, AbsoluteX;
        0x39 => AND, AbsoluteY;
        0x21 => AND, IndexedIndirectX;
        0x31 => AND, IndirectIndexedY;

        0x0A => ASL, Accumulator;
        0x06 => ASL, ZeroPage;
        0x16 => ASL, ZeroPageX;
        0x0E => ASL, Absolute;
        0x1E => ASL, AbsoluteX,, Handling::Modify;

        0x90 => BCC, Relative;

        0xB0 => BCS, Relative;

        0xF0 => BEQ, Relative;

        0x24 => BIT, ZeroPage;
        0x2C => BIT, Absolute;

        0x30 => BMI, Relative;

        0xD0 => BNE, Relative;

        0x10 => BPL, Relative;

        0x00 => BRK, Implied/*, break*/;

        0x50 => BVC, Relative;

        0x70 => BVS, Relative;

        0x18 => CLC, Implied;

        0xD8 => CLD, Implied;

        0x58 => CLI, Implied;

        0xB8 => CLV, Implied;

        0xC9 => CMP, Immediate;
        0xC5 => CMP, ZeroPage;
        0xD5 => CMP, ZeroPageX;
        0xCD => CMP, Absolute;
        0xDD => CMP, AbsoluteX;
        0xD9 => CMP, AbsoluteY;
        0xC1 => CMP, IndexedIndirectX;
        0xD1 => CMP, IndirectIndexedY;

        0xE0 => CPX, Immediate;
        0xE4 => CPX, ZeroPage;
        0xEC => CPX, Absolute;

        0xC0 => CPY, Immediate;
        0xC4 => CPY, ZeroPage;
        0xCC => CPY, Absolute;

        0xC6 => DEC, ZeroPage;
        0xD6 => DEC, ZeroPageX;
        0xCE => DEC, Absolute;
        0xDE => DEC, AbsoluteX,, Handling::Modify;

        0xCA => DEX, Implied;

        0x88 => DEY, Implied;

        0x49 => EOR, Immediate;
        0x45 => EOR, ZeroPage;
        0x55 => EOR, ZeroPageX;
        0x4D => EOR, Absolute;
        0x5D => EOR, AbsoluteX;
        0x59 => EOR, AbsoluteY;
        0x41 => EOR, IndexedIndirectX;
        0x51 => EOR, IndirectIndexedY;

        0xE6 => INC, ZeroPage;
        0xF6 => INC, ZeroPageX;
        0xEE => INC, Absolute;
        0xFE => INC, AbsoluteX,, Handling::Modify;

        0xE8 => INX, Implied;

        0xC8 => INY, Implied;

        // Compatibility note at https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
        0x4C => JMP, Absolute,, Handling::Jmp;
        0x6C => JMP, Indirect;

        0x20 => JSR, Absolute,, Handling::Jsr;

        0xA9 => LDA, Immediate;
        0xA5 => LDA, ZeroPage;
        0xB5 => LDA, ZeroPageX;
        0xAD => LDA, Absolute;
        0xBD => LDA, AbsoluteX;
        0xB9 => LDA, AbsoluteY;
        0xA1 => LDA, IndexedIndirectX;
        0xB1 => LDA, IndirectIndexedY;

        0xA2 => LDX, Immediate;
        0xA6 => LDX, ZeroPage;
        0xB6 => LDX, ZeroPageY;
        0xAE => LDX, Absolute;
        0xBE => LDX, AbsoluteY;

        0xA0 => LDY, Immediate;
        0xA4 => LDY, ZeroPage;
        0xB4 => LDY, ZeroPageX;
        0xAC => LDY, Absolute;
        0xBC => LDY, AbsoluteX;

        0x4A => LSR, Accumulator;
        0x46 => LSR, ZeroPage;
        0x56 => LSR, ZeroPageX;
        0x4E => LSR, Absolute;
        0x5E => LSR, AbsoluteX,, Handling::Modify;

        0xEA => NOP, Implied;

        0x09 => ORA, Immediate;
        0x05 => ORA, ZeroPage;
        0x15 => ORA, ZeroPageX;
        0x0D => ORA, Absolute;
        0x1D => ORA, AbsoluteX;
        0x19 => ORA, AbsoluteY;
        0x01 => ORA, IndexedIndirectX;
        0x11 => ORA, IndirectIndexedY;

        0x48 => PHA, Implied;

        0x08 => PHP, Implied;

        0x68 => PLA, Implied;

        0x28 => PLP, Implied;

        0x2A => ROL, Accumulator;
        0x26 => ROL, ZeroPage;
        0x36 => ROL, ZeroPageX;
        0x2E => ROL, Absolute;
        0x3E => ROL, AbsoluteX,, Handling::Modify;

        0x6A => ROR, Accumulator;
        0x66 => ROR, ZeroPage;
        0x76 => ROR, ZeroPageX;
        0x6E => ROR, Absolute;
        0x7E => ROR, AbsoluteX,, Handling::Modify;

        0x40 => RTI, Implied;

        0x60 => RTS, Implied;

        0xE9 => SBC, Immediate;
        0xE5 => SBC, ZeroPage;
        0xF5 => SBC, ZeroPageX;
        0xED => SBC, Absolute;
        0xFD => SBC, AbsoluteX;
        0xF9 => SBC, AbsoluteY;
        0xE1 => SBC, IndexedIndirectX;
        0xF1 => SBC, IndirectIndexedY;

        0x38 => SEC, Implied;

        0xF8 => SED, Implied;

        0x78 => SEI, Implied;

        0x85 => STA, ZeroPage;
        0x95 => STA, ZeroPageX;
        0x8D => STA, Absolute;
        0x9D => STA, AbsoluteX,, Handling::Store;
        0x99 => STA, AbsoluteY,, Handling::Store;
        0x81 => STA, IndexedIndirectX;
        0x91 => STA, IndirectIndexedY,, Handling::Store;

        0x86 => STX, ZeroPage;
        0x96 => STX, ZeroPageY;
        0x8E => STX, Absolute;

        0x84 => STY, ZeroPage;
        0x94 => STY, ZeroPageX;
        0x8C => STY, Absolute;

        0xAA => TAX, Implied;

        0xA8 => TAY, Implied;

        0xBA => TSX, Implied;

        0x8A => TXA, Implied;

        0x9A => TXS, Implied;

        0x98 => TYA, Implied;
    };

    pub fn new(bus: B) -> Self {
        Cpu {
            accumulator: 0,
            index_x: 0,
            index_y: 0,
            stack_pointer: 0,
            status: StatusFlags::empty(),
            program_counter: 0,
            bus,
            current_instruction: None,
            fetched_addr: None,
            addr_cache: [0; 3],
            data_cache: 0,
            buffered_interrupts: Interrupts::empty(),
            interrupt: false,
            reset_state: ResetState::Clear,
        }
    }

    pub fn state_string(&self) -> String {
        format!(
            "PC:  {:#06X} X: {:#04X}\nACC: {:#04X}   Y: {:#04X}\nSTACK: {:02X?}\nNO1BDIZC\n{:08b}",
            self.program_counter,
            self.index_x,
            self.accumulator,
            self.index_y,
            // if STACK_INIT - self.stack_pointer > 16 {
            //     &self.memory[STACK_OFFSET as usize + self.stack_pointer as usize + 1
            //         ..=STACK_OFFSET as usize + self.stack_pointer as usize + 16]
            // } else {
            //     &self.memory[STACK_OFFSET as usize + self.stack_pointer as usize + 1
            //         ..=STACK_OFFSET as usize + STACK_INIT as usize]
            // },
            [] as [u8; 0],
            self.status.bits(),
        )
    }

    fn format_next_opcode(&self) -> (String, String) {
        // TODO: Maybe remove some of the closures, using peek() these don't necessarily need to be
        // lazy anymore
        let code = self.bus.peek(self.program_counter).unwrap_or_default();
        let opcode = Self::OPCODES[code].expect("attempted to format an invalid opcode");
        let low = || self.bus.peek(self.program_counter.wrapping_add(1)).unwrap_or_default();
        let high = || self.bus.peek(self.program_counter.wrapping_add(2)).unwrap_or_default();
        let addr = || u16::from_le_bytes([low(), high()]);
        let one_byte = || format!("{code:02X}");
        let two_bytes = || format!("{code:02X} {:02X}", low());
        let three_bytes = || format!("{code:02X} {:02X} {:02X}", low(), high());
        let (raw, asm) = match opcode.mode {
            AddressingMode::Immediate => (two_bytes(), format!("#${:02X}", low())),
            AddressingMode::Accumulator => (one_byte(), "A".to_string()),
            AddressingMode::ZeroPage => (
                two_bytes(),
                format!(
                    "${:02X} = {:02X}",
                    low(),
                    self.bus.peek(low() as u16).unwrap_or_default()
                ),
            ),
            AddressingMode::ZeroPageX => (two_bytes(), {
                let addr_x = low().wrapping_add(self.index_x);
                format!(
                    "${:02X},X @ {:02X} = {:02X}",
                    low(),
                    addr_x,
                    self.bus.peek(addr_x as u16).unwrap_or_default(),
                )
            }),
            AddressingMode::ZeroPageY => (two_bytes(), {
                let addr_y = low().wrapping_add(self.index_y);
                format!(
                    "${:02X},Y @ {:02X} = {:02X}",
                    low(),
                    addr_y,
                    self.bus.peek(addr_y as u16).unwrap_or_default(),
                )
            }),
            AddressingMode::Relative => (
                two_bytes(),
                format!(
                    "${:04X}",
                    self.program_counter
                        .wrapping_add(2)
                        .wrapping_add_signed(low() as i8 as i16)
                ),
            ),
            AddressingMode::Absolute if opcode.name == "JMP" || opcode.name == "JSR" => {
                (three_bytes(), format!("${:04X}", addr()))
            }
            AddressingMode::Absolute => (
                three_bytes(),
                format!("${:04X} = {:02X}", addr(), self.bus.peek(addr()).unwrap_or_default()),
            ),
            AddressingMode::AbsoluteX => (three_bytes(), {
                let addr_x = addr().wrapping_add(self.index_x as u16);
                format!(
                    "${:04X},X @ {:04X} = {:02X}",
                    addr(),
                    addr_x,
                    self.bus.peek(addr_x).unwrap_or_default()
                )
            }),
            AddressingMode::AbsoluteY => (three_bytes(), {
                let addr_y = addr().wrapping_add(self.index_y as u16);
                format!(
                    "${:04X},Y @ {:04X} = {:02X}",
                    addr(),
                    addr_y,
                    self.bus.peek(addr_y).unwrap_or_default()
                )
            }),
            AddressingMode::Indirect => (three_bytes(), {
                let ptr_low = addr();
                let ptr_high = (ptr_low & 0xFF00) | (ptr_low.wrapping_add(1) & 0x00FF);
                format!(
                    "(${:04X}) = {:04X}",
                    addr(),
                    u16::from_le_bytes([
                        self.bus.peek(ptr_low).unwrap_or_default(),
                        self.bus.peek(ptr_high).unwrap_or_default()
                    ])
                )
            }),
            AddressingMode::IndexedIndirectX => (two_bytes(), {
                let ptr = low().wrapping_add(self.index_x);
                let addr = u16::from_le_bytes([
                    self.bus.peek(ptr as u16).unwrap_or_default(),
                    self.bus.peek(ptr.wrapping_add(1) as u16).unwrap_or_default(),
                ]);
                format!(
                    "(${:02X},X) @ {:02X} = {:04X} = {:02X}",
                    low(),
                    ptr,
                    addr,
                    self.bus.peek(addr).unwrap_or_default(),
                )
            }),
            AddressingMode::IndirectIndexedY => (two_bytes(), {
                let addr = u16::from_le_bytes([
                    self.bus.peek(low() as u16).unwrap_or_default(),
                    self.bus.peek(low().wrapping_add(1) as u16).unwrap_or_default(),
                ]);
                let addr_y = addr.wrapping_add(self.index_y as u16);
                format!(
                    "(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                    low(),
                    addr,
                    addr_y,
                    self.bus.peek(addr_y).unwrap_or_default()
                )
            }),
            AddressingMode::Implied => (one_byte(), String::new()),
        };

        (raw, format!("{} {}", opcode.name, asm))
    }

    pub fn trace(&self) -> String {
        let (raw, asm) = self.format_next_opcode();
        format!(
            "{:04X}  {: <8}  {: <30}  A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.program_counter,
            raw,
            asm,
            self.accumulator,
            self.index_x,
            self.index_y,
            self.status.bits(),
            self.stack_pointer
        )
    }

    pub fn reset(&mut self) {
        // self.accumulator = 0;
        // self.index_x = 0;
        // self.index_y = 0;
        // self.stack_pointer = STACK_INIT;
        // self.status = StatusFlags::_ONE | StatusFlags::INTERRUPT_DISABLE;
        //
        // self.program_counter = u16::from_le_bytes([
        //     self.mem_read(PROGRAM_START_LOC),
        //     self.mem_read(PROGRAM_START_LOC.wrapping_add(1)),
        // ])
        self.reset_state = ResetState::Waiting;
    }

    pub fn run(&mut self) {
        loop {
            if self.run_instruction().is_break() {
                return;
            };
        }
    }

    pub fn run_instruction(&mut self) -> ControlFlow<()> {
        // println!("{}", self.trace());
        // let opcode = self.get_pc_byte();
        // println!(
        //     "pc: {:#06X}, opcode: {opcode:#04X}",
        //     self.program_counter.wrapping_sub(1),
        // );

        // let control_flow = if let Some(opcode) = Self::OPCODES[opcode] {
        //     opcode.execute(self, 0);
        //     opcode.control_flow
        // } else {
        //     todo!("opcode {opcode:#04X}");
        // };

        // println!("{}\n", self.state_string());
        // control_flow

        self.run_cycle();
        let Some((opcode, _)) = self.current_instruction else {
            return ControlFlow::Break(());
        };
        while self.current_instruction.is_some() {
            self.run_cycle();
        }
        opcode.control_flow
    }

    pub fn run_cycle(&mut self) {
        // println!("cycle start, current instruction: {:?}", self.current_instruction);
        if self.reset_state == ResetState::Waiting {
            self.current_instruction = None;
        }

        let (opcode, cycle) = match self.current_instruction {
            // Continue instruction in progress
            Some(current_instruction) => current_instruction,
            // Start and set up for new instruction
            None => {
                // println!("{}", self.trace());
                let mut code = self.get_pc_byte();
                if self.reset_state == ResetState::Waiting {
                    // println!("interrupt: RES");
                    self.reset_state = ResetState::Executing;
                    code = 0x00;
                } else if self.interrupt {
                    // If an interrupt is starting, override the opcode with 0x00
                    // println!("interrupt: {:?}", self.buffered_interrupts);
                    code = 0x00;
                    self.program_counter = self.program_counter.wrapping_sub(1);
                }
                if let Some(opcode) = Self::OPCODES[code] {
                    self.current_instruction = Some((opcode, 0));
                    self.fetched_addr = None;
                    self.addr_cache = [0; 3];
                    self.data_cache = 0;
                    (opcode, 0)
                } else {
                    panic!("invalid opcode: {code:#04X}");
                }
            }
        };

        if cycle > 0 {
            self.get_operand_address(opcode, cycle);
            // if let Some(addr) = self.fetched_addr {
            //     println!("fetched address: {addr:#06X}");
            // }
            opcode.execute(self, cycle);
        }

        if let Some((_, cycle)) = &mut self.current_instruction {
            *cycle += 1;
        } else {
            // TODO: branch instructions have slightly different behavior as to when they poll for
            // interrupts
            // Check buffered interrupts
            self.interrupt = self
                .buffered_interrupts
                .intersects(Interrupts::IRQ | Interrupts::NMI);
            // TODO: clear these when we check in cycle 4 or 5 of BRK
        }
        // Buffer new interrupts
        self.buffered_interrupts |= self.bus.check_interrupts();
    }

    pub fn mem_read(&mut self, addr: u16) -> u8 {
        self.bus.read(addr)
    }

    pub fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.write(addr, data)
    }

    fn stack_addr(&self) -> u16 {
        STACK_OFFSET + self.stack_pointer as u16
    }

    fn stack_push(&mut self, data: u8) {
        self.mem_write(self.stack_addr(), data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_pull(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read(self.stack_addr())
    }

    fn stack_push_u16(&mut self, data: u16) {
        let [low, high] = data.to_le_bytes();
        self.stack_push(high);
        self.stack_push(low);
    }

    fn stack_pull_u16(&mut self) -> u16 {
        let low = self.stack_pull();
        let high = self.stack_pull();
        u16::from_le_bytes([low, high])
    }

    fn export_status(&self) -> u8 {
        (self.status | StatusFlags::B | StatusFlags::_ONE).bits()
    }

    fn export_status_interrupt(&self) -> u8 {
        ((self.status | StatusFlags::_ONE) & !StatusFlags::B).bits()
    }

    fn import_status(&mut self, status: u8) {
        // let status_after = StatusFlags::from_bits_retain(status) & !StatusFlags::B;
        // eprintln!("status: {status:02X} {:02X} {:?}", status_after.bits(), status_after);
        self.status = StatusFlags::from_bits_retain(status);
        self.status.insert(StatusFlags::_ONE);
        self.status.remove(StatusFlags::B);
    }

    fn get_pc_byte(&mut self) -> u8 {
        let byte = self.mem_read(self.program_counter);
        self.program_counter = self.program_counter.wrapping_add(1);
        byte
    }

    fn get_operand_address(&mut self, opcode: Opcode<B>, cycle: u8) {
        use AddressingMode as AM;
        // Exceptions:
        //   Absolute
        //   - JSR
        //   AbsoluteX, AbsoluteY
        //   - only read-only operations skip cycle 4 when no page is crossed
        // Checked: Accumulator, Implied, Immediate, ZeroPage(, X, Y), Relative, Absolute(, X, Y)

        let absolute_indexed_cycle_3 = |cpu: &mut Self, index: u8, handling: Option<Handling>| {
            let (result, carry) = cpu.addr_cache[0].overflowing_add(index);
            cpu.addr_cache[0] = result;
            let discard_read_addr = u16::from_le_bytes([cpu.addr_cache[0], cpu.addr_cache[1]]);
            if carry {
                cpu.addr_cache[1] = cpu.addr_cache[1].wrapping_add(1);
            }
            if carry || handling.is_some() {
                // Discard
                cpu.mem_read(discard_read_addr);
            } else {
                cpu.fetched_addr = Some(u16::from_le_bytes([cpu.addr_cache[0], cpu.addr_cache[1]]));
            }
        };

        // Last cycle is not "consumed"
        match (opcode.mode, cycle, opcode.handling) {
            (AM::Accumulator | AM::Implied, 1, _) => {
                // Discard
                self.mem_read(self.program_counter);
            }
            (AM::Immediate, 1, _) => {
                self.fetched_addr = Some(self.program_counter);
                self.program_counter = self.program_counter.wrapping_add(1);
            }
            (
                AM::ZeroPage
                | AM::ZeroPageX
                | AM::ZeroPageY
                | AM::Absolute
                | AM::AbsoluteX
                | AM::AbsoluteY
                | AM::Indirect
                | AM::IndexedIndirectX,
                1,
                _,
            ) => {
                self.addr_cache[0] = self.get_pc_byte();
            }
            (AM::ZeroPage, 2, _) => {
                self.fetched_addr = Some(self.addr_cache[0] as u16);
            }
            (AM::ZeroPageX | AM::ZeroPageY | AM::IndexedIndirectX, 2, _) => {
                // Discard
                self.mem_read(self.addr_cache[0] as u16);
            }
            (AM::ZeroPageX, 3, _) => {
                self.fetched_addr = Some(self.addr_cache[0].wrapping_add(self.index_x) as u16);
            }
            (AM::ZeroPageY, 3, _) => {
                self.fetched_addr = Some(self.addr_cache[0].wrapping_add(self.index_y) as u16);
            }
            (AM::Relative, 1, _) => {
                let offset = self.get_pc_byte() as i8;
                self.fetched_addr = Some(self.program_counter.wrapping_add_signed(offset as i16));
            }
            (AM::Absolute, 2, Some(Handling::Jmp)) => {
                self.fetched_addr =
                    Some(u16::from_le_bytes([self.addr_cache[0], self.get_pc_byte()]));
            }
            (AM::Absolute, 2, Some(Handling::Jsr)) => {}
            (AM::Absolute | AM::AbsoluteX | AM::AbsoluteY | AM::Indirect, 2, _) => {
                self.addr_cache[1] = self.get_pc_byte();
            }
            (AM::AbsoluteX, 3, handling) => {
                absolute_indexed_cycle_3(self, self.index_x, handling);
            }
            (AM::AbsoluteY, 3, handling) | (AM::IndirectIndexedY, 4, handling) => {
                absolute_indexed_cycle_3(self, self.index_y, handling);
            }
            (AM::Absolute, 3, None)
            | (AM::AbsoluteX | AM::AbsoluteY, 4, _)
            | (AM::IndirectIndexedY, 5, _) => {
                self.fetched_addr =
                    Some(u16::from_le_bytes([self.addr_cache[0], self.addr_cache[1]]));
            }
            (AM::Absolute, 5, Some(Handling::Jsr)) => {
                self.fetched_addr =
                    Some(u16::from_le_bytes([self.addr_cache[0], self.get_pc_byte()]));
            }
            (AM::Indirect, 3, _) => {
                self.addr_cache[2] =
                    self.mem_read(u16::from_le_bytes([self.addr_cache[0], self.addr_cache[1]]));
            }
            (AM::Indirect, 4, _) => {
                // https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
                let ptr_low = u16::from_le_bytes([self.addr_cache[0], self.addr_cache[1]]);
                let ptr_high = (ptr_low & 0xFF00) | (ptr_low.wrapping_add(1) & 0x00FF);
                self.fetched_addr = Some(u16::from_le_bytes([
                    self.addr_cache[2],
                    self.mem_read(ptr_high),
                ]));
            }
            (AM::IndexedIndirectX, 3, _) => {
                self.addr_cache[1] =
                    self.mem_read(self.addr_cache[0].wrapping_add(self.index_x) as u16);
            }
            (AM::IndexedIndirectX, 4, _) => {
                self.addr_cache[2] = self.mem_read(
                    self.addr_cache[0]
                        .wrapping_add(self.index_x)
                        .wrapping_add(1) as u16,
                );
            }
            (AM::IndexedIndirectX, 5, _) => {
                self.fetched_addr =
                    Some(u16::from_le_bytes([self.addr_cache[1], self.addr_cache[2]]))
            }
            (AM::IndirectIndexedY, 1, _) => {
                self.addr_cache[2] = self.get_pc_byte();
            }
            (AM::IndirectIndexedY, 2, _) => {
                self.addr_cache[0] = self.mem_read(self.addr_cache[2] as u16);
            }
            (AM::IndirectIndexedY, 3, _) => {
                self.addr_cache[1] = self.mem_read(self.addr_cache[2].wrapping_add(1) as u16);
            }
            _ => {
                // println!("{:?}, {}, {:?}", opcode.mode, cycle, opcode.handling);
            }
        }
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.set(StatusFlags::ZERO, result == 0);
        self.status
            .set(StatusFlags::NEGATIVE, result & 0b1000_0000 != 0);
    }

    fn add_unsigned(&mut self, operand: u8) {
        let carry_in = if self.status.contains(StatusFlags::CARRY) {
            1
        } else {
            0
        };

        // Do add and check for unsigned carry
        let (result, carry1) = self.accumulator.overflowing_add(operand);
        let (result, carry2) = result.overflowing_add(carry_in);

        // Check for unsigned overflow
        let (s_result, overflow1) = (self.accumulator as i8).overflowing_add(operand as i8);
        let overflow2 = s_result.overflowing_add_unsigned(carry_in).1;

        self.status.set(StatusFlags::CARRY, carry1 || carry2);
        self.status
            .set(StatusFlags::OVERFLOW, overflow1 ^ overflow2);

        self.accumulator = result;
        self.update_zero_and_negative_flags(self.accumulator);
        self.current_instruction = None;
    }

    fn branch_if(&mut self, condition: bool, cycle: u8) {
        match (cycle, self.fetched_addr) {
            (1, _) => {
                if !condition {
                    self.current_instruction = None;
                }
            }
            (2, Some(addr)) => {
                // Discard
                self.mem_read(self.program_counter);
                if self.program_counter & 0xFF00 == addr & 0xFF00 {
                    self.program_counter = addr;
                    self.current_instruction = None;
                }
            }
            (3, Some(addr)) => {
                // Discard
                self.mem_read((self.program_counter & 0xFF00) | (addr & 0x00FF));
                self.program_counter = addr;
                self.current_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn compare_mem(&mut self, register: u8) {
        if let Some(addr) = self.fetched_addr {
            let operand = self.mem_read(addr);
            self.status.set(StatusFlags::CARRY, register >= operand);
            self.update_zero_and_negative_flags(register.wrapping_sub(operand));
            self.current_instruction = None;
        }
    }

    fn modify_acc_or_mem<F>(&mut self, mode: AddressingMode, cycle: u8, f: F)
    where
        F: FnOnce(u8) -> (u8, bool),
    {
        use AddressingMode as AM;

        let (result, carry) = match ((mode, cycle), self.fetched_addr) {
            ((AM::Accumulator, 1), _) => {
                let (result, carry) = f(self.accumulator);
                self.accumulator = result;
                (result, carry)
            }
            (
                (AM::ZeroPage, 1) | (AM::ZeroPageX | AM::Absolute, 1..=2) | (AM::AbsoluteX, 1..=3),
                _,
            ) => {
                return;
            }
            (
                (AM::ZeroPage, 2) | (AM::ZeroPageX | AM::Absolute, 3) | (AM::AbsoluteX, 4),
                Some(addr),
            ) => {
                self.data_cache = self.mem_read(addr);
                return;
            }
            (
                (AM::ZeroPage, 3) | (AM::ZeroPageX | AM::Absolute, 4) | (AM::AbsoluteX, 5),
                Some(addr),
            ) => {
                // Spurious write
                self.mem_write(addr, self.data_cache);
                return;
            }
            (
                (AM::ZeroPage, 4) | (AM::ZeroPageX | AM::Absolute, 5) | (AM::AbsoluteX, 6),
                Some(addr),
            ) => {
                let (result, carry) = f(self.data_cache);
                self.mem_write(addr, result);
                (result, carry)
            }
            _ => unreachable!(
                "mode {:?}, cycle {:?}, addr {:?} is not supported",
                mode, cycle, self.fetched_addr
            ),
        };
        self.status.set(StatusFlags::CARRY, carry);
        self.update_zero_and_negative_flags(result);
        self.current_instruction = None;
    }

    fn adc(&mut self, _mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            let operand = self.mem_read(addr);
            self.add_unsigned(operand);
            self.current_instruction = None;
        }
    }

    fn and(&mut self, _mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            self.accumulator &= self.mem_read(addr);
            self.update_zero_and_negative_flags(self.accumulator);
            self.current_instruction = None;
        }
    }

    fn asl(&mut self, mode: AddressingMode, cycle: u8) {
        self.modify_acc_or_mem(mode, cycle, |operand| operand.overflowing_mul(2))
    }

    fn bcc(&mut self, _mode: AddressingMode, cycle: u8) {
        self.branch_if(!self.status.contains(StatusFlags::CARRY), cycle);
    }

    fn bcs(&mut self, _mode: AddressingMode, cycle: u8) {
        self.branch_if(self.status.contains(StatusFlags::CARRY), cycle);
    }

    fn beq(&mut self, _mode: AddressingMode, cycle: u8) {
        self.branch_if(self.status.contains(StatusFlags::ZERO), cycle);
    }

    fn bit(&mut self, _mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            let operand = self.mem_read(addr);
            self.status
                .set(StatusFlags::OVERFLOW, operand & 0b0100_0000 != 0);
            self.status
                .set(StatusFlags::NEGATIVE, operand & 0b1000_0000 != 0);
            self.status
                .set(StatusFlags::ZERO, operand & self.accumulator == 0);
            self.current_instruction = None;
        }
    }

    fn bmi(&mut self, _mode: AddressingMode, cycle: u8) {
        self.branch_if(self.status.contains(StatusFlags::NEGATIVE), cycle);
    }

    fn bne(&mut self, _mode: AddressingMode, cycle: u8) {
        self.branch_if(!self.status.contains(StatusFlags::ZERO), cycle);
    }

    fn bpl(&mut self, _mode: AddressingMode, cycle: u8) {
        self.branch_if(!self.status.contains(StatusFlags::NEGATIVE), cycle);
    }

    fn brk(&mut self, _mode: AddressingMode, cycle: u8) {
        // TODO: Reset vector
        match (cycle, self.reset_state == ResetState::Executing) {
            (1, _) => {
                if !self.interrupt {
                    self.program_counter = self.program_counter.wrapping_add(1);
                }
            }
            (2, false) => {
                self.stack_push((self.program_counter >> 8) as u8);
            }
            (3, false) => {
                self.stack_push((self.program_counter & 0x00FF) as u8);
            }
            (4, false) => {
                let status = if self.interrupt {
                    self.interrupt = false;
                    self.export_status_interrupt()
                } else {
                    self.export_status()
                };
                self.stack_push(status);
            }
            (2..=4, true) => {
                // If we're executing a reset, "push" but read and discard instead of writing
                self.mem_read(self.stack_addr());
                self.stack_pointer = self.stack_pointer.wrapping_sub(1);
            }
            (5, is_reset) => {
                // buffered_interrupts will be the interrupts polled last cycle, but save the
                // vector in data_cache so that we don't get half of two different vectors
                self.data_cache = if is_reset {
                    0xFC
                } else if self.buffered_interrupts.contains(Interrupts::NMI) {
                    0xFA
                } else {
                    0xFE
                };
                // TODO: See if I should clear these individually
                self.buffered_interrupts = Interrupts::empty();
                // Fetch the low byte of the interrupt vector
                self.program_counter = (self.program_counter & 0xFF00)
                    | self.mem_read(0xFF00 | self.data_cache as u16) as u16;
                self.status.insert(StatusFlags::INTERRUPT_DISABLE);
            }
            (6, is_reset) => {
                // Fetch the high byte of the interrupt vector
                self.program_counter = (self.program_counter & 0x00FF)
                    | ((self.mem_read(0xFF00 | self.data_cache.wrapping_add(1) as u16) as u16)
                        << 8);
                self.current_instruction = None;
                if is_reset {
                    self.reset_state = ResetState::Clear;
                }
            }
            _ => unreachable!(),
        }
    }

    fn bvc(&mut self, _mode: AddressingMode, cycle: u8) {
        self.branch_if(!self.status.contains(StatusFlags::OVERFLOW), cycle);
    }

    fn bvs(&mut self, _mode: AddressingMode, cycle: u8) {
        self.branch_if(self.status.contains(StatusFlags::OVERFLOW), cycle);
    }

    fn clc(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.status.remove(StatusFlags::CARRY);
        self.current_instruction = None;
    }

    fn cld(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.status.remove(StatusFlags::_DECIMAL);
        self.current_instruction = None;
    }

    fn cli(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.status.remove(StatusFlags::INTERRUPT_DISABLE);
        self.current_instruction = None;
    }

    fn clv(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.status.remove(StatusFlags::OVERFLOW);
        self.current_instruction = None;
    }

    fn cmp(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.compare_mem(self.accumulator);
    }

    fn cpx(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.compare_mem(self.index_x);
    }

    fn cpy(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.compare_mem(self.index_y);
    }

    fn dec(&mut self, mode: AddressingMode, cycle: u8) {
        let carry = self.status.contains(StatusFlags::CARRY);
        self.modify_acc_or_mem(mode, cycle, |operand| (operand.wrapping_sub(1), carry))
    }

    fn dex(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.index_x = self.index_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.index_x);
        self.current_instruction = None;
    }

    fn dey(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.index_y = self.index_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.index_y);
        self.current_instruction = None;
    }

    fn eor(&mut self, mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            self.accumulator ^= self.mem_read(addr);
            self.update_zero_and_negative_flags(self.accumulator);
            self.current_instruction = None;
        }
    }

    fn inc(&mut self, mode: AddressingMode, cycle: u8) {
        let carry = self.status.contains(StatusFlags::CARRY);
        self.modify_acc_or_mem(mode, cycle, |operand| (operand.wrapping_add(1), carry))
    }

    fn inx(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.index_x = self.index_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.index_x);
        self.current_instruction = None;
    }

    fn iny(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.index_y = self.index_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.index_y);
        self.current_instruction = None;
    }

    fn jmp(&mut self, _mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            self.program_counter = addr;
            self.current_instruction = None;
        }
    }

    fn jsr(&mut self, mode: AddressingMode, cycle: u8) {
        match (cycle, self.fetched_addr) {
            (1, _) => {}
            (2, _) => {
                // Discard
                self.mem_read(self.stack_addr());
            }
            (3, _) => {
                self.stack_push((self.program_counter >> 8) as u8);
            }
            (4, _) => {
                self.stack_push((self.program_counter & 0x00FF) as u8);
            }
            (5, Some(addr)) => {
                self.program_counter = addr;
                self.current_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn lda(&mut self, mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            self.accumulator = self.mem_read(addr);
            self.update_zero_and_negative_flags(self.accumulator);
            self.current_instruction = None;
        }
    }

    fn ldx(&mut self, mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            self.index_x = self.mem_read(addr);
            self.update_zero_and_negative_flags(self.index_x);
            self.current_instruction = None;
        }
    }

    fn ldy(&mut self, mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            self.index_y = self.mem_read(addr);
            self.update_zero_and_negative_flags(self.index_y);
            self.current_instruction = None;
        }
    }

    fn lsr(&mut self, mode: AddressingMode, cycle: u8) {
        self.modify_acc_or_mem(mode, cycle, |operand| (operand / 2, operand % 2 == 1))
    }

    fn nop(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.current_instruction = None;
    }

    fn ora(&mut self, _mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            self.accumulator |= self.mem_read(addr);
            self.update_zero_and_negative_flags(self.accumulator);
            self.current_instruction = None;
        }
    }

    fn pha(&mut self, _mode: AddressingMode, cycle: u8) {
        if cycle == 2 {
            self.stack_push(self.accumulator);
            self.current_instruction = None;
        }
    }

    fn php(&mut self, _mode: AddressingMode, cycle: u8) {
        if cycle == 2 {
            self.stack_push(self.export_status());
            self.current_instruction = None;
        }
    }

    fn pla(&mut self, _mode: AddressingMode, cycle: u8) {
        match cycle {
            1 => {}
            2 => {
                // Discard
                self.mem_read(self.stack_addr());
            }
            3 => {
                self.accumulator = self.stack_pull();
                self.update_zero_and_negative_flags(self.accumulator);
                self.current_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn plp(&mut self, _mode: AddressingMode, cycle: u8) {
        match cycle {
            1 => {}
            2 => {
                // Discard
                self.mem_read(self.stack_addr());
            }
            3 => {
                let status = self.stack_pull();
                self.import_status(status);
                self.current_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn rol(&mut self, mode: AddressingMode, cycle: u8) {
        let lsb = if self.status.contains(StatusFlags::CARRY) {
            0b0000_0001
        } else {
            0b0000_0000
        };
        self.modify_acc_or_mem(mode, cycle, |operand| {
            let (result, carry) = operand.overflowing_mul(2);
            (result | lsb, carry)
        })
    }

    fn ror(&mut self, mode: AddressingMode, cycle: u8) {
        let msb = if self.status.contains(StatusFlags::CARRY) {
            0b1000_0000
        } else {
            0b0000_0000
        };
        self.modify_acc_or_mem(mode, cycle, |operand| (operand / 2 | msb, operand % 2 == 1))
    }

    fn rti(&mut self, _mode: AddressingMode, cycle: u8) {
        match cycle {
            1 => {}
            2 => {
                // Discard
                self.mem_read(self.stack_addr());
            }
            3 => {
                let status = self.stack_pull();
                self.import_status(status);
            }
            4 => {
                self.data_cache = self.stack_pull();
            }
            5 => {
                self.program_counter = u16::from_le_bytes([self.data_cache, self.stack_pull()]);
                self.current_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn rts(&mut self, _mode: AddressingMode, cycle: u8) {
        match cycle {
            1 => {}
            2 => {
                // Discard
                self.mem_read(self.stack_addr());
            }
            3 => {
                self.data_cache = self.stack_pull();
            }
            4 => {
                // TODO: When exactly should the PC be assigned?
                self.program_counter = u16::from_le_bytes([self.data_cache, self.stack_pull()]);
            }
            5 => {
                // Discard
                self.mem_read(self.program_counter);
                self.program_counter = self.program_counter.wrapping_add(1);
                self.current_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn sbc(&mut self, _mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            // Negate by taking complement and add one, but we don't add one since add_unsigned() will
            // do it if there's a carry (otherwise we would subtract one from the negated operand if
            // there wasn't a carry, so they cancel out and so we just complement)
            let operand = self.mem_read(addr);
            self.add_unsigned(!operand);
        }
    }

    fn sec(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.status.insert(StatusFlags::CARRY);
        self.current_instruction = None;
    }

    fn sed(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.status.insert(StatusFlags::_DECIMAL);
        self.current_instruction = None;
    }

    fn sei(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.status.insert(StatusFlags::INTERRUPT_DISABLE);
        self.current_instruction = None;
    }

    fn sta(&mut self, _mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            self.mem_write(addr, self.accumulator);
            self.current_instruction = None;
        }
    }

    fn stx(&mut self, _mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            self.mem_write(addr, self.index_x);
            self.current_instruction = None;
        }
    }

    fn sty(&mut self, _mode: AddressingMode, _cycle: u8) {
        if let Some(addr) = self.fetched_addr {
            self.mem_write(addr, self.index_y);
            self.current_instruction = None;
        }
    }

    fn tax(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.index_x = self.accumulator;
        self.update_zero_and_negative_flags(self.index_x);
        self.current_instruction = None;
    }

    fn tay(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.index_y = self.accumulator;
        self.update_zero_and_negative_flags(self.index_y);
        self.current_instruction = None;
    }

    fn tsx(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.index_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.index_x);
        self.current_instruction = None;
    }

    fn txa(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.accumulator = self.index_x;
        self.update_zero_and_negative_flags(self.accumulator);
        self.current_instruction = None;
    }

    fn txs(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.stack_pointer = self.index_x;
        self.current_instruction = None;
    }

    fn tya(&mut self, _mode: AddressingMode, _cycle: u8) {
        self.accumulator = self.index_y;
        self.update_zero_and_negative_flags(self.accumulator);
        self.current_instruction = None;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Handling {
    Store,
    Modify,
    Jmp,
    Jsr,
}

#[derive(PartialEq, Eq)]
struct Opcode<B> {
    instruction: fn(&mut Cpu<B>, AddressingMode, u8),
    name: &'static str,
    mode: AddressingMode,
    handling: Option<Handling>,
    control_flow: ControlFlow<()>,
}

impl<B> Debug for Opcode<B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Opcode")
            .field("name", &self.name)
            .field("mode", &self.mode)
            .finish()
    }
}

// The Copy and Clone derives can't see that B doesn't need to be Clone + Copy
impl<B> Copy for Opcode<B> {}

impl<B> Clone for Opcode<B> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<B> Opcode<B> {
    fn execute(&self, cpu: &mut Cpu<B>, cycle: u8) {
        (self.instruction)(cpu, self.mode, cycle);
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct StatusFlags: u8 {
        const CARRY              = 0b0000_0001;
        const ZERO               = 0b0000_0010;
        const INTERRUPT_DISABLE  = 0b0000_0100;
        const _DECIMAL           = 0b0000_1000;
        const B                  = 0b0001_0000;
        const _ONE               = 0b0010_0000;
        const OVERFLOW           = 0b0100_0000;
        const NEGATIVE           = 0b1000_0000;
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Interrupts: u8 {
        const IRQ = 0b01;
        const NMI = 0b10;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResetState {
    Clear,
    Waiting,
    Executing,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressingMode {
    Immediate,
    Accumulator,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndexedIndirectX,
    IndirectIndexedY,
    Implied,
}

#[cfg(test)]
#[derive(Clone)]
struct TestCpu(Cpu<FlatRam>);

#[cfg(test)]
impl TestCpu {
    pub fn new(program: &[u8]) -> Self {
        let mut cpu = Cpu::new(FlatRam::new());
        cpu.load(program);
        cpu.reset();
        Self(cpu)
    }

    pub fn with_acc(mut self, value: u8) -> Self {
        self.0.accumulator = value;
        self
    }

    pub fn with_x(mut self, value: u8) -> Self {
        self.0.index_x = value;
        self
    }

    pub fn with_y(mut self, value: u8) -> Self {
        self.0.index_y = value;
        self
    }

    pub fn with_mem(mut self, addr: u16, data: &[u8]) -> Self {
        self.0.bus.memory[addr as usize..addr as usize + data.len()].copy_from_slice(data);
        self
    }

    pub fn with_sp(mut self, value: u8) -> Self {
        self.0.stack_pointer = value;
        self
    }

    pub fn with_stack(mut self, stack: &[u8]) -> Self {
        for &byte in stack {
            self.0.stack_push(byte);
        }
        self
    }

    pub fn with_status(mut self, status: StatusFlags) -> Self {
        self.0.status = status;
        self
    }

    pub fn run(mut self) -> Cpu<FlatRam> {
        self.0.run();
        self.0
    }

    pub fn test_status_branch(opcode: u8, status: StatusFlags) -> Cpu<FlatRam> {
        let cpu = Self::new(&[opcode, 0xFC, 0x00])
            .with_mem(PROGRAM_START - 2, &[0x00, 0x00])
            .with_status(status)
            .run();

        assert_eq!(
            cpu.program_counter,
            PROGRAM_START - 1,
            "branch opcode {opcode:#4X} failed"
        );
        cpu
    }

    pub fn test_modify_status(opcode: u8, before: StatusFlags, after: StatusFlags) -> Cpu<FlatRam> {
        let cpu = Self::new(&[opcode, 0x00]).with_status(before).run();

        assert_eq!(cpu.status, after, "modify status opcode {opcode} failed");
        cpu
    }
}

#[cfg(test)]
mod test {
    use bitflags::Flags;

    use super::*;

    #[test]
    fn test_5_ops_working() {
        let cpu = TestCpu::new(&[0xA9, 0xC0, 0xAA, 0xE8, 0x00]).run();
        assert_eq!(cpu.index_x, 0xC1);
    }

    #[test]
    fn test_inx_overflow() {
        let cpu = TestCpu::new(&[0xE8, 0xE8, 0x00]).with_x(0xFF).run();
        assert_eq!(cpu.index_x, 1);
    }

    #[test]
    fn test_lda_from_memory() {
        let cpu = TestCpu::new(&[0xA5, 0x10, 0x00])
            .with_mem(0x10, &[0x55])
            .run();
        assert_eq!(cpu.accumulator, 0x55)
    }

    #[test]
    fn test_0x69_adc() {
        let cpu = TestCpu::new(&[0x69, 0xA0, 0x00]).with_acc(0xA0).run();
        assert_eq!(cpu.accumulator, 0x40);
        assert!(cpu.status.contains(StatusFlags::CARRY));
        assert!(cpu.status.contains(StatusFlags::OVERFLOW));
    }

    #[test]
    fn test_0x29_and() {
        let cpu = TestCpu::new(&[0x29, 0b0000_1111, 0x00])
            .with_acc(0b0011_0011)
            .run();
        assert_eq!(cpu.accumulator, 0b0000_0011);
    }

    #[test]
    fn test_0x0a_asl_acc() {
        let cpu = TestCpu::new(&[0x0A, 0x00]).with_acc(0b0101_0101).run();

        assert_eq!(cpu.accumulator, 0b1010_1010);
        assert!(!cpu.status.contains(StatusFlags::CARRY));
    }

    #[test]
    fn test_0x06_asl_carry() {
        let cpu = TestCpu::new(&[0x16, 0x10, 0x00])
            .with_mem(0x10, &[0b1010_1010])
            .run();

        assert_eq!(cpu.bus.peek(0x10).unwrap(), 0b01010100);
        assert!(cpu.status.contains(StatusFlags::CARRY));
    }

    #[test]
    fn test_0x90_bcc() {
        TestCpu::test_status_branch(0x90, StatusFlags::empty());
    }

    #[test]
    fn test_0xb0_bcs() {
        TestCpu::test_status_branch(0xB0, StatusFlags::CARRY);
    }

    #[test]
    fn test_0xf0_beq() {
        TestCpu::test_status_branch(0xF0, StatusFlags::ZERO);
    }

    #[test]
    fn test_0x24_bit() {
        let cpu = TestCpu::new(&[0x24, 0x10, 0x00])
            .with_acc(0b0011_1111)
            .with_mem(0x10, &[0b1100_0000])
            .run();

        assert!(cpu
            .status
            .contains(StatusFlags::ZERO | StatusFlags::OVERFLOW | StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0x30_bmi() {
        TestCpu::test_status_branch(0x30, StatusFlags::NEGATIVE);
    }

    #[test]
    fn test_0xd0_bne() {
        TestCpu::test_status_branch(0xD0, StatusFlags::empty());
    }

    #[test]
    fn test_0x10_bpl() {
        TestCpu::test_status_branch(0x10, StatusFlags::empty());
    }

    #[test]
    fn test_0x50_bvc() {
        TestCpu::test_status_branch(0x50, StatusFlags::empty());
    }

    #[test]
    fn test_0x70_bvs() {
        TestCpu::test_status_branch(0x70, StatusFlags::OVERFLOW);
    }

    #[test]
    fn test_0x18_clc() {
        TestCpu::test_modify_status(0x18, StatusFlags::CARRY, StatusFlags::empty());
    }

    #[test]
    fn test_0xd8_cld() {
        TestCpu::test_modify_status(0xD8, StatusFlags::_DECIMAL, StatusFlags::empty());
    }

    #[test]
    fn test_0x58_cli() {
        TestCpu::test_modify_status(0x58, StatusFlags::INTERRUPT_DISABLE, StatusFlags::empty());
    }

    #[test]
    fn test_0xb8_clv() {
        TestCpu::test_modify_status(0xB8, StatusFlags::OVERFLOW, StatusFlags::empty());
    }

    #[test]
    fn test_0xc9_cmp() {
        let cpu = TestCpu::new(&[0xC9, 0x10, 0x00]);

        let greater = cpu.clone().with_acc(0x11).run().status;
        assert!(
            greater.contains(StatusFlags::CARRY)
                && !greater.intersects(StatusFlags::ZERO | StatusFlags::NEGATIVE)
        );

        let equal = cpu.clone().with_acc(0x10).run().status;
        assert!(
            equal.contains(StatusFlags::CARRY | StatusFlags::ZERO)
                && !equal.intersects(StatusFlags::NEGATIVE)
        );

        let lesser = cpu.clone().with_acc(0x0F).run().status;
        assert!(
            lesser.contains(StatusFlags::NEGATIVE)
                && !lesser.intersects(StatusFlags::CARRY | StatusFlags::ZERO)
        );
    }

    #[test]
    fn test_0xe0_cpx() {
        let cpu = TestCpu::new(&[0xE0, 0x10, 0x00]);

        let greater = cpu.clone().with_x(0x11).run().status;
        assert!(
            greater.contains(StatusFlags::CARRY)
                && !greater.intersects(StatusFlags::ZERO | StatusFlags::NEGATIVE)
        );

        let equal = cpu.clone().with_x(0x10).run().status;
        assert!(
            equal.contains(StatusFlags::CARRY | StatusFlags::ZERO)
                && !equal.intersects(StatusFlags::NEGATIVE)
        );

        let lesser = cpu.clone().with_x(0x0F).run().status;
        assert!(
            lesser.contains(StatusFlags::NEGATIVE)
                && !lesser.intersects(StatusFlags::CARRY | StatusFlags::ZERO)
        );
    }

    #[test]
    fn test_0xc0_cpy() {
        let cpu = TestCpu::new(&[0xC0, 0x10, 0x00]);

        let greater = cpu.clone().with_y(0x11).run().status;
        assert!(
            greater.contains(StatusFlags::CARRY)
                && !greater.intersects(StatusFlags::ZERO | StatusFlags::NEGATIVE)
        );

        let equal = cpu.clone().with_y(0x10).run().status;
        assert!(
            equal.contains(StatusFlags::CARRY | StatusFlags::ZERO)
                && !equal.intersects(StatusFlags::NEGATIVE)
        );

        let lesser = cpu.clone().with_y(0x0F).run().status;
        assert!(
            lesser.contains(StatusFlags::NEGATIVE)
                && !lesser.intersects(StatusFlags::CARRY | StatusFlags::ZERO)
        );
    }

    #[test]
    fn test_0xc6_dec() {
        let cpu = TestCpu::new(&[0xC6, 0x10, 0x00])
            .with_mem(0x10, &[0x01])
            .run();

        assert_eq!(cpu.bus.peek(0x10).unwrap(), 0x00);
        assert!(cpu.status.contains(StatusFlags::ZERO));
    }

    #[test]
    fn test_0xca_dex() {
        let cpu = TestCpu::new(&[0xCA, 0x00]).with_x(0x01).run();

        assert_eq!(cpu.index_x, 0x00);
        assert!(cpu.status.contains(StatusFlags::ZERO));
    }

    #[test]
    fn test_0x88_dey() {
        let cpu = TestCpu::new(&[0x88, 0x00]).with_y(0x01).run();

        assert_eq!(cpu.index_y, 0x00);
        assert!(cpu.status.contains(StatusFlags::ZERO));
    }

    #[test]
    fn test_0x49_eor() {
        let cpu = TestCpu::new(&[0x49, 0b0000_1111, 0x00])
            .with_acc(0b0011_0011)
            .run();
        assert_eq!(cpu.accumulator, 0b0011_1100);
    }

    #[test]
    fn test_0xe6_inc() {
        let cpu = TestCpu::new(&[0xE6, 0x10, 0x00])
            .with_mem(0x10, &[0xFF])
            .run();

        assert_eq!(cpu.bus.peek(0x10).unwrap(), 0x00);
        assert!(cpu.status.contains(StatusFlags::ZERO));
    }

    #[test]
    fn test_0xe8_inx() {
        let cpu = TestCpu::new(&[0xE8, 0x00]).with_x(0xFF).run();

        assert_eq!(cpu.index_x, 0x00);
        assert!(cpu.status.contains(StatusFlags::ZERO));
    }

    #[test]
    fn test_0xc8_iny() {
        let cpu = TestCpu::new(&[0xc8, 0x00]).with_y(0xFF).run();

        assert_eq!(cpu.index_y, 0x00);
        assert!(cpu.status.contains(StatusFlags::ZERO));
    }

    #[test]
    fn test_0x4c_jmp() {
        let [left, right] = (PROGRAM_START + 4).to_le_bytes();
        let cpu = TestCpu::new(&[0x4C, left, right, 0x00, 0x00]).run();

        assert_eq!(cpu.program_counter, PROGRAM_START + 5);
    }

    #[test]
    fn test_0x20_jsr() {
        let [left, right] = (PROGRAM_START + 4).to_le_bytes();
        let mut cpu = TestCpu::new(&[0x20, left, right, 0x00, 0x00]).run();

        assert_eq!(cpu.program_counter, PROGRAM_START + 5);
        assert_eq!(cpu.stack_pull_u16(), PROGRAM_START + 2);
    }

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let cpu = TestCpu::new(&[0xA9, 0x05, 0x00]).run();
        assert_eq!(cpu.accumulator, 0x05);
        assert!(!cpu.status.contains(StatusFlags::ZERO));
        assert!(!cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let cpu = TestCpu::new(&[0xA9, 0x00, 0x00]).run();
        assert!(cpu.status.contains(StatusFlags::ZERO));
    }

    #[test]
    fn test_0xa2_ldx() {
        let cpu = TestCpu::new(&[0xA2, 0x05, 0x00]).run();

        assert_eq!(cpu.index_x, 0x05);
    }

    #[test]
    fn test_0xa0_ldy() {
        let cpu = TestCpu::new(&[0xA0, 0x05, 0x00]).run();

        assert_eq!(cpu.index_y, 0x05);
    }

    #[test]
    fn test_0x4a_lsr_acc() {
        let cpu = TestCpu::new(&[0x4A, 0x00]).with_acc(0b1010_1010).run();

        assert_eq!(cpu.accumulator, 0b0101_0101);
        assert!(!cpu.status.contains(StatusFlags::CARRY));
    }

    #[test]
    fn test_0x46_lsr_carry() {
        let cpu = TestCpu::new(&[0x46, 0x10, 0x00])
            .with_mem(0x10, &[0b0101_0101])
            .run();

        assert_eq!(cpu.bus.peek(0x10).unwrap(), 0b0010_1010);
        assert!(cpu.status.contains(StatusFlags::CARRY));
    }

    #[test]
    fn test_0xea_nop() {
        let mut cpu1 = TestCpu::new(&[0x00, 0x00]).run();
        let cpu2 = TestCpu::new(&[0xEA, 0x00]).run();

        cpu1.mem_write(PROGRAM_START, 0xEA);
        cpu1.program_counter += 1;

        assert_eq!(cpu2, cpu1);
    }

    #[test]
    fn test_0x09_ora() {
        let cpu = TestCpu::new(&[0x09, 0b0000_1111, 0x00])
            .with_acc(0b0011_0011)
            .run();
        assert_eq!(cpu.accumulator, 0b0011_1111);
    }

    #[test]
    fn test_0x48_pha() {
        let mut cpu = TestCpu::new(&[0x48, 0x00]).with_acc(0x05).run();

        assert_eq!(cpu.stack_pull(), 0x05);
    }

    #[test]
    fn test_0x08_php() {
        let mut cpu = TestCpu::new(&[0x08, 0x00])
            .with_status(StatusFlags::OVERFLOW | StatusFlags::NEGATIVE)
            .run();

        assert_eq!(
            StatusFlags::from_bits_retain(cpu.stack_pull()),
            StatusFlags::B | StatusFlags::_ONE | StatusFlags::OVERFLOW | StatusFlags::NEGATIVE
        );
    }

    #[test]
    fn test_0x68_pla() {
        let cpu = TestCpu::new(&[0x68, 0x00]).with_stack(&[0xF5]).run();

        assert_eq!(cpu.accumulator, 0xF5);
        assert!(cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0x28_plp() {
        let cpu = TestCpu::new(&[0x28, 0x00])
            .with_stack(&[(StatusFlags::B
                | StatusFlags::_ONE
                | StatusFlags::OVERFLOW
                | StatusFlags::NEGATIVE)
                .bits()])
            .run();

        assert_eq!(
            cpu.status,
            StatusFlags::_ONE | StatusFlags::OVERFLOW | StatusFlags::NEGATIVE
        );
    }

    #[test]
    fn test_0x2a_rol() {
        let cpu = TestCpu::new(&[0x2A, 0x00])
            .with_acc(0b0011_1100)
            .with_status(StatusFlags::CARRY)
            .run();

        assert_eq!(cpu.accumulator, 0b0111_1001);
        assert!(!cpu.status.contains(StatusFlags::CARRY));
    }

    #[test]
    fn test_0x6a_ror() {
        let cpu = TestCpu::new(&[0x6A, 0x00])
            .with_acc(0b0011_1100)
            .with_status(StatusFlags::CARRY)
            .run();

        assert_eq!(cpu.accumulator, 0b1001_1110);
        assert!(!cpu.status.contains(StatusFlags::CARRY));
    }

    #[test]
    fn test_0x40_rti() {
        let [low, high] = (PROGRAM_START + 2).to_le_bytes();
        let cpu = TestCpu::new(&[0x40, 0x00, 0x00])
            .with_stack(&[
                high,
                low,
                (StatusFlags::B
                    | StatusFlags::_ONE
                    | StatusFlags::OVERFLOW
                    | StatusFlags::NEGATIVE)
                    .bits(),
            ])
            .run();

        assert_eq!(
            cpu.status,
            StatusFlags::_ONE | StatusFlags::OVERFLOW | StatusFlags::NEGATIVE
        );
        assert_eq!(cpu.program_counter, PROGRAM_START + 3);
    }

    #[test]
    fn test_0x60_rts() {
        let [low, high] = (PROGRAM_START + 2).to_le_bytes();
        let cpu = TestCpu::new(&[0x60, 0x00, 0x00, 0x00])
            .with_stack(&[high, low])
            .run();

        assert_eq!(cpu.program_counter, PROGRAM_START + 4);
    }

    #[test]
    fn test_0xe9_sbc() {
        let cpu = TestCpu::new(&[0xE9, 0x60, 0x00])
            .with_acc(0xA0)
            .with_status(StatusFlags::CARRY)
            .run();
        assert_eq!(cpu.accumulator, 0x40);
        assert!(cpu.status.contains(StatusFlags::CARRY));
        assert!(cpu.status.contains(StatusFlags::OVERFLOW));
    }

    #[test]
    fn test_0x38_sec() {
        TestCpu::test_modify_status(0x38, StatusFlags::empty(), StatusFlags::CARRY);
    }

    #[test]
    fn test_0xf8_sed() {
        TestCpu::test_modify_status(0xF8, StatusFlags::empty(), StatusFlags::_DECIMAL);
    }

    #[test]
    fn test_0x78_sei() {
        TestCpu::test_modify_status(0x78, StatusFlags::empty(), StatusFlags::INTERRUPT_DISABLE);
    }

    #[test]
    fn test_0x85_sta() {
        let cpu = TestCpu::new(&[0x85, 0x10, 0x00]).with_acc(0x55).run();

        assert_eq!(cpu.bus.peek(0x10).unwrap(), 0x55);
    }

    #[test]
    fn test_0x86_stx() {
        let cpu = TestCpu::new(&[0x86, 0x10, 0x00]).with_x(0x55).run();

        assert_eq!(cpu.bus.peek(0x10).unwrap(), 0x55);
    }

    #[test]
    fn test_0x84_sty() {
        let cpu = TestCpu::new(&[0x84, 0x10, 0x00]).with_y(0x55).run();

        assert_eq!(cpu.bus.peek(0x10).unwrap(), 0x55);
    }

    #[test]
    fn test_0xaa_tax() {
        let cpu = TestCpu::new(&[0xAA, 0x00]).with_acc(0xF0).run();

        assert_eq!(cpu.index_x, 0xF0);
        assert!(cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa8_tay() {
        let cpu = TestCpu::new(&[0xA8, 0x00]).with_acc(0xF0).run();

        assert_eq!(cpu.index_y, 0xF0);
        assert!(cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0xba_tsx() {
        let cpu = TestCpu::new(&[0xBA, 0x00]).with_sp(0xF0).run();

        assert_eq!(cpu.index_x, 0xF0);
        assert!(cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0x8a_txa() {
        let cpu = TestCpu::new(&[0x8A, 0x00]).with_x(0xF0).run();

        assert_eq!(cpu.accumulator, 0xF0);
        assert!(cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0x9a_txs() {
        let cpu = TestCpu::new(&[0x9A, 0x00]).with_x(0xF0).run();

        assert_eq!(cpu.stack_pointer, 0xF0);
        assert!(!cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0x98_tya() {
        let cpu = TestCpu::new(&[0x98, 0x00]).with_y(0xF0).run();

        assert_eq!(cpu.accumulator, 0xF0);
        assert!(cpu.status.contains(StatusFlags::NEGATIVE));
    }
}
