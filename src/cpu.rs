#[cfg(test)]
mod tom_harte_cpu_tests;

use std::{fmt::Debug, ops::ControlFlow};

use bitflags::bitflags;
use paste::paste;

const MEMORY_SIZE: usize = 0x10000;
// const PROGRAM_START: u16 = 0x8000;
const PROGRAM_START: u16 = 0x0600;
const PROGRAM_START_LOC: u16 = 0xFFFC;
const STACK_OFFSET: u16 = 0x0100;
const STACK_INIT: u8 = 0xFF;

macro_rules! opcodes {
    ( $value:expr; $cpu:expr; $break:literal => $break_expr:expr; $( $opcode:literal => $name:ident, $mode:expr; )+ $( _ => $default:expr; )? ) => {
        match $value {
        $break => $break_expr,
        $(
            $opcode => {
                paste! { $cpu.[<$name:lower>]($mode) }
            }
        )+
        $(
            _ => $default
        )?
        }
    };
}

#[derive(Clone, PartialEq, Eq)]
pub struct Cpu {
    pub accumulator: u8,
    pub index_x: u8,
    pub index_y: u8,
    pub stack_pointer: u8,
    pub status: StatusFlags,
    pub program_counter: u16,
    pub memory: [u8; MEMORY_SIZE],
}

impl Debug for Cpu {
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

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            accumulator: 0,
            index_x: 0,
            index_y: 0,
            stack_pointer: 0,
            status: StatusFlags::empty(),
            program_counter: 0,
            memory: [0; MEMORY_SIZE],
        }
    }

    pub fn state_string(&self) -> String {
        format!(
            "PC:  {:#06X} X: {:#04X}\nACC: {:#04X}   Y: {:#04X}\nSTACK: {:02X?}\nNO1BDIZC\n{:08b}",
            self.program_counter,
            self.index_x,
            self.accumulator,
            self.index_y,
            if STACK_INIT - self.stack_pointer > 16 {
                &self.memory[STACK_OFFSET as usize + self.stack_pointer as usize + 1
                    ..=STACK_OFFSET as usize + self.stack_pointer as usize + 16]
            } else {
                &self.memory[STACK_OFFSET as usize + self.stack_pointer as usize + 1
                    ..=STACK_OFFSET as usize + STACK_INIT as usize]
            },
            self.status.bits(),
        )
    }

    pub fn reset(&mut self) {
        self.accumulator = 0;
        self.index_x = 0;
        self.index_y = 0;
        self.stack_pointer = STACK_INIT;

        self.program_counter = self.mem_read_u16(PROGRAM_START_LOC);
    }

    pub fn load_and_run(&mut self, program: &[u8]) {
        self.load(program);
        self.reset();
        self.run()
    }

    pub fn load(&mut self, program: &[u8]) {
        self.memory[PROGRAM_START as usize..(PROGRAM_START as usize + program.len())]
            .copy_from_slice(program);
        self.mem_write_u16(PROGRAM_START_LOC, PROGRAM_START);
    }

    pub fn run(&mut self) {
        loop {
            if self.run_instruction().is_break() {
                return;
            };
        }
    }

    pub fn run_instruction(&mut self) -> ControlFlow<()> {
        use AddressingMode as AM;
        let opcode = self.get_opcode();
        // println!(
        //     "pc: {:#06X}, opcode: {opcode:#04X}",
        //     self.program_counter.wrapping_sub(1),
        // );

        opcodes! {opcode; self;
            0x00 => return ControlFlow::Break(()); // BRK

            0x69 => ADC, AM::Immediate;
            0x65 => ADC, AM::ZeroPage;
            0x75 => ADC, AM::ZeroPageX;
            0x6D => ADC, AM::Absolute;
            0x7D => ADC, AM::AbsoluteX;
            0x79 => ADC, AM::AbsoluteY;
            0x61 => ADC, AM::IndexedIndirectX;
            0x71 => ADC, AM::IndirectIndexedY;

            0x29 => AND, AM::Immediate;
            0x25 => AND, AM::ZeroPage;
            0x35 => AND, AM::ZeroPageX;
            0x2D => AND, AM::Absolute;
            0x3D => AND, AM::AbsoluteX;
            0x39 => AND, AM::AbsoluteY;
            0x21 => AND, AM::IndexedIndirectX;
            0x31 => AND, AM::IndirectIndexedY;

            0x0A => ASL, AM::Accumulator;
            0x06 => ASL, AM::ZeroPage;
            0x16 => ASL, AM::ZeroPageX;
            0x0E => ASL, AM::Absolute;
            0x1E => ASL, AM::AbsoluteX;

            0x90 => BCC, AM::Relative;

            0xB0 => BCS, AM::Relative;

            0xF0 => BEQ, AM::Relative;

            0x24 => BIT, AM::ZeroPage;
            0x2C => BIT, AM::Absolute;

            0x30 => BMI, AM::Relative;

            0xD0 => BNE, AM::Relative;

            0x10 => BPL, AM::Relative;

            0x50 => BVC, AM::Relative;

            0x70 => BVS, AM::Relative;

            0x18 => CLC, AM::Implied;

            0xD8 => CLD, AM::Implied;

            0x58 => CLI, AM::Implied;

            0xB8 => CLV, AM::Implied;

            0xC9 => CMP, AM::Immediate;
            0xC5 => CMP, AM::ZeroPage;
            0xD5 => CMP, AM::ZeroPageX;
            0xCD => CMP, AM::Absolute;
            0xDD => CMP, AM::AbsoluteX;
            0xD9 => CMP, AM::AbsoluteY;
            0xC1 => CMP, AM::IndexedIndirectX;
            0xD1 => CMP, AM::IndirectIndexedY;

            0xE0 => CPX, AM::Immediate;
            0xE4 => CPX, AM::ZeroPage;
            0xEC => CPX, AM::Absolute;

            0xC0 => CPY, AM::Immediate;
            0xC4 => CPY, AM::ZeroPage;
            0xCC => CPY, AM::Absolute;

            0xC6 => DEC, AM::ZeroPage;
            0xD6 => DEC, AM::ZeroPageX;
            0xCE => DEC, AM::Absolute;
            0xDE => DEC, AM::AbsoluteX;

            0xCA => DEX, AM::Implied;

            0x88 => DEY, AM::Implied;

            0x49 => EOR, AM::Immediate;
            0x45 => EOR, AM::ZeroPage;
            0x55 => EOR, AM::ZeroPageX;
            0x4D => EOR, AM::Absolute;
            0x5D => EOR, AM::AbsoluteX;
            0x59 => EOR, AM::AbsoluteY;
            0x41 => EOR, AM::IndexedIndirectX;
            0x51 => EOR, AM::IndirectIndexedY;

            0xE6 => INC, AM::ZeroPage;
            0xF6 => INC, AM::ZeroPageX;
            0xEE => INC, AM::Absolute;
            0xFE => INC, AM::AbsoluteX;

            0xE8 => INX, AM::Implied;

            0xC8 => INY, AM::Implied;

            // Compatibility note at https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
            0x4C => JMP, AM::Absolute;
            0x6C => JMP, AM::Indirect;

            0x20 => JSR, AM::Absolute;

            0xA9 => LDA, AM::Immediate;
            0xA5 => LDA, AM::ZeroPage;
            0xB5 => LDA, AM::ZeroPageX;
            0xAD => LDA, AM::Absolute;
            0xBD => LDA, AM::AbsoluteX;
            0xB9 => LDA, AM::AbsoluteY;
            0xA1 => LDA, AM::IndexedIndirectX;
            0xB1 => LDA, AM::IndirectIndexedY;

            0xA2 => LDX, AM::Immediate;
            0xA6 => LDX, AM::ZeroPage;
            0xB6 => LDX, AM::ZeroPageY;
            0xAE => LDX, AM::Absolute;
            0xBE => LDX, AM::AbsoluteY;

            0xA0 => LDY, AM::Immediate;
            0xA4 => LDY, AM::ZeroPage;
            0xB4 => LDY, AM::ZeroPageX;
            0xAC => LDY, AM::Absolute;
            0xBC => LDY, AM::AbsoluteX;

            0x4A => LSR, AM::Accumulator;
            0x46 => LSR, AM::ZeroPage;
            0x56 => LSR, AM::ZeroPageX;
            0x4E => LSR, AM::Absolute;
            0x5E => LSR, AM::AbsoluteX;

            0xEA => NOP, AM::Implied;

            0x09 => ORA, AM::Immediate;
            0x05 => ORA, AM::ZeroPage;
            0x15 => ORA, AM::ZeroPageX;
            0x0D => ORA, AM::Absolute;
            0x1D => ORA, AM::AbsoluteX;
            0x19 => ORA, AM::AbsoluteY;
            0x01 => ORA, AM::IndexedIndirectX;
            0x11 => ORA, AM::IndirectIndexedY;

            0x48 => PHA, AM::Implied;

            0x08 => PHP, AM::Implied;

            0x68 => PLA, AM::Implied;

            0x28 => PLP, AM::Implied;

            0x2A => ROL, AM::Accumulator;
            0x26 => ROL, AM::ZeroPage;
            0x36 => ROL, AM::ZeroPageX;
            0x2E => ROL, AM::Absolute;
            0x3E => ROL, AM::AbsoluteX;

            0x6A => ROR, AM::Accumulator;
            0x66 => ROR, AM::ZeroPage;
            0x76 => ROR, AM::ZeroPageX;
            0x6E => ROR, AM::Absolute;
            0x7E => ROR, AM::AbsoluteX;

            0x40 => RTI, AM::Implied;

            0x60 => RTS, AM::Implied;

            0xE9 => SBC, AM::Immediate;
            0xE5 => SBC, AM::ZeroPage;
            0xF5 => SBC, AM::ZeroPageX;
            0xED => SBC, AM::Absolute;
            0xFD => SBC, AM::AbsoluteX;
            0xF9 => SBC, AM::AbsoluteY;
            0xE1 => SBC, AM::IndexedIndirectX;
            0xF1 => SBC, AM::IndirectIndexedY;

            0x38 => SEC, AM::Implied;

            0xF8 => SED, AM::Implied;

            0x78 => SEI, AM::Implied;

            0x85 => STA, AM::ZeroPage;
            0x95 => STA, AM::ZeroPageX;
            0x8D => STA, AM::Absolute;
            0x9D => STA, AM::AbsoluteX;
            0x99 => STA, AM::AbsoluteY;
            0x81 => STA, AM::IndexedIndirectX;
            0x91 => STA, AM::IndirectIndexedY;

            0x86 => STX, AM::ZeroPage;
            0x96 => STX, AM::ZeroPageY;
            0x8E => STX, AM::Absolute;

            0x84 => STY, AM::ZeroPage;
            0x94 => STY, AM::ZeroPageX;
            0x8C => STY, AM::Absolute;

            0xAA => TAX, AM::Implied;

            0xA8 => TAY, AM::Implied;

            0xBA => TSX, AM::Implied;

            0x8A => TXA, AM::Implied;

            0x9A => TXS, AM::Implied;

            0x98 => TYA, AM::Implied;

            _ => todo!("opcode {opcode:#X}");
        }
        println!("{}\n", self.state_string());
        ControlFlow::Continue(())
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    pub fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    pub fn mem_read_u16(&self, addr: u16) -> u16 {
        // Not sure if this should be wrapping or not
        u16::from_le_bytes([self.mem_read(addr), self.mem_read(addr.wrapping_add(1))])
    }

    pub fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let [low, high] = data.to_le_bytes();
        self.mem_write(addr, low);
        // Not sure if this should be wrapping or not
        self.mem_write(addr + 1, high);
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

    fn import_status(&mut self, status: u8) {
        self.status = StatusFlags::from_bits_retain(status) & !(StatusFlags::B | StatusFlags::_ONE)
    }

    fn get_opcode(&mut self) -> u8 {
        let byte = self.mem_read(self.program_counter);
        self.program_counter = self.program_counter.wrapping_add(1);
        byte
    }

    fn get_operand_address(&mut self, mode: AddressingMode) -> u16 {
        use AddressingMode as AM;
        let (addr, pc_offset) = match mode {
            AM::Immediate => (self.program_counter, 1),
            AM::ZeroPage => (self.mem_read(self.program_counter) as u16, 1),
            AM::ZeroPageX => (
                self.mem_read(self.program_counter)
                    .wrapping_add(self.index_x) as u16,
                1,
            ),
            AM::ZeroPageY => (
                self.mem_read(self.program_counter)
                    .wrapping_add(self.index_y) as u16,
                1,
            ),
            AM::Relative => (
                self.program_counter
                    .wrapping_add(1)
                    .wrapping_add_signed(self.mem_read(self.program_counter) as i8 as i16),
                1,
            ),
            AM::Absolute => (self.mem_read_u16(self.program_counter), 2),
            AM::AbsoluteX => (
                self.mem_read_u16(self.program_counter)
                    .wrapping_add(self.index_x as u16),
                2,
            ),
            AM::AbsoluteY => (
                self.mem_read_u16(self.program_counter)
                    .wrapping_add(self.index_y as u16),
                2,
            ),
            AM::Indirect => {
                let ptr_low = self.mem_read_u16(self.program_counter);
                // https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP
                let ptr_high = (ptr_low & 0xFF00) | (ptr_low.wrapping_add(1) & 0x00FF);
                (
                    u16::from_le_bytes([self.mem_read(ptr_low), self.mem_read(ptr_high)]),
                    2,
                )
            }
            AM::IndexedIndirectX => {
                let ptr = (self.mem_read(self.program_counter) as u8).wrapping_add(self.index_x);
                (
                    u16::from_le_bytes([
                        self.mem_read(ptr as u16),
                        self.mem_read(ptr.wrapping_add(1) as u16),
                    ]),
                    1,
                )
            }
            AM::IndirectIndexedY => {
                let ptr = self.mem_read(self.program_counter);
                (
                    u16::from_le_bytes([
                        self.mem_read(ptr as u16),
                        self.mem_read(ptr.wrapping_add(1) as u16),
                    ])
                    .wrapping_add(self.index_y as u16),
                    1,
                )
            }
            _ => {
                panic!("mode {:?} is not supported", mode);
            }
        };
        self.program_counter = self.program_counter.wrapping_add(pc_offset);
        addr
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
    }

    fn branch_if(&mut self, condition: bool, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        if condition {
            self.program_counter = addr;
        }
    }

    fn compare_mem(&mut self, register: u8, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        let operand = self.mem_read(addr);

        self.status.set(StatusFlags::CARRY, register >= operand);
        self.update_zero_and_negative_flags(register.wrapping_sub(operand));
    }

    fn modify_acc_or_mem<F>(&mut self, mode: AddressingMode, f: F)
    where
        F: FnOnce(u8) -> (u8, bool),
    {
        use AddressingMode as AM;

        let (result, carry) = match mode {
            AM::Accumulator => {
                let (result, carry) = f(self.accumulator);
                self.accumulator = result;
                (result, carry)
            }
            AM::ZeroPage | AM::ZeroPageX | AM::Absolute | AM::AbsoluteX => {
                let addr = self.get_operand_address(mode);
                let (result, carry) = f(self.mem_read(addr));
                self.mem_write(addr, result);
                (result, carry)
            }
            _ => unreachable!("mode {:?} is not supported", mode),
        };
        self.status.set(StatusFlags::CARRY, carry);
        self.update_zero_and_negative_flags(result);
    }

    fn adc(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.add_unsigned(self.mem_read(addr));
    }

    fn and(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.accumulator &= self.mem_read(addr);
        self.update_zero_and_negative_flags(self.accumulator);
    }

    fn asl(&mut self, mode: AddressingMode) {
        self.modify_acc_or_mem(mode, |operand| operand.overflowing_mul(2))
    }

    fn bcc(&mut self, mode: AddressingMode) {
        self.branch_if(!self.status.contains(StatusFlags::CARRY), mode);
    }

    fn bcs(&mut self, mode: AddressingMode) {
        self.branch_if(self.status.contains(StatusFlags::CARRY), mode);
    }

    fn beq(&mut self, mode: AddressingMode) {
        self.branch_if(self.status.contains(StatusFlags::ZERO), mode);
    }

    fn bit(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        let operand = self.mem_read(addr);
        self.status
            .set(StatusFlags::OVERFLOW, operand & 0b0100_0000 != 0);
        self.status
            .set(StatusFlags::NEGATIVE, operand & 0b1000_0000 != 0);
        self.status
            .set(StatusFlags::ZERO, operand & self.accumulator == 0);
    }

    fn bmi(&mut self, mode: AddressingMode) {
        self.branch_if(self.status.contains(StatusFlags::NEGATIVE), mode);
    }

    fn bne(&mut self, mode: AddressingMode) {
        self.branch_if(!self.status.contains(StatusFlags::ZERO), mode);
    }

    fn bpl(&mut self, mode: AddressingMode) {
        self.branch_if(!self.status.contains(StatusFlags::NEGATIVE), mode);
    }

    fn bvc(&mut self, mode: AddressingMode) {
        self.branch_if(!self.status.contains(StatusFlags::OVERFLOW), mode);
    }

    fn bvs(&mut self, mode: AddressingMode) {
        self.branch_if(self.status.contains(StatusFlags::OVERFLOW), mode);
    }

    fn clc(&mut self, _mode: AddressingMode) {
        self.status.remove(StatusFlags::CARRY);
    }

    fn cld(&mut self, _mode: AddressingMode) {
        self.status.remove(StatusFlags::_DECIMAL);
    }

    fn cli(&mut self, _mode: AddressingMode) {
        self.status.remove(StatusFlags::INTERRUPT_DISABLE);
    }

    fn clv(&mut self, _mode: AddressingMode) {
        self.status.remove(StatusFlags::OVERFLOW);
    }

    fn cmp(&mut self, mode: AddressingMode) {
        self.compare_mem(self.accumulator, mode);
    }

    fn cpx(&mut self, mode: AddressingMode) {
        self.compare_mem(self.index_x, mode);
    }

    fn cpy(&mut self, mode: AddressingMode) {
        self.compare_mem(self.index_y, mode);
    }

    fn dec(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        let result = self.mem_read(addr).wrapping_sub(1);
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    fn dex(&mut self, _mode: AddressingMode) {
        self.index_x = self.index_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.index_x);
    }

    fn dey(&mut self, _mode: AddressingMode) {
        self.index_y = self.index_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.index_y);
    }

    fn eor(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.accumulator ^= self.mem_read(addr);
        self.update_zero_and_negative_flags(self.accumulator);
    }

    fn inc(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        let result = self.mem_read(addr).wrapping_add(1);
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    fn inx(&mut self, _mode: AddressingMode) {
        self.index_x = self.index_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.index_x);
    }

    fn iny(&mut self, _mode: AddressingMode) {
        self.index_y = self.index_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.index_y);
    }

    fn jmp(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.program_counter = addr;
    }

    fn jsr(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.stack_push_u16(self.program_counter.wrapping_sub(1));
        self.program_counter = addr;
    }

    fn lda(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.accumulator = self.mem_read(addr);
        self.update_zero_and_negative_flags(self.accumulator);
    }

    fn ldx(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.index_x = self.mem_read(addr);
        self.update_zero_and_negative_flags(self.index_x);
    }

    fn ldy(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.index_y = self.mem_read(addr);
        self.update_zero_and_negative_flags(self.index_y);
    }

    fn lsr(&mut self, mode: AddressingMode) {
        self.modify_acc_or_mem(mode, |operand| (operand / 2, operand % 2 == 1))
    }

    fn nop(&self, _mode: AddressingMode) {
        // NOP
    }

    fn ora(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.accumulator |= self.mem_read(addr);
        self.update_zero_and_negative_flags(self.accumulator);
    }

    fn pha(&mut self, _mode: AddressingMode) {
        self.stack_push(self.accumulator);
    }

    fn php(&mut self, _mode: AddressingMode) {
        self.stack_push(self.export_status());
    }

    fn pla(&mut self, _mode: AddressingMode) {
        self.accumulator = self.stack_pull();
        self.update_zero_and_negative_flags(self.accumulator);
    }

    fn plp(&mut self, _mode: AddressingMode) {
        let status = self.stack_pull();
        self.import_status(status);
    }

    fn rol(&mut self, mode: AddressingMode) {
        let lsb = if self.status.contains(StatusFlags::CARRY) {
            0b0000_0001
        } else {
            0b0000_0000
        };
        self.modify_acc_or_mem(mode, |operand| {
            let (result, carry) = operand.overflowing_mul(2);
            (result | lsb, carry)
        })
    }

    fn ror(&mut self, mode: AddressingMode) {
        let msb = if self.status.contains(StatusFlags::CARRY) {
            0b1000_0000
        } else {
            0b0000_0000
        };
        self.modify_acc_or_mem(mode, |operand| (operand / 2 | msb, operand % 2 == 1))
    }

    fn rti(&mut self, _mode: AddressingMode) {
        let status = self.stack_pull();
        self.import_status(status);
        self.program_counter = self.stack_pull_u16();
    }

    fn rts(&mut self, _mode: AddressingMode) {
        self.program_counter = self.stack_pull_u16().wrapping_add(1);
    }

    fn sbc(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        // Negate by taking complement and add one, but we don't add one since add_unsigned() will
        // do it if there's a carry (otherwise we would subtract one from the negated operand if
        // there wasn't a carry, so they cancel out and so we just complement)
        self.add_unsigned(!self.mem_read(addr));
    }

    fn sec(&mut self, _mode: AddressingMode) {
        self.status.insert(StatusFlags::CARRY);
    }

    fn sed(&mut self, _mode: AddressingMode) {
        self.status.insert(StatusFlags::_DECIMAL);
    }

    fn sei(&mut self, _mode: AddressingMode) {
        self.status.insert(StatusFlags::INTERRUPT_DISABLE);
    }

    fn sta(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.accumulator);
    }

    fn stx(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.index_x);
    }

    fn sty(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.index_y);
    }

    fn tax(&mut self, _mode: AddressingMode) {
        self.index_x = self.accumulator;
        self.update_zero_and_negative_flags(self.index_x);
    }

    fn tay(&mut self, _mode: AddressingMode) {
        self.index_y = self.accumulator;
        self.update_zero_and_negative_flags(self.index_y);
    }

    fn tsx(&mut self, _mode: AddressingMode) {
        self.index_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.index_x);
    }

    fn txa(&mut self, _mode: AddressingMode) {
        self.accumulator = self.index_x;
        self.update_zero_and_negative_flags(self.accumulator);
    }

    fn txs(&mut self, _mode: AddressingMode) {
        self.stack_pointer = self.index_x;
    }

    fn tya(&mut self, _mode: AddressingMode) {
        self.accumulator = self.index_y;
        self.update_zero_and_negative_flags(self.accumulator);
    }
}

#[cfg(test)]
#[derive(Clone)]
struct TestCpu(Cpu);

#[cfg(test)]
impl TestCpu {
    pub fn new(program: &[u8]) -> Self {
        let mut cpu = Cpu::new();
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
        self.0.memory[addr as usize..addr as usize + data.len()].copy_from_slice(data);
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

    pub fn run(mut self) -> Cpu {
        self.0.run();
        self.0
    }

    pub fn test_status_branch(opcode: u8, status: StatusFlags) -> Cpu {
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

    pub fn test_modify_status(opcode: u8, before: StatusFlags, after: StatusFlags) -> Cpu {
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

        assert_eq!(cpu.mem_read(0x10), 0b01010100);
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

        assert_eq!(cpu.mem_read(0x10), 0x00);
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

        assert_eq!(cpu.mem_read(0x10), 0x00);
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

        assert_eq!(cpu.mem_read(0x10), 0b0010_1010);
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

        assert_eq!(cpu.status, StatusFlags::OVERFLOW | StatusFlags::NEGATIVE);
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

        assert_eq!(cpu.status, StatusFlags::OVERFLOW | StatusFlags::NEGATIVE);
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

        assert_eq!(cpu.mem_read(0x10), 0x55);
    }

    #[test]
    fn test_0x86_stx() {
        let cpu = TestCpu::new(&[0x86, 0x10, 0x00]).with_x(0x55).run();

        assert_eq!(cpu.mem_read(0x10), 0x55);
    }

    #[test]
    fn test_0x84_sty() {
        let cpu = TestCpu::new(&[0x84, 0x10, 0x00]).with_y(0x55).run();

        assert_eq!(cpu.mem_read(0x10), 0x55);
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

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct StatusFlags: u8 {
        const CARRY = 0b0000_0001;
        const ZERO = 0b0000_0010;
        const INTERRUPT_DISABLE = 0b0000_0100;
        const _DECIMAL = 0b0000_1000;
        const B = 0b0001_0000;
        const _ONE = 0b0010_0000;
        const OVERFLOW = 0b0100_0000;
        const NEGATIVE = 0b1000_0000;
    }
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
