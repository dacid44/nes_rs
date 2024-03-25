use bitflags::bitflags;
use paste::paste;

const MEMORY_SIZE: usize = 0xFFFF;
const PROGRAM_START: u16 = 0x8000;
const PROGRAM_START_LOC: u16 = 0xFFFC;

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

pub struct Cpu {
    pub accumulator: u8,
    pub index_x: u8,
    pub index_y: u8,
    pub status: StatusFlags,
    pub program_counter: u16,
    memory: [u8; MEMORY_SIZE],
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            accumulator: 0,
            index_x: 0,
            index_y: 0,
            status: StatusFlags::empty(),
            program_counter: 0,
            memory: [0; MEMORY_SIZE],
        }
    }

    pub fn reset(&mut self) {
        self.accumulator = 0;
        self.index_x = 0;
        self.index_y = 0;

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
        use AddressingMode as AM;
        loop {
            let opcode = self.get_opcode();

            opcodes! {opcode; self;
                0x00 => return; // BRK

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

                0xE8 => INX, AM::Implied;

                0xA9 => LDA, AM::Immediate;
                0xA5 => LDA, AM::ZeroPage;
                0xB5 => LDA, AM::ZeroPageX;
                0xAD => LDA, AM::Absolute;
                0xBD => LDA, AM::AbsoluteX;
                0xB9 => LDA, AM::AbsoluteY;
                0xA1 => LDA, AM::IndexedIndirectX;
                0xB1 => LDA, AM::IndirectIndexedY;

                0x85 => STA, AM::ZeroPage;
                0x95 => STA, AM::ZeroPageX;
                0x8D => STA, AM::Absolute;
                0x9D => STA, AM::AbsoluteX;
                0x99 => STA, AM::AbsoluteY;
                0x81 => STA, AM::IndexedIndirectX;
                0x91 => STA, AM::IndirectIndexedY;

                0xAA => TAX, AM::Implied;
                _ => todo!("opcode {opcode:#X}");
            }
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_read_u16(&self, addr: u16) -> u16 {
        // Not sure if this should be wrapping or not
        u16::from_le_bytes([self.mem_read(addr), self.mem_read(addr + 1)])
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let bytes = data.to_le_bytes();
        self.mem_write(addr, bytes[0]);
        // Not sure if this should be wrapping or not
        self.mem_write(addr + 1, bytes[1]);
    }

    fn get_opcode(&mut self) -> u8 {
        let byte = self.mem_read(self.program_counter);
        self.program_counter += 1;
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
                (self.mem_read(self.program_counter) as u16).wrapping_add(self.program_counter),
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
        self.program_counter += pc_offset;
        addr
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.set(StatusFlags::ZERO, result == 0);
        self.status
            .set(StatusFlags::NEGATIVE, result & 0b1000_0000 != 0);
    }

    fn adc(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        let operand = self.mem_read(addr);

        let (result, carry) = self.accumulator.overflowing_add(operand);
        let overflow = (self.accumulator as i8).overflowing_add(operand as i8).1;

        self.status.set(StatusFlags::CARRY, carry);
        self.status.set(StatusFlags::OVERFLOW, overflow);

        self.accumulator = result;
        self.update_zero_and_negative_flags(self.accumulator);
    }

    fn and(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.accumulator &= self.mem_read(addr);
        self.update_zero_and_negative_flags(self.accumulator);
    }

    fn asl(&mut self, mode: AddressingMode) {
        use AddressingMode as AM;

        let (result, carry) = match mode {
            AM::Accumulator => {
                let (result, carry) = self.accumulator.overflowing_mul(2);
                self.accumulator = result;
                (result, carry)
            }
            AM::ZeroPage | AM::ZeroPageX | AM::Absolute | AM::AbsoluteX => {
                let addr = self.get_operand_address(mode);
                let (result, carry) = self.mem_read(addr).overflowing_mul(2);
                self.mem_write(addr, result);
                (result, carry)
            }
            _ => unreachable!("mode {:?} is not supported", mode),
        };
        self.status.set(StatusFlags::CARRY, carry);
        self.update_zero_and_negative_flags(result);
    }

    fn bcc(&mut self, mode: AddressingMode) {
        if !self.status.contains(StatusFlags::CARRY) {
            self.program_counter = self.get_operand_address(mode);
        }
    }

    fn bcs(&mut self, mode: AddressingMode) {
        if self.status.contains(StatusFlags::CARRY) {
            self.program_counter = self.get_operand_address(mode);
        }
    }

    fn inx(&mut self, _mode: AddressingMode) {
        self.index_x = self.index_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.index_x);
    }

    fn lda(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.accumulator = self.mem_read(addr);
        self.update_zero_and_negative_flags(self.accumulator);
    }

    fn sta(&mut self, mode: AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.accumulator);
    }

    fn tax(&mut self, _mode: AddressingMode) {
        self.index_x = self.accumulator;
        self.update_zero_and_negative_flags(self.index_x);
    }
}

#[cfg(test)]
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

    pub fn with_status(mut self, status: StatusFlags) -> Self {
        self.0.status = status;
        self
    }

    pub fn run(mut self) -> Cpu {
        self.0.run();
        self.0
    }
}

#[cfg(test)]
mod test {
    use bitflags::Flags;

    use super::*;

    #[test]
    fn test_0xa9_immediate_load_data() {
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
    fn test_0xaa_tax_move_a_to_x() {
        let cpu = TestCpu::new(&[0xAA, 0x00]).with_acc(10).run();
        assert_eq!(cpu.index_x, 10);
    }

    #[test]
    fn test_0xe8_increment_x() {
        let cpu = TestCpu::new(&[0xE8, 0xE8, 0x00]).with_x(10).run();
        assert_eq!(cpu.index_x, 12);
    }

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
    fn test_sta_to_memory() {
        let cpu = TestCpu::new(&[0x85, 0x10, 0x00]).with_acc(0x55).run();
        assert_eq!(cpu.mem_read(0x10), 0x55);
    }

    #[test]
    fn test_0x69_adc_carry_set() {
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
        let cpu = TestCpu::new(&[0x90, 0x02, 0x00, 0x00, 0x00]).run();

        assert_eq!(cpu.program_counter, PROGRAM_START + 4);
    }

    #[test]
    fn test_0xb0_bcs() {
        let cpu = TestCpu::new(&[0xB0, 0x02, 0x00, 0x00, 0x00])
            .with_status(StatusFlags::CARRY)
            .run();

        assert_eq!(cpu.program_counter, PROGRAM_START + 4);
    }
}

bitflags! {
    struct StatusFlags: u8 {
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
    IndexedIndirectX,
    IndirectIndexedY,
    Implied,
}
