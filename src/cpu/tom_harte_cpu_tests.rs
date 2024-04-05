use std::{cell::RefCell, fs::File, rc::Rc};

use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::Deserialize;

use crate::bus::{Bus, CpuBus, FlatRam};

use super::{Cpu, StatusFlags};

#[test]
fn run_tom_harte_tests() {
    glob::glob("roms/tom_harte_tests/*.json")
        .expect("could not find test files")
        .collect::<Vec<_>>()
        .par_iter()
        .for_each(|path| {
            let path = path.as_ref().unwrap();
            if !path.to_str().unwrap().contains("00") {
                let test_cases: Vec<TestCase> =
                    serde_json::from_reader(File::open(path).unwrap()).unwrap();
                for test_case in test_cases {
                    test_case.run(path.to_str().unwrap());
                }
            }
        })
}

#[ignore]
#[test]
fn run_one_tom_harte_test() {
    const TEST: &str = "00.json";
    let path = format!("roms/tom_harte_tests/{}", TEST);
    let test_cases: Vec<TestCase> =
        serde_json::from_reader(File::open(&path).unwrap()).unwrap();
    test_cases.iter().for_each(|test_case| {
        test_case.run(&path);
    });
}

#[derive(Deserialize)]
struct TestCase {
    name: String,
    initial: TestState,
    // "final" is a reserved keyword
    #[serde(rename = "final")]
    end: TestState,
    cycles: Vec<(u16, u8, Access)>,
}

impl TestCase {
    fn run(&self, file_name: &str) {
        let name = format!("{}; {}", file_name, self.name);
        // println!("running {}", name);
        let mut cpu = self.initial.create_cpu();
        cpu.run_instruction();
        self.end.check_cpu(&cpu, &name);

        assert_eq!(cpu.bus.access_log, self.cycles, "{}; cycles", name);
    }
}

#[derive(Deserialize)]
struct TestState {
    pc: u16,
    s: u8,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    ram: Vec<(u16, u8)>,
}


#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
enum Access {
    Read,
    Write,
}

struct TestRam {
    bus: FlatRam,
    access_log: Vec<(u16, u8, Access)>,
}

impl TestRam {
    fn new() -> Self {
        Self {
            bus: FlatRam::new(),
            access_log: Vec::new(),
        }
    }
}

impl Bus for TestRam {
    fn read(&mut self, addr: u16) -> u8 {
        // println!("{}", std::backtrace::Backtrace::force_capture());
        let result = self.bus.read(addr);
        println!("read from {addr:#06X}: {result:#04X}");
        self.access_log.push((addr, result, Access::Read));
        result
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.access_log.push((addr, data, Access::Write));
        self.bus.write(addr, data);
    }

    fn peek(&self, addr: u16) -> Option<u8> {
        self.bus.peek(addr)
    }
}

impl CpuBus for TestRam {}

impl TestState {
    fn create_cpu(&self) -> Cpu<TestRam> {
        let mut cpu = Cpu::new(TestRam::new());
        for &(addr, data) in &self.ram {
            cpu.bus.bus.write(addr, data);
        }
        // cpu.reset();
        cpu.program_counter = self.pc;
        cpu.stack_pointer = self.s;
        cpu.accumulator = self.a;
        cpu.index_x = self.x;
        cpu.index_y = self.y;
        cpu.status = StatusFlags::from_bits_retain(self.p) & !(StatusFlags::_ONE);
        cpu
    }

    fn check_cpu(&self, cpu: &Cpu<TestRam>, name: &str) {
        for &(addr, data) in &self.ram {
            assert_eq!(
                cpu.bus.bus.peek(addr).unwrap(),
                data,
                "{}; memory address {addr:#06X}",
                name
            );
        }
        assert_eq!(cpu.program_counter, self.pc, "{}; program counter", name);
        assert_eq!(cpu.stack_pointer, self.s, "{}; stack pointer", name);
        assert_eq!(cpu.accumulator, self.a, "{}; accumulator", name);
        assert_eq!(cpu.index_x, self.x, "{}; index x", name);
        assert_eq!(cpu.index_y, self.y, "{}; index y", name);
        // println!(
        //     "{} {} {}",
        //     cpu.status.contains(StatusFlags::CARRY),
        //     cpu.status.contains(StatusFlags::OVERFLOW),
        //     StatusFlags::from_bits_retain(self.p).contains(StatusFlags::OVERFLOW)
        // );
        assert_eq!(
            cpu.status & !(StatusFlags::_ONE),
            StatusFlags::from_bits_retain(self.p) & !(StatusFlags::_ONE),
            "{}; status flags",
            name
        );
    }
}
