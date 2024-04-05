use crate::op_6502::{initialize_op, Op};

#[derive(Debug)]
pub struct Cpu {
    register_pc: u16,
    register_sp: u8,
    register_a: u8,
    register_x: u8,
    register_y: u8,
    /* Processor status
     * 0  1  2  3  4  5  6  7
     * N  V     B  D  I  Z  C
     *
     * Negative
     * Overflow
     * Break
     * Decimal Mode
     * Interrupt Disable
     * Zero
     * Carry
     */
    register_p: u8,
    memory: [u8; 0xFFFF],
}
impl Default for Cpu {
    fn default() -> Self {
        Cpu {
            register_pc: 0,
            register_sp: 0,
            register_a: 0,
            register_x: 0,
            register_y: 0,
            register_p: 0,
            memory: [0; 0xFFFF],
        }
    }
}
impl Cpu {
    pub fn new() -> Self {
        Cpu::default()
    }
    pub fn init_and_run(&mut self, program: &[u8]) {
        initialize_op();
        self.interrupt_rst();
        self.interrupt_load(program);
        self.process();
    }
    //for test
    //set memory, pc and other register
    pub fn run_without_init(&mut self, program: &[u8]) {
        initialize_op();
        self.interrupt_load(program);
        self.process();
    }
    fn process(&mut self) {
        self.register_pc = 0;
        while self.register_pc < self.memory.len() as u16 {
            self.interpret();
            self.register_pc += 1;
        }
    }
    fn interpret(&mut self) {
        let cur_op = self.memory[self.register_pc as usize];
        let cur_op = Op::try_from(cur_op);
        if let Ok(cur_op) = cur_op {
            match cur_op.name.as_ref() {
                "ADC" => self.op_adc(cur_op.mode),
                "AND" => self.op_and(cur_op.mode),
                "ASL" => self.op_asl(cur_op.mode),
                "BCC" => self.op_bcc(cur_op.mode),
                "BCS" => self.op_bcs(cur_op.mode),
                "BEQ" => self.op_beq(cur_op.mode),
                "BNE" => self.op_bne(cur_op.mode),
                "BMI" => self.op_bmi(cur_op.mode),
                "BPL" => self.op_bpl(cur_op.mode),
                "BVC" => self.op_bvc(cur_op.mode),
                "BVS" => self.op_bvs(cur_op.mode),
                "BIT" => self.op_bit(cur_op.mode),
                "CLC" => self.op_clc(),
                "CLD" => self.op_cld(),
                "CLI" => self.op_cli(),
                "CLV" => self.op_clv(),
                "LDA" => self.op_lda(cur_op.mode),
                "TAX" => self.op_tax(),
                "BRK" => self.op_brk(),
                _ => {}
            }
        }
    }
    fn op_adc(&mut self, mode: MODE) {
        if let Some(m) = self.mode_to_data(mode) {
            let m = self.memory_read_u8(m);
            let carry = if (self.register_p & 0b_0000_0001) == 0b_0000_0001 {
                self.register_p &= 0b_1111_1110;
                1
            } else {
                0
            };
            let (sum_ac, carry_ac) = self.register_a.overflowing_add(carry);
            let (sum, carry_acm) = sum_ac.overflowing_add(m as u8);
            if carry_ac || carry_acm {
                self.register_p_set_flag(Flag::Carry);
            }
            self.register_a_write_u8(sum);
        }
    }
    fn op_and(&mut self, mode: MODE) {
        if let Some(m) = self.mode_to_data(mode) {
            let m = self.memory_read_u8(m);
            self.register_a &= m as u8;
            self.register_a_write_u8(self.register_a);
        }
    }
    fn op_asl(&mut self, mode: MODE) {
        if let Some(m) = self.mode_to_data(mode) {
            let old = self.memory_read_u8(m);
            if self.memory[old as usize] & 0b_1000_0000 != 0 {
                self.register_p_set_flag(Flag::Carry);
            } else {
                self.register_p_clear_flag(Flag::Carry);
            }
            self.memory[m as usize] = old << 1;
            if self.memory[m as usize] & 0b_1000_0000 != 0 {
                self.register_p_set_flag(Flag::Negative);
            } else {
                self.register_p_clear_flag(Flag::Negative);
            }
        } else {
            if self.register_a & 0b_1000_0000 != 0 {
                self.register_p_set_flag(Flag::Carry);
            } else {
                self.register_p_clear_flag(Flag::Carry);
            }
            self.register_a = self.register_a << 1;
            if self.register_a & 0b_1000_0000 != 0 {
                self.register_p_set_flag(Flag::Negative);
            } else {
                self.register_p_clear_flag(Flag::Negative);
            }
            if self.register_a == 0 {
                self.register_p_set_flag(Flag::Zero);
            } else {
                self.register_p_clear_flag(Flag::Zero);
            }
        }
    }
    fn op_bcc(&mut self, mode: MODE) {
        if let MODE::Relative = mode {
            if let Some(offset) = self.mode_to_data(mode) {
                if self.register_p_flag_is_set(Flag::Carry) {
                    return;
                }
                self.register_pc += offset;
            }
        }
    }
    fn op_bcs(&mut self, mode: MODE) {
        if let MODE::Relative = mode {
            if let Some(offset) = self.mode_to_data(mode) {
                if !self.register_p_flag_is_set(Flag::Carry) {
                    return;
                }
                self.register_pc += offset;
            }
        }
    }
    fn op_beq(&mut self, mode: MODE) {
        if let MODE::Relative = mode {
            if let Some(offset) = self.mode_to_data(mode) {
                if !self.register_p_flag_is_set(Flag::Zero) {
                    return;
                }
                self.register_pc += offset;
            }
        }
    }
    fn op_bne(&mut self, mode: MODE) {
        if let MODE::Relative = mode {
            if let Some(offset) = self.mode_to_data(mode) {
                if self.register_p_flag_is_set(Flag::Zero) {
                    return;
                }
                self.register_pc += offset;
            }
        }
    }
    fn op_bmi(&mut self, mode: MODE) {
        if let MODE::Relative = mode {
            if let Some(offset) = self.mode_to_data(mode) {
                if !self.register_p_flag_is_set(Flag::Negative) {
                    return;
                }
                self.register_pc += offset;
            }
        }
    }
    fn op_bpl(&mut self, mode: MODE) {
        if let MODE::Relative = mode {
            if let Some(offset) = self.mode_to_data(mode) {
                if self.register_p_flag_is_set(Flag::Negative) {
                    return;
                }
                self.register_pc += offset;
            }
        }
    }
    fn op_bvc(&mut self, mode: MODE) {
        if let MODE::Relative = mode {
            if let Some(offset) = self.mode_to_data(mode) {
                if self.register_p_flag_is_set(Flag::OverFLow) {
                    return;
                }
                self.register_pc += offset;
            }
        }
    }
    fn op_bvs(&mut self, mode: MODE) {
        if let MODE::Relative = mode {
            if let Some(offset) = self.mode_to_data(mode) {
                if !self.register_p_flag_is_set(Flag::OverFLow) {
                    return;
                }
                self.register_pc += offset;
            }
        }
    }
    fn op_bit(&mut self, mode: MODE) {
        if let Some(m) = self.mode_to_data(mode) {
            let m = self.memory[m as usize];
            if m & 0b_0100_0000 != 0 {
                self.register_p_set_flag(Flag::OverFLow);
            }
            if m & 0b_1000_0000 != 0 {
                self.register_p_set_flag(Flag::Negative);
            }
            if m & self.register_a == 0b_0000_0000 {
                self.register_p_set_flag(Flag::Zero);
            }
        }
    }
    fn op_clc(&mut self) {
        self.register_p_clear_flag(Flag::Carry);
    }
    fn op_cld(&mut self) {
        self.register_p_clear_flag(Flag::DecimalMode);
    }
    fn op_cli(&mut self) {
        self.register_p_clear_flag(Flag::InterruptDisable);
    }
    fn op_clv(&mut self) {
        self.register_p_clear_flag(Flag::OverFLow);
    }
    fn op_lda(&mut self, mode: MODE) {
        if let Some(m) = self.mode_to_data(mode) {
            let m = self.memory_read_u8(m);
            self.register_a_write_u8(m as u8);
        }
    }
    fn op_tax(&mut self) {
        self.register_x_write_u8(self.register_a);
    }
    fn op_brk(&mut self) {}

    fn interrupt_rst(&mut self) {
        self.register_x = 0;
        self.register_y = 0;
        self.register_a = 0;
        self.register_p = 0;
        self.register_sp = 0;
        self.register_pc = 0xFFFC;
    }

    fn interrupt_load(&mut self, program: &[u8]) {
        memory_write_be_u16(0x8000, &mut self.memory, 0xFFFC);
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(program);
    }
    fn mode_to_data(&mut self, mode: MODE) -> Option<u16> {
        match mode {
            MODE::Implied => None,

            MODE::Accumulator => None,

            MODE::Immediate => {
                self.register_pc += 1;
                Some(self.register_pc as u16)
            }
            MODE::ZeroPage => {
                self.register_pc += 1;
                Some(self.memory_read_u8(self.register_pc) as u16)
            }
            MODE::ZeroPageX => {
                self.register_pc += 1;
                Some(
                    (self
                        .memory_read_u8(self.register_pc)
                        .wrapping_add(self.register_x)) as u16,
                )
            }
            MODE::ZeroPageY => {
                self.register_pc += 1;
                Some(
                    (self
                        .memory_read_u8(self.register_pc)
                        .wrapping_add(self.register_y)) as u16,
                )
            }
            MODE::Relative => {
                self.register_pc += 1;
                Some(self.memory_read_u8(self.register_pc) as u16)
            }
            MODE::Absolute => {
                self.register_pc += 1;
                let addr = self.memory_read_u16(self.register_pc);
                self.register_pc += 1;
                Some(addr)
            }
            MODE::AbsoluteX => {
                self.register_pc += 1;
                let addr = self
                    .memory_read_u16(self.register_pc)
                    .wrapping_add(self.register_x as u16);
                self.register_pc += 1;
                Some(addr)
            }
            MODE::AbsoluteY => {
                self.register_pc += 1;
                let addr = self
                    .memory_read_u16(self.register_pc)
                    .wrapping_add(self.register_x as u16);
                self.register_pc += 1;
                Some(addr)
            }
            MODE::IndirectX => {
                self.register_pc += 1;
                Some(
                    self.memory_read_u16(self.register_pc)
                        .wrapping_add(self.register_x as u16),
                )
            }
            MODE::IndirectY => {
                self.register_pc += 1;
                let addr = self.memory_read_u8((self.memory_read_u8(self.register_pc)) as u16);
                Some(addr.wrapping_add(self.register_y) as u16)
            }
            _ => None,
        }
    }
    fn memory_read_u8(&self, index: u16) -> u8 {
        self.memory[index as usize]
    }
    fn memory_read_from_pc_u8(&self) -> u8 {
        self.memory_read_u8(self.memory_read_u8(self.register_pc) as u16)
    }
    fn memory_read_u16(&mut self, index: u16) -> u16 {
        let low = self.memory_read_u8(index) as u16;
        let high = self.memory_read_u8(index.wrapping_add(1)) as u16;
        (high << 8) | low
    }

    /* check Zero and Negative  */
    fn register_a_write_u8(&mut self, from: u8) {
        self.register_a = from;
        if self.register_a == 0 {
            self.register_p_set_flag(Flag::Zero);
        } else {
            self.register_p_clear_flag(Flag::Zero);
        }
        if self.register_a & 0b1000_0000 != 0 {
            self.register_p_set_flag(Flag::Negative);
        } else {
            self.register_p_clear_flag(Flag::Negative);
        }
    }
    fn register_x_write_u8(&mut self, from: u8) {
        self.register_x = from;
        if self.register_x == 0 {
            self.register_p_set_flag(Flag::Zero);
        } else {
            self.register_p_clear_flag(Flag::Zero);
        }
        if self.register_x & 0b1000_0000 != 0 {
            self.register_p_set_flag(Flag::Negative);
        } else {
            self.register_p_clear_flag(Flag::Negative);
        }
    }

    fn register_p_set_flag(&mut self, flag: Flag) {
        let mask = match flag {
            Flag::Carry => 0b_0000_0001,
            Flag::Zero => 0b_0000_0010,
            Flag::InterruptDisable => 0b_0000_0100,
            Flag::DecimalMode => 0b_0000_1000,
            Flag::Break => 0b_0001_0000,
            Flag::OverFLow => 0b_0100_0000,
            Flag::Negative => 0b_1000_0000,
            _ => 0b_0000_0000,
        };
        self.register_p |= mask;
    }
    fn register_p_clear_flag(&mut self, flag: Flag) {
        let mask = match flag {
            Flag::Carry => 0b_1111_1110,
            Flag::Zero => 0b_1111_1101,
            Flag::InterruptDisable => 0b_1111_1011,
            Flag::DecimalMode => 0b_1111_0111,
            Flag::Break => 0b_1110_1111,
            Flag::OverFLow => 0b_1011_1111,
            Flag::Negative => 0b_0111_1111,
            _ => 0b_1111_1111,
        };
        self.register_p &= mask;
    }
    fn register_p_flag_is_set(&mut self, flag: Flag) -> bool {
        let mask = match flag {
            Flag::Carry => 0b_0000_0001,
            Flag::Zero => 0b_0000_0010,
            Flag::InterruptDisable => 0b_0000_0100,
            Flag::DecimalMode => 0b_0000_1000,
            Flag::Break => 0b_0001_0000,
            Flag::OverFLow => 0b_0100_0000,
            Flag::Negative => 0b_1000_0000,
            _ => 0b_0000_0000,
        };
        self.register_p & mask != 0
    }
}

#[derive(Clone)]
pub enum MODE {
    Implied,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
}
pub enum Flag {
    Carry,
    Zero,
    InterruptDisable,
    DecimalMode,
    Break,
    OverFLow,
    Negative,
}

fn le_to_be_u16(le: u16) -> u16 {
    (le >> 8) | ((le & 0x00ff) << 8)
}
fn be_to_le_u16(be: u16) -> u16 {
    (be >> 8) | ((be & 0x00ff) << 8)
}
fn memory_write_be_u16(from: u16, to: &mut [u8], addr: u16) {
    to[addr as usize] = (be_to_le_u16(from) & 0b_0000_1111) as u8;
    to[(addr + 1) as usize] = (be_to_le_u16(from) >> 8) as u8;
}
#[cfg(test)]
mod test {
    #![allow(unused_imports)]
    use super::*;
    use core::panic;

    #[test]
    fn test_addr_method() {
        let mut cpu = Cpu::new();
        //Immediate
        cpu.init_and_run(&[0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);

        //ZeroPage
        cpu.interrupt_rst();
        cpu.memory[0x00] = 0xFF;
        cpu.register_pc = 0x8000;
        cpu.run_without_init(&[0xA5, 0x00, 0x00]);
        assert_eq!(cpu.register_a, cpu.memory[0x00]);

        //ZeroPageX
        cpu.interrupt_rst();
        cpu.memory[0x01] = 0xFF;
        cpu.register_pc = 0x8000;
        cpu.register_x = 0x01;
        cpu.run_without_init(&[0xB5, 0x00, 0x00]);
        assert_eq!(cpu.register_a, 0xFF);

        //Absolute
        cpu.interrupt_rst();
        cpu.memory[0xAAAA] = 0x0F;
        cpu.register_pc = 0x8000;
        cpu.run_without_init(&[0xAD, 0xAA, 0xAA, 0x00]);
        assert_eq!(cpu.register_a, 0x0F);

        //IndirectX
        cpu.interrupt_rst();
        cpu.memory[0x01] = 0x0F;
        cpu.register_pc = 0x8000;
        cpu.register_x = 0x01;
        cpu.run_without_init(&[0xBD, 0x00, 0x00]);
        assert_eq!(cpu.register_a, 0x0F);

        //IndirectY
        cpu.interrupt_rst();
        cpu.memory[0x00] = 0x0F;
        cpu.memory[0x10] = 0xFF;
        cpu.register_pc = 0x8000;
        cpu.register_y = 0x01;
        cpu.run_without_init(&[0xB1, 0x00, 0x00]);
        assert_eq!(cpu.register_a, 0xFF);
    }

    #[test]
    fn test_adc() {
        let mut cpu = Cpu::new();
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_p |= 0b_0000_0001;
        cpu.run_without_init(&[0x69, 0xFF, 0x00]);
        assert_eq!(cpu.register_a, 0);
        assert_eq!(cpu.register_p & 0b_0000_0001, 1);
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_p |= 0b_0000_0001;
        cpu.run_without_init(&[0x69, 0x01, 0x00]);
        assert_eq!(cpu.register_a, 0x02);
        assert_eq!(cpu.register_p & 0b_0000_0001, 0);
    }
    #[test]
    fn test_and() {
        let mut cpu = Cpu::new();
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0xFF;
        assert_eq!(cpu.register_a, 0xFF);
        cpu.run_without_init(&[0x29, 0x00, 0x00]);
        assert_eq!(cpu.register_a, 0x00);
    }
    #[test]
    fn test_asl() {
        let mut cpu = Cpu::new();
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0xFF;
        assert_eq!(cpu.register_a, 0xFF);
        cpu.run_without_init(&[0x0A, 0x00]);
        assert_eq!(cpu.register_p_flag_is_set(Flag::Carry), true);
        assert_eq!(cpu.register_a, 0xFE);
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.memory[0x00] = 0x01;
        cpu.run_without_init(&[0x06, 0x00, 0x00]);
        assert_eq!(cpu.memory[0x00], 2);
    }
    #[test]
    fn test_bcc_and_bcs() {
        let mut cpu = Cpu::new();
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0xFF;
        cpu.run_without_init(&[0x0A, 0x00, 0x29, 0x00, 0x00]);
        assert_eq!(cpu.register_a, 0x00);
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0xFF;
        cpu.run_without_init(&[0x0A, 0x90, 0x02, 0x29, 0x00, 0x00]); /* ASL -- BCC -- AND  */
        assert_eq!(cpu.register_p_flag_is_set(Flag::Carry), true);
        assert_eq!(cpu.register_a, 0x00);
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0x01;
        cpu.run_without_init(&[0x0A, 0x90, 0x02, 0x29, 0x00, 0x00]); /* ASL -- BCC -- AND  */
        assert_eq!(cpu.register_p_flag_is_set(Flag::Carry), false);
        assert_eq!(cpu.register_a, 0x02);
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0xFF;
        cpu.run_without_init(&[0x0A, 0xB0, 0x02, 0x29, 0x00, 0x00]); /* ASL -- BCS -- AND  */
        assert_eq!(cpu.register_p_flag_is_set(Flag::Carry), true);
        assert_eq!(cpu.register_a, 0xFE);
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0x01;
        cpu.run_without_init(&[0x0A, 0xB0, 0x02, 0x29, 0x00, 0x00]); /* ASL -- BCS -- AND  */
        assert_eq!(cpu.register_p_flag_is_set(Flag::Carry), false);
        assert_eq!(cpu.register_a, 0x00);
    }
    #[test]
    fn test_bit() {
        let mut cpu = Cpu::new();
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.memory[0x00] = 0x01;
        cpu.run_without_init(&[0x24, 0x00, 0x00]);
        assert_eq!(cpu.register_p_flag_is_set(Flag::Zero), true);
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.memory[0x00] = 0x01;
        cpu.register_a = 0xFF;
        cpu.run_without_init(&[0x24, 0x00, 0x00]);
        assert_eq!(cpu.register_p_flag_is_set(Flag::Zero), false);
    }
    #[test]
    fn test_bmi_and_bpl() {
        //BMI
        let mut cpu = Cpu::new();
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0xFF;
        cpu.run_without_init(&[0x0A, 0x30, 0x02, 0x29, 0x00, 0x00]); /* ASL -- BMI -- AND  */
        assert_eq!(cpu.register_a, 0xFE);
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0x01;
        cpu.run_without_init(&[0x0A, 0x30, 0x02, 0x29, 0x00, 0x00]); /* ASL -- BMI -- AND  */
        assert_eq!(cpu.register_a, 0x00);
        //BPL
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0x01;
        cpu.run_without_init(&[0x0A, 0x10, 0x02, 0x29, 0x00, 0x00]); /* ASL -- BPL -- AND  */
        assert_eq!(cpu.register_a, 0x02);
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0xFF;
        cpu.run_without_init(&[0x0A, 0x10, 0x02, 0x29, 0x00, 0x00]); /* ASL -- BPL -- AND  */
        assert_eq!(cpu.register_a, 0x00);
    }
    #[test]
    fn test_beq_bne() {
        //BEQ
        let mut cpu = Cpu::new();
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0xFF;
        cpu.register_p_set_flag(Flag::Zero);
        cpu.run_without_init(&[0xF0, 0x02, 0x29, 0x00, 0x00]); /* ASL -- BEQ -- AND  */
        assert_eq!(cpu.register_a, 0xFF);
        //BNE
        cpu.interrupt_rst();
        cpu.register_pc = 0x8000;
        cpu.register_a = 0xFF;
        cpu.register_p_clear_flag(Flag::Zero);
        cpu.run_without_init(&[0xD0, 0x02, 0x29, 0x00, 0x00]); /* ASL -- BNE -- AND  */
        assert_eq!(cpu.register_a, 0xFF);
    }
    #[test]
    fn test_bvc_and_bvs() {
        //just skip testing it (too simple and boring =_=);
    }
    #[test]
    fn test_clc_cld_cli_clv() {
        //we have done it in other test module (right?)
    }
}
