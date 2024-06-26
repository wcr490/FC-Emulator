use std::sync::Arc;

use once_cell::sync::OnceCell;

use crate::cpu_6502::MODE;

#[derive(Clone)]
pub struct Op {
    pub name: String,
    pub code: u8,
    pub cycles: u8,
    pub bytes: u8,
    pub mode: MODE,
}

impl Op {
    pub fn new(name: String, code: u8, cycles: u8, bytes: u8, mode: MODE) -> Self {
        Self {
            name,
            code,
            cycles,
            bytes,
            mode,
        }
    }
}

pub static OP_COLLECTION: OnceCell<Arc<Vec<Op>>> = OnceCell::new();

pub fn initialize_op() {
    OP_COLLECTION
        .set(Arc::new(vec![
            /* BRK  */
            Op::new("BRK".to_string(), 0x00, 7, 1, MODE::Implied),
            /* ADC  */
            Op::new("ADC".to_string(), 0x69, 2, 2, MODE::Immediate),
            Op::new("ADC".to_string(), 0x65, 3, 2, MODE::ZeroPage),
            Op::new("ADC".to_string(), 0x75, 4, 2, MODE::ZeroPageX),
            Op::new("ADC".to_string(), 0x6D, 4, 3, MODE::Absolute),
            Op::new("ADC".to_string(), 0x7D, 4, 3, MODE::AbsoluteX), //cycle + 1 if page crossed
            Op::new("ADC".to_string(), 0x79, 4, 3, MODE::AbsoluteY), //cycle + 1 if page crossed
            Op::new("ADC".to_string(), 0x61, 6, 2, MODE::Immediate),
            Op::new("ADC".to_string(), 0x71, 5, 2, MODE::Immediate), //cycle + 1 if page crossed
            /* AND  */
            Op::new("AND".to_string(), 0x29, 2, 2, MODE::Immediate),
            Op::new("AND".to_string(), 0x25, 3, 2, MODE::ZeroPage),
            Op::new("AND".to_string(), 0x35, 4, 2, MODE::ZeroPageX),
            Op::new("AND".to_string(), 0x2D, 4, 3, MODE::Absolute),
            Op::new("AND".to_string(), 0x3D, 4, 3, MODE::AbsoluteX), //cycle + 1 if page crossed
            Op::new("AND".to_string(), 0x39, 4, 3, MODE::AbsoluteY), //cycle + 1 if page crossed
            Op::new("AND".to_string(), 0x21, 6, 2, MODE::IndirectX),
            Op::new("AND".to_string(), 0x31, 5, 2, MODE::IndirectY), //cycle + 1 if page crossed
            /* EOR  */
            Op::new("EOR".to_string(), 0x49, 2, 2, MODE::Immediate),
            Op::new("EOR".to_string(), 0x45, 3, 2, MODE::ZeroPage),
            Op::new("EOR".to_string(), 0x55, 4, 2, MODE::ZeroPageX),
            Op::new("EOR".to_string(), 0x4D, 4, 3, MODE::Absolute),
            Op::new("EOR".to_string(), 0x5D, 4, 3, MODE::AbsoluteX), //cycle + 1 if page crossed
            Op::new("EOR".to_string(), 0x59, 4, 3, MODE::AbsoluteY), //cycle + 1 if page crossed
            Op::new("EOR".to_string(), 0x41, 6, 2, MODE::IndirectX),
            Op::new("EOR".to_string(), 0x51, 5, 2, MODE::IndirectY), //cycle + 1 if page crossed
            /* ASL  */
            Op::new("ASL".to_string(), 0x0A, 2, 1, MODE::Accumulator),
            Op::new("ASL".to_string(), 0x06, 5, 2, MODE::ZeroPage),
            Op::new("ASL".to_string(), 0x16, 6, 2, MODE::ZeroPageX),
            Op::new("ASL".to_string(), 0x0E, 6, 3, MODE::Absolute),
            Op::new("ASL".to_string(), 0x1E, 7, 3, MODE::AbsoluteX),
            /* BCC  */
            Op::new("BCC".to_string(), 0x90, 2, 2, MODE::Relative),
            /* BCS  */
            Op::new("BCS".to_string(), 0xB0, 2, 2, MODE::Relative),
            /* BEQ  */
            Op::new("BEQ".to_string(), 0xF0, 2, 2, MODE::Relative),
            /* BNE  */
            Op::new("BNE".to_string(), 0xD0, 2, 2, MODE::Relative),
            /* BMI  */
            Op::new("BMI".to_string(), 0x30, 2, 2, MODE::Relative),
            /* BPL  */
            Op::new("BPL".to_string(), 0x10, 2, 2, MODE::Relative),
            /* BVC  */
            Op::new("BVC".to_string(), 0x50, 2, 2, MODE::Relative),
            /* BVS  */
            Op::new("BVS".to_string(), 0x70, 2, 2, MODE::Relative),
            /* BIT  */
            Op::new("BIT".to_string(), 0x24, 3, 2, MODE::ZeroPage),
            Op::new("BIT".to_string(), 0x2C, 4, 3, MODE::Absolute),
            /* CLC  CLD  CLI CLV  */
            Op::new("CLC".to_string(), 0x18, 2, 1, MODE::Implied),
            Op::new("CLD".to_string(), 0xD8, 2, 1, MODE::Implied),
            Op::new("CLI".to_string(), 0x58, 2, 1, MODE::Implied),
            Op::new("CLV".to_string(), 0xB8, 2, 1, MODE::Implied),
            /* CMP  */
            Op::new("CMP".to_string(), 0xC9, 2, 2, MODE::Immediate),
            Op::new("CMP".to_string(), 0xC5, 3, 2, MODE::ZeroPage),
            Op::new("CMP".to_string(), 0xD5, 4, 2, MODE::ZeroPageX),
            Op::new("CMP".to_string(), 0xCD, 4, 3, MODE::Absolute),
            Op::new("CMP".to_string(), 0xDD, 4, 3, MODE::AbsoluteX), //cycle + 1 if page crossed
            Op::new("CMP".to_string(), 0xD9, 4, 3, MODE::AbsoluteY), //cycle + 1 if page crossed
            Op::new("CMP".to_string(), 0xC1, 6, 2, MODE::IndirectX),
            Op::new("CMP".to_string(), 0xD1, 5, 2, MODE::IndirectY), //cycle + 1 if page crossed
            /* CPX  */
            Op::new("CPX".to_string(), 0xE0, 2, 2, MODE::Immediate),
            Op::new("CPX".to_string(), 0xE4, 3, 2, MODE::ZeroPage),
            Op::new("CPX".to_string(), 0xEC, 4, 3, MODE::Absolute),
            /* CPY  */
            Op::new("CPY".to_string(), 0xC0, 2, 2, MODE::Immediate),
            Op::new("CPY".to_string(), 0xC4, 3, 2, MODE::ZeroPage),
            Op::new("CPY".to_string(), 0xCC, 4, 3, MODE::Absolute),
            /* DEC  */
            Op::new("DEC".to_string(), 0xC6, 5, 2, MODE::ZeroPage),
            Op::new("DEC".to_string(), 0xD6, 6, 2, MODE::ZeroPageX),
            Op::new("DEC".to_string(), 0xCE, 6, 3, MODE::Absolute),
            Op::new("DEC".to_string(), 0xDE, 7, 3, MODE::AbsoluteX),
            /* DEX  */
            Op::new("DEX".to_string(), 0xCA, 2, 1, MODE::Implied),
            /* DEY  */
            Op::new("DEY".to_string(), 0x88, 2, 1, MODE::Implied),
            /* INC  */
            Op::new("INC".to_string(), 0xE6, 5, 2, MODE::ZeroPage),
            Op::new("INC".to_string(), 0xF6, 6, 2, MODE::ZeroPageX),
            Op::new("INC".to_string(), 0xEE, 6, 3, MODE::Absolute),
            Op::new("INC".to_string(), 0xFE, 7, 3, MODE::AbsoluteX),
            /* INX  */
            Op::new("INX".to_string(), 0xE8, 2, 1, MODE::Implied),
            /* INY  */
            Op::new("INY".to_string(), 0xC8, 2, 1, MODE::Implied),
            /* LDA  */
            Op::new("LDA".to_string(), 0xA9, 2, 2, MODE::Immediate),
            Op::new("LDA".to_string(), 0xA5, 3, 2, MODE::ZeroPage),
            Op::new("LDA".to_string(), 0xB5, 2, 2, MODE::ZeroPageX),
            Op::new("LDA".to_string(), 0xAD, 3, 2, MODE::Absolute),
            Op::new("LDA".to_string(), 0xBD, 2, 2, MODE::AbsoluteX),
            Op::new("LDA".to_string(), 0xB9, 3, 2, MODE::AbsoluteY),
            Op::new("LDA".to_string(), 0xA1, 2, 2, MODE::IndirectX),
            Op::new("LDA".to_string(), 0xB1, 3, 2, MODE::IndirectY),
            /* TAX  */
            Op::new("TAX".to_string(), 0xAA, 2, 1, MODE::Implied),
        ]))
        .ok();
}

impl TryFrom<u8> for Op {
    type Error = ();
    fn try_from(value: u8) -> Result<Op, Self::Error> {
        let mut ret: Op = OP_COLLECTION.get().unwrap()[0].clone();
        OP_COLLECTION
            .get()
            .unwrap()
            .iter()
            .map(|ele| if value == ele.code { Some(ele) } else { None })
            .for_each(|ele| {
                if let Some(op) = ele {
                    ret = op.clone();
                }
            });
        Ok(ret)
    }
}
