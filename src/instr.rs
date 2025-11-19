use std::fmt::format;

use crate::ast::Op;

#[derive(Debug, Copy, Clone)]
pub enum Reg {
    RAX,
    RCX,
    RDX,
    RBX,
    RSI,
    RDI,
    RSP,
    RBP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl ToString for Reg {
    fn to_string(&self) -> String {
        match *self {
            Reg::RAX => "%rax".into(),
            Reg::RCX => "%rcx".into(),
            Reg::RDX => "%rdx".into(),
            Reg::RBX => "%rbx".into(),
            Reg::RSI => "%rsi".into(),
            Reg::RDI => "%rdi".into(),
            Reg::RSP => "%rsp".into(),
            Reg::RBP => "%rbp".into(),
            Reg::R8 => "%r8".into(),
            Reg::R9 => "%r9".into(),
            Reg::R10 => "%r10".into(),
            Reg::R11 => "%r11".into(),
            Reg::R12 => "%r12".into(),
            Reg::R13 => "%r13".into(),
            Reg::R14 => "%r14".into(),
            Reg::R15 => "%r15".into(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Operand {
    Imm(i64),
    Reg(Reg),
    Mem(Option<i64>, Option<Reg>, Option<Reg>, Option<i64>),
}

impl Into<Operand> for i64 {
    fn into(self) -> Operand {
        Operand::Imm(self)
    }
}

impl Into<Operand> for Reg {
    fn into(self) -> Operand {
        Operand::Reg(self)
    }
}

impl ToString for Operand {
    fn to_string(&self) -> String {
        match *self {
            Operand::Imm(val) => format!("${}", val),
            Operand::Reg(reg) => format!("{}", reg.to_string()),
            Operand::Mem(disp, base, index, scale) => {
                let scale = match (index, scale) {
                    (Some(_), Some(s)) if s != 1 => s.to_string(),
                    _ => String::new(),
                };

                let disp = disp.map_or(String::new(), |d| d.to_string());
                let base = base.map_or(String::new(), |r| r.to_string());
                let index = index.map_or(String::new(), |r| r.to_string());

                if base.is_empty() && index.is_empty() {
                    return format!("{}()", disp);
                }

                let inside = if index.is_empty() {
                    base
                } else if scale.is_empty() {
                    format!("{},{}", base, index)
                } else {
                    format!("{},{},{}", base, index, scale)
                };

                format!("{}({})", disp, inside)
            }
        }
    }
}

impl Operand {
    pub fn local_variable(index: usize) -> Operand {
        Operand::Mem(Some(-8 * (index as i64)), Some(Reg::RSP), None, None)
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Mov(Operand, Operand),
    Add(Operand, Operand),
    Cmp(Operand, Operand),
    Je(String),
    Jmp(String),
    Label(String),
    Syscall,
}

impl ToString for Instr {
    fn to_string(&self) -> String {
        match self {
            Instr::Mov(s, d) => format!("mov {}, {}", s.to_string(), d.to_string()),
            Instr::Add(s, d) => format!("add {}, {}", s.to_string(), d.to_string()),
            Instr::Cmp(s, d) => format!("cmp {}, {}", s.to_string(), d.to_string()),
            Instr::Je(s) => format!("je {}", s),
            Instr::Jmp(s) => format!("jmp {}", s),
            Instr::Label(s) => format!("{}:", s),
            Instr::Syscall => "syscall".to_string(),
        }
    }
}

impl Instr {
    pub fn mov<S: Into<Operand>, D: Into<Operand>>(s: S, d: D) -> Instr {
        Instr::Mov(s.into(), d.into())
    }

    pub fn add<S: Into<Operand>, D: Into<Operand>>(s: S, d: D) -> Instr {
        Instr::Add(s.into(), d.into())
    }

    pub fn cmp<S: Into<Operand>, D: Into<Operand>>(s: S, d: D) -> Instr {
        Instr::Cmp(s.into(), d.into())
    }

    pub fn je<S: Into<String>>(s: S) -> Instr {
        Instr::Je(s.into())
    }

    pub fn jmp<S: Into<String>>(s: S) -> Instr {
        Instr::Jmp(s.into())
    }

    pub fn label<S: Into<String>>(s: S) -> Instr {
        Instr::Label(s.into())
    }
}
