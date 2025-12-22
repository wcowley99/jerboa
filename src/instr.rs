#[derive(Debug, Copy, Clone)]
pub enum Reg {
    // 8-bit registers
    AL,

    // 64-bit registers
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
            Reg::AL => "%al".into(),
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
    pub fn local(index: usize) -> Operand {
        Operand::Mem(Some(-8 * (index + 1) as i64), Some(Reg::RBP), None, None)
    }

    pub fn arg(index: usize) -> Operand {
        match index {
            0 => Reg::RDI.into(),
            1 => Reg::RSI.into(),
            2 => Reg::RDX.into(),
            3 => Reg::RCX.into(),
            4 => Reg::R8.into(),
            5 => Reg::R9.into(),
            _ => Operand::Mem(Some(8 * (index - 4) as i64), Some(Reg::RBP), None, None),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Mov(Operand, Operand),

    Add(Operand, Operand),
    Sub(Operand, Operand),
    IMul(Operand, Operand),

    Neg(Reg),

    Cmp(Operand, Operand),

    Je(String),
    Jmp(String),

    // TODO: set operations only work on byte registers, make sure to enforce this rule
    Sete(Reg),
    Setne(Reg),
    Setle(Reg),
    Setge(Reg),
    Setl(Reg),
    Setg(Reg),

    Label(String),
    Call(String),
    Push(Operand),
    Pop(Operand),
    Ret,
    Syscall,
}

impl ToString for Instr {
    fn to_string(&self) -> String {
        match self {
            Instr::Mov(s, d) => format!("mov {}, {}", s.to_string(), d.to_string()),

            Instr::Add(s, d) => format!("add {}, {}", s.to_string(), d.to_string()),
            Instr::Sub(s, d) => format!("sub {}, {}", s.to_string(), d.to_string()),
            Instr::IMul(s, d) => format!("imul {}, {}", s.to_string(), d.to_string()),

            Instr::Neg(s) => format!("neg {}", s.to_string()),

            Instr::Cmp(s, d) => format!("cmp {}, {}", s.to_string(), d.to_string()),

            Instr::Je(s) => format!("je {}", s),
            Instr::Jmp(s) => format!("jmp {}", s),

            Instr::Sete(r) => format!("sete {}", r.to_string()),
            Instr::Setne(r) => format!("setne {}", r.to_string()),
            Instr::Setle(r) => format!("setle {}", r.to_string()),
            Instr::Setge(r) => format!("setge {}", r.to_string()),
            Instr::Setl(r) => format!("setl {}", r.to_string()),
            Instr::Setg(r) => format!("setg {}", r.to_string()),

            Instr::Label(s) => format!("{}:", s),
            Instr::Call(s) => format!("call {}", s),
            Instr::Push(s) => format!("push {}", s.to_string()),
            Instr::Pop(s) => format!("pop {}", s.to_string()),
            Instr::Ret => "ret".to_string(),

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

    pub fn sub<S: Into<Operand>, D: Into<Operand>>(s: S, d: D) -> Instr {
        Instr::Sub(s.into(), d.into())
    }

    pub fn imul<S: Into<Operand>, D: Into<Operand>>(s: S, d: D) -> Instr {
        Instr::IMul(s.into(), d.into())
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

    pub fn push<T: Into<Operand>>(operand: T) -> Instr {
        Instr::Push(operand.into())
    }

    pub fn pop<T: Into<Operand>>(operand: T) -> Instr {
        Instr::Pop(operand.into())
    }
}
