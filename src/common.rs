use std::fmt::Display;

use crate::instr::{Instr, Reg};

pub enum Value {
    I64(i64),
    Bool(bool),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Leq,
    Geq,
    Lt,
    Gt,
}

impl BinOp {
    pub fn to_asm(&self) -> Vec<Instr> {
        let cmp = Instr::cmp(Reg::RCX, Reg::RAX);
        match self {
            BinOp::Add => vec![Instr::add(Reg::RCX, Reg::RAX)],
            BinOp::Sub => vec![Instr::sub(Reg::RCX, Reg::RAX)],
            BinOp::Mul => vec![Instr::imul(Reg::RCX, Reg::RAX)],
            BinOp::Div => todo!("Divide not implemented"),

            BinOp::Eq => vec![cmp, Instr::Sete(Reg::AL)],
            BinOp::Neq => vec![cmp, Instr::Setne(Reg::AL)],
            BinOp::Leq => vec![cmp, Instr::Setle(Reg::AL)],
            BinOp::Geq => vec![cmp, Instr::Setge(Reg::AL)],
            BinOp::Lt => vec![cmp, Instr::Setl(Reg::AL)],
            BinOp::Gt => vec![cmp, Instr::Setg(Reg::AL)],
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sym = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Eq => "==",
            BinOp::Neq => "!=",
            BinOp::Leq => "<=",
            BinOp::Geq => ">=",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
        };
        write!(f, "{}", sym)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct ExprRef(pub usize);

#[derive(Debug, PartialEq, Eq)]
pub struct FlatTree<T> {
    program: Vec<T>,
}

impl<T> FlatTree<T> {
    pub fn new() -> FlatTree<T> {
        Self {
            program: Vec::new(),
        }
    }

    pub fn get(&self, index: ExprRef) -> Result<&T, String> {
        self.program
            .get(index.0)
            .ok_or_else(|| format!("Failed to find Expr at index {}", index.0))
    }

    pub fn add(&mut self, x: T) -> ExprRef {
        let index = self.program.len();
        self.program.push(x);

        ExprRef(index)
    }
}
