use std::fmt::Display;

use crate::instr::{Instr, Operand, Reg};

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
    pub fn to_asm(&self, lhs: Operand, rhs: Operand) -> Vec<Instr> {
        let cmp = Instr::cmp(rhs, lhs);
        match self {
            BinOp::Add => vec![Instr::add(rhs, lhs)],
            BinOp::Sub => vec![Instr::sub(rhs, lhs)],
            BinOp::Mul => vec![Instr::imul(rhs, lhs)],
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env {
    locals: Vec<String>,
    args: Vec<String>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            args: Vec::new(),
        }
    }

    pub fn push(&mut self, local: String) {
        self.locals.push(local);
    }

    pub fn pop(&mut self) {
        self.locals.pop();
    }

    pub fn set_args(&mut self, args: &Vec<String>) {
        self.args = args.clone();
    }

    pub fn clear_args(&mut self) {
        self.args = Vec::new();
    }

    pub fn lookup(&self, var: &String) -> Option<Operand> {
        let local = self
            .locals
            .iter()
            .rposition(|l| l == var)
            .map(|i| Operand::local(i));
        if local.is_some() {
            local
        } else {
            let args = self
                .args
                .iter()
                .position(|a| a == var)
                .map(|i| Operand::arg(i));

            args
        }
    }

    pub fn num_locals(&self) -> usize {
        self.locals.len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameGen {
    names: Vec<(String, String)>,
    local_counter: usize,
    arg_counter: usize,
}

impl NameGen {
    pub fn new() -> Self {
        Self {
            names: Vec::new(),
            local_counter: 0,
            arg_counter: 0,
        }
    }

    pub fn push_arg(&mut self, name: &String) -> String {
        let new_name = format!("arg{}", self.arg_counter);
        self.arg_counter += 1;

        self.names.push((name.clone(), new_name.clone()));

        new_name
    }

    pub fn push_local(&mut self, name: &String) -> String {
        let new_name = format!("x{}", self.local_counter);
        self.local_counter += 1;

        self.names.push((name.clone(), new_name.clone()));

        new_name
    }

    pub fn lookup(&self, name: &String) -> String {
        self.names
            .iter()
            .rfind(|(old, _)| old == name)
            .map(|(_, new)| new.clone())
            .unwrap()
    }

    pub fn pop(&mut self) {
        self.names.pop();
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct ExprRef(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub name: String,
    pub args: Vec<String>,
    pub body: ExprRef,
}

impl FnDecl {
    pub fn new(name: String, args: Vec<String>, body: ExprRef) -> Self {
        Self { name, args, body }
    }
}

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
