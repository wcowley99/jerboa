use std::fmt::Display;

use crate::{
    error::TypeError,
    instr::{Instr, Operand, Reg},
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Tag {
    pub begin: usize,
    pub end: usize,
}

impl Tag {
    pub fn new(begin: usize, end: usize) -> Self {
        Tag { begin, end }
    }
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

    pub fn type_check(&self, lhs: Type, rhs: Type, tag: Tag) -> Result<Type, Vec<TypeError>> {
        match self {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                if lhs == Type::I64 && rhs == Type::I64 {
                    Ok(Type::I64)
                } else {
                    Err(vec![TypeError::IncorrectTypes(
                        Type::I64,
                        vec![lhs, rhs],
                        tag,
                    )])
                }
            }
            BinOp::Eq | BinOp::Neq => {
                if lhs == rhs {
                    Ok(Type::Bool)
                } else {
                    Err(vec![TypeError::TypeMismatch(lhs, rhs, tag)])
                }
            }
            BinOp::Leq | BinOp::Geq | BinOp::Lt | BinOp::Gt => {
                if lhs == Type::I64 && rhs == Type::I64 {
                    Ok(Type::Bool)
                } else {
                    Err(vec![TypeError::IncorrectTypes(
                        Type::I64,
                        vec![lhs, rhs],
                        tag,
                    )])
                }
            }
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
    locals: Vec<(String, Type)>,
    args: Vec<(String, Type)>,
    fns: Vec<(String, Vec<Type>, Type)>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            args: Vec::new(),
            fns: Vec::new(),
        }
    }

    pub fn push(&mut self, local: String, typ: Type) {
        self.locals.push((local, typ));
    }

    pub fn pop(&mut self) {
        self.locals.pop();
    }

    pub fn set_args(&mut self, args: &Vec<(String, Type)>) {
        self.args = args.clone();
    }

    pub fn clear_args(&mut self) {
        self.args = Vec::new();
    }

    fn contains_fn(&self, name: &String, args: &Vec<Type>) -> bool {
        self.fns.iter().any(|(n, a, _)| name == n && args == a)
    }

    pub fn add_func(&mut self, name: &String, args: &Vec<Type>, ret: Type) -> bool {
        if self.contains_fn(name, args) {
            false
        } else {
            self.fns.push((name.clone(), args.clone(), ret));
            true
        }
    }

    pub fn func_type(&self, name: &String, args: &Vec<Type>) -> Option<Type> {
        self.fns
            .iter()
            .find(|(n, a, _)| name == n && args == a)
            .map(|(_, _, t)| *t)
    }

    pub fn into_operand(&self, var: &String) -> Option<Operand> {
        let local = self
            .locals
            .iter()
            .rposition(|(l, _)| l == var)
            .map(|i| Operand::local(i));

        if local.is_some() {
            local
        } else {
            let args = self
                .args
                .iter()
                .position(|(a, _)| a == var)
                .map(|i| Operand::arg(i));

            args
        }
    }

    pub fn type_of(&self, var: &String) -> Option<Type> {
        let local = self.locals.iter().rfind(|(l, _)| l == var).map(|(_, t)| *t);

        if local.is_some() {
            local
        } else {
            self.args.iter().find(|(a, _)| a == var).map(|(_, t)| *t)
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    I64,
    Bool,
    Any,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I64 => write!(f, "i64"),
            Type::Bool => write!(f, "bool"),
            Type::Any => write!(f, "any"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub body: ExprRef,
    pub ret: Type,
    pub tag: Tag,
}

impl FnDecl {
    pub fn new(
        name: String,
        args: Vec<(String, Type)>,
        body: ExprRef,
        ret: Type,
        tag: Tag,
    ) -> Self {
        Self {
            name,
            args,
            body,
            ret,
            tag,
        }
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
