use std::{collections::HashMap, hash::Hash, iter::Map};

use crate::instr::{Instr, Operand, Reg};

#[derive(Debug, Copy, Clone)]
pub enum Op {
    ADD,
    SUB,
    MUL,
    DIV,
}

impl ToString for Op {
    fn to_string(&self) -> String {
        match *self {
            Op::ADD => "+".to_string(),
            Op::SUB => "-".to_string(),
            Op::MUL => "*".to_string(),
            Op::DIV => "/".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Expr {
    Literal(i64),
    Add1(Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Id(String),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Into<Expr> for i64 {
    fn into(self) -> Expr {
        Expr::Literal(self)
    }
}

impl Expr {
    pub fn add1(expr: Expr) -> Expr {
        Expr::Add1(Box::new(expr))
    }

    pub fn id<S: Into<String>>(name: S) -> Expr {
        Expr::Id(name.into())
    }

    pub fn let_binding<T: Into<String>, U: Into<Expr>, V: Into<Expr>>(
        id: T,
        expr: U,
        body: V,
    ) -> Expr {
        Expr::Let(id.into(), Box::new(expr.into()), Box::new(body.into()))
    }

    pub fn conditional<T: Into<Expr>, U: Into<Expr>, V: Into<Expr>>(
        cond: T,
        body: U,
        branch: V,
    ) -> Expr {
        Expr::If(
            Box::new(cond.into()),
            Box::new(body.into()),
            Box::new(branch.into()),
        )
    }

    fn to_asm(&self, env: &HashMap<String, usize>) -> Vec<Instr> {
        match self {
            Expr::Literal(val) => vec![Instr::mov(*val, Reg::RAX)],
            Expr::Add1(expr) => {
                let mut program = expr.to_asm(env);
                program.push(Instr::add(1, Reg::RAX));
                program
            }
            Expr::Let(id, expr, body) => {
                let mut new_env = env.clone();
                let x = 1 + env.len();
                new_env.insert(id.clone(), x);

                [
                    expr.to_asm(env),
                    vec![Instr::mov(Reg::RAX, Operand::local_variable(x))],
                    body.to_asm(&new_env),
                ]
                .concat()
            }
            Expr::Id(id) => {
                vec![Instr::mov(
                    Operand::local_variable(*env.get(id).unwrap()),
                    Reg::RAX,
                )]
            }
            Expr::If(cond, body, branch) => {
                todo!()
            }
        }
    }

    pub fn compile(&self, env: HashMap<String, usize>) -> String {
        let preamble = ".text \n\
        .global _start \n\
        \n\
        _start:"
            .to_string();
        let envoi = vec![
            Instr::mov(Reg::RAX, Reg::RDI),
            Instr::mov(60, Reg::RAX),
            Instr::Syscall,
        ];
        let program = [self.to_asm(&env), envoi].concat();

        format!(
            "{}\n",
            [
                preamble,
                program
                    .iter()
                    .map(|n| n.to_string())
                    .collect::<Vec<_>>()
                    .join("\n"),
            ]
            .join("\n")
        )
    }

    pub fn subst(&self, env: &HashMap<String, Expr>) -> Expr {
        match self {
            Expr::Id(id) => env.get(id).unwrap().clone(),
            _ => self.clone(),
        }
    }

    pub fn eval(&self, env: &HashMap<String, Expr>) -> i64 {
        match self {
            Expr::Literal(val) => *val,
            Expr::Add1(expr) => 1 + expr.eval(env),
            Expr::Let(id, expr, body) => {
                let mut env = env.clone();
                env.insert(id.clone(), expr.subst(&env));

                body.eval(&env)
            }
            Expr::Id(id) => env.get(id).unwrap().clone().eval(env),
            Expr::If(cond, body, branch) => {
                if cond.eval(env) != 0 {
                    body.eval(env)
                } else {
                    branch.eval(env)
                }
            }
        }
    }

    pub fn interp(&self) -> i64 {
        let env = HashMap::new();
        self.eval(&env)
    }
}
