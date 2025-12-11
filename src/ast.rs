use std::{collections::HashMap, fmt::format};

use lalrpop_util::lalrpop_mod;

use crate::instr::{Instr, Operand, Reg};
lalrpop_mod!(pub grammar);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct ExprRef(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Add1(ExprRef),
    Neg(ExprRef),
    Bin(BinOp, ExprRef, ExprRef),
    Let(String, ExprRef, ExprRef),
    Var(String),
    If(ExprRef, ExprRef, ExprRef),
}

impl Expr {
    pub fn var<S: Into<String>>(name: S) -> Expr {
        Expr::Var(name.into())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Arena {
    program: Vec<Expr>,
    entrypoint: Option<ExprRef>,
}

impl Arena {
    pub fn from(input: &str) -> Self {
        let mut arena = Self {
            program: Vec::new(),
            entrypoint: None,
        };
        let parser = grammar::ExprParser::new();
        let entrypoint = parser.parse(&mut arena, input).unwrap();

        arena.entrypoint = Some(entrypoint);

        arena
    }

    pub fn eval_input(input: &str) -> i64 {
        let arena = Arena::from(input);

        arena.interp()
    }

    pub fn add(&mut self, expr: Expr) -> ExprRef {
        let index = self.program.len();
        self.program.push(expr);

        ExprRef(index)
    }

    fn get(&self, index: ExprRef) -> Result<&Expr, String> {
        self.program
            .get(index.0)
            .ok_or_else(|| format!("Failed to find Expr at index {}", index.0))
    }

    fn subst(&self, expr: ExprRef, env: &HashMap<String, ExprRef>) -> ExprRef {
        match self.get(expr).unwrap() {
            Expr::Var(var) => *env.get(var).unwrap(),
            _ => expr,
        }
    }

    fn eval(&self, expr: ExprRef, env: &HashMap<String, ExprRef>) -> i64 {
        match self.get(expr).unwrap() {
            Expr::Num(val) => *val,
            Expr::Add1(expr) => 1 + self.eval(*expr, env),
            Expr::Neg(expr) => -self.eval(*expr, env),
            Expr::Let(var, expr, body) => {
                let mut env = env.clone();
                env.insert(var.clone(), self.subst(*expr, &env));

                self.eval(*body, &env)
            }
            Expr::Var(var) => self.eval(env.get(var).unwrap().clone(), env),
            Expr::Bin(op, lhs, rhs) => match op {
                BinOp::Add => self.eval(*lhs, env) + self.eval(*rhs, env),
                BinOp::Sub => self.eval(*lhs, env) - self.eval(*rhs, env),
                BinOp::Mul => self.eval(*lhs, env) * self.eval(*rhs, env),
                BinOp::Div => self.eval(*lhs, env) / self.eval(*rhs, env),
            },
            Expr::If(cond, body, branch) => {
                if self.eval(*cond, env) != 0 {
                    self.eval(*body, env)
                } else {
                    self.eval(*branch, env)
                }
            }
        }
    }

    pub fn interp(&self) -> i64 {
        let env = HashMap::new();
        self.eval(self.entrypoint.unwrap(), &env)
    }

    pub fn compile(&self) -> Vec<Instr> {
        let mut env = Vec::new();
        self.to_asm(self.entrypoint.unwrap(), &mut env)
    }

    fn to_asm(&self, expr: ExprRef, mut env: &mut Vec<String>) -> Vec<Instr> {
        match self.get(expr).unwrap() {
            Expr::Num(val) => vec![Instr::mov(*val, Reg::RAX)],
            Expr::Add1(expr) => {
                let mut program = self.to_asm(*expr, env);
                program.push(Instr::add(1, Reg::RAX));
                program
            }
            Expr::Neg(expr) => {
                let mut program = self.to_asm(*expr, env);
                program.push(Instr::Neg(Reg::RAX));
                program
            }
            Expr::Let(var, assn, body) => {
                let assn_asm = self.to_asm(*assn, env);

                let pos = env.len();
                env.push(var.clone());

                let prog = [
                    assn_asm,
                    vec![Instr::mov(Reg::RAX, Operand::local(pos))],
                    self.to_asm(*body, &mut env),
                ]
                .concat();

                env.pop();

                prog
            }
            Expr::Var(var) => vec![Instr::mov(
                Operand::local(env.iter().rposition(|x| x == var).unwrap()),
                Reg::RAX,
            )],
            Expr::Bin(_, _, _) => todo!(),
            Expr::If(cond, body, branch) => {
                let label_true = format!("if_zero{}", expr.0);
                let label_false = format!("if_nz{}", expr.0);
                let label_done = format!("done{}", expr.0);
                [
                    self.to_asm(*cond, env),
                    vec![
                        Instr::cmp(0, Reg::RAX),
                        Instr::Je(label_false.clone()),
                        Instr::label(label_true),
                    ],
                    self.to_asm(*body, env),
                    vec![Instr::jmp(label_done.clone()), Instr::label(label_false)],
                    self.to_asm(*branch, env),
                    vec![Instr::label(label_done)],
                ]
                .concat()
            }
        }
    }
}

#[cfg(test)]
mod test {

    use crate::ast::Arena;

    #[test]
    fn interp_negative_number() {
        assert_eq!(Arena::eval_input("-10"), -10);
    }

    #[test]
    fn interp_addition_basic() {
        assert_eq!(Arena::eval_input("5+5"), 10);
    }

    #[test]
    fn interp_arithmetic_complex() {
        assert_eq!(Arena::eval_input("5+12*5"), 65);
    }

    #[test]
    fn interp_all_arithmetic() {
        assert_eq!(Arena::eval_input("5+6*8-12/4"), 50);
    }

    #[test]
    fn interp_let_basic() {
        assert_eq!(Arena::eval_input("let x = 6 in { x }"), 6);
    }

    #[test]
    fn interp_let_as_term() {
        assert_eq!(Arena::eval_input("5 + let x = 5 in { x + 1 }"), 11);
    }

    #[test]
    fn interp_shadowing1() {
        assert_eq!(
            Arena::eval_input("let x = let x = 5 in {1 + x} in {x + 1}"),
            7
        );
    }

    #[test]
    fn interp_shadowing2() {
        assert_eq!(Arena::eval_input("let x = 5 in {let x = 3 in {x + 1}}"), 4);
    }

    #[test]
    fn simple_if() {
        assert_eq!(Arena::eval_input("if 5 then {1} else {2}"), 1);
    }
}
