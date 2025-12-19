use std::fmt::Display;

use crate::{
    common::{BinOp, CmpOp, ExprRef, FlatTree},
    instr::{Instr, Operand, Reg},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImmExpr {
    Num(i64),
    Bool(bool),
    Var(String),
}

impl ImmExpr {
    pub fn to_asm(&self, env: &Vec<String>) -> Instr {
        match self {
            ImmExpr::Num(val) => Instr::mov(*val, Reg::RAX),
            ImmExpr::Bool(b) => {
                let repr = if *b { 1 } else { 0 };
                Instr::mov(repr, Reg::RAX)
            }
            ImmExpr::Var(var) => Instr::mov(
                Operand::local(env.iter().rposition(|x| x == var).unwrap()),
                Reg::RAX,
            ),
        }
    }
}

impl Display for ImmExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImmExpr::Num(n) => write!(f, "{}", n),
            ImmExpr::Bool(b) => write!(f, "{}", b),
            ImmExpr::Var(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnfExpr {
    Imm(ImmExpr),
    Neg(ExprRef),
    Bin(BinOp, ImmExpr, ImmExpr),
    Cmp(CmpOp, ImmExpr, ImmExpr),
    Let(String, ExprRef, ExprRef),
    If(ImmExpr, ExprRef, ExprRef),
}

#[derive(Debug, PartialEq, Eq)]
pub struct AnfTree {
    tree: FlatTree<AnfExpr>,
    entrypoint: ExprRef,
}

impl AnfTree {
    pub fn from(tree: FlatTree<AnfExpr>, entrypoint: ExprRef) -> Self {
        Self { tree, entrypoint }
    }
    fn lookup(&self, var: &String, env: &Vec<(String, i64)>) -> Option<i64> {
        env.iter().rfind(|(s, _)| s == var).map(|(_, n)| *n)
    }

    pub fn compile(&self) -> Vec<Instr> {
        let mut env = Vec::new();
        self.to_asm(self.entrypoint, &mut env)
    }

    fn to_asm(&self, expr: ExprRef, mut env: &mut Vec<String>) -> Vec<Instr> {
        match self.tree.get(expr).unwrap() {
            AnfExpr::Imm(e) => vec![e.to_asm(env)],
            AnfExpr::Neg(expr) => {
                let mut program = self.to_asm(*expr, env);
                program.push(Instr::Neg(Reg::RAX));
                program
            }
            AnfExpr::Let(var, assn, body) => {
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
            AnfExpr::Bin(op, lhs, rhs) => {
                let op_instr = match op {
                    BinOp::Add => Instr::add(Reg::RCX, Reg::RAX),
                    BinOp::Sub => Instr::sub(Reg::RCX, Reg::RAX),
                    BinOp::Mul => Instr::imul(Reg::RCX, Reg::RAX),
                    BinOp::Div => todo!("Divide not implemented"),
                };

                vec![
                    rhs.to_asm(env),
                    Instr::mov(Reg::RAX, Reg::RCX),
                    lhs.to_asm(env),
                    op_instr,
                ]
            }
            AnfExpr::Cmp(cmp, lhs, rhs) => {
                let op_instr = match cmp {
                    CmpOp::Eq => Instr::Sete(Reg::AL),
                    CmpOp::Neq => Instr::Setne(Reg::AL),
                    CmpOp::Leq => Instr::Setle(Reg::AL),
                    CmpOp::Geq => Instr::Setge(Reg::AL),
                    CmpOp::Lt => Instr::Setl(Reg::AL),
                    CmpOp::Gt => Instr::Setg(Reg::AL),
                };

                vec![
                    rhs.to_asm(env),
                    Instr::mov(Reg::RAX, Reg::RCX),
                    lhs.to_asm(env),
                    Instr::cmp(Reg::RCX, Reg::RAX),
                    op_instr,
                ]
            }
            AnfExpr::If(cond, body, branch) => {
                let label_true = format!("if_zero{}", expr.0);
                let label_false = format!("if_nz{}", expr.0);
                let label_done = format!("done{}", expr.0);
                [
                    vec![
                        cond.to_asm(env),
                        Instr::cmp(0, Reg::AL),
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

    pub fn print(&self) {
        self.print_helper(self.entrypoint, 0);
    }

    fn print_helper(&self, expr: ExprRef, depth: usize) {
        match self.tree.get(expr).unwrap() {
            AnfExpr::Imm(e) => print!("{}", e),
            AnfExpr::Neg(e) => {
                print!("-");
                self.print_helper(*e, depth);
            }
            AnfExpr::Bin(op, lhs, rhs) => {
                print!("{} {} {}", op, lhs, rhs)
            }
            AnfExpr::Cmp(cmp, lhs, rhs) => {
                print!("{} {} {}", cmp, lhs, rhs)
            }
            AnfExpr::Let(var, assn, body) => {
                print!("let {} = ", var);
                self.print_helper(*assn, depth);
                println!("in");
                self.print_helper(*body, depth);
            }
            AnfExpr::If(cond, body, branch) => {
                println!("if {}:", cond);
                self.print_helper(*body, depth);
                println!("else:");
                self.print_helper(*branch, depth);
            }
        }
    }
}
