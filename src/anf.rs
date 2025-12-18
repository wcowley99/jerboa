use crate::{
    common::{BinOp, ExprRef, FlatTree},
    instr::{Instr, Operand, Reg},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImmExpr {
    Num(i64),
    Var(String),
}

impl ImmExpr {
    pub fn to_asm(&self, env: &Vec<String>) -> Instr {
        match self {
            ImmExpr::Num(val) => Instr::mov(*val, Reg::RAX),
            ImmExpr::Var(var) => Instr::mov(
                Operand::local(env.iter().rposition(|x| x == var).unwrap()),
                Reg::RAX,
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnfExpr {
    Imm(ImmExpr),
    Neg(ExprRef),
    Bin(BinOp, ImmExpr, ImmExpr),
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

    fn eval_imm(&self, expr: &ImmExpr, env: &Vec<(String, i64)>) -> Option<i64> {
        match expr {
            ImmExpr::Num(n) => Some(*n),
            ImmExpr::Var(var) => self.lookup(var, env),
        }
    }

    fn eval(&self, expr: ExprRef, mut env: &mut Vec<(String, i64)>) -> Option<i64> {
        match self.tree.get(expr).unwrap() {
            AnfExpr::Imm(e) => self.eval_imm(e, env),
            AnfExpr::Neg(expr) => Some(-self.eval(*expr, env)?),
            AnfExpr::Let(var, assn, body) => {
                let assn_result = self.eval(*assn, env)?;
                env.push((var.clone(), assn_result));

                let result = self.eval(*body, &mut env);
                env.pop();

                result
            }
            AnfExpr::Bin(op, lhs, rhs) => Some(match op {
                BinOp::Add => self.eval_imm(lhs, env)? + self.eval_imm(rhs, env)?,
                BinOp::Sub => self.eval_imm(lhs, env)? - self.eval_imm(rhs, env)?,
                BinOp::Mul => self.eval_imm(lhs, env)? * self.eval_imm(rhs, env)?,
                BinOp::Div => self.eval_imm(lhs, env)? / self.eval_imm(rhs, env)?,
            }),
            AnfExpr::If(cond, body, branch) => {
                if self.eval_imm(cond, env)? != 0 {
                    self.eval(*body, env)
                } else {
                    self.eval(*branch, env)
                }
            }
        }
    }

    pub fn interp(&self) -> Option<i64> {
        let mut env = Vec::new();
        self.eval(self.entrypoint, &mut env)
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
            AnfExpr::If(cond, body, branch) => {
                let label_true = format!("if_zero{}", expr.0);
                let label_false = format!("if_nz{}", expr.0);
                let label_done = format!("done{}", expr.0);
                [
                    vec![
                        cond.to_asm(env),
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
