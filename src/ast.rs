use std::collections::HashMap;

use lalrpop_util::lalrpop_mod;

use crate::{
    anf::{AnfExpr, AnfTree, ImmExpr},
    common::{BinOp, ExprRef, FlatTree},
    instr::{Instr, Operand, Reg},
};
lalrpop_mod!(pub grammar);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
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
pub struct AST {
    tree: FlatTree<Expr>,
    entrypoint: ExprRef,
}

impl AST {
    pub fn from(input: &str) -> Self {
        let mut tree = FlatTree::<Expr>::new();
        let parser = grammar::ExprParser::new();
        let entrypoint = parser.parse(&mut tree, input).unwrap();

        Self { tree, entrypoint }
    }

    pub fn to_anf(&self) -> AnfTree {
        let mut tree = FlatTree::<AnfExpr>::new();

        let entrypoint = self.anf_helper(self.entrypoint, &mut tree);

        AnfTree::from(tree, entrypoint)
    }

    fn anf_helper(&self, expr: ExprRef, tree: &mut FlatTree<AnfExpr>) -> ExprRef {
        match self.tree.get(expr).unwrap() {
            Expr::Num(n) => tree.add(AnfExpr::Imm(ImmExpr::Num(*n))),
            Expr::Var(n) => tree.add(AnfExpr::Imm(ImmExpr::Var(n.clone()))),
            Expr::Neg(expr) => {
                let result = self.anf_helper(*expr, tree);
                tree.add(AnfExpr::Neg(result))
            }
            Expr::Let(var, assn, body) => {
                let assn_anf = self.anf_helper(*assn, tree);
                let body_anf = self.anf_helper(*body, tree);
                tree.add(AnfExpr::Let(var.clone(), assn_anf, body_anf))
            }
            Expr::If(cond, body, branch) => {
                let cond = self.anf_helper(*cond, tree);
                let body_anf = self.anf_helper(*body, tree);
                let branch_anf = self.anf_helper(*branch, tree);

                let cond_name = format!("cond_{}", cond.0);

                let if_expr = tree.add(AnfExpr::If(
                    ImmExpr::Var(cond_name.clone()),
                    body_anf,
                    branch_anf,
                ));

                tree.add(AnfExpr::Let(cond_name, cond, if_expr))
            }
            Expr::Bin(op, lhs, rhs) => {
                let left = self.anf_helper(*lhs, tree);
                let right = self.anf_helper(*rhs, tree);

                let lhs_name = format!("Binary_lhs_{}", left.0);
                let rhs_name = format!("Binary_rhs_{}", right.0);
                let result = tree.add(AnfExpr::Bin(
                    *op,
                    ImmExpr::Var(lhs_name.clone()),
                    ImmExpr::Var(rhs_name.clone()),
                ));
                let if1 = tree.add(AnfExpr::Let(rhs_name, right, result));

                tree.add(AnfExpr::Let(lhs_name, left, if1))
            }
        }
    }

    pub fn eval_input(input: &str) -> Option<i64> {
        let arena = AST::from(input);

        arena.interp()
    }

    fn subst(&self, expr: ExprRef, env: &HashMap<String, ExprRef>) -> ExprRef {
        match self.tree.get(expr).unwrap() {
            Expr::Var(var) => *env.get(var).unwrap(),
            _ => expr,
        }
    }

    fn eval(&self, expr: ExprRef, env: &HashMap<String, ExprRef>) -> Option<i64> {
        match self.tree.get(expr).unwrap() {
            Expr::Num(val) => Some(*val),
            Expr::Neg(expr) => self.eval(*expr, env).map(|x| -x),
            Expr::Let(var, expr, body) => {
                let mut env = env.clone();
                env.insert(var.clone(), self.subst(*expr, &env));

                self.eval(*body, &env)
            }
            Expr::Var(var) => self.eval(env.get(var).copied()?, env),
            Expr::Bin(op, lhs, rhs) => Some(match op {
                BinOp::Add => self.eval(*lhs, env)? + self.eval(*rhs, env)?,
                BinOp::Sub => self.eval(*lhs, env)? - self.eval(*rhs, env)?,
                BinOp::Mul => self.eval(*lhs, env)? * self.eval(*rhs, env)?,
                BinOp::Div => self.eval(*lhs, env)? / self.eval(*rhs, env)?,
            }),
            Expr::If(cond, body, branch) => {
                if self.eval(*cond, env)? != 0 {
                    self.eval(*body, env)
                } else {
                    self.eval(*branch, env)
                }
            }
        }
    }

    pub fn interp(&self) -> Option<i64> {
        let env = HashMap::new();
        self.eval(self.entrypoint, &env)
    }
}

#[cfg(test)]
mod test {

    use crate::ast::AST;

    fn test_interp_program(input: &str, expected: i64) {
        let ast = AST::from(input);

        assert_eq!(ast.interp(), Some(expected));
        assert_eq!(ast.interp(), ast.to_anf().interp());
    }

    #[test]
    fn interp_negative_number() {
        test_interp_program("-10", -10);
    }

    #[test]
    fn interp_addition_basic() {
        test_interp_program("5+5", 10);
    }

    #[test]
    fn interp_arithmetic_complex() {
        test_interp_program("5+12*5", 65);
    }

    #[test]
    fn interp_all_arithmetic() {
        test_interp_program("5+6*8-12/4", 50);
    }

    #[test]
    fn interp_let_basic() {
        test_interp_program("let x = 6 in { x }", 6);
    }

    #[test]
    fn interp_let_as_term() {
        test_interp_program("5 + let x = 5 in { x + 1 }", 11);
    }

    #[test]
    fn interp_shadowing1() {
        test_interp_program("let x = let x = 5 in {1 + x} in {x + 1}", 7);
    }

    #[test]
    fn interp_shadowing2() {
        test_interp_program("let x = 5 in {let x = 3 in {x + 1}}", 4);
    }

    #[test]
    fn simple_if() {
        test_interp_program("if 5 then {1} else {2}", 1);
    }
}
