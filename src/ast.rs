use std::env::Args;

use lalrpop_util::lalrpop_mod;

use crate::{
    anf::{AnfExpr, AnfTree, ImmExpr},
    common::{BinOp, CmpOp, ExprRef, FlatTree},
};
lalrpop_mod!(pub grammar);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Bool(bool),
    Var(String),

    Neg(ExprRef),
    Bin(BinOp, ExprRef, ExprRef),
    Let(String, ExprRef, ExprRef),
    If(ExprRef, ExprRef, ExprRef),
    Cmp(CmpOp, ExprRef, ExprRef),

    FnCall(String, Vec<ExprRef>),
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
        let parser = grammar::ProgramParser::new();
        let entrypoint = parser.parse(&mut tree, input).unwrap();

        Self { tree, entrypoint }
    }

    pub fn to_anf(&self) -> AnfTree {
        let mut tree = FlatTree::<AnfExpr>::new();

        let entrypoint = self.anf_c(self.entrypoint, &mut tree);

        AnfTree::from(tree, entrypoint)
    }

    fn anf_c(&self, expr: ExprRef, tree: &mut FlatTree<AnfExpr>) -> ExprRef {
        match self.tree.get(expr).unwrap() {
            Expr::Num(n) => tree.add(AnfExpr::Imm(ImmExpr::Num(*n))),
            Expr::Bool(b) => tree.add(AnfExpr::Imm(ImmExpr::Bool(*b))),
            Expr::Var(n) => tree.add(AnfExpr::Imm(ImmExpr::Var(n.clone()))),
            Expr::Neg(expr) => {
                let result = self.anf_c(*expr, tree);
                tree.add(AnfExpr::Neg(result))
            }
            Expr::Let(var, assn, body) => {
                let assn_anf = self.anf_c(*assn, tree);
                let body_anf = self.anf_c(*body, tree);
                tree.add(AnfExpr::Let(var.clone(), assn_anf, body_anf))
            }
            Expr::If(cond, body, branch) => {
                let cond = self.anf_c(*cond, tree);
                let body_anf = self.anf_c(*body, tree);
                let branch_anf = self.anf_c(*branch, tree);

                let cond_name = format!("cond_{}", cond.0);

                let if_expr = tree.add(AnfExpr::If(
                    ImmExpr::Var(cond_name.clone()),
                    body_anf,
                    branch_anf,
                ));

                tree.add(AnfExpr::Let(cond_name, cond, if_expr))
            }
            Expr::Bin(op, lhs, rhs) => {
                let (left, left_context) = self.anf_i(*lhs, tree);
                let (right, right_context) = self.anf_i(*rhs, tree);

                let mut result = tree.add(AnfExpr::Bin(*op, left, right));

                if let Some((new_arg, context)) = right_context {
                    result = tree.add(AnfExpr::Let(new_arg, context, result));
                }

                if let Some((new_arg, context)) = left_context {
                    result = tree.add(AnfExpr::Let(new_arg, context, result));
                }

                result
            }
            Expr::Cmp(cmp, lhs, rhs) => {
                let (left, left_context) = self.anf_i(*lhs, tree);
                let (right, right_context) = self.anf_i(*rhs, tree);

                let mut result = tree.add(AnfExpr::Cmp(*cmp, left, right));

                if let Some((new_arg, context)) = right_context {
                    result = tree.add(AnfExpr::Let(new_arg, context, result));
                }

                if let Some((new_arg, context)) = left_context {
                    result = tree.add(AnfExpr::Let(new_arg, context, result));
                }

                result
            }
            Expr::FnCall(name, args) => {
                let mut lets = Vec::new();
                let mut new_args = Vec::new();
                for arg in args {
                    let (new_arg, context) = self.anf_i(*arg, tree);
                    if let Some(context) = context {
                        lets.push(context);
                    }
                    new_args.push(new_arg);
                }
                let mut result = tree.add(AnfExpr::Fn(name.clone(), new_args));

                for (name, assn) in lets.into_iter().rev() {
                    result = tree.add(AnfExpr::Let(name, assn, result));
                }

                result
            }
        }
    }

    fn anf_i(
        &self,
        expr: ExprRef,
        tree: &mut FlatTree<AnfExpr>,
    ) -> (ImmExpr, Option<(String, ExprRef)>) {
        match self.tree.get(expr).unwrap() {
            Expr::Num(n) => (ImmExpr::Num(*n), None),
            Expr::Bool(b) => (ImmExpr::Bool(*b), None),
            Expr::Var(s) => (ImmExpr::Var(s.clone()), None),
            _ => {
                let name = format!("y{}", expr.0);
                (
                    ImmExpr::Var(name.clone()),
                    Some((name, self.anf_c(expr, tree))),
                )
            }
        }
    }

    pub fn rename(&self) -> Result<AST, String> {
        let mut renamed = FlatTree::<Expr>::new();
        let mut env = Vec::new();
        let mut count = 0;
        let entrypoint = self.rename_helper(&mut renamed, self.entrypoint, &mut env, &mut count);

        if let Some(e) = entrypoint {
            Ok(AST {
                tree: renamed,
                entrypoint: e,
            })
        } else {
            Err("Failed to rename AST.".to_string())
        }
    }

    fn rename_lookup(&self, var: &String, env: &Vec<(String, String)>) -> Option<String> {
        env.iter()
            .rfind(|(s, _)| s == var)
            .map(|(_, renamed)| renamed)
            .cloned()
    }

    fn rename_helper(
        &self,
        mut renamed: &mut FlatTree<Expr>,
        expr: ExprRef,
        env: &mut Vec<(String, String)>,
        mut count: &mut usize,
    ) -> Option<ExprRef> {
        Some(match self.tree.get(expr).unwrap() {
            Expr::Num(n) => renamed.add(Expr::Num(*n)),
            Expr::Bool(b) => renamed.add(Expr::Bool(*b)),
            Expr::Var(s) => renamed.add(Expr::Var(self.rename_lookup(s, env)?)),
            Expr::Neg(e) => {
                let expr = self.rename_helper(renamed, *e, env, count)?;

                renamed.add(Expr::Neg(expr))
            }
            Expr::Bin(op, lhs, rhs) => {
                let lhs_renamed = self.rename_helper(renamed, *lhs, env, count)?;
                let rhs_renamed = self.rename_helper(renamed, *rhs, env, count)?;

                renamed.add(Expr::Bin(*op, lhs_renamed, rhs_renamed))
            }
            Expr::Let(s, assn, body) => {
                let assn_expr = self.rename_helper(&mut renamed, *assn, env, &mut count)?;

                env.push((s.clone(), format!("x{}", count)));
                *count += 1;

                let body_expr = self.rename_helper(&mut renamed, *body, env, &mut count)?;

                let (_, var) = env.pop()?;

                renamed.add(Expr::Let(var, assn_expr, body_expr))
            }
            Expr::If(cond, body, branch) => {
                let cond_expr = self.rename_helper(renamed, *cond, env, count)?;
                let body_expr = self.rename_helper(renamed, *body, env, count)?;
                let branch_expr = self.rename_helper(renamed, *branch, env, count)?;

                renamed.add(Expr::If(cond_expr, body_expr, branch_expr))
            }
            Expr::Cmp(cmp, lhs, rhs) => {
                let lhs_renamed = self.rename_helper(renamed, *lhs, env, count)?;
                let rhs_renamed = self.rename_helper(renamed, *rhs, env, count)?;

                renamed.add(Expr::Cmp(*cmp, lhs_renamed, rhs_renamed))
            }
            Expr::FnCall(s, args) => {
                let args = args
                    .iter()
                    .map(|e| self.rename_helper(renamed, *e, env, count))
                    .collect::<Option<Vec<_>>>()?;

                renamed.add(Expr::FnCall(s.clone(), args))
            }
        })
    }
}
