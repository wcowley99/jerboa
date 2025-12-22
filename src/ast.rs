use lalrpop_util::lalrpop_mod;

use crate::{
    anf::{AnfExpr, AnfTree, ImmExpr},
    common::{BinOp, Env, ExprRef, FlatTree, FnDecl, NameGen},
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
    functions: Vec<FnDecl>,
    entrypoint: ExprRef,
}

impl AST {
    /// Parses source code into an Abstract Syntax Tree
    pub fn from(input: &str) -> Self {
        let mut tree = FlatTree::<Expr>::new();
        let parser = grammar::ProgramParser::new();
        let (functions, entrypoint) = parser.parse(&mut tree, input).unwrap();

        Self {
            tree,
            functions,
            entrypoint,
        }
    }

    /// Converts this AST into [A-normal form](https://en.wikipedia.org/wiki/A-normal_form)
    ///
    /// Assuming that the AST has been properly type-checked and scope-checked, a transformation to
    /// ANF should be infallible.
    pub fn to_anf(&self) -> AnfTree {
        let mut tree = FlatTree::<AnfExpr>::new();

        let functions = self
            .functions
            .iter()
            .map(|decl| {
                let mut env = Env::new();
                FnDecl::new(
                    decl.name.clone(),
                    decl.args.clone(),
                    self.anf_c(decl.body, &mut tree, &mut env),
                )
            })
            .collect::<Vec<_>>();

        let mut env = Env::new();
        let entrypoint = self.anf_c(self.entrypoint, &mut tree, &mut env);

        AnfTree::from(tree, functions, entrypoint)
    }

    /// Converts an Expression into a complex ANF expression.
    ///
    /// Recursively translates each `Expr` into an `AnfExpr`. Returns the index of the new
    /// `AnfExpr` in `tree`. This function is mutually recursive with `anf_i`, and needs to manage
    /// the environment by popping from `env` as it leaves let-bindings.
    fn anf_c(&self, expr: ExprRef, tree: &mut FlatTree<AnfExpr>, env: &mut Env) -> ExprRef {
        match self.tree.get(expr).unwrap() {
            Expr::Num(n) => tree.add(AnfExpr::Imm(ImmExpr::Num(*n))),
            Expr::Bool(b) => tree.add(AnfExpr::Imm(ImmExpr::Bool(*b))),
            Expr::Var(n) => tree.add(AnfExpr::Imm(ImmExpr::Var(n.clone()))),
            Expr::Neg(expr) => {
                let result = self.anf_c(*expr, tree, env);
                tree.add(AnfExpr::Neg(result))
            }
            Expr::Let(var, assn, body) => {
                let assn_anf = self.anf_c(*assn, tree, env);
                let body_anf = self.anf_c(*body, tree, env);
                tree.add(AnfExpr::Let(var.clone(), assn_anf, body_anf))
            }
            Expr::If(cond, body, branch) => {
                let (cond, context) = self.anf_i(*cond, tree, env);
                let body_anf = self.anf_c(*body, tree, env);
                let branch_anf = self.anf_c(*branch, tree, env);

                let if_expr = tree.add(AnfExpr::If(cond, body_anf, branch_anf));

                if let Some((var, assn)) = context {
                    env.pop();
                    tree.add(AnfExpr::Let(var, assn, if_expr))
                } else {
                    if_expr
                }
            }
            Expr::Bin(op, lhs, rhs) => {
                let (left, left_context) = self.anf_i(*lhs, tree, env);
                let (right, right_context) = self.anf_i(*rhs, tree, env);

                let mut result = tree.add(AnfExpr::Bin(*op, left, right));

                if let Some((new_arg, context)) = right_context {
                    env.pop();
                    result = tree.add(AnfExpr::Let(new_arg, context, result));
                }

                if let Some((new_arg, context)) = left_context {
                    env.pop();
                    result = tree.add(AnfExpr::Let(new_arg, context, result));
                }

                result
            }
            Expr::FnCall(name, args) => {
                let mut lets = Vec::new();
                let mut new_args = Vec::new();
                for arg in args {
                    let (new_arg, context) = self.anf_i(*arg, tree, env);
                    if let Some(context) = context {
                        lets.push(context);
                    }
                    new_args.push(new_arg);
                }
                let mut result = tree.add(AnfExpr::Fn(name.clone(), new_args));

                for (name, assn) in lets.into_iter().rev() {
                    env.pop();
                    result = tree.add(AnfExpr::Let(name, assn, result));
                }

                result
            }
        }
    }

    /// Converts an `Expr`` into an immediate expression with the required context to evaluate it.
    ///
    /// This function is mutually recursive with `anf_c`, and expects that the calling function
    /// will construct a let-binding for the context, in the case that such a context is needed.
    fn anf_i(
        &self,
        expr: ExprRef,
        tree: &mut FlatTree<AnfExpr>,
        env: &mut Env,
    ) -> (ImmExpr, Option<(String, ExprRef)>) {
        match self.tree.get(expr).unwrap() {
            Expr::Num(n) => (ImmExpr::Num(*n), None),
            Expr::Bool(b) => (ImmExpr::Bool(*b), None),
            Expr::Var(s) => (ImmExpr::Var(s.clone()), None),
            _ => {
                let name = format!("y{}", expr.0);
                (
                    ImmExpr::Var(name.clone()),
                    Some((name, self.anf_c(expr, tree, env))),
                )
            }
        }
    }

    pub fn rename(&self) -> Result<AST, String> {
        let mut renamed = FlatTree::<Expr>::new();
        let mut name_gen = NameGen::new();

        let functions = self
            .functions
            .iter()
            .map(|decl| {
                let mut new_args = Vec::new();
                for arg in &decl.args {
                    new_args.push(name_gen.push_arg(arg));
                }

                let new_decl = FnDecl::new(
                    decl.name.clone(),
                    new_args,
                    self.rename_helper(&mut renamed, decl.body, &mut name_gen)
                        .unwrap(),
                );

                for _ in &decl.args {
                    name_gen.pop();
                }

                new_decl
            })
            .collect::<Vec<_>>();

        let entrypoint = self.rename_helper(&mut renamed, self.entrypoint, &mut name_gen);

        if let Some(e) = entrypoint {
            Ok(AST {
                tree: renamed,
                functions,
                entrypoint: e,
            })
        } else {
            Err("Failed to rename AST.".to_string())
        }
    }

    fn rename_helper(
        &self,
        mut renamed: &mut FlatTree<Expr>,
        expr: ExprRef,
        name_gen: &mut NameGen,
    ) -> Option<ExprRef> {
        Some(match self.tree.get(expr).unwrap() {
            Expr::Num(n) => renamed.add(Expr::Num(*n)),
            Expr::Bool(b) => renamed.add(Expr::Bool(*b)),
            Expr::Var(s) => renamed.add(Expr::Var(name_gen.lookup(s))),
            Expr::Neg(e) => {
                let expr = self.rename_helper(renamed, *e, name_gen)?;

                renamed.add(Expr::Neg(expr))
            }
            Expr::Bin(op, lhs, rhs) => {
                let lhs_renamed = self.rename_helper(renamed, *lhs, name_gen)?;
                let rhs_renamed = self.rename_helper(renamed, *rhs, name_gen)?;

                renamed.add(Expr::Bin(*op, lhs_renamed, rhs_renamed))
            }
            Expr::Let(s, assn, body) => {
                let assn_expr = self.rename_helper(&mut renamed, *assn, name_gen)?;

                let new_name = name_gen.push_local(s);

                let body_expr = self.rename_helper(&mut renamed, *body, name_gen)?;

                name_gen.pop();

                renamed.add(Expr::Let(new_name, assn_expr, body_expr))
            }
            Expr::If(cond, body, branch) => {
                let cond_expr = self.rename_helper(renamed, *cond, name_gen)?;
                let body_expr = self.rename_helper(renamed, *body, name_gen)?;
                let branch_expr = self.rename_helper(renamed, *branch, name_gen)?;

                renamed.add(Expr::If(cond_expr, body_expr, branch_expr))
            }
            Expr::FnCall(s, params) => {
                let params = params
                    .iter()
                    .map(|e| self.rename_helper(renamed, *e, name_gen))
                    .collect::<Option<Vec<_>>>()?;

                renamed.add(Expr::FnCall(s.clone(), params))
            }
        })
    }
}
