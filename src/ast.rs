use lalrpop_util::{ParseError, lalrpop_mod};

use crate::{
    anf::{AnfExpr, AnfTree, ImmExpr},
    common::{BinOp, Env, ExprRef, FlatTree, FnDecl, NameGen, Tag, Type},
    error::{TypeError, format_error, merge_errors},
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
    tree: FlatTree<(Tag, Expr)>,
    functions: Vec<FnDecl>,
    entrypoint: ExprRef,
}

impl AST {
    /// Parses source code into an Abstract Syntax Tree
    pub fn from<'a>(input: &'a str) -> Result<Self, String> {
        let mut tree = FlatTree::<(Tag, Expr)>::new();
        let parser = grammar::ProgramParser::new();
        let result = parser.parse(&mut tree, input);

        match result {
            Err(ParseError::InvalidToken { location }) => {
                let err = format_error(input, location, None, "Invalid Token", None);

                Err(err)
            }
            Err(ParseError::UnrecognizedEof { location, expected }) => {
                let err = format_error(
                    input,
                    location,
                    None,
                    &format!("Unexpected end of file, expected one of {:?}", expected),
                    None,
                );

                Err(err)
            }
            Err(ParseError::UnrecognizedToken { token, expected }) => {
                let (begin, tok, end) = token;

                let err = format_error(
                    input,
                    begin,
                    Some(end),
                    &format!(
                        "Unexpected token: '{}', expected one of {:?}",
                        tok.1, expected
                    ),
                    None,
                );

                Err(err)
            }
            Err(ParseError::ExtraToken { token: _ }) => {
                todo!("Implement ParseError::ExtraToken error handling")
            }
            Err(ParseError::User { error }) => Err(error.to_string()),
            Ok((fns, entry)) => Ok(Self {
                tree,
                functions: fns,
                entrypoint: entry,
            }),
        }
    }

    fn get_expr(&self, expr: ExprRef) -> &(Tag, Expr) {
        self.tree.get(expr).unwrap()
    }

    pub fn type_check(&self, source: &str) -> Result<(), Vec<TypeError>> {
        let mut env = Env::new();

        for decl in &self.functions {
            let args = decl.args.iter().map(|(_, t)| *t).collect();
            if !env.add_func(&decl.name, &args, decl.ret) {
                return Err(vec![TypeError::DuplicateFn(
                    decl.name.clone(),
                    args,
                    decl.tag,
                )]);
            }
        }

        let functions = self
            .functions
            .iter()
            .map(|f| {
                env.set_args(&f.args);

                let result = match self.type_check_expr(f.body, &mut env, source) {
                    Ok(t) => {
                        if t == f.ret {
                            Ok(())
                        } else {
                            Err(vec![TypeError::IncorrectReturnType(
                                f.name.clone(),
                                f.ret,
                                t,
                                f.tag,
                            )])
                        }
                    }
                    Err(e) => Err(e),
                };

                env.clear_args();

                result
            })
            .reduce(|r1, r2| merge_errors(r1, r2));

        let fn_results = match functions {
            Some(r) => r,
            None => Ok(()),
        };

        let entrypoint_result = self
            .type_check_expr(self.entrypoint, &mut env, source)
            .map(|_| ());

        merge_errors(fn_results, entrypoint_result)
    }

    fn type_check_expr(
        &self,
        expr: ExprRef,
        env: &mut Env,
        source: &str,
    ) -> Result<Type, Vec<TypeError>> {
        match self.get_expr(expr) {
            (_, Expr::Num(_)) => Ok(Type::I64),
            (_, Expr::Bool(_)) => Ok(Type::Bool),
            (_, Expr::Var(s)) => Ok(env.type_of(s).expect("Variable not in scope.")),
            (tag, Expr::If(cond, body, branch)) => {
                let check_cond = self.type_check_expr(*cond, env, source);

                let check_body = self.type_check_expr(*body, env, source);
                let check_branch = self.type_check_expr(*branch, env, source);

                let check_if = match (check_body, check_branch) {
                    (Ok(t1), Ok(t2)) => {
                        if t1 == t2 {
                            Ok(t1)
                        } else {
                            Err(vec![TypeError::TypeMismatch(t1, t2, *tag)])
                        }
                    }
                    (o1, o2) => merge_errors(o1, o2),
                };

                match check_cond {
                    Err(e) => merge_errors(check_if, Err(e)),
                    Ok(t) => {
                        if let Type::Bool = t {
                            check_if
                        } else {
                            Err(vec![TypeError::ConditionalMismatch(t, *tag)])
                        }
                    }
                }
            }
            (tag, Expr::Bin(op, lhs, rhs)) => {
                let check_lhs = self.type_check_expr(*lhs, env, source);
                let check_rhs = self.type_check_expr(*rhs, env, source);

                match (check_lhs, check_rhs) {
                    (Err(e1), Err(e2)) => merge_errors(Err(e1), Err(e2)),
                    (Err(e1), _) => Err(e1),
                    (_, Err(e2)) => Err(e2),
                    (Ok(t1), Ok(t2)) => op.type_check(t1, t2, *tag),
                }
            }
            (tag, Expr::Neg(e)) => {
                let check_e = self.type_check_expr(*e, env, source);

                if let Ok(t) = check_e {
                    if let Type::I64 = t {
                        Ok(Type::I64)
                    } else {
                        Err(vec![TypeError::IncorrectTypes(Type::I64, vec![t], *tag)])
                    }
                } else {
                    check_e
                }
            }
            (_, Expr::Let(var, assn, body)) => {
                let assn_type = self.type_check_expr(*assn, env, source)?;

                env.push(var.clone(), assn_type);

                let result = self.type_check_expr(*body, env, source);
                env.pop();

                result
            }
            (tag, Expr::FnCall(name, args)) => {
                let args = args
                    .iter()
                    .map(|a| self.type_check_expr(*a, env, source))
                    .collect::<Vec<_>>();

                if args.iter().any(|x| x.is_err()) {
                    args.into_iter()
                        .reduce(|a, b| merge_errors(a, b))
                        .expect("At least one element must exist in iter")
                } else {
                    let args = args
                        .into_iter()
                        .map(|x| x.expect("No elements are of type Err"))
                        .collect::<Vec<_>>();

                    if let Some(t) = env.func_type(name, &args) {
                        Ok(t)
                    } else {
                        Err(vec![TypeError::NoSuchFn(name.clone(), args, *tag)])
                    }
                }
            }
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
                    decl.ret,
                    decl.tag,
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
        match self.get_expr(expr) {
            (_, Expr::Num(n)) => tree.add(AnfExpr::Imm(ImmExpr::Num(*n))),
            (_, Expr::Bool(b)) => tree.add(AnfExpr::Imm(ImmExpr::Bool(*b))),
            (_, Expr::Var(n)) => tree.add(AnfExpr::Imm(ImmExpr::Var(n.clone()))),
            (_, Expr::Neg(expr)) => {
                let result = self.anf_c(*expr, tree, env);
                tree.add(AnfExpr::Neg(result))
            }
            (_, Expr::Let(var, assn, body)) => {
                let assn_anf = self.anf_c(*assn, tree, env);
                let body_anf = self.anf_c(*body, tree, env);
                tree.add(AnfExpr::Let(var.clone(), assn_anf, body_anf))
            }
            (_, Expr::If(cond, body, branch)) => {
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
            (_, Expr::Bin(op, lhs, rhs)) => {
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
            (_, Expr::FnCall(name, args)) => {
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
        match self.get_expr(expr) {
            (_, Expr::Num(n)) => (ImmExpr::Num(*n), None),
            (_, Expr::Bool(b)) => (ImmExpr::Bool(*b), None),
            (_, Expr::Var(s)) => (ImmExpr::Var(s.clone()), None),
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
        let mut renamed = FlatTree::<(Tag, Expr)>::new();
        let mut name_gen = NameGen::new();

        let functions = self
            .functions
            .iter()
            .map(|decl| {
                let mut new_args = Vec::new();
                for (arg, typ) in &decl.args {
                    new_args.push((name_gen.push_arg(arg), *typ));
                }

                let new_decl = FnDecl::new(
                    decl.name.clone(),
                    new_args,
                    self.rename_expr(&mut renamed, decl.body, &mut name_gen)
                        .unwrap(),
                    decl.ret,
                    decl.tag,
                );

                for _ in &decl.args {
                    name_gen.pop();
                }

                new_decl
            })
            .collect::<Vec<_>>();

        let entrypoint = self.rename_expr(&mut renamed, self.entrypoint, &mut name_gen);

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

    fn rename_expr(
        &self,
        mut renamed: &mut FlatTree<(Tag, Expr)>,
        expr: ExprRef,
        name_gen: &mut NameGen,
    ) -> Option<ExprRef> {
        Some(match self.get_expr(expr) {
            (tag, Expr::Num(n)) => renamed.add((*tag, Expr::Num(*n))),
            (tag, Expr::Bool(b)) => renamed.add((*tag, Expr::Bool(*b))),
            (tag, Expr::Var(s)) => renamed.add((*tag, Expr::Var(name_gen.lookup(s)))),
            (tag, Expr::Neg(e)) => {
                let expr = self.rename_expr(renamed, *e, name_gen)?;

                renamed.add((*tag, Expr::Neg(expr)))
            }
            (tag, Expr::Bin(op, lhs, rhs)) => {
                let lhs_renamed = self.rename_expr(renamed, *lhs, name_gen)?;
                let rhs_renamed = self.rename_expr(renamed, *rhs, name_gen)?;

                renamed.add((*tag, Expr::Bin(*op, lhs_renamed, rhs_renamed)))
            }
            (tag, Expr::Let(s, assn, body)) => {
                let assn_expr = self.rename_expr(&mut renamed, *assn, name_gen)?;

                let new_name = name_gen.push_local(s);

                let body_expr = self.rename_expr(&mut renamed, *body, name_gen)?;

                name_gen.pop();

                renamed.add((*tag, Expr::Let(new_name, assn_expr, body_expr)))
            }
            (tag, Expr::If(cond, body, branch)) => {
                let cond_expr = self.rename_expr(renamed, *cond, name_gen)?;
                let body_expr = self.rename_expr(renamed, *body, name_gen)?;
                let branch_expr = self.rename_expr(renamed, *branch, name_gen)?;

                renamed.add((*tag, Expr::If(cond_expr, body_expr, branch_expr)))
            }
            (tag, Expr::FnCall(s, params)) => {
                let params = params
                    .iter()
                    .map(|e| self.rename_expr(renamed, *e, name_gen))
                    .collect::<Option<Vec<_>>>()?;

                renamed.add((*tag, Expr::FnCall(s.clone(), params)))
            }
        })
    }
}
