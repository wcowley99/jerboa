use std::fmt::Display;

use crate::{
    common::{BinOp, Env, ExprRef, FlatTree, FnDecl},
    instr::{Instr, Operand, Reg},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImmExpr {
    Num(i64),
    Bool(bool),
    Var(String),
}

impl ImmExpr {
    fn as_operand(&self, env: &Env) -> Operand {
        match self {
            ImmExpr::Num(n) => Operand::Imm(*n),
            ImmExpr::Bool(b) => {
                if *b {
                    Operand::Imm(1)
                } else {
                    Operand::Imm(0)
                }
            }
            ImmExpr::Var(s) => env.lookup(s).unwrap(),
        }
    }

    pub fn to_asm(&self, env: &Env) -> Operand {
        match self {
            ImmExpr::Num(val) => Operand::Imm(*val),
            ImmExpr::Bool(b) => {
                let repr = if *b { 1 } else { 0 };
                Operand::Imm(repr)
            }
            ImmExpr::Var(var) => env.lookup(var).unwrap(),
        }
    }

    pub fn gen_param(&self, param_num: usize, env: &Env) -> Instr {
        let loc = self.as_operand(env);
        match param_num {
            0 => Instr::mov(loc, Reg::RDI),
            1 => Instr::mov(loc, Reg::RSI),
            2 => Instr::mov(loc, Reg::RDX),
            3 => Instr::mov(loc, Reg::RCX),
            4 => Instr::mov(loc, Reg::R8),
            5 => Instr::mov(loc, Reg::R9),
            _ => Instr::Push(loc),
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
    Let(String, ExprRef, ExprRef),
    If(ImmExpr, ExprRef, ExprRef),
    Fn(String, Vec<ImmExpr>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct AnfTree {
    tree: FlatTree<AnfExpr>,
    functions: Vec<FnDecl>,
    entrypoint: ExprRef,
}

impl AnfTree {
    pub fn from(tree: FlatTree<AnfExpr>, functions: Vec<FnDecl>, entrypoint: ExprRef) -> Self {
        Self {
            tree,
            functions,
            entrypoint,
        }
    }

    pub fn compile(&self) -> (Vec<Instr>, Vec<Vec<Instr>>) {
        let mut env = Env::new();

        let prologue = vec![
            Instr::label("_start"),
            Instr::push(Reg::RBP),
            Instr::mov(Reg::RSP, Reg::RBP),
            Instr::sub(8 * self.stack_frame_size(self.entrypoint) as i64, Reg::RSP),
        ];

        let epilogue = vec![Instr::mov(Reg::RBP, Reg::RSP), Instr::pop(Reg::RBP)];

        let entrypoint = self.to_asm(self.entrypoint, &mut env);
        (
            [prologue, entrypoint, epilogue].concat(),
            self.functions
                .iter()
                .map(|decl| {
                    let compiled = self.compile_function(decl, &mut env);

                    compiled
                })
                .collect::<Vec<_>>(),
        )
    }

    fn compile_function(&self, decl: &FnDecl, mut env: &mut Env) -> Vec<Instr> {
        let prologue = vec![
            Instr::Label(decl.name.clone()),
            Instr::push(Reg::RBP),
            Instr::mov(Reg::RSP, Reg::RBP),
            Instr::sub(8 * self.stack_frame_size(decl.body) as i64, Reg::RSP),
        ];

        env.set_args(&decl.args);

        let body = self.to_asm(decl.body, &mut env);

        env.clear_args();

        let epilogue = vec![
            Instr::mov(Reg::RBP, Reg::RSP),
            Instr::pop(Reg::RBP),
            Instr::Ret,
        ];

        [prologue, body, epilogue].concat()
    }

    fn stack_frame_size(&self, e: ExprRef) -> usize {
        match self.tree.get(e).unwrap() {
            AnfExpr::Bin(_, _, _) | AnfExpr::Imm(_) | AnfExpr::Fn(_, _) => 0,
            AnfExpr::If(_, body, branch) => {
                std::cmp::max(self.stack_frame_size(*body), self.stack_frame_size(*branch))
            }
            AnfExpr::Neg(e) => self.stack_frame_size(*e),
            AnfExpr::Let(_, assn, body) => {
                1 + std::cmp::max(self.stack_frame_size(*assn), self.stack_frame_size(*body))
            }
        }
    }

    fn to_asm(&self, expr: ExprRef, mut env: &mut Env) -> Vec<Instr> {
        match self.tree.get(expr).unwrap() {
            AnfExpr::Imm(e) => vec![Instr::mov(e.to_asm(env), Reg::RAX)],
            AnfExpr::Neg(expr) => {
                let mut program = self.to_asm(*expr, env);
                program.push(Instr::Neg(Reg::RAX));
                program
            }
            AnfExpr::Let(var, assn, body) => {
                let assn_asm = self.to_asm(*assn, env);

                env.push(var.clone());

                let prog = [
                    assn_asm,
                    vec![Instr::mov(Reg::RAX, env.lookup(var).unwrap())],
                    self.to_asm(*body, &mut env),
                ]
                .concat();

                env.pop();

                prog
            }
            AnfExpr::Bin(op, lhs, rhs) => {
                let lhs = lhs.to_asm(env);
                let rhs = rhs.to_asm(env);
                let op_instrs = op.to_asm(Operand::Reg(Reg::RAX), rhs);

                [vec![Instr::mov(lhs, Reg::RAX)], op_instrs].concat()
            }
            AnfExpr::If(cond, body, branch) => {
                let label_true = format!("if_true{}", expr.0);
                let label_false = format!("if_false{}", expr.0);
                let label_done = format!("done{}", expr.0);
                [
                    vec![
                        Instr::mov(cond.to_asm(env), Reg::RAX),
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
            AnfExpr::Fn(name, args) => {
                let num_args = args.len();

                let cleanup_instr = if num_args > 6 {
                    vec![Instr::add(((num_args - 6) * 8) as i64, Reg::RSP)]
                } else {
                    vec![]
                };

                let caller_save_args = [
                    Reg::RCX,
                    Reg::RDX,
                    Reg::RDI,
                    Reg::RSI,
                    Reg::R8,
                    Reg::R9,
                    Reg::R10,
                    Reg::R11,
                ];

                let save = caller_save_args
                    .iter()
                    .enumerate()
                    .map(|(i, x)| Instr::mov(*x, Operand::local(i + env.num_locals())))
                    .collect::<Vec<_>>();

                let restore = caller_save_args
                    .iter()
                    .enumerate()
                    .rev()
                    .map(|(i, x)| Instr::mov(Operand::local(i + env.num_locals()), *x))
                    .collect::<Vec<_>>();

                [
                    save,
                    vec![Instr::sub(64, Reg::RSP)],
                    args.iter()
                        .enumerate()
                        .map(|(i, arg)| arg.gen_param(i, &env))
                        .collect(),
                    vec![Instr::Call(name.clone())],
                    vec![Instr::add(64, Reg::RSP)],
                    cleanup_instr,
                    restore,
                ]
                .concat()
            }
        }
    }

    pub fn print(&self) {
        for func in &self.functions {
            println!("{}({:?}):", func.name, func.args);
            self.print_helper(func.body, 0);
            println!("\n")
        }
        self.print_helper(self.entrypoint, 0);
        println!("");
    }

    fn print_helper(&self, expr: ExprRef, depth: usize) {
        match self.tree.get(expr).unwrap() {
            AnfExpr::Imm(e) => print!("{}", e),
            AnfExpr::Neg(e) => {
                print!("-");
                self.print_helper(*e, depth);
            }
            AnfExpr::Bin(op, lhs, rhs) => {
                print!("{} {} {}", lhs, op, rhs)
            }
            AnfExpr::Let(var, assn, body) => {
                print!("let {} = ", var);
                self.print_helper(*assn, depth);
                println!(" in");
                self.print_helper(*body, depth);
            }
            AnfExpr::If(cond, body, branch) => {
                println!("if {}:", cond);
                self.print_helper(*body, depth);
                println!("else:");
                self.print_helper(*branch, depth);
            }
            AnfExpr::Fn(name, args) => {
                print!("fn {}(", name);
                for (idx, arg) in args.iter().enumerate() {
                    print!("{}", arg);
                    if idx < args.len() - 1 {
                        print!(", ");
                    }
                }
                print!(")");
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        anf::{AnfExpr, AnfTree, ImmExpr},
        common::FlatTree,
    };

    #[test]
    fn test_stack_frame_size_1() {
        let mut tree = FlatTree::<AnfExpr>::new();
        let num = AnfExpr::Imm(ImmExpr::Num(5));
        let var = AnfExpr::Imm(ImmExpr::Var("x".to_string()));
        let e1 = tree.add(num);
        let e2 = tree.add(var);
        let e3 = tree.add(AnfExpr::Let("x".to_string(), e1, e2));

        let anf = AnfTree::from(tree, Vec::new(), e3);

        assert_eq!(anf.stack_frame_size(e1), 0);
        assert_eq!(anf.stack_frame_size(e2), 0);
        assert_eq!(anf.stack_frame_size(e3), 1);
    }

    #[test]
    fn test_stack_frame_size_nested() {
        let mut tree = FlatTree::<AnfExpr>::new();
        let num = AnfExpr::Imm(ImmExpr::Num(5));
        let var = AnfExpr::Imm(ImmExpr::Var("x".to_string()));
        let e1 = tree.add(num);
        let e2 = tree.add(var);
        let e3 = tree.add(AnfExpr::Let("x".to_string(), e1, e2));
        let e4 = tree.add(AnfExpr::Let("y".to_string(), e3, e1));

        let anf = AnfTree::from(tree, Vec::new(), e3);

        assert_eq!(anf.stack_frame_size(e3), 1);
        assert_eq!(anf.stack_frame_size(e4), 2);
    }

    #[test]
    fn test_stack_frame_size_if() {
        let mut tree = FlatTree::<AnfExpr>::new();
        let num = AnfExpr::Imm(ImmExpr::Num(5));
        let var = AnfExpr::Imm(ImmExpr::Var("x".to_string()));
        let boolean = ImmExpr::Bool(true);
        let e1 = tree.add(num);
        let e2 = tree.add(var);
        let e3 = tree.add(AnfExpr::Let("x".to_string(), e1, e2));
        let e4 = tree.add(AnfExpr::Let("y".to_string(), e3, e1));
        let e5 = tree.add(AnfExpr::If(boolean, e3, e4));

        let anf = AnfTree::from(tree, Vec::new(), e3);

        assert_eq!(anf.stack_frame_size(e3), 1);
        assert_eq!(anf.stack_frame_size(e4), 2);
        assert_eq!(anf.stack_frame_size(e5), 2);
    }
}
