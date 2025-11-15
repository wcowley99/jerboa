use std::{
    collections::HashMap,
    process::{Command, Stdio},
};

use crate::{
    ast::Expr,
    parser::{lex, parse},
};

mod ast;
mod instr;
mod parser;

fn main() {
    // let ast = Expr::Add1(Box::new(Expr::Add1(Box::new(Expr::Literal(5)))));
    let ast = Expr::let_binding("x", 5, Expr::add1(Expr::add1(Expr::id("x"))));
    let ast = Expr::let_binding(
        "x",
        Expr::let_binding("x", 5, Expr::add1(Expr::id("x"))),
        Expr::add1(Expr::id("x")),
    );

    let source = "inc(inc(55))";
    println!("{}", parse(lex(source)).compile(HashMap::new()));

    // println!("{}", ast.compile(HashMap::new()))

    // let ps_asm = Command::new("echo")
    //     .arg(ast.compile())
    //     .stdout(Stdio::piped())
    //     .spawn()
    //     .unwrap();
    //
    // let mut ps_obj = Command::new("as")
    //     .stdin(Stdio::from(ps_asm.stdout.unwrap()))
    //     .arg("-o")
    //     .arg("tmp.o")
    //     .spawn()
    //     .unwrap();
    //
    // ps_obj.wait().unwrap();
    //
    // let mut ld = Command::new("ld")
    //     .arg("tmp.o")
    //     .arg("-o")
    //     .arg("tmp")
    //     .spawn()
    //     .unwrap();
    //
    // ld.wait().unwrap();
}
