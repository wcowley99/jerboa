use std::{
    io::{self, Read, Write},
    process::{Command, Stdio},
};

use crate::{expr::ExprPool, lex::Lex};

mod expr;
mod instr;
mod lex;
mod parse;

fn compile<S: Into<String>>(input: S) -> String {
    let lex = Lex::new(input);
    let mut pool = ExprPool::new();
    // let compiled = exprs.compile(HashMap::new());

    "temp".to_owned()
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let asm = compile(buffer);

    let objfile = "tmp.o";

    Command::new("as")
        .args(["-o", objfile])
        .stdin(Stdio::piped())
        .spawn()
        .and_then(|mut child| {
            child.stdin.take().unwrap().write_all(asm.as_bytes())?;
            child.wait_with_output()
        })?;

    Command::new("ld").args([objfile, "-o", "a.out"]).output()?;

    Command::new("rm").arg(objfile).output()?;

    Ok(())
}
