use std::{
    collections::HashMap,
    env,
    io::{self, Read, Write},
    process::{Command, Stdio},
};

use crate::{
    ast::Expr,
    parser::{lex, parse},
};

mod ast;
mod instr;
mod parser;

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let asm = parse(lex(buffer)).compile(HashMap::new());

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
