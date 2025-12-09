use std::{
    io::{self, Read, Write},
    process::{Command, Stdio},
};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar);

use crate::ast::Arena;

mod ast;
mod instr;

fn compile<S: Into<String>>(input: S) -> String {
    let mut arena = Arena::from(&input.into());
    let compiled = arena.compile();

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
