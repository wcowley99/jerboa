use std::{
    env,
    io::{self, Read, Write},
    process::{Command, Stdio},
};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar);

use crate::{
    ast::AST,
    instr::{Instr, Reg},
};

mod anf;
mod ast;
mod common;
mod instr;

fn compile<S: Into<String>>(input: S) -> String {
    let preamble = ".text \n\
        .global _start \n\
        \n\
        _start:"
        .to_string();

    let envoi = vec![
        Instr::mov(Reg::RAX, Reg::RDI),
        Instr::mov(60, Reg::RAX),
        Instr::Syscall,
    ];

    let ast = AST::from(&input.into());
    let anf = ast.to_anf();
    let compiled = anf.compile();

    let prog = [compiled, envoi].concat();

    format!(
        "{}\n",
        [
            preamble,
            prog.iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        ]
        .join("\n")
    )
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;

    let verbose = args.contains(&"-v".to_string()) || args.contains(&"--verbose".to_string());
    let dry_run = args.contains(&"--dry-run".to_string());

    let asm = compile(buffer);

    if verbose {
        println!("{}", asm);
    }

    if dry_run {
        return Ok(());
    }

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
