use std::{
    env, fs,
    io::{self, ErrorKind, Read, Write},
    path::Path,
    process::{Command, Stdio, exit},
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

struct CLIArgs {
    pub verbose: bool,
    pub dry_run: bool,
}

impl CLIArgs {
    fn get() -> Self {
        let args: Vec<String> = env::args().collect();

        let verbose = args.contains(&"-v".to_string()) || args.contains(&"--verbose".to_string());
        let dry_run = args.contains(&"--dry-run".to_string());

        Self { verbose, dry_run }
    }
}

fn to_asm<S: Into<String>>(input: S, args: &CLIArgs) -> String {
    let preamble = ".extern cmax \n\n\
        .text \n\
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
    let renamed = ast.rename().unwrap();
    let anf = renamed.to_anf();

    if args.verbose {
        println!("ANF:\n--------------------");
        anf.print();
    }

    let (entry, decls) = anf.compile();

    let prog = [entry, envoi, decls].concat();

    let asm = format!(
        "{}\n",
        [
            preamble,
            prog.iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        ]
        .join("\n")
    );

    if args.verbose {
        println!("Assembly:\n--------------------");
        println!("{}", asm);
    }

    return asm;
}

fn run_assembler(asm: &str, output_path: &str) -> Result<(), String> {
    let result = Command::new("as")
        .args(["-o", output_path])
        .stdin(Stdio::piped())
        .spawn()
        .and_then(|mut child| {
            child.stdin.take().unwrap().write_all(asm.as_bytes())?;
            child.wait_with_output()
        });

    match result {
        Ok(o) => {
            if o.status.success() {
                Ok(())
            } else {
                Err(format!("`as` exited with status {}!", o.status))
            }
        }
        Err(e) => {
            if let ErrorKind::NotFound = e.kind() {
                Err("`as` was not found! Check your PATH or make sure it is installed!".to_string())
            } else {
                Err(format!("Failed to run `as`: {}", e.to_string()))
            }
        }
    }
}

fn run_linker(object_path: &str) -> Result<(), String> {
    let result = Command::new("ld")
        .args([
            object_path,
            "-o",
            "a.out",
            "-dynamic-linker",
            "/lib64/ld-linux-x86-64.so.2",
            "-L.",
            "-l:test_lib.so",
            "-lc",
        ])
        .output();

    match result {
        Ok(o) => {
            if o.status.success() {
                Ok(())
            } else {
                Err(format!(
                    "`ld` failed with status {}!\n\n{}",
                    o.status,
                    String::from_utf8(o.stderr).unwrap()
                ))
            }
        }
        Err(e) => {
            if let ErrorKind::NotFound = e.kind() {
                Err("`ld` was not found! Check your PATH or make sure it is installed!".to_string())
            } else {
                Err(format!("Failed to run `ld`: {}", e.to_string()))
            }
        }
    }
}

fn cleanup(object_path: &str) -> Result<(), String> {
    match fs::remove_file(object_path) {
        Ok(_) => Ok(()),
        Err(_) => Err(format!(
            "Could not delete file `{}`, did something happen to it?",
            object_path
        )),
    }
}

fn compile() -> Result<(), String> {
    let args = CLIArgs::get();

    let mut buffer = String::new();
    if let Err(_) = io::stdin().read_to_string(&mut buffer) {
        return Err("Failed to read stdin.".to_string());
    }

    let asm = to_asm(buffer, &args);

    if args.dry_run {
        return Ok(());
    }

    let object_path = "output.o";

    run_assembler(&asm, object_path)?;

    if let Err(e) = run_linker(object_path) {
        let _ = cleanup(object_path);
        Err(e)
    } else {
        cleanup(object_path)
    }
}

fn main() {
    if let Err(e) = compile() {
        eprintln!("{}", e);
        exit(1);
    } else {
        exit(0);
    }
}
