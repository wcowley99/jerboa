# Jerboa Compiler

Jerboa is a custom compiler for the `Jerb` programming language. It targets Linux x86_64 systems, and compiles `.jerb` files (or any file extension, really) into an executable file format.

This project is primarily built for learning and experimentation with compiler design, including semantic analysis, optimization, stack & heap management, and intermediate forms. That said, this project aims to be designed in a practical way such that it can be used in real-world projects.

---

## Major Features
- [x] Recursive functions 
- [x] Multiple data types
- [x] Assembly-code generation
- [x] C foreign-function interface
- [x] Command-line interface
- [x] Helpful error messages

---

## Planned Additions
- More primitive data types
- Algebraic data types (structs, enums, tuples, etc.)
- Custom defined data types

---

## Dependencies

`Jerboa` is lightweight, and doesn't require much installed to work. Make sure that these commands are available on your machine:

- `cargo` - the Rust package manager
- `as` - for assembling the `Jerboa` output into machine code
- `ld` - for linking to `libc` and converting to the ELF executable format.

---

## Example

A `Jerb` program consists of a sequence of function declarations followed by a single expression. The following example shows a recursive implementation of the fibonacci sequence.

Filename: main.jerb
```
fn fib(n) {
  if n == 0 {
    0
  } else {
    if n == 1 {
      1
    } else {
      fib(n-2) + fib(n-1)
    }
  }
}

fib(10)
```

--- 

## Compilation and Execution

We have a `Jerb` program in the file `main.jerb`. So now let's see how we can compile this.

Before running our program, we need to compile it. We can do so by running the following command:

```
cargo run -- main.jerb
```


After running this command, `Jerboa` has produced an executable named `a.out`. If you run `cargo build` instead, you can change the compile command to the following:

```
./path/to/jerboa main.jerb
```

If your `main.jerb` was the fibonacci function above, the exit code of the program contains the result of `fib(10)`.

```
$ ./a.out
$ echo $? # <-- this prints the exit code of the previous command
55
```
