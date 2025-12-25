use std::fmt::Debug;

use colored::Colorize;

use crate::common::{FnDecl, Tag, Type};

#[derive(Debug, Clone)]
pub enum TypeError {
    TypeMismatch(Type, Type, Tag),
    ConditionalMismatch(Type, Tag),
    IncorrectTypes(Type, Vec<Type>, Tag),
    NoSuchFn(String, Vec<Type>, Tag),
    DuplicateFn(String, Vec<Type>, Tag),
    IncorrectReturnType(String, Type, Type, Tag),
}

impl TypeError {
    pub fn to_string(&self, source: &str) -> String {
        match self {
            TypeError::TypeMismatch(lhs, rhs, tag) => format_error(
                source,
                tag.begin,
                Some(tag.end),
                &format!("Expected type {}, found {}.", lhs, rhs),
                None,
            ),
            TypeError::ConditionalMismatch(typ, tag) => format_error(
                source,
                tag.begin,
                Some(tag.end),
                &format!(
                    "Type mismatch. `if` conditional should be type bool, is {}",
                    typ
                ),
                None,
            ),
            TypeError::IncorrectTypes(expected, found, tag) => {
                let types = found
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(", ");

                let reason = if found.len() == 1 {
                    format!("Expected type {}, found {}", expected, types)
                } else {
                    format!("Expected all types to be {}, found ({})", expected, types)
                };

                format_error(source, tag.begin, Some(tag.end), &reason, None)
            }
            TypeError::NoSuchFn(name, args, tag) => {
                let types = args
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(", ");
                format_error(
                    source,
                    tag.begin,
                    Some(tag.end),
                    &format!(
                        "Cannot find function `{}` with arguments ({}) in this scope",
                        name, types
                    ),
                    None,
                )
            }
            TypeError::DuplicateFn(name, args, tag) => {
                let types = args
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(", ");
                format_error(
                    source,
                    tag.begin,
                    Some(tag.end),
                    &format!(
                        "Function `{}` with arguments ({}) already exists in this scope",
                        name, types
                    ),
                    None,
                )
            }
            TypeError::IncorrectReturnType(name, expected, given, tag) => format_error(
                source,
                tag.begin,
                Some(tag.end),
                &format!(
                    "Function `{}` expected `{}` because of return type, found `{}`",
                    name, *expected, *given
                ),
                None,
            ),
        }
    }
}

/// Given some input and the begin position of a token, returns the (begin, end) indices of the
/// line that the token is on
fn get_token_context(input: &str, tok: usize) -> Option<(usize, usize, usize)> {
    let mut begin = 0;
    for (line_num, line) in input.lines().enumerate() {
        let end = begin + line.len();

        if (begin <= tok) && (tok <= end) {
            return Some((begin, end, line_num + 1));
        } else {
            begin += line.len() + "\n".len();
        }
    }

    None
}

/// Produces a formatted compiler error message
///
/// If the end of the error token is unknown (indicated by passing None to @param end), then the
/// error underline points to the beginning of the error. Otherwise it underlines the erronious
/// token
pub fn format_error(
    input: &str,
    begin: usize,
    end: Option<usize>,
    reason: &str,
    help: Option<&str>,
) -> String {
    let (line_begin, line_end, line_num) = get_token_context(input, begin).unwrap();
    let line = &input[line_begin..line_end];

    let underline = if let Some(end) = end {
        " ".repeat(begin - line_begin)
            + &"^".repeat(end - begin)
            + &format!(" ---- error occurs here (line {})", line_num)
    } else {
        " ".repeat(begin - line_begin) + &format!("^ ---- error occurs here (line {})", line_num)
    };

    let ident = " |".white().bold();

    let msg = format!(
        "{}{}\n {}\n {} {}\n {} {}",
        "Error".red().bold(),
        format!(": {}", reason).white().bold(),
        ident,
        ident,
        line,
        ident,
        underline.red().bold()
    );

    if let Some(help) = help {
        format!("{}\n\nHelp: {}", msg, help)
    } else {
        msg
    }
}

/// Merges the error messages of two Results
///
/// If both results are Ok, then the ok value is the value in r2.
pub fn merge_errors<T: Debug, U: Clone>(
    r1: Result<T, Vec<U>>,
    r2: Result<T, Vec<U>>,
) -> Result<T, Vec<U>> {
    if r1.is_err() && r2.is_err() {
        Err([r1.unwrap_err(), r2.unwrap_err()].concat())
    } else {
        r1.and_then(|_| r2)
    }
}
