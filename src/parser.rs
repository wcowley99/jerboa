use std::{
    collections::{HashMap, VecDeque, btree_map::IntoIter},
    iter::Peekable,
    str::Chars,
};

use crate::ast::Expr;

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub enum Lexeme {
    // Single-character tokens
    LParen,
    RParen,
    Comma,
    Dot,
    Eq,
    Plus,
    Minus,
    Star,
    Slash,

    // Literals
    Id(String),
    Str(String),
    Num(i64),

    // Keywords
    Inc,
    Let,
    In,
    If,
    Then,
    Else,
}

impl Lexeme {
    fn id<S: Into<String>>(name: S) -> Lexeme {
        Lexeme::Id(name.into())
    }
}

pub struct Lex {
    lexemes: VecDeque<Lexeme>,
}

impl Lex {
    pub fn new(lexemes: Vec<Lexeme>) -> Self {
        Self {
            lexemes: VecDeque::from(lexemes),
        }
    }

    pub fn peek(&self) -> Option<&Lexeme> {
        self.lexemes.get(0)
    }

    pub fn next(&mut self) -> Option<Lexeme> {
        self.lexemes.pop_front()
    }

    pub fn require_next(&mut self) -> Result<Lexeme, String> {
        self.next().ok_or("Expected Some token, found None".into())
    }
}

fn next_number(chars: &mut Peekable<Chars>) -> Lexeme {
    let mut builder = String::new();
    while let Some(c) = chars.peek() {
        if c.is_numeric() {
            builder.push(chars.next().expect("Failed with non-empty iterator"));
        } else {
            break;
        }
    }

    Lexeme::Num(builder.parse().expect("Failed to parse String as i64"))
}

fn next_string(chars: &mut Peekable<Chars>) -> Lexeme {
    let mut builder = String::new();
    _ = chars.next(); // discard opening quotation
    while let Some(c) = chars.peek() {
        if *c == '"' {
            break;
        } else {
            builder.push(chars.next().expect("Failed with non-empty iterator"));
        }
    }

    Lexeme::Str(builder)
}

fn next_identifier(chars: &mut Peekable<Chars>) -> Lexeme {
    let mut builder = String::new();
    while let Some(c) = chars.peek() {
        if c.is_alphanumeric() || *c == '_' {
            builder.push(chars.next().expect("Failed with non-empty iterator"));
        } else {
            break;
        }
    }

    match builder.as_str() {
        "inc" => Lexeme::Inc,
        "let" => Lexeme::Let,
        "in" => Lexeme::In,
        "if" => Lexeme::If,
        "then" => Lexeme::Then,
        "else" => Lexeme::Else,
        _ => Lexeme::Id(builder),
    }
}

pub fn lex<S: Into<String>>(input: S) -> Vec<Lexeme> {
    let mut tokens = vec![];

    let source: String = input.into();
    let mut chars = source.chars().peekable();

    let single_chars = HashMap::from([
        ('(', Lexeme::LParen),
        (')', Lexeme::RParen),
        (',', Lexeme::Comma),
        ('.', Lexeme::Dot),
        ('=', Lexeme::Eq),
    ]);

    while let Some(c) = chars.peek() {
        match c {
            '(' | ')' | ',' | '.' | '=' => {
                tokens.push(single_chars.get(c).unwrap().clone());
                _ = chars.next();
            }
            '"' => tokens.push(next_string(&mut chars)),
            ' ' => _ = chars.next(),
            c => {
                if c.is_numeric() {
                    tokens.push(next_number(&mut chars));
                } else if c.is_alphabetic() || *c == '_' {
                    tokens.push(next_identifier(&mut chars));
                } else {
                    _ = chars.next()
                }
            }
        }
    }

    tokens
}

fn parse_expr(input: &mut VecDeque<Lexeme>) -> Expr {
    let word = input.pop_front();
    if let Some(w) = word {
        match w {
            Lexeme::Num(n) => Expr::Literal(n),
            Lexeme::Id(s) => Expr::Id(s),
            Lexeme::Inc => {
                assert!(input.pop_front().unwrap() == Lexeme::LParen);

                let expr = parse_expr(input);

                assert!(input.pop_front().unwrap() == Lexeme::RParen);

                Expr::add1(expr)
            }
            Lexeme::Let => {
                let id = input.pop_front().unwrap();
                match id {
                    Lexeme::Id(name) => {
                        assert!(input.pop_front().unwrap() == Lexeme::Eq);

                        let assn = parse_expr(input);

                        assert!(input.pop_front().unwrap() == Lexeme::In);

                        let body = parse_expr(input);

                        Expr::let_binding(name, assn, body)
                    }
                    _ => panic!("Expected identifier"),
                }
            }
            Lexeme::If => {
                let cond = parse_expr(input);

                assert!(input.pop_front().unwrap() == Lexeme::Then);

                let body = parse_expr(input);

                assert!(input.pop_front().unwrap() == Lexeme::Else);

                let branch = parse_expr(input);

                Expr::conditional(cond, body, branch)
            }
            _ => panic!(),
        }
    } else {
        panic!()
    }
}

pub fn parse(input: Vec<Lexeme>) -> Expr {
    let mut lexemes = VecDeque::from(input);
    parse_expr(&mut lexemes)
}

/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_numbers_test() {
        let a = "5";
        assert_eq!(lex(a), vec![Lexeme::Num(5)]);

        let a = "59";
        assert_eq!(lex(a), vec![Lexeme::Num(59)]);

        let a = "123456789";
        assert_eq!(lex(a), vec![Lexeme::Num(123456789)]);
    }

    #[test]
    fn lexer_identifiers_test() {
        let a = "x";
        assert_eq!(lex(a), vec![Lexeme::Id("x".into())]);

        let a = "_validId123";
        assert_eq!(lex(a), vec![Lexeme::Id("_validId123".into())]);

        let a = "_super_1ong_val1d_1d_123";
        assert_eq!(lex(a), vec![Lexeme::Id("_super_1ong_val1d_1d_123".into())]);
    }

    #[test]
    fn lexer_basic_test() {
        let input = "inc(512)";
        assert_eq!(
            lex(input),
            vec![
                Lexeme::Inc,
                Lexeme::LParen,
                Lexeme::Num(512),
                Lexeme::RParen
            ]
        );
    }

    #[test]
    fn lexer_nested_test() {
        let input = "inc(inc(512))";
        assert_eq!(
            lex(input),
            vec![
                Lexeme::Inc,
                Lexeme::LParen,
                Lexeme::Inc,
                Lexeme::LParen,
                Lexeme::Num(512),
                Lexeme::RParen,
                Lexeme::RParen,
            ]
        );
    }

    #[test]
    fn lexer_let_test() {
        println!("HERE!!!");
        let input = "let x = 5 in x";
        assert_eq!(
            lex(input),
            vec![
                Lexeme::Let,
                Lexeme::id("x"),
                Lexeme::Eq,
                Lexeme::Num(5),
                Lexeme::In,
                Lexeme::id("x")
            ]
        );
    }

    #[test]
    fn lexer_if_test() {
        println!("HERE!!!");
        let input = "if 10 then 0 else 1";
        assert_eq!(
            lex(input),
            vec![
                Lexeme::If,
                Lexeme::Num(10),
                Lexeme::Then,
                Lexeme::Num(0),
                Lexeme::Else,
                Lexeme::Num(1)
            ]
        );
    }

    #[test]
    fn parser_basic_test() {
        let input = "inc(512)";
        assert_eq!(parse(lex(input)), Expr::add1(Expr::Literal(512)));
    }

    #[test]
    fn parser_nested_test() {
        let input = "inc(inc(512))";
        assert_eq!(
            parse(lex(input)),
            Expr::add1(Expr::add1(Expr::Literal(512)))
        );
    }

    #[test]
    fn parser_if_test() {
        let input = "if 10 then 0 else 1";
        assert_eq!(parse(lex(input)), Expr::conditional(10, 0, 1));
    }

    #[test]
    fn source_to_interp1() {
        let input = "inc(inc(12))";
        assert_eq!(parse(lex(input)).interp(), 14);
    }

    #[test]
    fn source_to_interp2() {
        let input = "let x = 5 in inc(x)";
        assert_eq!(parse(lex(input)).interp(), 6);
    }

    #[test]
    fn source_to_interp3() {
        let input = "if 5 then 1 else 2";
        assert_eq!(parse(lex(input)).interp(), 1);
    }
}
*/
