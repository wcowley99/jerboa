use std::iter::Peekable;

use crate::parser::{Lex, Lexeme};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct ExprRef(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Binary(BinOp, ExprRef, ExprRef),
    Let(String, ExprRef, ExprRef),
    Id(String),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum State {
    MkNum,
    MkAdd,
    MkMul,
}

pub struct ExprPool(Vec<Expr>);

impl ExprPool {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    fn add(&mut self, expr: Expr) -> ExprRef {
        let index = self.0.len();
        self.0.push(expr);

        ExprRef(index)
    }

    fn get(&mut self, index: ExprRef) -> Result<&Expr, String> {
        self.0
            .get(index.0)
            .ok_or_else(|| format!("Failed to find Expr at index {}", index.0))
    }

    pub fn parse(&mut self, mut lex: Lex) -> Result<ExprRef, String> {
        self.expr_parse(&mut lex)
    }

    fn factor_start(&mut self, lex: &mut Lex) -> Result<ExprRef, String> {
        let token = lex.require_next()?;

        match token {
            Lexeme::Num(n) => {
                let expr = self.add(Expr::Num(n));
                match lex.peek() {
                    None => Ok(expr),
                    t => Err(format!("Unexpected token {:?}", t)),
                }
            }
            t => Err(format!("Unexpected token {:?}", t)),
        }
    }

    fn expr_start(&mut self, lex: &mut Lex) -> Result<ExprRef, String> {
        let token = lex.require_next()?;
        match token {
            Lexeme::Num(n) => {
                let expr = self.add(Expr::Num(n));
                match lex.next() {
                    Some(Lexeme::Plus) => {
                        let factor = self.expr_start(lex)?;
                        Ok(self.add(Expr::Binary(BinOp::Add, expr, factor)))
                    }
                    Some(Lexeme::RParen) | Some(Lexeme::In) | None => Ok(expr),
                    t => Err(format!("Unexpected token {:?}", t)),
                }
            }
            Lexeme::Id(s) => Ok(self.add(Expr::Id(s))),
            t => Err(format!("Unexpected token {:?}", t)),
        }
    }

    fn expr_parse(&mut self, lex: &mut Lex) -> Result<ExprRef, String> {
        if let None = lex.peek() {
            return Err("No tokens".into());
        }

        let mut terms = Vec::new();
        let mut states = Vec::new();

        while let Some(token) = lex.next() {
            match (states.last(), &token) {
                (None, Lexeme::Num(n)) => {
                    terms.push(Expr::Num(*n));
                    states.push(State::MkNum);
                }
                (Some(State::MkAdd), Lexeme::Num(n)) => terms.push(Expr::Num(*n)),
                (Some(State::MkNum), Lexeme::Plus) => {
                    states.pop();
                    states.push(State::MkAdd);
                }
                (Some(State::MkAdd), Lexeme::Plus) => {
                    let e2 = terms.pop().unwrap();
                    let e1 = terms.pop().unwrap();

                    terms.push(Expr::Binary(BinOp::Add, self.add(e1), self.add(e2)));
                }
                (Some(State::MkMul), Lexeme::Plus) => todo!(),
                (None, Lexeme::Plus) => return Err(format!("Unexpected token {:?}", token)),
                _ => todo!(),
            }
        }

        while let Some(state) = states.pop() {
            match state {
                State::MkAdd => {
                    let e2 = terms.pop().unwrap();
                    let e1 = terms.pop().unwrap();

                    terms.push(Expr::Binary(BinOp::Add, self.add(e1), self.add(e2)));
                }
                State::MkNum => (),
                _ => todo!(),
            }
        }

        let expr = self.add(terms.pop().unwrap());
        if !terms.is_empty() {
            Err(format!("Dangling terms: {:?}", terms))
        } else {
            Ok(expr)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        expr::{BinOp, Expr, ExprPool, ExprRef},
        parser::{Lex, Lexeme},
    };

    #[test]
    fn test_parse_int_literal() {
        let mut pool = ExprPool::new();
        let lex = Lex::new(vec![Lexeme::Num(13)]);
        let result = pool.parse(lex);

        assert_eq!(result, Ok(ExprRef(0)));
        assert_eq!(pool.0, vec![Expr::Num(13)]);
    }

    #[test]
    fn test_parse_binary_operation() {
        let mut pool = ExprPool::new();
        let lex = Lex::new(vec![Lexeme::Num(12), Lexeme::Plus, Lexeme::Num(25)]);
        let result = pool.parse(lex);

        assert_eq!(result, Ok(ExprRef(2)));
        assert_eq!(
            pool.0,
            vec![
                Expr::Num(12),
                Expr::Num(25),
                Expr::Binary(BinOp::Add, ExprRef(0), ExprRef(1))
            ]
        )
    }

    #[test]
    fn test_add_ops_left_associative() {
        let mut pool = ExprPool::new();
        let lex = Lex::new(vec![
            Lexeme::Num(12),
            Lexeme::Plus,
            Lexeme::Num(25),
            Lexeme::Plus,
            Lexeme::Num(9),
        ]);
        let result = pool.parse(lex);

        assert_eq!(result, Ok(ExprRef(4)));
        assert_eq!(
            pool.0,
            vec![
                Expr::Num(12),
                Expr::Num(25),
                Expr::Binary(BinOp::Add, ExprRef(0), ExprRef(1)),
                Expr::Num(9),
                Expr::Binary(BinOp::Add, ExprRef(2), ExprRef(3)),
            ]
        )
    }
}
