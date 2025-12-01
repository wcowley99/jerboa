use std::{
    array::IntoIter,
    collections::{HashMap, HashSet},
};

use crate::{
    expr::{BinOp, Expr, ExprPool, ExprRef},
    lex::{Lex, Lexeme},
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Terminal {
    Let,
    Id,
    Eq,
    In,
    Plus,
    Star,
    Num,
    LParen,
    RParen,
    Eof,
}

impl Into<Token> for Terminal {
    fn into(self) -> Token {
        Token::Terminal(self)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum NonTerminal {
    Start,
    Expr,
    Factor,
    Term,
}

impl Into<Token> for NonTerminal {
    fn into(self) -> Token {
        Token::NonTerminal(self)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Token {
    Terminal(Terminal),
    NonTerminal(NonTerminal),
}

struct Rule {
    pub symbol: NonTerminal,
    pub expr: Vec<Token>,
}

impl Rule {
    pub fn new(symbol: NonTerminal, expr: Vec<Token>) -> Self {
        Self { symbol, expr }
    }

    pub fn windows(&self) -> impl Iterator<Item = (Token, Token)> {
        self.expr.windows(2).map(|w| (w[0], w[1])).into_iter()
    }

    pub fn first_pos(&self) -> Position {
        Position::new(self.symbol, Vec::new(), self.expr.clone())
    }
}

fn extend_itemsets_table(table: &mut HashMap<Token, ItemSet>, item: Item) {
    let lhs = *item.pos.parsed.last().expect("This should never fail");
    match table.get_mut(&lhs) {
        Some(item_set) => {
            item_set.insert(item);
        }
        None => {
            table.insert(lhs, ItemSet(HashSet::from([item])));
        }
    }
}

fn extend_table(
    table: &mut HashMap<NonTerminal, HashSet<Terminal>>,
    lhs: NonTerminal,
    rhs: Token,
) -> bool {
    match rhs {
        Token::Terminal(t) => match table.get_mut(&lhs) {
            Some(terms) => terms.insert(t),
            None => {
                table.insert(lhs, HashSet::from([t]));
                true
            }
        },
        Token::NonTerminal(t) => {
            let empty = HashSet::new();
            let terms = table.get(&t).unwrap_or(&empty).clone();

            let mut changed = false;

            match table.get_mut(&lhs) {
                Some(knowledge) => {
                    for t in terms {
                        changed |= knowledge.insert(t);
                    }

                    changed
                }
                None => {
                    table.insert(lhs, terms);
                    true
                }
            }
        }
    }
}

fn gen_first_table(rules: &Vec<Rule>) -> HashMap<NonTerminal, HashSet<Terminal>> {
    let mut table: HashMap<NonTerminal, HashSet<Terminal>> = HashMap::new();
    let mut changed = true;
    while changed {
        changed = false;
        for rule in rules {
            let lhs = rule.symbol;
            let first = rule.expr[0];
            changed |= extend_table(&mut table, lhs, first);
        }
    }
    return table;
}

fn gen_follow_table(rules: &Vec<Rule>) -> HashMap<NonTerminal, HashSet<Terminal>> {
    let mut table: HashMap<NonTerminal, HashSet<Terminal>> = HashMap::new();
    let mut changed = true;
    while changed {
        changed = false;
        for rule in rules {
            for (l, r) in rule.windows() {
                if let Token::NonTerminal(t) = l {
                    changed |= extend_table(&mut table, t, r);
                }
            }

            if let Some(Token::NonTerminal(lhs)) = rule.expr.last() {
                changed |= extend_table(&mut table, *lhs, Token::NonTerminal(rule.symbol));
            }
        }
    }
    return table;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Position {
    lhs: NonTerminal,
    parsed: Vec<Token>,
    expected: Vec<Token>,
}

impl Position {
    pub fn new<T: Into<Vec<Token>>, U: Into<Vec<Token>>>(
        lhs: NonTerminal,
        parsed: T,
        expected: U,
    ) -> Self {
        Self {
            lhs,
            parsed: parsed.into(),
            expected: expected.into(),
        }
    }

    pub fn next(&self) -> Option<Self> {
        if self.expected.len() == 0 {
            None
        } else {
            Some(Self {
                lhs: self.lhs,
                parsed: [self.parsed.clone(), vec![*self.expected.first()?]].concat(),
                expected: self.expected.clone().into_iter().skip(1).collect(),
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Item {
    pos: Position,
    lookahead: Vec<Terminal>,
}

impl Item {
    pub fn new<T: Into<Vec<Terminal>>>(pos: Position, lookahead: T) -> Self {
        Self {
            pos,
            lookahead: lookahead.into(),
        }
    }

    pub fn next_lookahead(
        &self,
        first: &HashMap<NonTerminal, HashSet<Terminal>>,
    ) -> Option<Vec<Terminal>> {
        match self.pos.expected.first() {
            Some(Token::Terminal(t)) => Some(vec![*t]),
            Some(Token::NonTerminal(nt)) => Some(
                first
                    .get(nt)
                    .unwrap()
                    .iter()
                    .map(|x| *x)
                    .collect::<Vec<_>>(),
            ),
            None => None,
        }
    }

    pub fn next_pos(&self) -> Option<Item> {
        Some(Item::new(self.pos.next()?, self.lookahead.clone()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ItemSet(HashSet<Item>);

impl ItemSet {
    pub fn from_rules(rules: &Vec<Rule>) -> Vec<ItemSet> {
        let first_table = gen_first_table(rules);
        let follow_table = gen_follow_table(rules);
        let first = Item::new(rules[0].first_pos(), [Terminal::Eof.into()]);
        let mut worklist = vec![ItemSet(HashSet::from([first]))];
        let mut item_sets = Vec::new();

        while let Some(mut itemset) = worklist.pop() {
            itemset.expand(rules, &first_table, &follow_table);

            if item_sets.contains(&itemset) {
                continue;
            }

            let mut next_item_sets = HashMap::new();
            for item in itemset.0.iter() {
                if let Some(i) = item.next_pos() {
                    extend_itemsets_table(&mut next_item_sets, i);
                }
            }

            for next in next_item_sets.into_values() {
                worklist.push(next);
            }

            item_sets.push(itemset);
        }

        item_sets
    }

    pub fn insert(&mut self, item: Item) -> bool {
        self.0.insert(item)
    }

    fn expand(
        &mut self,
        rules: &Vec<Rule>,
        first: &HashMap<NonTerminal, HashSet<Terminal>>,
        follow: &HashMap<NonTerminal, HashSet<Terminal>>,
    ) {
        let mut changed = true;
        while changed {
            changed = false;
            let mut to_add = HashSet::new();
            for item in self.0.iter() {
                if let Some(Token::NonTerminal(t)) = item.pos.expected.first() {
                    for rule in rules.iter() {
                        if rule.symbol == *t {
                            to_add.insert(Item::new(
                                rule.first_pos(),
                                item.next_lookahead(first).unwrap_or(
                                    follow
                                        .get(&rule.symbol)
                                        .unwrap()
                                        .iter()
                                        .map(|x| *x)
                                        .collect::<Vec<_>>(),
                                ),
                            ));
                        }
                    }
                }
            }

            for position in to_add {
                changed |= self.insert(position.clone());
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum State {
    MkNum,
    MkAdd,
    MkMul,
    MkLet(String),
    MkVar,
    MkLetId,
    MkLetEq,
    MkLetBind(String),
    MkLetIn,
    MkLetBody(String, Expr),
}

enum Action {
    ShiftPush(State),
    ShiftTo(State),
    ShiftSame,
    MoveTo(State),
    ReduceTo(State),
    Reduce,
    Error,
    Accept,
}

fn next_action(state: Option<&State>, token: Option<&Lexeme>) -> Action {
    use Lexeme::*;
    use State::*;

    match (state, token) {
        (None, Some(Num(_))) => Action::ShiftPush(MkNum),

        (Some(MkAdd) | Some(MkMul), Some(Num(_))) => Action::ShiftSame,

        (Some(MkNum) | Some(MkVar), Some(Plus)) => Action::ShiftTo(MkAdd),
        (Some(MkAdd) | Some(MkMul), Some(Plus)) => Action::ReduceTo(MkAdd),

        (Some(MkNum), Some(Star)) => Action::ShiftTo(MkMul),
        (Some(MkAdd), Some(Star)) => Action::ShiftPush(MkMul),
        (Some(MkMul), Some(Star)) => Action::ReduceTo(MkMul),

        (
            None
            | Some(MkNum)
            | Some(MkAdd)
            | Some(MkMul)
            | Some(MkLetBind(_))
            | Some(MkLetBody(_, _)),
            Some(Let),
        ) => Action::ShiftPush(MkLetId),
        (Some(MkLetId), Some(Id(id))) => Action::MoveTo(MkLet(id.clone())),
        (Some(MkLet(id)), Some(Eq)) => Action::ShiftTo(MkLetBind(id.clone())),
        (Some(MkLetBind(_)), Some(In)) => Action::Reduce,
        (Some(MkNum) | Some(MkVar) | Some(MkAdd) | Some(MkMul), Some(In)) => Action::Reduce,
        (Some(MkLetBody(_, _)), Some(In)) => Action::ShiftSame,

        (Some(MkLetBind(_)), Some(Num(_))) => Action::ShiftPush(MkNum),
        (Some(MkLetBody(_, _)), Some(Num(_))) => Action::ShiftPush(MkNum),

        (_, Some(Id(_))) => Action::ShiftPush(MkVar),

        (Some(_), None) => Action::Reduce,
        (None, None) => Action::Accept,

        (None, Some(Plus) | Some(Star)) => Action::Error,
        _ => Action::Error,
    }
}

pub fn parse(mut pool: &mut ExprPool, mut lex: Lex) -> Result<ExprRef, String> {
    expr_parse(&mut pool, &mut lex)
}

fn reduce(
    pool: &mut ExprPool,
    terms: &mut Vec<Expr>,
    states: &mut Vec<State>,
) -> Result<(), String> {
    match states.pop() {
        Some(State::MkAdd) => {
            let e2 = terms.pop().unwrap();
            let e1 = terms.pop().unwrap();

            terms.push(Expr::Binary(BinOp::Add, pool.add(e1), pool.add(e2)));
            Ok(())
        }
        Some(State::MkMul) => {
            let e2 = terms.pop().unwrap();
            let e1 = terms.pop().unwrap();

            terms.push(Expr::Binary(BinOp::Mul, pool.add(e1), pool.add(e2)));
            Ok(())
        }
        Some(State::MkLetBody(id, assn)) => {
            let body = terms.pop().unwrap();

            terms.push(Expr::Let(id, pool.add(assn), pool.add(body)));
            Ok(())
        }
        Some(State::MkLetBind(id)) => {
            let assn = terms.pop().unwrap();

            states.push(State::MkLetBody(id, assn));
            Ok(())
        }
        Some(State::MkVar) => Ok(()),
        Some(State::MkNum) => Ok(()),
        s => Err(format!("Attempted to reduce irreducible state {:?}", s)),
    }
}

fn lexeme_push(terms: &mut Vec<Expr>, token: Option<&Lexeme>) {
    match token {
        Some(Lexeme::Num(n)) => terms.push(Expr::Num(*n)),
        Some(Lexeme::Id(id)) => terms.push(Expr::Id(id.clone())),
        _ => (),
    }
}

fn expr_parse(mut pool: &mut ExprPool, lex: &mut Lex) -> Result<ExprRef, String> {
    if let None = lex.peek() {
        return Err("No tokens".into());
    }

    let mut terms = Vec::new();
    let mut states = Vec::new();

    loop {
        let token = lex.peek();
        match next_action(states.last(), token) {
            Action::ShiftPush(s) => {
                lexeme_push(&mut terms, token);

                states.push(s);
                lex.consume();
            }
            Action::ShiftTo(s) => {
                lexeme_push(&mut terms, token);

                if let Some(last) = states.last_mut() {
                    *last = s;
                }
                lex.consume();
            }
            Action::ShiftSame => {
                dbg!("ShiftSame----------------", &states, &terms, token);
                lexeme_push(&mut terms, token);
                lex.consume();
            }
            Action::MoveTo(s) => {
                if let Some(last) = states.last_mut() {
                    *last = s;
                }
                lex.consume();
            }
            Action::ReduceTo(s) => {
                reduce(&mut pool, &mut terms, &mut states)?;

                states.push(s);
                lex.consume();
            }
            Action::Reduce => {
                dbg!("Reduce----------------", &states, &terms, token);
                reduce(&mut pool, &mut terms, &mut states)?;
            }
            Action::Accept => {
                let expr = pool.add(terms.pop().unwrap());
                if !terms.is_empty() {
                    return Err(format!("Dangling terms: {:?}", terms));
                } else {
                    return Ok(expr);
                }
            }
            Action::Error => {
                dbg!("Error----------------", &states, &terms, token);
                return Err(format!("Unexpected token {:?}", token));
            }
        }
    }
}

struct ParseTable {
    table: Vec<(HashMap<Terminal, Action>, HashMap<NonTerminal, usize>)>,
}

impl ParseTable {
    pub fn construct(rules: &Vec<Rule>) -> Self {
        let terminals = rules
            .iter()
            .flat_map(|r| {
                r.expr.iter().filter_map(|x| {
                    if let Token::Terminal(t) = x {
                        Some(*t)
                    } else {
                        None
                    }
                })
            })
            .collect::<Vec<_>>();
        let nonterminals = rules
            .iter()
            .flat_map(|r| {
                r.expr.iter().filter_map(|x| {
                    if let Token::NonTerminal(t) = x {
                        Some(*t)
                    } else {
                        None
                    }
                })
            })
            .collect::<Vec<_>>();
    }
}

#[cfg(test)]
mod test {
    use std::collections::{HashMap, HashSet};

    use ntest::timeout;

    use crate::{
        expr::{BinOp, Expr, ExprPool, ExprRef},
        lex::Lex,
        parse::{
            Item, ItemSet, NonTerminal, Position, Rule, Terminal, gen_first_table,
            gen_follow_table, parse,
        },
    };

    #[test]
    fn test_parse_int_literal() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("13");
        let result = parse(&mut pool, lex);

        assert_eq!(result, Ok(ExprRef(0)));
        assert_eq!(pool, ExprPool(vec![Expr::Num(13)]));
    }

    #[test]
    fn test_parse_binary_operation() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("12 + 25");
        let result = parse(&mut pool, lex);

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
        let lex = Lex::new("12 + 25 + 9");
        let result = parse(&mut pool, lex);

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

    #[test]
    fn test_bin_ops_observe_precedence() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("12 + 25 * 9");
        let result = parse(&mut pool, lex);

        assert_eq!(result, Ok(ExprRef(4)));
        assert_eq!(
            pool.0,
            vec![
                Expr::Num(25),
                Expr::Num(9),
                Expr::Num(12),
                Expr::Binary(BinOp::Mul, ExprRef(0), ExprRef(1)),
                Expr::Binary(BinOp::Add, ExprRef(2), ExprRef(3)),
            ]
        )
    }

    #[test]
    fn test_let_basic() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("let x = 5 in x");
        let result = parse(&mut pool, lex);

        assert_eq!(result, Ok(ExprRef(2)));
        assert_eq!(
            pool.0,
            vec![
                Expr::Num(5),
                Expr::id("x"),
                Expr::Let("x".into(), ExprRef(0), ExprRef(1)),
            ]
        );
    }

    #[test]
    fn test_let_as_term() {
        let mut pool = ExprPool::new();
        let lex = Lex::new("5 + let x = 5 in x + 1");
        let result = parse(&mut pool, lex);

        assert_eq!(result, Ok(ExprRef(6)));
        assert_eq!(
            pool.0,
            vec![
                Expr::id("x"),
                Expr::Num(1),
                Expr::Num(5),
                Expr::Binary(BinOp::Add, ExprRef(0), ExprRef(1)),
                Expr::Num(5),
                Expr::Let("x".into(), ExprRef(2), ExprRef(3)),
                Expr::Binary(BinOp::Add, ExprRef(4), ExprRef(5))
            ]
        );
    }

    fn rules() -> Vec<Rule> {
        use NonTerminal::*;
        use Terminal::*;

        vec![
            Rule::new(Start, vec![Expr.into()]),
            Rule::new(Expr, vec![Expr.into(), Plus.into(), Factor.into()]),
            Rule::new(Expr, vec![Factor.into()]),
            Rule::new(Factor, vec![Factor.into(), Star.into(), Term.into()]),
            Rule::new(Factor, vec![Term.into()]),
            Rule::new(Term, vec![LParen.into(), Expr.into(), RParen.into()]),
            Rule::new(Term, vec![Num.into()]),
            Rule::new(Term, vec![Id.into()]),
        ]
    }

    #[test]
    fn test_gen_first_table() {
        use NonTerminal::*;
        use Terminal::*;
        let rules = rules();
        let first_table = gen_first_table(&rules);

        let expected = HashMap::from([
            (Start, HashSet::from([LParen, Num, Id])),
            (Expr, HashSet::from([LParen, Num, Id])),
            (Factor, HashSet::from([LParen, Num, Id])),
            (Term, HashSet::from([LParen, Num, Id])),
        ]);
        assert_eq!(first_table, expected);
    }

    #[test]
    fn test_gen_follow_table() {
        use NonTerminal::*;
        use Terminal::*;
        let rules = rules();
        let follow_table = gen_follow_table(&rules);

        let expected = HashMap::from([
            (Start, HashSet::from([Eof])),
            (Expr, HashSet::from([Plus, RParen, Eof])),
            (Factor, HashSet::from([Plus, Star, RParen, Eof])),
            (Term, HashSet::from([Plus, Star, RParen, Eof])),
        ]);
        assert_eq!(follow_table, expected);
    }

    #[test]
    #[timeout(200)]
    fn test_gen_item_sets() {
        use NonTerminal::*;
        use Terminal::*;

        let rules = vec![
            Rule::new(Start, vec![Expr.into(), Eof.into()]),
            Rule::new(Expr, vec![Expr.into(), Plus.into(), Factor.into()]),
            Rule::new(Expr, vec![Factor.into()]),
            Rule::new(Factor, vec![Factor.into(), Star.into(), Term.into()]),
            Rule::new(Factor, vec![Term.into()]),
            Rule::new(Term, vec![Num.into()]),
            Rule::new(Term, vec![Id.into()]),
        ];
        let item_set_table = ItemSet::from_rules(&rules);

        // assert_eq!(item_set_table.len(), 10);

        dbg!(&item_set_table);

        assert!(item_set_table.contains(&ItemSet(HashSet::from([
            Item::new(Position::new(Start, [], [Expr.into(), Eof.into()]), []),
            Item::new(
                Position::new(Expr, [], [Expr.into(), Plus.into(), Term.into()]),
                [Eof.into()]
            ),
            Item::new(
                Position::new(Expr, [], [Term.into()]),
                [Eof.into(), Plus.into()]
            ),
            Item::new(
                Position::new(Term, [], [LParen.into(), Expr.into(), RParen.into()]),
                [Eof.into(), Plus.into()]
            ),
            Item::new(
                Position::new(Term, [], [Num.into()]),
                [Eof.into(), Plus.into()]
            ),
        ]))));
        // assert!(
        //     item_set_table.contains(&ItemSet(HashSet::from([Position::new(
        //         Term,
        //         [Num.into()],
        //         []
        //     )])))
        // );
        // assert!(item_set_table.contains(&ItemSet(HashSet::from([
        //     Position::new(Start, [Expr.into()], [Eof.into()]),
        //     Position::new(Expr, [Expr.into()], [Plus.into(), Term.into()]),
        // ]))));
        // assert!(
        //     item_set_table.contains(&ItemSet(HashSet::from([Position::new(
        //         Expr,
        //         [Term.into()],
        //         []
        //     )])))
        // );
        // assert!(item_set_table.contains(&ItemSet(HashSet::from([
        //     Position::new(Term, [], [LParen.into(), Expr.into(), RParen.into()]),
        //     Position::new(Expr, [], [Expr.into(), Plus.into(), Term.into()]),
        //     Position::new(Expr, [], [Term.into()]),
        //     Position::new(Term, [LParen.into()], [Expr.into(), RParen.into()]),
        //     Position::new(Term, [], [Num.into()]),
        // ]))));
        // assert!(item_set_table.contains(&ItemSet(HashSet::from([
        //     Position::new(Term, [LParen.into(), Expr.into()], [RParen.into()]),
        //     Position::new(Expr, [Expr.into()], [Plus.into(), Term.into()])
        // ]))));
        // assert!(item_set_table.contains(&ItemSet(HashSet::from([
        //     Position::new(Expr, [Expr.into(), Plus.into()], [Term.into()]),
        //     Position::new(Term, [], [LParen.into(), Expr.into(), RParen.into()]),
        //     Position::new(Term, [], [Num.into()]),
        // ]))));
        // assert!(
        //     item_set_table.contains(&ItemSet(HashSet::from([Position::new(
        //         Expr,
        //         [Expr.into(), Plus.into(), Term.into()],
        //         []
        //     )])))
        // );
        // assert!(
        //     item_set_table.contains(&ItemSet(HashSet::from([Position::new(
        //         Start,
        //         [Expr.into(), Eof.into()],
        //         []
        //     )])))
        // );
    }
}
