use core::num;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
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

impl Terminal {
    fn from_lexeme(lexeme: &Lexeme) -> Terminal {
        match lexeme {
            Lexeme::LParen => Terminal::LParen,
            Lexeme::RParen => Terminal::RParen,
            // Comma,
            // Dot,
            Lexeme::Eq => Terminal::Eq,
            Lexeme::Plus => Terminal::Plus,
            // Minus,
            Lexeme::Star => Terminal::Star,
            // Slash,

            // Literals
            Lexeme::Id(_) => Terminal::Id,
            // Str(String),
            Lexeme::Num(_) => Terminal::Num,

            // Keywords
            // Inc,
            Lexeme::Let => Terminal::Let,
            Lexeme::In => Terminal::In,
            // If,
            // Then,
            // Else,
            //     }
            _ => panic!("Not prepared to handle this case"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum NonTerminal {
    Start,
    Expr,
    Factor,
    Term,
}

impl Into<Token> for Terminal {
    fn into(self) -> Token {
        Token::Terminal(self)
    }
}

impl Into<Token> for NonTerminal {
    fn into(self) -> Token {
        Token::NonTerminal(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Token {
    NonTerminal(NonTerminal),
    Terminal(Terminal),
}

impl Token {
    pub fn is_non_terminal(&self) -> bool {
        match self {
            Token::NonTerminal(_) => true,
            _ => false,
        }
    }
}

impl Display for Terminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminal::Let => write!(f, "Let"),
            Terminal::Id => write!(f, "Id"),
            Terminal::Eq => write!(f, "="),
            Terminal::In => write!(f, "In"),
            Terminal::Plus => write!(f, "+"),
            Terminal::Star => write!(f, "*"),
            Terminal::Num => write!(f, "Num"),
            Terminal::LParen => write!(f, "("),
            Terminal::RParen => write!(f, ")"),
            Terminal::Eof => write!(f, "$"),
        }
    }
}

impl Display for NonTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NonTerminal::Start => write!(f, "<Start>"),
            NonTerminal::Expr => write!(f, "<Expr>"),
            NonTerminal::Factor => write!(f, "<Factor>"),
            NonTerminal::Term => write!(f, "<Term>"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Terminal(t) => write!(f, "{}", t),
            Token::NonTerminal(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Rule {
    head: NonTerminal,
    body: Vec<Token>,
}

impl Rule {
    pub fn new<T: Into<Vec<Token>>>(head: NonTerminal, body: T) -> Self {
        Self {
            head,
            body: body.into(),
        }
    }

    pub fn as_item(&self, lookahead: Option<Terminal>) -> Item {
        Item::new(self.clone(), 0, lookahead)
    }

    pub fn at(&self, pos: usize) -> Option<Token> {
        if pos < self.body.len() {
            Some(self.body[pos])
        } else {
            None
        }
    }

    pub fn pairs(&self) -> impl Iterator<Item = (Token, Token)> {
        self.body.windows(2).map(|w| (w[0], w[1])).into_iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Item {
    rule: Rule,
    lookahead: Option<Terminal>,
    pos: usize,
}

impl Item {
    pub fn new(rule: Rule, pos: usize, lookahead: Option<Terminal>) -> Self {
        Self {
            rule,
            pos,
            lookahead,
        }
    }

    pub fn locus(&self) -> Option<Token> {
        self.rule.at(self.pos)
    }

    pub fn next(&self) -> Option<Token> {
        self.rule.at(self.pos + 1)
    }

    pub fn successor(&self) -> Option<Self> {
        if self.pos + 1 <= self.rule.body.len() {
            Some(Item::new(self.rule.clone(), self.pos + 1, self.lookahead))
        } else {
            None
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ->", self.rule.head)?;

        for (i, r) in self.rule.body.iter().enumerate() {
            if i == self.pos {
                write!(f, " .")?;
            }
            write!(f, " {}", r)?;
        }

        if let Some(t) = self.lookahead {
            write!(f, ", {}", t)?;
        } else {
            write!(f, ",")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ItemSet {
    items: HashSet<Item>,
}

impl ItemSet {
    pub fn from<T: Into<Vec<Rule>>>(rules: T) -> Self {
        let rules: Vec<Rule> = rules.into();
        Self {
            items: rules
                .iter()
                .map(|x| x.as_item(None))
                .collect::<HashSet<_>>(),
        }
    }

    pub fn from_set(items: HashSet<Item>) -> Self {
        Self { items }
    }

    pub fn closed(
        &self,
        rules: &Vec<Rule>,
        firsts: &HashMap<NonTerminal, HashSet<Terminal>>,
    ) -> ItemSet {
        let mut items = ItemSet::from_set(self.items.clone());
        let mut changed = true;
        while changed {
            changed = false;

            let mut to_add = HashSet::new();
            for item in items.items.iter() {
                // dbg!(item);
                match item.locus() {
                    Some(Token::NonTerminal(t)) => {
                        let lookaheads = items.lookaheads(t, firsts);
                        to_add.extend(
                            rules
                                .iter()
                                .filter(|x| x.head == t)
                                .flat_map(|x| lookaheads.iter().map(|t| x.as_item(Some(*t)))),
                        );
                    }
                    _ => (),
                }
            }

            for t in to_add {
                changed |= items.items.insert(t);
            }
        }
        // dbg!("==========================");

        items
    }

    pub fn successor(&self) -> Option<ItemSet> {
        let set = self
            .items
            .iter()
            .filter_map(|x| x.successor())
            .collect::<HashSet<_>>();
        if set.len() != 0 {
            Some(ItemSet::from_set(set))
        } else {
            None
        }
    }

    pub fn lookaheads(
        &self,
        locus: NonTerminal,
        firsts: &HashMap<NonTerminal, HashSet<Terminal>>,
    ) -> HashSet<Terminal> {
        self.items
            .iter()
            .filter(|x| x.locus() == Some(Token::NonTerminal(locus)))
            .flat_map(|x| match x.next() {
                Some(Token::NonTerminal(t)) => firsts
                    .get(&t)
                    .into_iter()
                    .flat_map(|x| x.clone().into_iter())
                    .collect::<Vec<_>>(),
                Some(Token::Terminal(t)) => [t].into_iter().collect::<Vec<_>>(),
                None => x.lookahead.into_iter().collect::<Vec<_>>(),
            })
            .collect::<HashSet<_>>()
    }

    pub fn partition_on_locus(&self, alphabet: &HashSet<Token>) -> Vec<ItemSet> {
        alphabet
            .iter()
            .map(|x| ItemSet {
                items: self
                    .items
                    .clone()
                    .into_iter()
                    .filter(|i| i.locus() == Some(*x))
                    .collect::<HashSet<_>>(),
            })
            .collect::<Vec<_>>()
    }
}

impl Display for ItemSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for item in self.items.iter() {
            write!(f, "{}\n", item)?;
        }

        Ok(())
    }
}

enum ReduceAction {
    MkAdd,
    MkMul,
}

impl ReduceAction {
    fn pop_n<T: Sized>(stack: &mut Vec<T>, n: usize) -> Vec<T> {
        let mut popped = Vec::new();
        for _ in 0..n {
            popped.push(stack.pop().unwrap());
        }

        popped
    }

    pub fn apply(&self, ast: &mut ExprPool, states: &mut Vec<usize>, inputs: &mut Vec<ExprRef>) {
        match self {
            ReduceAction::MkAdd => {
                let _ = Self::pop_n(states, 3);

                let exprs = Self::pop_n(inputs, 2);
                inputs.push(ast.add(Expr::Binary(BinOp::Add, exprs[0], exprs[1])));
            }
            ReduceAction::MkMul => {
                let _ = Self::pop_n(states, 3);

                let exprs = Self::pop_n(inputs, 2);
                inputs.push(ast.add(Expr::Binary(BinOp::Mul, exprs[0], exprs[1])));
            }
        }
    }
}

enum Action {
    Shift(usize),
    Accept,
    Reduce(Item, ReduceAction),
    Expected,
}

struct Grammar {
    rules: Vec<Rule>,
    item_sets: Vec<ItemSet>,
}

impl Grammar {
    pub fn new<T: Into<Vec<Rule>>, U: Into<Vec<Token>>>(rules: T, start_production: U) -> Self {
        let mut rules: Vec<Rule> = rules.into();
        rules.push(Rule::new(NonTerminal::Start, start_production.into()));

        let alphabet = rules
            .iter()
            .flat_map(|x| [vec![Token::NonTerminal(x.head)], x.body.clone()].concat())
            .collect::<HashSet<_>>();

        let firsts = Self::generate_first_sets(&rules);

        let mut worklist = vec![ItemSet::from_set(HashSet::from([rules
            .last()
            .unwrap()
            .as_item(Some(Terminal::Eof))]))];
        let mut done = Vec::new();

        while let Some(set) = worklist.pop() {
            let set = set.closed(&rules, &firsts);

            if done.contains(&set) {
                continue;
            }

            worklist.extend(
                set.partition_on_locus(&alphabet)
                    .iter()
                    .filter_map(|x| x.successor()),
            );

            done.push(set);
        }

        Self {
            rules,
            item_sets: done,
        }
    }

    fn generate_first_sets(rules: &Vec<Rule>) -> HashMap<NonTerminal, HashSet<Terminal>> {
        let mut table: HashMap<NonTerminal, HashSet<Terminal>> = HashMap::new();
        let mut changed = true;
        while changed {
            changed = false;
            for rule in rules {
                changed |= Self::extend_table(&mut table, rule.head, rule.body[0]);
            }
        }
        return table;
    }

    fn generate_follow_sets(rules: &Vec<Rule>) -> HashMap<NonTerminal, HashSet<Terminal>> {
        let mut table: HashMap<NonTerminal, HashSet<Terminal>> = HashMap::new();
        let mut changed = true;
        while changed {
            changed = false;
            for rule in rules {
                for (l, r) in rule.pairs() {
                    if let Token::NonTerminal(t) = l {
                        changed |= Self::extend_table(&mut table, t, r);
                    }
                }

                if let Some(Token::NonTerminal(lhs)) = rule.body.last() {
                    changed |= Self::extend_table(&mut table, *lhs, Token::NonTerminal(rule.head));
                }
            }
        }
        return table;
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

    fn action(&self, state: usize, term: Terminal) -> Action {
        todo!()
    }

    fn goto(&self, state: usize, item: &Item) -> usize {
        todo!()
    }

    fn expected(&self, state: usize) -> Vec<Terminal> {
        Vec::new()
    }

    pub fn into_ast(&self, mut lex: Lex) -> Option<(ExprPool, ExprRef)> {
        let mut ast = ExprPool::new();

        let mut states = vec![0 as usize];
        let mut inputs = Vec::new();

        while true {
            let token = lex.peek()?;
            let state = *states.last()?;

            match self.action(state, Terminal::from_lexeme(token)) {
                Action::Shift(s) => {
                    states.push(s);
                    match token {
                        Lexeme::Num(n) => inputs.push(ast.add(Expr::Num(*n))),
                        Lexeme::Id(id) => inputs.push(ast.add(Expr::Id(id.clone()))),
                        _ => (),
                    }
                    lex.consume();
                }
                Action::Accept => {
                    assert!(inputs.len() == 1);
                    let start = inputs.pop()?;
                    return Some((ast, start));
                }
                Action::Reduce(item, action) => {
                    action.apply(&mut ast, &mut states, &mut inputs);

                    let state = *states.last()?;
                    states.push(self.goto(state, &item));
                }
                Action::Expected => {
                    let expected = self.expected(state);
                    println!("Expected one of {:?}", expected);
                    return None;
                }
            }
        }

        None
    }
}

impl Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, item_set) in self.item_sets.iter().enumerate() {
            write!(f, "{}: {}\n", i, item_set)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::collections::HashSet;

    use crate::parse::Grammar;
    use crate::parse::NonTerminal;
    use crate::parse::Rule;
    use crate::parse::Terminal;

    fn rules() -> Vec<Rule> {
        use NonTerminal::*;
        use Terminal::*;

        vec![
            Rule::new(Start, vec![Expr.into(), Eof.into()]),
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
        let first_table = Grammar::generate_first_sets(&rules);

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
        let follow_table = Grammar::generate_follow_sets(&rules);

        let expected = HashMap::from([
            (Expr, HashSet::from([Plus, RParen, Eof])),
            (Factor, HashSet::from([Plus, Star, RParen, Eof])),
            (Term, HashSet::from([Plus, Star, RParen, Eof])),
        ]);
        assert_eq!(follow_table, expected);
    }

    #[test]
    fn generate_parse_tables_test() {
        use NonTerminal::*;
        use Terminal::*;

        let rules = [
            Rule::new(Expr, [Term.into()]),
            Rule::new(Expr, [LParen.into(), Expr.into(), RParen.into()]),
            Rule::new(Term, [Num.into()]),
            Rule::new(Term, [Plus.into(), Term.into()]),
            Rule::new(Term, [Term.into(), Plus.into(), Num.into()]),
        ];
        let grammar = Grammar::new(rules, [Expr.into()]);

        println!("{}", grammar);

        assert!(false);
    }
}
