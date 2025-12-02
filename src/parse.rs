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

    pub fn first_pos(&self) -> Item {
        Item::new(self.symbol, Vec::new(), self.expr.clone())
    }
}

fn extend_itemsets_table(table: &mut HashMap<Token, ItemSet>, item: Item) {
    let lhs = *item.parsed.last().expect("This should never fail");
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
struct Item {
    lhs: NonTerminal,
    parsed: Vec<Token>,
    expected: Vec<Token>,
}

impl Item {
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

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ->", self.lhs)?;
        for tok in self.parsed.iter() {
            write!(f, " {}", tok)?;
        }
        write!(f, " .")?;
        for tok in self.expected.iter() {
            write!(f, " {}", tok)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ItemSet(HashSet<Item>);

impl ItemSet {
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
                if let Some(Token::NonTerminal(t)) = item.expected.first() {
                    for rule in rules.iter() {
                        if rule.symbol == *t {
                            to_add.insert(rule.first_pos());
                        }
                    }
                }
            }

            for item in to_add {
                changed |= self.insert(item.clone());
            }
        }
    }

    fn partition(&self, token: &Token) -> ItemSet {
        let mut partition = ItemSet(HashSet::new());
        for item in self.0.iter() {
            let t = item.expected.first();
            if t.is_some_and(|x| token == x) {
                partition.insert(item.clone());
            }
        }

        partition
    }

    fn advance_all(&self) -> ItemSet {
        let mut advanced = ItemSet(HashSet::new());
        for item in self.0.iter() {
            if let Some(next) = item.next() {
                advanced.insert(next);
            }
        }

        advanced
    }
}

impl Display for ItemSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for item in self.0.iter() {
            write!(f, "{}\n", item)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Grammar {
    states: Vec<ItemSet>,
    transitions: Vec<HashMap<Token, usize>>,
}

impl Grammar {
    pub fn new(rules: &Vec<Rule>, tokens: &Vec<Token>) -> Self {
        let first_table = gen_first_table(rules);
        let follow_table = gen_follow_table(rules);
        let first = rules[0].first_pos();
        let mut worklist = vec![ItemSet(HashSet::from([first]))];
        let mut item_sets = Vec::new();
        let mut transitions: Vec<HashMap<Token, usize>> = Vec::new();

        while let Some(mut itemset) = worklist.pop() {
            itemset.expand(rules, &first_table, &follow_table);

            if item_sets.contains(&itemset) {
                continue;
            }

            let mut next_item_sets = HashMap::new();
            for item in itemset.0.iter() {
                if let Some(i) = item.next() {
                    extend_itemsets_table(&mut next_item_sets, i);
                }
            }

            for next in next_item_sets.into_values() {
                worklist.push(next);
            }

            item_sets.push(itemset);
        }

        for item_set in item_sets.iter() {
            let mut t: HashMap<Token, usize> = HashMap::new();
            for token in tokens {
                let mut transition_state = item_set.partition(token).advance_all();
                transition_state.expand(rules, &first_table, &follow_table);
                let pos = item_sets.iter().position(|x| x == &transition_state);

                if let Some(idx) = pos {
                    t.insert(*token, idx);
                }
            }

            transitions.push(t);
        }

        Self {
            states: item_sets,
            transitions: transitions,
        }
    }

    pub fn index_of(&self, item_set: &ItemSet) -> Option<usize> {
        self.states.iter().position(|x| x == item_set)
    }

    pub fn transitions(&self, state: usize) -> &HashMap<Token, usize> {
        &self.transitions[state]
    }
}

impl Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, item_set) in self.states.iter().enumerate() {
            write!(f, "{}: {}\n", i, item_set)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::collections::{HashMap, HashSet};

    use crate::parse::{
        Grammar, Item, ItemSet, NonTerminal, Rule, Terminal, Token, gen_first_table,
        gen_follow_table,
    };

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
            (Expr, HashSet::from([Plus, RParen, Eof])),
            (Factor, HashSet::from([Plus, Star, RParen, Eof])),
            (Term, HashSet::from([Plus, Star, RParen, Eof])),
        ]);
        assert_eq!(follow_table, expected);
    }

    #[test]
    fn test_gen_item_sets() {
        use NonTerminal::*;
        use Terminal::*;

        let rules = vec![
            Rule::new(Start, vec![Expr.into(), Eof.into()]),
            Rule::new(Expr, vec![Expr.into(), Plus.into(), Term.into()]),
            Rule::new(Expr, vec![Term.into()]),
            Rule::new(Term, vec![LParen.into(), Expr.into(), RParen.into()]),
            Rule::new(Term, vec![Num.into()]),
        ];
        let tokens: Vec<Token> = vec![
            Expr.into(),
            Term.into(),
            Num.into(),
            Plus.into(),
            LParen.into(),
            RParen.into(),
            Eof.into(),
        ];
        let grammar = Grammar::new(&rules, &tokens);

        assert_eq!(grammar.states.len(), 10);

        println!("{}", grammar);

        assert!(grammar.states.contains(&ItemSet(HashSet::from([
            Item::new(Start, [], [Expr.into(), Eof.into()]),
            Item::new(Expr, [], [Expr.into(), Plus.into(), Term.into()]),
            Item::new(Expr, [], [Term.into()]),
            Item::new(Term, [], [LParen.into(), Expr.into(), RParen.into()]),
            Item::new(Term, [], [Num.into()]),
        ]))));
        assert!(grammar.states.contains(&ItemSet(HashSet::from([Item::new(
            Term,
            [Num.into()],
            []
        )]))));
        assert!(grammar.states.contains(&ItemSet(HashSet::from([
            Item::new(Start, [Expr.into()], [Eof.into()]),
            Item::new(Expr, [Expr.into()], [Plus.into(), Term.into()]),
        ]))));
        assert!(grammar.states.contains(&ItemSet(HashSet::from([Item::new(
            Expr,
            [Term.into()],
            []
        )]))));
        assert!(grammar.states.contains(&ItemSet(HashSet::from([
            Item::new(Term, [], [LParen.into(), Expr.into(), RParen.into()]),
            Item::new(Expr, [], [Expr.into(), Plus.into(), Term.into()]),
            Item::new(Expr, [], [Term.into()]),
            Item::new(Term, [LParen.into()], [Expr.into(), RParen.into()]),
            Item::new(Term, [], [Num.into()]),
        ]))));
        assert!(grammar.states.contains(&ItemSet(HashSet::from([
            Item::new(Term, [LParen.into(), Expr.into()], [RParen.into()]),
            Item::new(Expr, [Expr.into()], [Plus.into(), Term.into()])
        ]))));
        assert!(grammar.states.contains(&ItemSet(HashSet::from([
            Item::new(Expr, [Expr.into(), Plus.into()], [Term.into()]),
            Item::new(Term, [], [LParen.into(), Expr.into(), RParen.into()]),
            Item::new(Term, [], [Num.into()]),
        ]))));
        assert!(grammar.states.contains(&ItemSet(HashSet::from([Item::new(
            Expr,
            [Expr.into(), Plus.into(), Term.into()],
            []
        )]))));
        assert!(grammar.states.contains(&ItemSet(HashSet::from([Item::new(
            Start,
            [Expr.into(), Eof.into()],
            []
        )]))));
    }

    /// Table constructed from rules as shown [here](https://en.wikipedia.org/wiki/LR_parser#Finding_the_reachable_item_sets_and_the_transitions_between_them)
    /// We have one extra state because our Grammar considers S -> E $ . to be a valid state
    #[test]
    fn test_lr0_parse_table() {
        use NonTerminal::*;
        use Terminal::*;

        let rules = vec![
            Rule::new(Start, vec![Expr.into(), Eof.into()]),
            Rule::new(Expr, vec![Expr.into(), Plus.into(), Term.into()]),
            Rule::new(Expr, vec![Expr.into(), Star.into(), Term.into()]),
            Rule::new(Expr, vec![Term.into()]),
            Rule::new(Term, vec![Num.into()]),
            Rule::new(Term, vec![Id.into()]),
        ];
        let tokens = vec![
            Expr.into(),
            Eof.into(),
            Plus.into(),
            Term.into(),
            Star.into(),
            Num.into(),
            Id.into(),
        ];
        let grammar = Grammar::new(&rules, &tokens);

        println!("{}", grammar);

        assert_eq!(grammar.states.len(), 10);

        // States with no transitions
        let s1 = ItemSet(HashSet::from([Item::new(Term, [Id.into()], [])]));
        let s2 = ItemSet(HashSet::from([Item::new(Term, [Num.into()], [])]));
        let s4 = ItemSet(HashSet::from([Item::new(Expr, [Term.into()], [])]));
        let s7 = ItemSet(HashSet::from([Item::new(
            Expr,
            [Expr.into(), Star.into(), Term.into()],
            [],
        )]));
        let s8 = ItemSet(HashSet::from([Item::new(
            Expr,
            [Expr.into(), Plus.into(), Term.into()],
            [],
        )]));
        let s9 = ItemSet(HashSet::from([Item::new(
            Start,
            [Expr.into(), Eof.into()],
            [],
        )]));

        assert!(grammar.states.contains(&s1));
        assert!(grammar.states.contains(&s2));
        assert!(grammar.states.contains(&s4));
        assert!(grammar.states.contains(&s7));
        assert!(grammar.states.contains(&s8));
        assert!(grammar.states.contains(&s9));

        assert_eq!(
            grammar.transitions(grammar.index_of(&s1).unwrap()),
            &HashMap::new()
        );
        assert_eq!(
            grammar.transitions(grammar.index_of(&s2).unwrap()),
            &HashMap::new()
        );
        assert_eq!(
            grammar.transitions(grammar.index_of(&s4).unwrap()),
            &HashMap::new()
        );
        assert_eq!(
            grammar.transitions(grammar.index_of(&s7).unwrap()),
            &HashMap::new()
        );
        assert_eq!(
            grammar.transitions(grammar.index_of(&s8).unwrap()),
            &HashMap::new()
        );
        assert_eq!(
            grammar.transitions(grammar.index_of(&s9).unwrap()),
            &HashMap::new()
        );

        // States with transitions
        let s0 = ItemSet(HashSet::from([
            Item::new(Start, [], [Expr.into(), Eof.into()]),
            Item::new(Expr, [], [Expr.into(), Star.into(), Term.into()]),
            Item::new(Expr, [], [Expr.into(), Plus.into(), Term.into()]),
            Item::new(Expr, [], [Term.into()]),
            Item::new(Term, [], [Id.into()]),
            Item::new(Term, [], [Num.into()]),
        ]));

        let s3 = ItemSet(HashSet::from([
            Item::new(Start, [Expr.into()], [Eof.into()]),
            Item::new(Expr, [Expr.into()], [Plus.into(), Term.into()]),
            Item::new(Expr, [Expr.into()], [Star.into(), Term.into()]),
        ]));

        let s5 = ItemSet(HashSet::from([
            Item::new(Expr, [Expr.into(), Star.into()], [Term.into()]),
            Item::new(Term, [], [Id.into()]),
            Item::new(Term, [], [Num.into()]),
        ]));

        let s6 = ItemSet(HashSet::from([
            Item::new(Expr, [Expr.into(), Plus.into()], [Term.into()]),
            Item::new(Term, [], [Id.into()]),
            Item::new(Term, [], [Num.into()]),
        ]));

        assert!(grammar.states.contains(&s0));
        assert!(grammar.states.contains(&s3));
        assert!(grammar.states.contains(&s5));
        assert!(grammar.states.contains(&s6));

        assert_eq!(
            grammar.transitions(grammar.index_of(&s0).unwrap()),
            &HashMap::from([
                (Id.into(), grammar.index_of(&s1).unwrap()),
                (Num.into(), grammar.index_of(&s2).unwrap()),
                (Expr.into(), grammar.index_of(&s3).unwrap()),
                (Term.into(), grammar.index_of(&s4).unwrap())
            ])
        );
        assert_eq!(
            grammar.transitions(grammar.index_of(&s3).unwrap()),
            &HashMap::from([
                (Star.into(), grammar.index_of(&s5).unwrap()),
                (Plus.into(), grammar.index_of(&s6).unwrap()),
                (Eof.into(), grammar.index_of(&s9).unwrap()),
            ])
        );
        assert_eq!(
            grammar.transitions(grammar.index_of(&s5).unwrap()),
            &HashMap::from([
                (Id.into(), grammar.index_of(&s1).unwrap()),
                (Num.into(), grammar.index_of(&s2).unwrap()),
                (Term.into(), grammar.index_of(&s7).unwrap()),
            ])
        );
        assert_eq!(
            grammar.transitions(grammar.index_of(&s6).unwrap()),
            &HashMap::from([
                (Id.into(), grammar.index_of(&s1).unwrap()),
                (Num.into(), grammar.index_of(&s2).unwrap()),
                (Term.into(), grammar.index_of(&s8).unwrap()),
            ])
        );
    }
}
