#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct ExprRef(pub usize);

#[derive(Debug, PartialEq, Eq)]
pub struct FlatTree<T> {
    program: Vec<T>,
}

impl<T> FlatTree<T> {
    pub fn new() -> FlatTree<T> {
        Self {
            program: Vec::new(),
        }
    }

    pub fn get(&self, index: ExprRef) -> Result<&T, String> {
        self.program
            .get(index.0)
            .ok_or_else(|| format!("Failed to find Expr at index {}", index.0))
    }

    pub fn add(&mut self, x: T) -> ExprRef {
        let index = self.program.len();
        self.program.push(x);

        ExprRef(index)
    }
}
