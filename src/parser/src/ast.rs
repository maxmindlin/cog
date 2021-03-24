#[derive(Debug)]
pub enum NodeKind {
    Program(Program),
    Stmt(StmtKind),
    Expr(ExprKind),
}

#[derive(Debug, Default)]
pub struct Program {
    pub stmts: Vec<StmtKind>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    Let(Identifier, Option<ExprKind>),
    Return(Option<ExprKind>),
    Expr(ExprKind),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    Ident(Identifier),
    Int(i64),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}
