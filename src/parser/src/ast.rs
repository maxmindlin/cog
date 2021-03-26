use lexer::TokenKind;

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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StmtKind {
    Let(Identifier, Option<ExprKind>),
    Assign(Identifier, ExprKind),
    Return(Option<ExprKind>),
    Expr(ExprKind),
    While(Box<ExprKind>, Block),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Boolean(bool),
    Ident(Identifier),
    Int(i64),
    Str(String),
    If(Box<ExprKind>, Block, Block),
    Switch(Box<ExprKind>, Box<Vec<SwitchCase>>, Block),
    Prefix(TokenKind, Box<ExprKind>),
    Infix(TokenKind, Box<ExprKind>, Box<ExprKind>),
    Func(Vec<Identifier>, Block),
    Call(Box<ExprKind>, Box<Vec<ExprKind>>),
    Chain(Box<Vec<ExprKind>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SwitchCase {
    pub cond: ExprKind,
    pub conseq: Block,
}

impl SwitchCase {
    pub fn new(cond: ExprKind, conseq: Block) -> Self {
        Self { cond, conseq }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Block {
    pub stmts: Vec<StmtKind>,
}

impl Block {
    pub fn new(stmts: Vec<StmtKind>) -> Self {
        Self { stmts }
    }
}
