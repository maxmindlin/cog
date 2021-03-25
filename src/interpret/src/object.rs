use crate::EnvPointer;

use parser::{Block, Identifier};

use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvalError {
    TypeMismatch,
    InvalidUsage,
    NonFunction,
    UnknownIdent,
    UnknownPrefixOp,
    UnknownInfixOp,
    IdentExists,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Boolean(bool),
    Int(i64),
    Null,
    Error(EvalError),
    Return(Rc<Object>),
    Func(Rc<FuncLiteral>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncLiteral {
    pub params: Vec<Identifier>,
    pub body: Block,
    pub env: EnvPointer,
}

impl FuncLiteral {
    pub fn new(params: Vec<Identifier>, body: Block, env: EnvPointer) -> Self {
        Self { params, body, env }
    }
}
