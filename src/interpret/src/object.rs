use crate::EnvPointer;

use parser::{Block, Identifier};

use std::rc::Rc;

// TODO add parameters for better debugging.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvalError {
    TypeMismatch,
    InvalidUsage,
    NonFunction,
    UnknownIdent,
    UnknownPrefixOp,
    UnknownInfixOp,
    DuplicateDeclare,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Builtin(BuiltinKind),
    Boolean(bool),
    Int(i64),
    Str(String),
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BuiltinKind {
    Puts,
}

impl BuiltinKind {
    pub fn is_from(s: &str) -> Option<Self> {
        use BuiltinKind::*;
        match s {
            "puts" => Some(Puts),
            _ => None,
        }
    }

    pub fn apply(&self, args: &Vec<Rc<Object>>) -> Rc<Object> {
        use BuiltinKind::*;
        match self {
            Puts => {
                args.iter().for_each(|a| println!("{}", a.to_string()));
                Rc::new(Object::Null)
            }
        }
    }
}

impl ToString for Object {
    fn to_string(&self) -> String {
        use Object::*;
        match self {
            Builtin(b) => b.to_string(),
            Boolean(b) => b.to_string(),
            Int(i) => i.to_string(),
            Str(s) => s.clone(),
            Null => "null".to_string(),
            Error(e) => e.to_string(),
            Return(obj) => obj.to_string(),
            Func(fn_lit) => {
                let p_str = fn_lit
                    .params
                    .iter()
                    .map(|p| p.name.clone())
                    .collect::<Vec<String>>()
                    .join(",");
                format!("fn ({}) {{body}}", p_str)
            }
        }
    }
}

impl ToString for EvalError {
    fn to_string(&self) -> String {
        use EvalError::*;
        match self {
            TypeMismatch => "type mismatch".to_owned(),
            InvalidUsage => "invalid usage".to_owned(),
            NonFunction => "not a function".to_owned(),
            UnknownIdent => "unknown identifier".to_owned(),
            UnknownPrefixOp => "unknown prefix operator".to_owned(),
            UnknownInfixOp => "unknown infix operator".to_owned(),
            DuplicateDeclare => "attempted duplicate ident declaration".to_owned(),
        }
    }
}

impl ToString for BuiltinKind {
    fn to_string(&self) -> String {
        use BuiltinKind::*;
        match self {
            Puts => "puts".to_owned(),
        }
    }
}
