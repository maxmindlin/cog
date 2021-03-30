use crate::EnvPointer;

use parser::{Block, Identifier};

use std::rc::Rc;

// TODO add parameters for better debugging.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvalError {
    TypeMismatch,
    InvalidUsage,
    InvalidFnParams,
    NonFunction,
    UnknownIdent,
    UnknownPrefixOp,
    UnknownInfixOp,
    DuplicateDeclare,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Builtin(BuiltinKind),
    Boolean(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Null,
    Error(EvalError),
    Return(Rc<Object>),
    Func(Rc<FuncLiteral>),
}

impl Object {
    pub fn type_literal(&self) -> String {
        use Object::*;
        match self {
            Builtin(_) => "function".to_owned(),
            Boolean(_) => "bool".to_owned(),
            Int(_) => "int".to_owned(),
            Float(_) => "float".to_owned(),
            Str(_) => "string".to_owned(),
            Null => "null".to_owned(),
            Error(_) => "error".to_owned(),
            Return(o) => o.type_literal(),
            Func(_) => "function".to_owned(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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
    Type,
}

impl BuiltinKind {
    pub fn is_from(s: &str) -> Option<Self> {
        use BuiltinKind::*;
        match s {
            "puts" => Some(Puts),
            "type" => Some(Type),
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
            Type => {
                if args.len() != 1 {
                    return Rc::new(Object::Error(EvalError::InvalidFnParams));
                }
                Rc::new(Object::Str(args[0].type_literal()))
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
            Float(f) => f.to_string(),
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
            InvalidFnParams => "wrong number of fn params".to_owned(),
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
            Type => "type".to_owned(),
        }
    }
}
