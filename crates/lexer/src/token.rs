#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Illegal,
    EOF,
    Ident,
    Int,
    Float,
    Str,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    EQ,
    NEQ,
    Or,
    And,
    Modulo,

    LT,
    GT,

    Comma,
    Dot,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    Arrow,
    LowDash,
    Pipe,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Elif,
    Else,
    Return,
    Switch,
    While,
}

impl TokenKind {
    pub fn is_to_keyword(literal: &str) -> Option<Self> {
        use TokenKind::*;
        match literal {
            "fn" => Some(Function),
            "let" => Some(Let),
            "true" => Some(True),
            "false" => Some(False),
            "if" => Some(If),
            "elif" => Some(Elif),
            "else" => Some(Else),
            "return" => Some(Return),
            "switch" => Some(Switch),
            "while" => Some(While),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
}

impl Token {
    pub fn new(kind: TokenKind, literal: String) -> Self {
        Self { kind, literal }
    }
}
