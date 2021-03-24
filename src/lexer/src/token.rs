#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Illegal,
    EOF,
    Ident,
    Int,
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

    LT,
    GT,

    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
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
            "else" => Some(Else),
            "return" => Some(Return),
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
