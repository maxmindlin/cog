mod ast;

pub use ast::{ExprKind, Identifier, NodeKind, Program, StmtKind};

use lexer::{Lexer, Token, TokenKind};

type ParseResult<T> = Result<T, ParseError>;
type PrefixParseFn = fn(parser: &mut Parser) -> ParseResult<ExprKind>;
type InfixParseFn = fn(parser: &mut Parser, ExprKind) -> ParseResult<ExprKind>;

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl From<TokenKind> for Precedence {
    fn from(value: TokenKind) -> Self {
        use TokenKind::*;
        match value {
            EQ => Self::Equals,
            NEQ => Self::Equals,
            LT => Self::LessGreater,
            GT => Self::LessGreater,
            Plus => Self::Sum,
            Minus => Self::Sum,
            Slash => Self::Product,
            Asterisk => Self::Product,
            LParen => Self::Call,
            LBracket => Self::Index,
            _ => Self::Lowest,
        }
    }
}

fn map_prefix_fn(kind: &TokenKind) -> Option<PrefixParseFn> {
    use TokenKind::*;
    match kind {
        Ident => Some(Parser::parse_ident),
        Int => Some(Parser::parse_int_literal),
        _ => None,
    }
}

fn map_infix_fn(kind: &TokenKind) -> Option<InfixParseFn> {
    use TokenKind::*;
    match kind {
        _ => None,
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(TokenKind, TokenKind),
    UnknownPrefix(TokenKind),
    InvalidInt(String),
}

pub struct Parser {
    lex: Lexer,
    curr: Token,
    peek: Token,
}

impl Parser {
    pub fn new(mut lex: Lexer) -> Self {
        let curr = lex.next_token();
        let peek = lex.next_token();
        Self { lex, curr, peek }
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut prgm = Program::default();
        while self.curr.kind != TokenKind::EOF {
            let stmt = self.parse_stmt()?;
            prgm.stmts.push(stmt);
            self.next_token();
        }
        Ok(prgm)
    }

    fn expect_peek(&mut self, expected: TokenKind) -> ParseResult<()> {
        if self.peek.kind == expected {
            self.next_token();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken(expected, self.peek.kind))
        }
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::from(self.peek.kind)
    }

    fn curr_precedence(&self) -> Precedence {
        Precedence::from(self.curr.kind)
    }

    fn next_token(&mut self) {
        let prev = std::mem::replace(&mut self.peek, self.lex.next_token());
        self.curr = prev;
    }

    fn parse_stmt(&mut self) -> ParseResult<StmtKind> {
        match self.curr.kind {
            TokenKind::Let => self.parse_let_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> ParseResult<StmtKind> {
        self.expect_peek(TokenKind::Ident)?;
        let id = Identifier::new(self.curr.literal.clone());
        // If the next token is semicolon, it signifies a ident
        // declaration with no expression value - which is valid
        // (ex: `let x;`). Consume semicolon token before
        // returning empty let stmt.
        if self.peek.kind == TokenKind::Semicolon {
            self.next_token();
            self.next_token();
            return Ok(StmtKind::Let(id, None));
        }
        self.expect_peek(TokenKind::Assign)?;
        self.next_token();

        let val = self.parse_expr(Precedence::Lowest)?;
        if self.peek.kind == TokenKind::Semicolon {
            self.next_token();
        }
        Ok(StmtKind::Let(id, Some(val)))
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<StmtKind> {
        let expr = self.parse_expr(Precedence::Lowest)?;
        let stmt = StmtKind::Expr(expr);
        if self.peek.kind == TokenKind::Semicolon {
            self.next_token();
        }
        Ok(stmt)
    }

    fn parse_expr(&mut self, precedence: Precedence) -> ParseResult<ExprKind> {
        match map_prefix_fn(&self.curr.kind) {
            None => Err(ParseError::UnknownPrefix(self.curr.kind)),
            Some(f) => {
                let mut lhs = f(self)?;
                while self.peek.kind != TokenKind::Semicolon && precedence < self.peek_precedence()
                {
                    match map_infix_fn(&self.peek.kind) {
                        None => return Ok(lhs),
                        Some(in_fn) => {
                            self.next_token();
                            lhs = in_fn(self, lhs)?;
                        }
                    }
                }

                Ok(lhs)
            }
        }
    }

    fn parse_ident(&mut self) -> ParseResult<ExprKind> {
        Ok(ExprKind::Ident(Identifier::new(self.curr.literal.clone())))
    }

    fn parse_int_literal(&mut self) -> ParseResult<ExprKind> {
        let to_parse = self.curr.literal.clone();
        match to_parse.parse::<i64>() {
            Ok(i) => Ok(ExprKind::Int(i)),
            Err(_) => Err(ParseError::InvalidInt(to_parse)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    fn setup_parser(input: &str) -> Parser {
        let l = Lexer::new(input);
        Parser::new(l)
    }

    #[test_case(
        "let x = 1;",
        StmtKind::Let(
            Identifier::new("x".into()),
            Some(ExprKind::Int(1)),
        );
        "int assignment"
    )]
    #[test_case("let x;", StmtKind::Let(Identifier::new("x".into()), None); "empty")]
    fn test_parse_let(input: &str, exp: StmtKind) {
        let prgm = setup_parser(input).parse_program().unwrap();
        assert_eq!(prgm.stmts.len(), 1);
        let stmt = &prgm.stmts[0];
        assert_eq!(*stmt, exp);
    }
}
