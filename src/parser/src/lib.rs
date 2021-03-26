mod ast;

pub use ast::{ExprKind, Identifier, NodeKind, Program, StmtKind, Block, SwitchCase, IfBlock};

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
            And => Self::Equals,
            Or => Self::Equals,
            EQ => Self::Equals,
            NEQ => Self::Equals,
            Modulo => Self::Equals,
            LT => Self::LessGreater,
            GT => Self::LessGreater,
            Plus => Self::Sum,
            Minus => Self::Sum,
            Slash => Self::Product,
            Asterisk => Self::Product,
            LParen => Self::Call,
            LBracket => Self::Index,
            Pipe => Self::Index,
            _ => Self::Lowest,
        }
    }
}

fn map_prefix_fn(kind: &TokenKind) -> Option<PrefixParseFn> {
    use TokenKind::*;
    match kind {
        Ident => Some(Parser::parse_ident),
        Int => Some(Parser::parse_int_literal),
        True => Some(Parser::parse_boolean),
        False => Some(Parser::parse_boolean),
        Bang => Some(Parser::parse_prefix),
        Minus => Some(Parser::parse_prefix),
        Function => Some(Parser::parse_fn_expr),
        If => Some(Parser::parse_if_expr),
        Switch => Some(Parser::parse_switch_expr),
        Str => Some(Parser::parse_str_literal),
        _ => None,
    }
}

fn map_infix_fn(kind: &TokenKind) -> Option<InfixParseFn> {
    use TokenKind::*;
    match kind {
        Plus => Some(Parser::parse_infix),
        Minus => Some(Parser::parse_infix),
        Slash => Some(Parser::parse_infix),
        Asterisk => Some(Parser::parse_infix),
        EQ => Some(Parser::parse_infix),
        NEQ => Some(Parser::parse_infix),
        LT => Some(Parser::parse_infix),
        GT => Some(Parser::parse_infix),
        And => Some(Parser::parse_infix),
        Or => Some(Parser::parse_infix),
        Modulo => Some(Parser::parse_infix),
        LParen => Some(Parser::parse_call_expr),
        Pipe => Some(Parser::parse_chain_expr),
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
            TokenKind::Ident => {
                match self.peek.kind {
                    TokenKind::Assign => self.parse_assign_stmt(),
                    _ => self.parse_expr_stmt(),
                }
            },
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::While => self.parse_while_expr(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_return_stmt(&mut self) -> ParseResult<StmtKind> {
        self.next_token();
        if self.curr.kind == TokenKind::Semicolon {
            return Ok(StmtKind::Return(None));
        }
        let val = self.parse_expr(Precedence::Lowest)?;
        if self.peek.kind == TokenKind::Semicolon {
            self.next_token();
        }
        Ok(StmtKind::Return(Some(val)))
    }

    fn parse_assign_stmt(&mut self) -> ParseResult<StmtKind> {
        let id = Identifier::new(self.curr.literal.clone());
        self.expect_peek(TokenKind::Assign)?;
        self.next_token();
        let val = self.parse_expr(Precedence::Lowest)?;
        if self.peek.kind == TokenKind::Semicolon {
            self.next_token();
        }
        Ok(StmtKind::Assign(id, val))
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
                while self.peek.kind != TokenKind::Semicolon
                    && precedence < self.peek_precedence()
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

    fn parse_str_literal(&mut self) -> ParseResult<ExprKind> {
        Ok(ExprKind::Str(self.curr.literal.clone()))
    }

    fn parse_boolean(&mut self) -> ParseResult<ExprKind> {
        let val = self.curr.kind == TokenKind::True;
        Ok(ExprKind::Boolean(val))
    }

    fn parse_prefix(&mut self) -> ParseResult<ExprKind> {
        let op = self.curr.kind.clone();
        self.next_token();
        let rhs = self.parse_expr(Precedence::Prefix)?;
        Ok(ExprKind::Prefix(op, Box::new(rhs)))
    }

    fn parse_infix(&mut self, lhs: ExprKind) -> ParseResult<ExprKind> {
        let op = self.curr.kind.clone();
        let precedence = self.curr_precedence();
        self.next_token();
        let rhs = self.parse_expr(precedence)?;
        Ok(ExprKind::Infix(op, Box::new(lhs), Box::new(rhs)))
    }

    fn parse_fn_expr(&mut self) -> ParseResult<ExprKind> {
        self.expect_peek(TokenKind::LParen)?;
        let params = self.parse_fn_params()?;
        self.expect_peek(TokenKind::LBrace)?;
        let body = self.parse_block()?;
        Ok(ExprKind::Func(params, body))
    }

    fn parse_fn_params(&mut self) -> ParseResult<Vec<Identifier>> {
        let mut params = Vec::new();
        if self.peek.kind == TokenKind::RParen {
            self.next_token();
            return Ok(params);
        }

        self.next_token();
        params.push(Identifier::new(self.curr.literal.clone()));
        while self.peek.kind == TokenKind::Comma {
            self.next_token();
            self.next_token();
            params.push(Identifier::new(self.curr.literal.clone()));
        }

        self.expect_peek(TokenKind::RParen)?;
        Ok(params)
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        let mut stmts: Vec<StmtKind> = Vec::new();
        self.next_token();

        while self.curr.kind != TokenKind::RBrace
            && self.curr.kind != TokenKind::EOF
        {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.next_token();
        }

        Ok(Block::new(stmts))
    }

    fn parse_expr_list(&mut self, end: TokenKind) -> ParseResult<Box<Vec<ExprKind>>> {
        let mut args: Vec<ExprKind> = Vec::new();
        if self.peek.kind == end {
            self.next_token();
            return Ok(Box::new(args));
        }

        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        args.push(expr);

        while self.peek.kind == TokenKind::Comma {
            self.next_token();
            self.next_token();
            let e = self.parse_expr(Precedence::Lowest)?;
            args.push(e);
        }

        self.expect_peek(end)?;
        Ok(Box::new(args))
    }

    fn parse_call_expr(&mut self, func: ExprKind) -> ParseResult<ExprKind> {
        let args = self.parse_expr_list(TokenKind::RParen)?;
        Ok(ExprKind::Call(Box::new(func), args))
    }

    fn parse_chain_expr(&mut self, first: ExprKind) -> ParseResult<ExprKind> {
        let mut out = vec![first];
        while self.peek.kind != TokenKind::EOF
        {
            self.next_token();
            let id = self.parse_ident()?;
            self.expect_peek(TokenKind::LParen)?;
            let call = self.parse_call_expr(id)?;
            out.push(call);
            if self.peek.kind == TokenKind::Semicolon {
                break;
            }
            self.next_token();
        }
        Ok(ExprKind::Chain(Box::new(out)))
    }

    fn parse_if_block(&mut self) -> ParseResult<IfBlock> {
        self.expect_peek(TokenKind::LParen)?;
        self.next_token();
        let cond = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RParen)?;
        self.expect_peek(TokenKind::LBrace)?;
        let conseq = self.parse_block()?;
        Ok(IfBlock::new(cond, conseq))
    }

    fn parse_if_expr(&mut self) -> ParseResult<ExprKind> {
        let mut conds = vec![self.parse_if_block()?];
        while self.peek.kind == TokenKind::Elif {
            self.next_token();
            conds.push(self.parse_if_block()?);
        }
        let mut alt = Block::default();
        if self.peek.kind == TokenKind::Else {
            self.next_token();
            self.expect_peek(TokenKind::LBrace)?;
            alt = self.parse_block()?;
        }
        Ok(ExprKind::If(Box::new(conds), alt))
    }

    fn parse_while_expr(&mut self) -> ParseResult<StmtKind> {
        self.expect_peek(TokenKind::LParen)?;
        self.next_token();
        let cond = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RParen)?;
        self.expect_peek(TokenKind::LBrace)?;
        let body = self.parse_block()?;
        if self.peek.kind == TokenKind::Semicolon {
            self.next_token();
        }
        Ok(StmtKind::While(Box::new(cond), body))
    }

    fn parse_switch_expr(&mut self) -> ParseResult<ExprKind> {
        self.expect_peek(TokenKind::LParen)?;
        self.next_token();
        let cond = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RParen)?;
        self.expect_peek(TokenKind::LBrace)?;

        let mut cases: Vec<SwitchCase> = Vec::new();
        while self.peek.kind != TokenKind::RBrace
            && self.peek.kind != TokenKind::EOF
            && self.peek.kind != TokenKind::LowDash
        {
            self.expect_peek(TokenKind::LParen)?;
            self.next_token();
            let case = self.parse_expr(Precedence::Lowest)?;
            self.expect_peek(TokenKind::RParen)?;
            self.expect_peek(TokenKind::Arrow)?;
            self.expect_peek(TokenKind::LBrace)?;
            let conseq = self.parse_block()?;
            let scase = SwitchCase::new(case, conseq);
            cases.push(scase);
        }

        let mut def = Block::default();
        if self.peek.kind == TokenKind::LowDash {
            self.next_token();
            self.expect_peek(TokenKind::Arrow)?;
            self.expect_peek(TokenKind::LBrace)?;
            def = self.parse_block()?;
        }
        self.expect_peek(TokenKind::RBrace)?;

        Ok(ExprKind::Switch(Box::new(cond), Box::new(cases), def))
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

    fn get_single_output(input: &str) -> StmtKind {
        let prgm = setup_parser(input).parse_program().unwrap();
        assert_eq!(prgm.stmts.len(), 1);
        let stmt = &prgm.stmts[0];
        stmt.clone()
    }

    fn assert_single_output(input: &str, exp: StmtKind) {
        let stmt = get_single_output(input);
        assert_eq!(stmt, exp);
    }

    fn assert_single_expr(input: &str, exp: ExprKind) {
        let stmt = get_single_output(input);
        match stmt {
            StmtKind::Expr(e) => assert_eq!(e, exp),
            _ => panic!("expected exprkind"),
        }
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
        assert_single_output(input, exp);
    }

    #[test_case(
        "x = 5;",
        StmtKind::Assign(
            Identifier::new("x".into()),
            ExprKind::Int(5)
        ); "int assign"
    )]
    #[test_case(
        "x = false;",
        StmtKind::Assign(
            Identifier::new("x".into()),
            ExprKind::Boolean(false)
        ); "boolean assign"
    )]
    #[test_case(
        "x = y;",
        StmtKind::Assign(
            Identifier::new("x".into()),
            ExprKind::Ident(Identifier::new("y".into()))
        ); "ident assign"
    )]
    fn test_assign(input: &str, exp: StmtKind) {
        assert_single_output(input, exp);
    }

    #[test_case(
        "return 5;",
        StmtKind::Return(Some(ExprKind::Int(5)));
        "return int"
    )]
    #[test_case(
        "return false;",
        StmtKind::Return(Some(ExprKind::Boolean(false)));
        "return bool"
    )]
    #[test_case("return;", StmtKind::Return(None); "return null")]
    fn test_return(input: &str, exp: StmtKind) {
        assert_single_output(input, exp);
    }

    #[test_case(
        "3 + 4;",
        ExprKind::Infix(
            TokenKind::Plus,
            Box::new(ExprKind::Int(3)),
            Box::new(ExprKind::Int(4)),
        );
        "basic plus infix"
    )]
    #[test_case(
        "3 - 4;",
        ExprKind::Infix(
            TokenKind::Minus,
            Box::new(ExprKind::Int(3)),
            Box::new(ExprKind::Int(4)),
        );
        "basic minus infix"
    )]
    fn test_infix(input: &str, exp: ExprKind) {
        assert_single_expr(input, exp);
    }

    #[test_case(
        "fn (x, y) { 5 };",
        StmtKind::Expr(
            ExprKind::Func(
                vec!(Identifier::new("x".into()), Identifier::new("y".into())),
                Block::new(vec!(
                    StmtKind::Expr(
                        ExprKind::Int(5)
                    )
                ))
            )
        );
        "basic fn"
    )]
    fn test_fn(input: &str, exp: StmtKind) {
        assert_single_output(input, exp);
    }

    #[test_case(
        "while (true) {};",
        StmtKind::While(
            Box::new(ExprKind::Boolean(true)),
            Block::default(),
        );
        "while loop empty body"
    )]
    fn test_while(input: &str, exp: StmtKind) {
        assert_single_output(input, exp);
    }

    #[test_case(
        "if (true) { 1 } else { 2 };",
        ExprKind::If(
            Box::new(
                vec!(
                    IfBlock::new(
                        ExprKind::Boolean(true),
                        Block::new(vec!(StmtKind::Expr(ExprKind::Int(1))))
                    ),
                )
            ),
            Block::new(vec!(StmtKind::Expr(ExprKind::Int(2))))
        );
        "basic full if"
    )]
    #[test_case(
        "if (true) { 1 };",
        ExprKind::If(
            Box::new(
                vec!(
                    IfBlock::new(
                        ExprKind::Boolean(true),
                        Block::new(vec!(StmtKind::Expr(ExprKind::Int(1))))
                    ),
                )
            ),
            Block::new(vec!()),
        );
        "if missing alt"
    )]
    #[test_case(
        "if (true) { 1 } elif (true) { 3 } else { 2 };",
        ExprKind::If(
            Box::new(
                vec!(
                    IfBlock::new(
                        ExprKind::Boolean(true),
                        Block::new(vec!(StmtKind::Expr(ExprKind::Int(1))))
                    ),
                    IfBlock::new(
                        ExprKind::Boolean(true),
                        Block::new(vec!(StmtKind::Expr(ExprKind::Int(3))))
                    ),
                )
            ),
            Block::new(vec!(StmtKind::Expr(ExprKind::Int(2))))
        );
        "basic elif"
    )]
    fn test_if(input: &str, exp: ExprKind) {
        assert_single_expr(input, exp);
    }

    #[test_case("
switch (true) {
    (1) => { 2 }
}",
        ExprKind::Switch(
            Box::new(ExprKind::Boolean(true)),
            Box::new(vec!(
                SwitchCase::new(
                    ExprKind::Int(1),
                    Block::new(vec!(
                            StmtKind::Expr(ExprKind::Int(2)),
                    ))
                )
            )),
            Block::default(),
        );
        "basic switch"
    )]
    #[test_case("
switch (true) {
    (1) => { 2 }
    _ => { 3 }
}",
        ExprKind::Switch(
            Box::new(ExprKind::Boolean(true)),
            Box::new(vec!(
                SwitchCase::new(
                    ExprKind::Int(1),
                    Block::new(vec!(
                            StmtKind::Expr(ExprKind::Int(2)),
                    ))
                )
            )),
            Block::new(vec!(
                StmtKind::Expr(ExprKind::Int(3)),
            )),
        );
        "basic switch with default"
    )]
    fn test_switch(input: &str, exp: ExprKind) {
        assert_single_expr(input, exp);
    }
}
