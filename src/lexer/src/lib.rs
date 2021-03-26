mod token;

pub use token::{Token, TokenKind};

#[derive(Default)]
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    read_pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            ..Default::default()
        }
    }

    pub fn next_token(&mut self) -> Token {
        use TokenKind::*;
        match self.next() {
            Some(c) => match *c {
                '=' => match self.peek() {
                    Some(pc) => match *pc {
                        '=' => {
                            let mut lit = '='.to_string();
                            lit.push(*self.next().unwrap());
                            Token::new(EQ, lit)
                        }
                        '>' => {
                            let mut lit = '='.to_string();
                            lit.push(*self.next().unwrap());
                            Token::new(Arrow, lit)
                        }
                        _ => Token::new(Assign, '='.to_string()),
                    },
                    None => Token::new(Assign, '='.to_string()),
                },
                '%' => Token::new(Modulo, c.to_string()),
                ';' => Token::new(Semicolon, c.to_string()),
                '(' => Token::new(LParen, c.to_string()),
                ')' => Token::new(RParen, c.to_string()),
                ',' => Token::new(Comma, c.to_string()),
                '+' => Token::new(Plus, c.to_string()),
                '{' => Token::new(LBrace, c.to_string()),
                '}' => Token::new(RBrace, c.to_string()),
                '[' => Token::new(LBracket, c.to_string()),
                ']' => Token::new(RBracket, c.to_string()),
                ':' => Token::new(Colon, c.to_string()),
                '-' => Token::new(Minus, c.to_string()),
                '!' => match self.peek() {
                    Some(pc) => match *pc {
                        '=' => {
                            let mut lit = '!'.to_string();
                            lit.push(*self.next().unwrap());
                            Token::new(NEQ, lit)
                        }
                        _ => Token::new(Bang, '!'.to_string()),
                    },
                    _ => Token::new(Bang, '!'.to_string()),
                },
                '*' => Token::new(Asterisk, c.to_string()),
                '/' => Token::new(Slash, c.to_string()),
                '<' => Token::new(LT, c.to_string()),
                '>' => Token::new(GT, c.to_string()),
                '"' => {
                    let literal = self.read_string();
                    Token::new(Str, literal)
                },
                '_' => Token::new(LowDash, c.to_string()),
                '&' => Token::new(And, c.to_string()),
                '|' => match self.peek() {
                    Some(pc) => match *pc {
                        '>' => {
                            let mut lit = '|'.to_string();
                            lit.push(*self.next().unwrap());
                            Token::new(Pipe, lit)
                        },
                        _ => Token::new(Or, '|'.to_string())
                    },
                    None => Token::new(Or, '|'.to_string())
                },
                _ if c.is_whitespace() => self.next_token(),
                _ if c.is_numeric() => {
                    let lit = self.read_numeric();
                    Token::new(Int, lit)
                }
                _ if c.is_alphabetic() => {
                    let lit = self.read_identifier();
                    match TokenKind::is_to_keyword(&lit) {
                        Some(t) => Token::new(t, lit),
                        None => Token::new(Ident, lit)
                    }
                }
                _ => Token::new(Illegal, c.to_string()),
            },
            None => Token::new(EOF, "".into()),
        }
    }

    fn next(&mut self) -> Option<&char> {
        let out = self.input.get(self.read_pos);
        self.pos = self.read_pos;
        self.read_pos += 1;
        out
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.get(self.read_pos)
    }

    fn read_identifier(&mut self) -> String {
        let start = self.pos;
        let mut i = vec![self.input[start]];
        while self.peek().is_some()
            //&& self.peek().unwrap().is_alphabetic()
            && is_valid_identifier(self.peek().unwrap())
        {
            i.push(*self.next().unwrap());
        }
        i.iter().collect()
    }

    fn read_numeric(&mut self) -> String {
        let start = self.pos;
        let mut i = vec![self.input[start]];
        while self.peek().is_some()
            && self.peek().unwrap().is_numeric()
        {
            i.push(*self.next().unwrap());
        }
        i.iter().collect()
    }

    fn read_string(&mut self) -> String {
        let mut i: Vec<char> = Vec::new();
        loop {
            match self.peek() {
                Some(c) if *c == '"' => {
                    let _ = self.next();
                    break;
                },
                Some(_) => {
                    i.push(*self.next().unwrap());
                },
                None => {
                    break;
                },
            };
        }
        i.iter().collect()
    }
}

fn is_valid_identifier(c: &char) -> bool {
    c.is_alphabetic() || c.is_numeric() || *c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenKind::*;
    use test_case::test_case;

    #[test_case(
        "let x = 1 => |> foo_bar5 % while elif",
        vec!(
            Token::new(Let, "let".into()),
            Token::new(Ident, "x".into()),
            Token::new(Assign, "=".into()),
            Token::new(Int, "1".into()),
            Token::new(Arrow, "=>".into()),
            Token::new(Pipe, "|>".into()),
            Token::new(Ident, "foo_bar5".into()),
            Token::new(Modulo, "%".into()),
            Token::new(While, "while".into()),
            Token::new(Elif, "elif".into()),
        ); "assignment"
    )]
    fn test_token(input: &str, exp: Vec<Token>) {
        let mut l = Lexer::new(input);
        for tt in exp.iter() {
            let t = l.next_token();
            assert_eq!(t, *tt);
        }
    }
}
