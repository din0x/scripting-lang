use std::fmt::Display;

use super::token::{Keyword, Lit, Op, Paren, Punc, Token};

type Result<T> = std::result::Result<T, Error>;

pub fn parse_tokens(s: &str) -> Result<Vec<Token>> {
    let mut lexer = Lexer::new(s);
    let mut tokens = vec![];

    skip_whitespace(&mut lexer);

    while let Some(ch) = lexer.curr() {
        let token = match ch {
            ch if ch.is_ascii_alphabetic() || ch == '_' => parse_ident(&mut lexer),
            ch if ch.is_ascii_digit() => parse_num(&mut lexer),
            '"' => parse_str(&mut lexer)?,
            ch if Op::ALL
                .iter()
                .map(|el| el.0.chars().next().expect("op should not be 0 len"))
                .any(|el| el == ch) =>
            {
                Token::Op(parse_op(&mut lexer))
            }
            _ if let Some(punc) = Punc::from_char(ch) => {
                lexer.next();

                Token::Punc(punc)
            }
            _ if let Some(paren) = Paren::from_char(ch) => {
                lexer.next();

                Token::Paren(paren)
            }
            _ => {
                lexer.next();
                return Err(Error::Illegal(ch));
            }
        };

        tokens.push(token);

        skip_whitespace(&mut lexer);
    }

    tokens.push(Token::Eof);

    Ok(tokens)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Error {
    Illegal(char),
    UnfinishedString,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Illegal(ch) => write!(f, "found unexpected character `{ch}`"),
            Error::UnfinishedString => write!(f, "missing ending `\"`"),
        }
    }
}

fn parse_ident(lexer: &mut Lexer) -> Token {
    let mut name = String::new();

    if let Some(ch) = lexer.next() {
        name.push(ch);
    }

    while let Some(ch) = lexer.curr() {
        if !ch.is_ascii_alphanumeric() && ch != '_' {
            break;
        }

        name.push(ch);
        lexer.next();
    }

    if let Ok(keyword) = Keyword::try_from(name.as_str()) {
        return Token::Keyword(keyword);
    }

    Token::Ident(name.into_boxed_str())
}

fn parse_num(lexer: &mut Lexer) -> Token {
    let all = lexer.data;
    let mut num = "";
    let mut i = 1;

    while let Some(ch) = lexer.curr() {
        if !ch.is_ascii_digit() {
            break;
        }

        num = &all[0..i];
        i += 1;
        lexer.next();
    }

    Token::Lit(Lit::Int(num.parse().expect("num should be a valid number")))
}

fn parse_str(lexer: &mut Lexer) -> Result<Token> {
    lexer.next();

    let mut str = String::new();
    let mut finished = false;

    while let Some(ch) = lexer.next() {
        if ch == '"' {
            finished = true;
            break;
        }

        str.push(ch);
    }

    if !finished {
        return Err(Error::UnfinishedString);
    }

    Ok(Token::Lit(Lit::String(str)))
}

fn parse_op(lexer: &mut Lexer) -> Op {
    let mut max_len = 0;
    let mut op = None;

    for el in Op::ALL.iter().filter(|el| lexer.data.starts_with(el.0)) {
        if max_len < el.0.len() {
            max_len = el.0.len();
            op = Some(el.1);
        }
    }

    for _ in 0..max_len {
        lexer.next();
    }

    op.expect("lexer.curr() should be an operator")
}

fn skip_whitespace(lexer: &mut Lexer) {
    while let Some(ch) = lexer.curr() {
        if !ch.is_whitespace() {
            break;
        }

        lexer.next();
    }
}

struct Lexer<'a> {
    data: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(data: &'a str) -> Self {
        Self { data, pos: 0 }
    }

    fn next(&mut self) -> Option<char> {
        let curr = self.curr()?;

        self.pos += 1;
        self.data = &self.data[1..];

        Some(curr)
    }

    fn curr(&self) -> Option<char> {
        self.data.chars().next()
    }
}
