use std::fmt::Display;

use super::{BinaryOp, UnaryOp};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Op(Op),
    Punc(Punc),
    Paren(Paren),
    Lit(Lit),
    Keyword(Keyword),
    Ident(Box<str>),
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Op(op) => write!(f, "{op}"),
            Token::Punc(punc) => write!(f, "{punc}"),
            Token::Paren(paren) => write!(f, "{paren}"),
            Token::Lit(lit) => write!(f, "{lit}"),
            Token::Keyword(kw) => write!(f, "{kw}"),
            Token::Ident(name) => write!(f, "{name}"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Op {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Assign,
    Not,
    Eq,
    Nq,
    Lt,
    Le,
    Gt,
    Ge,
    Or,
    And,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = Self::ALL
            .iter()
            .find(|op| op.1 == *self)
            .unwrap_or_else(|| unreachable!())
            .0;

        write!(f, "{s}")
    }
}

impl Op {
    pub const ALL: &'static [(&'static str, Op)] = &[
        ("+", Op::Plus),
        ("-", Op::Minus),
        ("*", Op::Mul),
        ("/", Op::Div),
        ("%", Op::Mod),
        ("=", Op::Assign),
        ("!", Op::Not),
        ("==", Op::Eq),
        ("!=", Op::Nq),
        ("<", Op::Lt),
        ("<=", Op::Le),
        (">", Op::Gt),
        (">=", Op::Ge),
        ("||", Op::Or),
        ("&&", Op::And),
    ];

    pub fn as_str(self) -> &'static str {
        Op::ALL
            .iter()
            .find(|e| e.1 == self)
            .expect("operator not found")
            .0
    }
}

impl From<BinaryOp> for Op {
    fn from(value: BinaryOp) -> Self {
        BinaryOp::ALL
            .iter()
            .find(|e| e.1 == value)
            .expect("operator not found")
            .0
    }
}

impl From<UnaryOp> for Op {
    fn from(value: UnaryOp) -> Self {
        UnaryOp::ALL
            .iter()
            .find(|e| e.1 == value)
            .expect("operator not found")
            .0
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Punc {
    Comma,
    Dot,
    Semicolon,
}

impl Punc {
    const ALL: &'static [(char, Punc)] =
        &[(',', Punc::Comma), ('.', Punc::Dot), (';', Punc::Semicolon)];

    pub fn from_char(ch: char) -> Option<Punc> {
        Punc::ALL.iter().find(|el| el.0 == ch).map(|el| el.1)
    }
}

impl Display for Punc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = Punc::ALL
            .iter()
            .find(|x| x.1 == *self)
            .unwrap_or_else(|| unreachable!())
            .0;

        write!(f, "{s}")
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Paren {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LCurly,
    RCurly,
}

impl Paren {
    const ALL: &'static [(char, Paren)] = &[
        ('(', Paren::LParen),
        (')', Paren::RParen),
        ('[', Paren::LBracket),
        (']', Paren::RBracket),
        ('{', Paren::LCurly),
        ('}', Paren::RCurly),
    ];

    pub fn from_char(ch: char) -> Option<Paren> {
        Paren::ALL.iter().find(|el| el.0 == ch).map(|el| el.1)
    }
}

impl Display for Paren {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Paren::LParen => "(",
                Paren::RParen => ")",
                Paren::LBracket => "[",
                Paren::RBracket => "]",
                Paren::LCurly => "{",
                Paren::RCurly => "}",
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Lit {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(val) => write!(f, "{val}"),
            Lit::Float(val) => write!(f, "{val}"),
            Lit::Bool(val) => write!(f, "{val}"),
            Lit::String(val) => write!(f, "\"{val}\""),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Keyword {
    Let,
    Fn,
    If,
    Else,
    For,
    In,
}

impl Keyword {
    const ALL: &'static [(&'static str, Self)] = &[
        ("let", Self::Let),
        ("fn", Self::Fn),
        ("if", Self::If),
        ("else", Self::Else),
        ("for", Self::For),
        ("in", Self::In),
    ];
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = Self::ALL
            .iter()
            .find(|kw| kw.1 == *self)
            .unwrap_or_else(|| unreachable!())
            .0;

        write!(f, "{s}")
    }
}

pub struct NotKeywordError;

impl TryFrom<&str> for Keyword {
    type Error = NotKeywordError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Keyword::ALL
            .iter()
            .find(|el| el.0 == value)
            .map(|el| el.1)
            .ok_or(NotKeywordError {})
    }
}
