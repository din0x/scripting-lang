use std::fmt::Display;
use token::{Lit, Op};

pub mod lexer;
pub mod parser;
pub mod token;

pub fn parse(s: &str) -> Result<Expr, Error> {
    let tokens = lexer::parse_tokens(s)?;
    let ast = parser::parse_ast(&tokens)?;

    Ok(ast)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Block(Box<Block>),
    Decl(Box<Decl>),
    Func(Box<Func>),
    Binary(Box<Binary>),
    Unary(Box<Unary>),
    Call(Box<Call>),
    Index(Box<Index>),
    Ident(Box<str>),
    List(Vec<Expr>),
    Lit(Lit),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub content: Vec<Expr>,
    pub tail: Option<Expr>,
}

impl Block {
    pub fn new(content: Vec<Expr>, tail: Option<Expr>) -> Self {
        Self { content, tail }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Decl {
    pub name: Box<str>,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    pub name: Option<Box<str>>,
    pub args: Vec<Box<str>>,
    pub body: Expr,
}

impl Func {
    pub fn new(name: Option<Box<str>>, args: Vec<Box<str>>, body: Expr) -> Self {
        Self { args, body, name }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
    pub left: Expr,
    pub op: BinaryOp,
    pub right: Expr,
}

impl Binary {
    pub fn new(left: Expr, op: BinaryOp, right: Expr) -> Self {
        Self { left, op, right }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Eq,
    Nq,
    Lt,
    Le,
    Gt,
    Ge,
    Or,
    And,
}

impl BinaryOp {
    pub const ALL: &'static [(Op, BinaryOp)] = &[
        (Op::Plus, Self::Plus),
        (Op::Minus, Self::Minus),
        (Op::Mul, Self::Mul),
        (Op::Div, Self::Div),
        (Op::Mod, Self::Mod),
        (Op::Eq, Self::Eq),
        (Op::Nq, Self::Nq),
        (Op::Lt, Self::Lt),
        (Op::Le, Self::Le),
        (Op::Gt, Self::Gt),
        (Op::Ge, Self::Ge),
        (Op::Or, Self::Or),
        (Op::And, Self::And),
    ];

    pub fn as_str(self) -> &'static str {
        Op::from(self).as_str()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct NotBinaryOpError;

impl TryFrom<Op> for BinaryOp {
    type Error = NotBinaryOpError;

    fn try_from(value: Op) -> Result<Self, Self::Error> {
        match BinaryOp::ALL.iter().find(|el| el.0 == value).map(|el| el.1) {
            Some(op) => Ok(op),
            None => Err(NotBinaryOpError),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = self.as_str();
        write!(f, "{op}")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub expr: Expr,
}

impl Unary {
    pub fn new(op: UnaryOp, expr: Expr) -> Self {
        Self { op, expr }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

impl UnaryOp {
    pub const ALL: &'static [(Op, UnaryOp)] = &[
        (Op::Plus, Self::Plus),
        (Op::Minus, Self::Minus),
        (Op::Not, Self::Not),
    ];

    pub fn as_str(self) -> &'static str {
        UnaryOp::ALL
            .iter()
            .find(|e| e.1 == self)
            .expect("operator not found")
            .0
            .as_str()
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Op::from(*self).fmt(f)
    }
}

pub struct NotUnaryOpError;

impl TryFrom<Op> for UnaryOp {
    type Error = NotUnaryOpError;

    fn try_from(value: Op) -> Result<Self, Self::Error> {
        match UnaryOp::ALL.iter().find(|el| el.0 == value).map(|el| el.1) {
            Some(op) => Ok(op),
            None => Err(NotUnaryOpError),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub expr: Expr,
    pub args: Vec<Expr>,
}

impl Call {
    pub fn new(expr: Expr, args: Vec<Expr>) -> Self {
        Self { expr, args }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Index {
    pub expr: Expr,
    pub index: Expr,
}

impl Index {
    pub fn new(expr: Expr, index: Expr) -> Self {
        Self { expr, index }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Lexer(lexer::Error),
    Parser(parser::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Lexer(err) => write!(f, "{err}"),
            Error::Parser(err) => write!(f, "{err}"),
        }
    }
}

impl From<lexer::Error> for Error {
    fn from(value: lexer::Error) -> Self {
        Self::Lexer(value)
    }
}

impl From<parser::Error> for Error {
    fn from(value: parser::Error) -> Self {
        Self::Parser(value)
    }
}
