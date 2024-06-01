#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(lazy_cell)]

use std::fmt::Display;

use vm::{Ctx, Value};

pub mod ast;
pub mod vm;

pub type Result = std::result::Result<Value, Error>;

pub fn exec(s: &str, ctx: &mut Ctx) -> Result {
    let expr = ast::parse(s)?;
    let value = vm::exec(expr, ctx)?;

    Ok(value)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Parser(ast::Error),
    Runtime(vm::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parser(err) => write!(f, "{err}"),
            Error::Runtime(err) => write!(f, "{err}"),
        }
    }
}

impl From<ast::Error> for Error {
    fn from(value: ast::Error) -> Self {
        Self::Parser(value)
    }
}

impl From<vm::Error> for Error {
    fn from(value: vm::Error) -> Self {
        Self::Runtime(value)
    }
}
