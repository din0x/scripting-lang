use crate::ast::{BinaryOp, Expr, UnaryOp};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

mod eval;
mod value;

pub use value::{Func, Type, Value};

pub type Result = std::result::Result<Value, Error>;

pub fn exec(expr: Expr, ctx: &mut Ctx) -> Result {
    eval::eval(expr, ctx)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    UnaryUsage {
        op: UnaryOp,
        val: Type,
    },
    BinaryUsage {
        op: BinaryOp,
        left: Type,
        right: Type,
    },
    Undeclared(Box<str>),
    NotFunc(Type),
    ArgCount {
        expected: usize,
        found: usize,
    },
    CannotIndex {
        value: Type,
        with: Type,
    },
    NoElement {
        index: usize,
        len: usize,
    },
    Recursion,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnaryUsage { op, val } => write!(f, "cannot use `{op}` operator with `{val}`"),
            Error::BinaryUsage { op, left, right } => {
                write!(f, "cannot use `{op}` operator with `{left}` and `{right}`")
            }
            Error::Undeclared(name) => write!(f, "cannot find `{name}` in current scope"),
            Error::NotFunc(t) => write!(f, "cannot call `{t}`"),
            Error::ArgCount { expected, found } => write!(
                f,
                "function expected {expected} arguments, but {found} were given"
            ),
            Error::CannotIndex { value, with } => {
                write!(f, "cannot index `{value}`, with `{with}`")
            }
            Error::NoElement { index, len } => {
                write!(f, "trying to get element {index}, but length is {len}")
            }
            Error::Recursion => write!(f, "program has reached recursion limit"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ctx {
    recursion: usize,
    inner: Rc<RefCell<CtxInner>>,
}

#[derive(Debug, Clone)]
struct CtxInner {
    parent: Option<Ctx>,
    vars: HashMap<Box<str>, Value>,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            recursion: 256,
            inner: Rc::new(RefCell::new(CtxInner {
                parent: None,
                vars: HashMap::new(),
            })),
        }
    }

    pub fn nested(inner: Ctx) -> Self {
        Self {
            recursion: inner.recursion,
            inner: Rc::new(RefCell::new(CtxInner {
                parent: Some(inner),
                vars: HashMap::new(),
            })),
        }
    }

    pub fn decl(&mut self, name: Box<str>, value: Value) {
        self.inner.borrow_mut().vars.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.inner.borrow().vars.get(name) {
            return Some(val.clone());
        }

        let parent = self.inner.borrow().parent.clone()?;

        parent.get(name)
    }

    pub fn set(&mut self, name: &str, value: Value) -> Option<Value> {
        if let Some(val) = self.inner.borrow_mut().vars.get_mut(name) {
            let old = val.clone();
            *val = value;
            return Some(old);
        }

        let mut parent = self.inner.borrow().parent.clone()?;

        parent.set(name, value)
    }
}
