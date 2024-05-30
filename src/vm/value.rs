use super::Ctx;
use crate::ast::Expr;
use std::{fmt::Display, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Func(Rc<Func>),
    Unit,
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Bool(_) => Type::Bool,
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::String(_) => Type::String,
            Value::Func(_) => Type::Func,
            Value::Unit => Type::Unit,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(val) => write!(f, "{}", val),
            Value::Int(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::String(val) => write!(f, "{val}"),
            Value::Func(_) => write!(f, "func"),
            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub args: Vec<Box<str>>,
    pub body: Expr,
    pub ctx: Ctx,
}

impl Func {
    pub fn new(args: Vec<Box<str>>, body: Expr, ctx: Ctx) -> Self {
        Self { args, body, ctx }
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::addr_eq(self, other)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Bool,
    Int,
    Float,
    String,
    Func,
    Unit,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;

        let s = match self {
            Bool => "bool",
            Int => "int",
            Float => "float",
            String => "str",
            Func => "func",
            Unit => "()",
        };

        write!(f, "{s}")
    }
}
