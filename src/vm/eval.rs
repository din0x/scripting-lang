use super::{
    value::{Func, Value},
    Ctx, Result,
};
use crate::{
    ast::{self, token::Lit, Binary, BinaryOp, Block, Call, Decl, Expr, Index, Unary, UnaryOp},
    vm::Error,
};
use std::{borrow::Borrow, collections::HashMap, rc::Rc};

pub(super) fn eval(expr: Expr, ctx: &mut Ctx) -> Result {
    match expr {
        Expr::Block(block) => eval_block(*block, ctx),
        Expr::Decl(decl) => eval_decl(*decl, ctx),
        Expr::Func(func) => eval_func(*func, ctx),
        Expr::Binary(binary) => eval_binary(*binary, ctx),
        Expr::Unary(unary) => eval_unary(*unary, ctx),
        Expr::Call(call) => eval_call(*call, ctx),
        Expr::Index(index) => eval_index(*index, ctx),
        Expr::Ident(name) => eval_ident(name, ctx),
        Expr::Lit(lit) => Ok(eval_literal(lit)),
        Expr::List(list) => eval_list(list, ctx),
    }
}

fn eval_block(block: Block, ctx: &mut Ctx) -> Result {
    let mut ctx = Ctx::nested(ctx.clone());

    for expr in block.content {
        eval(expr, &mut ctx)?;
    }

    block
        .tail
        .map(|val| eval(val, &mut ctx))
        .unwrap_or(Ok(Value::Unit))
}

fn eval_decl(decl: Decl, ctx: &mut Ctx) -> Result {
    let value = eval(decl.expr, ctx)?;

    ctx.decl(decl.name, value);

    Ok(Value::Unit)
}

fn eval_func(expr: ast::Func, ctx: &mut Ctx) -> Result {
    let func = Value::Func(Rc::new(Func::new(expr.args, expr.body, ctx.clone())));

    if let Some(name) = expr.name {
        ctx.decl(name, func);
        return Ok(Value::Unit);
    }

    Ok(func)
}

fn eval_binary(expr: Binary, ctx: &mut Ctx) -> Result {
    let left = eval(expr.left, ctx)?;
    let right = eval(expr.right, ctx)?;

    use BinaryOp::*;
    use Value::*;

    let value = match (expr.op, left, right) {
        (Eq, v0, v1) if v0.get_type() == v1.get_type() => Bool(v0 == v1),
        (Nq, v0, v1) if v0.get_type() == v1.get_type() => Bool(v0 != v1),
        (Plus, Int(i0), Int(i1)) => Int(i0 + i1),
        (Minus, Int(i0), Int(i1)) => Int(i0 - i1),
        (Mul, Int(i0), Int(i1)) => Int(i0 * i1),
        (Div, Int(i0), Int(i1)) => Int(i0 / i1),
        (Mod, Int(i0), Int(i1)) => Int(i0 % i1),
        (Lt, Int(i0), Int(i1)) => Bool(i0 < i1),
        (Le, Int(i0), Int(i1)) => Bool(i0 <= i1),
        (Gt, Int(i0), Int(i1)) => Bool(i0 > i1),
        (Ge, Int(i0), Int(i1)) => Bool(i0 >= i1),
        (Plus, Float(f0), Float(f1)) => Float(f0 + f1),
        (Minus, Float(f0), Float(f1)) => Float(f0 - f1),
        (Mul, Float(f0), Float(f1)) => Float(f0 * f1),
        (Div, Float(f0), Float(f1)) => Float(f0 / f1),
        (Mod, Float(f0), Float(f1)) => Float(f0 % f1),
        (Lt, Float(f0), Float(f1)) => Bool(f0 < f1),
        (Le, Float(f0), Float(f1)) => Bool(f0 <= f1),
        (Gt, Float(f0), Float(f1)) => Bool(f0 > f1),
        (Ge, Float(f0), Float(f1)) => Bool(f0 >= f1),
        (Plus, String(mut s0), String(s1)) => String({
            s0.push_str(&s1);
            s0
        }),
        (Mul, Int(i), String(s)) => String(s.repeat(i as usize)),
        (Mul, String(s), Int(i)) => String(s.repeat(i as usize)),
        (op, left, right) => {
            return Err(Error::BinaryUsage {
                op,
                left: left.get_type(),
                right: right.get_type(),
            })
        }
    };

    Ok(value)
}

fn eval_unary(expr: Unary, ctx: &mut Ctx) -> Result {
    let value = eval(expr.expr, ctx)?;

    use UnaryOp::*;
    use Value::*;

    let value = match (expr.op, value) {
        (Plus, Int(i)) => Int(i),
        (Minus, Int(i)) => Int(-i),
        (Plus, Float(i)) => Float(i),
        (Minus, Float(i)) => Float(-i),
        (Not, Bool(b)) => Bool(!b),
        (op, val) => {
            return Err(Error::UnaryUsage {
                op,
                val: val.get_type(),
            })
        }
    };

    Ok(value)
}

fn eval_call(call: Call, ctx: &mut Ctx) -> Result {
    let val = eval(call.expr, ctx)?;

    let Value::Func(func) = val else {
        return Err(Error::NotFunc(val.get_type()));
    };

    let func: &Func = func.borrow();
    let mut args = HashMap::with_capacity(call.args.len());

    if call.args.len() != func.args.len() {
        return Err(Error::ArgCount {
            expected: func.args.len(),
            found: call.args.len(),
        });
    }

    for (arg, name) in call.args.into_iter().zip(func.args.iter()) {
        let val = eval(arg, ctx)?;
        args.insert(name.clone(), val);
    }

    let mut func_ctx = Ctx::nested(func.ctx.clone());

    func_ctx.recursion = ctx.recursion - 1;

    if func_ctx.recursion == 0 {
        return Err(Error::Recursion);
    }

    eval(func.body.clone(), &mut func_ctx)
}

fn eval_index(expr: Index, ctx: &mut Ctx) -> Result {
    let value = eval(expr.expr, ctx)?;
    let index = eval(expr.index, ctx)?;

    match (value, index) {
        (Value::List(list), Value::Int(i)) => {
            let index = if i >= 0 {
                i as usize
            } else {
                list.len().wrapping_sub(i.abs() as usize)
            };

            list.get(index).cloned().ok_or(Error::NoElement {
                index,
                len: list.len(),
            })
        }
        (value, index) => Err(Error::CannotIndex {
            value: value.get_type(),
            with: index.get_type(),
        }),
    }
}

fn eval_ident(name: Box<str>, ctx: &Ctx) -> Result {
    ctx.get(&name).ok_or(Error::Undeclared(name))
}

fn eval_literal(lit: Lit) -> Value {
    match lit {
        Lit::Bool(b) => Value::Bool(b),
        Lit::Float(f) => Value::Float(f),
        Lit::Int(i) => Value::Int(i),
        Lit::String(s) => Value::String(s),
    }
}

fn eval_list(exprs: Vec<Expr>, ctx: &mut Ctx) -> Result {
    let mut list = Vec::with_capacity(exprs.len());

    for expr in exprs.into_iter() {
        let value = eval(expr, ctx)?;

        list.push(value);
    }

    Ok(Value::List(list))
}
