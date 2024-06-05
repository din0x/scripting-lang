use clap::Parser;
use scripting_lang::vm::{Ctx, Error, Value};
use std::{
    fs,
    io::{stdin, stdout, Write},
    path::PathBuf,
};

#[derive(Parser)]
struct Args {
    file: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let Some(path) = args.file else { start_cli() };

    let content = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(err) => {
            println!("error: cannot open file: {err}");
            return;
        }
    };

    let mut ctx = make_ctx();

    match scripting_lang::exec(&content, &mut ctx) {
        Ok(_) => {}
        Err(err) => println!("error: {err}"),
    }
}

fn start_cli() -> ! {
    loop {
        let mut buf = String::new();
        let mut ctx = make_ctx();

        loop {
            print!("> ");
            _ = stdout().flush();
            _ = stdin().read_line(&mut buf);

            match scripting_lang::exec(&buf, &mut ctx) {
                Ok(val) => {
                    println!("{}", val);
                }
                Err(err) => {
                    eprintln!("error: {}", err);
                }
            }

            buf.clear();
        }
    }
}

fn make_ctx() -> Ctx {
    let mut ctx = Ctx::new();

    ctx.decl("pi".into(), Value::Float(std::f64::consts::PI));
    ctx.decl_extern("print", |args| {
        for arg in args {
            match arg {
                Value::String(s) => print!("{s} "),
                other => print!("{other} "),
            }
        }

        println!();

        Ok(Value::Unit)
    });
    ctx.decl_extern("input", |args| {
        for arg in args {
            match arg {
                Value::String(s) => print!("{s} "),
                other => print!("{other} "),
            }
        }

        _ = stdout().flush();

        let mut buf = String::new();
        _ = stdin().read_line(&mut buf);

        Ok(Value::String(buf))
    });

    ctx.decl_extern("int", |args| {
        if args.len() != 1 {
            return Err(Error::ArgCount { expected: 1, found:args.len() });
        }

        let val = match args.into_iter().next().unwrap() {
            Value::Bool(b) => if b { 1 } else { 0 },
            Value::Int(i) => i,
            Value::Float(f) => f as i64,
            Value::String(s) => {
                let Ok(i) = s.trim().parse() else {
                    return Err(Error::Custom("cannot convert to int".into()))
                };

                i
            }
            _ => return Err(Error::Custom("cannot convert to int".into())),
        };

        Ok(Value::Int(val))
    });
    ctx.decl_extern("float", |args| {
        if args.len() != 1 {
            return Err(Error::ArgCount { expected: 1, found:args.len() });
        }

        let val = match args.into_iter().next().unwrap() {
            Value::Int(i) => i as f64,
            Value::Float(f) => f,
            Value::String(s) => {
                let Ok(i) = s.trim().parse() else {
                    return Err(Error::Custom("cannot convert to float".into()))
                };

                i
            }
            _ => return Err(Error::Custom("cannot convert to float".into())),
        };

        Ok(Value::Float(val))
    });
    ctx.decl_extern("str", |args| {
        if args.len() != 1 {
            return Err(Error::ArgCount { expected: 1, found:args.len() });
        }

        let val = args.into_iter().next().unwrap().to_string();

        Ok(Value::String(val))
    });
    ctx.decl_extern("len", |args| {
        if args.len() != 1 {
            return Err(Error::ArgCount { expected: 1, found:args.len() });
        }

        let val = match args.into_iter().next().unwrap() {
            Value::String(s) =>s.len(),
            Value::List(l) => l.borrow().len(),
            val => return Err(Error::Custom(format!("cannot call len() on `{}`", val.get_type()))),
        };

        Ok(Value::Int(val as i64))
    });

    ctx
}
