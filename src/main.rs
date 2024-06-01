use clap::Parser;
use scripting_lang::vm::{Ctx, Value};
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
    ctx.decl(
        "print".into(),
        Value::Extern(|args| {
            for arg in args {
                match arg {
                    Value::String(s) => print!("{s} "),
                    other => print!("{other} "),
                }
            }

            println!();

            Ok(Value::Unit)
        }),
    );

    ctx
}
