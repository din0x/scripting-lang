use std::io::{stdin, stdout, Write};

use scripting_lang::vm::Ctx;

fn main() {
    let mut buf = String::new();
    let mut ctx = Ctx::new();

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
