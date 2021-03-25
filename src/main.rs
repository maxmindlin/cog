mod repl;
use repl::run_repl;
use std::{env, fs};

use interpret::{eval, Env};
use parser::{NodeKind, Parser};
use lexer::Lexer;

use std::rc::Rc;
use std::cell::RefCell;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => run_repl(),
        2 => {
            let filename = &args[1];
            let contents = fs::read_to_string(filename)
                .expect("unable to read file");
            let env = Rc::new(RefCell::new(Env::new()));
            let lex = Lexer::new(&contents);
            let mut parser = Parser::new(lex);
            match parser.parse_program() {
                Ok(prgm) => {
                    let out = eval(NodeKind::Program(prgm), env);
                    println!("{:#?}", out);
                },
                Err(e) => println!("parser error: {:#?}", e)
            }
        }
        _ => panic!("unsupported number of args"),
    }
}
