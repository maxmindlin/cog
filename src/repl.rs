use rustyline::error::ReadlineError;
use rustyline::Editor;

use interpret::{eval, Env};
use parser::{NodeKind, Parser};
use lexer::Lexer;

use std::rc::Rc;
use std::cell::RefCell;

const PROMPT: &str = ">> ";

pub fn run_repl() {
    let mut rl = Editor::<()>::new();
    let env = Rc::new(RefCell::new(Env::new()));
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    println!("Welcome to the Cog programming language");
    println!("Press CTRL-c to exit.");
    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let lexer = Lexer::new(&line);
                let mut parser = Parser::new(lexer);
                match parser.parse_program() {
                    Ok(prgm) => {
                        let out = eval(NodeKind::Program(prgm), Rc::clone(&env));
                        println!("{}", out.to_string());
                    }
                    Err(e) => println!("parser error: {:#?}", e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Exiting.");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Eof");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
