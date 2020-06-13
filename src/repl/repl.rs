use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use std::io::{stdin, stdout, Write};

static PROMPT: &str = ">> ";

pub fn start() {
    loop {
        let mut input = String::new();
        println!("");
        print!("{}", PROMPT);
        let _ = stdout().flush();
        match stdin().read_line(&mut input) {
            Ok(_) => {
                let l = Lexer::new(&input);
                let mut p = Parser::new(l);
                let program = p.parse_program();
                print!("{}", program.to_string());
            }
            Err(error) => println!("Error: {}", error),
        }
    }
}
