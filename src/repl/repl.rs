use std::io::{stdin, stdout, Write};
use crate::lexer::lexer::Lexer;
use crate::token::token_types::TokenTypes;

static PROMPT: &str = ">> ";

// TEMPORARY
#[allow(irrefutable_let_patterns)]
pub fn start() {
  loop {
    let mut input = String::new();
    print!("{}", PROMPT);
    let _ = stdout().flush();
    match stdin().read_line(&mut input) {
      Ok(_) => {
        print!("{}", input);
        let mut l = Lexer::new(&input);
        while let token = l.next_token() {
          if token.token_type == TokenTypes::EOF || token.token_type == TokenTypes::ILLEGAL {
            break;
          }
          println!("{:?}", token);
        }
      },
      Err(error) => println!("Error: {}", error),
    }
  }
}