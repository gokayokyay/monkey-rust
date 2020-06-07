pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod token;

fn main() {
    // let t = token::token::Token::new();
    // let a = token::token_types::ILLEGAL;
    let l = lexer::lexer::Lexer::new("sa");
    println!("Hello, world! {:?}", l);
    // repl::repl::start();
}
