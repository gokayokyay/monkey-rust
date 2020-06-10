pub mod token;
pub mod lexer;
pub mod repl;
pub mod ast;
pub mod parser;

fn main() {
    // let t = token::token::Token::new();
    // let a = token::token_types::ILLEGAL;
    let l = lexer::lexer::Lexer::new("sa");
    println!("Hello, world! {:?}", l);
    repl::repl::start();
}
