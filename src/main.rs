pub mod token;
mod lexer;

fn main() {
    // let t = token::token::Token::new();
    // let a = token::token_types::ILLEGAL;
    let l = lexer::lexer::Lexer::new("sa");
    // println!("Hello, world! {}", a);
}
