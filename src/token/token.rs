// pub type TokenType = String;
use crate::token::token_types::TokenTypes;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenTypes,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenTypes, literal: &str) -> Token {
        Token {
            token_type: token_type,
            literal: String::from(literal),
        }
    }
}
