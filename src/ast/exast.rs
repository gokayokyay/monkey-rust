use crate::token::token_types::TokenTypes;

pub type Identifier = String;

pub trait Node {
    fn token_literal(&mut self) -> String;
}

pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Clone, Eq)]
pub enum Statement {
    Let(Identifier, Expression),
}

impl Node for Statement {
    fn token_literal(&mut self) -> String {
        match self {
            Statement::Let(ident, expr) => "asd".to_string(),
        }
    }
}

pub fn variant_eq(a: &Statement, b: &Statement) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

#[derive(PartialEq, Clone, Eq)]
pub enum Expression {
    None,
}
