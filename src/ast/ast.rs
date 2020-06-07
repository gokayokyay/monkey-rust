use crate::token::token::Token;
use crate::token::token_types::TokenTypes;

#[derive(Clone, PartialEq, Debug)]
pub enum StatementType {
    Let,
    Return,
    Expression,
    None,
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub token: Token,
    pub name: Identifier,
    pub ident_val: Identifier,
    pub expression_val: Expression,
    pub statement_type: StatementType,
    pub expression: Expression,
}
#[derive(Clone, Debug)]
pub struct Expression {
    pub token: Token,
}
#[derive(Clone, Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}
#[derive(Clone, Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

pub trait Node {
    fn token_literal(&self) -> String;
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        self.statements[0].token_literal()
    }
}

impl Statement {
    pub fn new_empty() -> Self {
        let st = Statement {
            token: Token::new(TokenTypes::ILLEGAL, ""),
            name: Identifier {
                token: Token::new(TokenTypes::ILLEGAL, ""),
                value: "".to_string(),
            },
            expression_val: Expression {
                token: Token::new(TokenTypes::ILLEGAL, ""),
            },
            statement_type: StatementType::None,
            expression: Expression {
                token: Token::new(TokenTypes::ILLEGAL, ""),
            },
            ident_val: Identifier {
                token: Token::new(TokenTypes::ILLEGAL, ""),
                value: "".to_string(),
            },
        };
        st
    }
}

impl ToString for Identifier {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        match self.statement_type {
            StatementType::Let => {
                let mut out = String::from("");
                out = out + &(self.token_literal() + " ");
                println!("{:?}", self.token_literal());
                out = out + &self.name.to_string();
                out = out + " = ";
                if self.expression_val.token.token_type != TokenTypes::ILLEGAL {
                    out = out + &self.expression_val.token_literal();
                } else {
                    out = out + &self.ident_val.token_literal();
                }
                out = out + ";";
                out
            }
            StatementType::Return => {
                let out = String::from("");
                let out = out + &self.token_literal();
                // Later we'll add expression to_string method
                // let out = out + &self.value.to_string();
                let out = out + ";";
                out
            }
            StatementType::Expression => {
                return "".to_string();
            }
            _ => "".to_string(),
        }
    }
}

impl ToString for Program {
    fn to_string(&self) -> String {
        let mut out = String::from("");
        for s in &self.statements {
            out = out + &s.to_string();
        }
        return out;
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn test_string() {
        use super::*;
        let program = Program {
            statements: vec![{
                let mut stm = Statement::new_empty();
                stm.token = Token::new(TokenTypes::LET, "let");
                stm.name = Identifier {
                    token: Token::new(TokenTypes::IDENT, "myVar"),
                    value: "myVar".to_string(),
                };
                stm.ident_val = Identifier {
                    token: Token::new(TokenTypes::IDENT, "anotherVar"),
                    value: "anotherVar".to_string(),
                };
                stm.statement_type = StatementType::Let;
                stm
            }],
        };
        assert_eq!(program.to_string(), "let myVar = anotherVar;".to_string());
    }
}
