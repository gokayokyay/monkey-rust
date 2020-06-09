// use crate::token::token_types::TokenTypes;
use crate::token::token::Token;

#[derive(Clone, Debug)]
pub enum NodeType {
    Statement { statement_type: StatementType },
    Expression { expression_type: ExpressionType }
}

#[derive(Clone, Debug)]
pub enum StatementType {
    Let,
    Return,
    Expression,
    None
}

#[derive(Clone, Debug)]
pub enum ExpressionType {
    None
}

#[derive(Clone, Debug)]
pub struct Node {
    pub node_type: NodeType,
    pub token: Token,
    pub value: String,
    pub name: Identifier
}

impl Node {
    pub fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl ToString for Node {
    fn to_string(&self) -> String {
        use StatementType::*;
        match &self.node_type {
            NodeType::Statement { statement_type } => {
                match statement_type {
                    Let => {
                        let out = String::from(self.token_literal().to_string() + " ");
                        let out = out + &self.name.value;
                        let mut out = out + " = ";
                        if self.value != "" {
                            out = out + &self.value;
                        }
                        let out = out + ";";
                        return out;
                    },
                    Return => {
                        let mut out = String::from(self.token_literal().to_string() + " ");
                        if self.value != "" {
                            out = out + &self.value;
                        }
                        let out = out + ";";
                        return out;
                    },
                    Expression => {
                        let out = "".to_string();
                        return out;
                    },
                    _ => "".to_string()
                }
            },
            NodeType::Expression { expression_type } => {
                match expression_type {
                    _ => "".to_string()
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    pub statements: Vec<Node>
}

impl Program {
    pub fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
}

impl ToString for Program {
    fn to_string(&self) -> String {
        let mut out = String::from("");
        for statement in &self.statements {
            out = out + &statement.to_string();
        }
        return out;
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String
}

impl Identifier {
    pub fn token_literal(&self) -> &str {
        &self.token.literal
    }
    pub fn new(token: Token, value: String) -> Self {
        Identifier {
            token: token,
            value: value
        }
    }
}