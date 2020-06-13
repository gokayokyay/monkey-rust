use crate::token::token::Token;
// use crate::token::token_types::TokenTypes;

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}

impl ToString for Identifier {
    fn to_string(&self) -> String {
        return self.value.clone();
    }
}

#[derive(Debug, Clone)]
pub enum StatementType {
    Let {
        token: Token,
        value: Box<ExpressionType>,
        name: Identifier,
    },
    Return {
        token: Token,
        return_value: Box<ExpressionType>,
    },
    Expression {
        token: Token,
        expression: Box<ExpressionType>,
    },
    Block {
        token: Token,
        statements: Vec<Box<StatementType>>,
    },
    None,
}

impl StatementType {
    pub fn token_literal(&self) -> String {
        match &self {
            StatementType::Let {
                token,
                value: _,
                name: _,
            } => {
                return token.clone().literal;
            }
            StatementType::Return { token, .. } => {
                return token.clone().literal;
            }
            StatementType::Expression { token, .. } => {
                return token.clone().literal;
            }
            StatementType::Block { token, .. } => {
                return token.clone().literal;
            }
            _ => "".to_string(),
        }
    }
}

impl ToString for StatementType {
    fn to_string(&self) -> String {
        let mut out = String::from("");
        match &self {
            StatementType::Let { name, value, .. } => {
                out = out + &self.token_literal();
                out = out + " ";
                out = out + &name.to_string();
                out = out + " = ";
                out = out + &value.to_string();
                out = out + ";";
            }
            StatementType::Return { return_value, .. } => {
                out = out + &self.token_literal();
                out = out + " ";
                out = out + &return_value.to_string();
                out = out + ";";
            }
            StatementType::Expression { expression, .. } => {
                out = out + &expression.to_string();
            }
            StatementType::Block { statements, .. } => {
                for s in statements {
                    out = out + &s.to_string();
                }
            }
            _ => out = out + "",
        };
        return out;
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
    Identifier {
        identifier: Identifier,
    },
    Integer {
        token: Token,
        value: i64,
    },
    Prefix {
        token: Token,
        operator: String,
        right: Box<ExpressionType>,
    },
    Infix {
        token: Token,
        left: Box<ExpressionType>,
        operator: String,
        right: Box<ExpressionType>,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    If {
        token: Token,
        condition: Box<ExpressionType>,
        // BlockStatement
        consequence: StatementType,
        // BlockStatement
        alternative: Option<StatementType>,
    },
    FunctionLiteral {
        token: Token,
        parameters: Vec<Identifier>,
        body: Box<StatementType>,
    },
    CallExpression {
        token: Token,
        function: Box<ExpressionType>,
        arguments: Vec<ExpressionType>,
    },
    None,
}

impl ExpressionType {
    pub fn token_literal(&self) -> String {
        match &self {
            ExpressionType::Identifier { identifier } => identifier.token_literal(),
            ExpressionType::Integer { token, .. } => token.literal.clone(),
            ExpressionType::Prefix { token, .. } => token.literal.clone(),
            ExpressionType::Infix { token, .. } => token.literal.clone(),
            ExpressionType::Boolean { token, .. } => token.literal.clone(),
            ExpressionType::If { token, .. } => token.literal.clone(),
            ExpressionType::FunctionLiteral { token, .. } => token.literal.clone(),
            ExpressionType::CallExpression { token, .. } => token.literal.clone(),
            _ => "".to_string(),
        }
    }
}

impl ToString for ExpressionType {
    fn to_string(&self) -> String {
        match &self {
            ExpressionType::Identifier { identifier } => identifier.to_string(),
            ExpressionType::Integer { token, .. } => token.literal.clone(),
            ExpressionType::Prefix {
                operator, right, ..
            } => {
                let mut out = String::from("");
                out = out + "(";
                out = out + operator;
                out = out + &right.to_string();
                out = out + ")";
                out
            }
            ExpressionType::Infix {
                left,
                operator,
                right,
                ..
            } => {
                let mut out = String::from("");
                out = out + "(";
                out = out + &left.to_string();
                out = out + &format!(" {} ", operator);
                out = out + &right.to_string();
                out = out + ")";
                out
            }
            ExpressionType::Boolean { token, .. } => token.literal.clone(),
            ExpressionType::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                let mut out = String::from("");
                out = out + "if";
                out = out + &condition.to_string();
                out = out + " ";
                out = out + &consequence.to_string();
                if let Some(x) = alternative {
                    out = out + "else ";
                    out = out + &x.to_string();
                }
                out
            }
            ExpressionType::FunctionLiteral {
                parameters,
                body,
                token,
            } => {
                let mut out = String::from("");
                let mut params = vec![];
                for p in parameters {
                    params.push(p.to_string());
                }
                out = out + &token.literal.clone();
                out = out + "(";
                out = out + &params.join(", ");
                out = out + ")";
                out = out + &body.to_string();
                return out;
            }
            ExpressionType::CallExpression {
                arguments,
                function,
                ..
            } => {
                let mut out = String::from("");
                let mut args = vec![];
                for a in arguments {
                    args.push(a.to_string());
                }
                out = out + &function.to_string();
                out = out + "(";
                out = out + &args.join(", ");
                out = out + ")";
                return out;
            }
            _ => "".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Statement { r#type: StatementType },
    Expression { r#type: ExpressionType },
}

impl NodeType {
    pub fn token_literal(&self) -> String {
        match &self {
            NodeType::Statement { r#type: stm_type } => {
                return stm_type.token_literal();
            }
            NodeType::Expression { r#type: expr_type } => {
                return expr_type.token_literal();
            }
        }
    }
}

impl ToString for NodeType {
    fn to_string(&self) -> String {
        match &self {
            NodeType::Statement { r#type } => r#type.to_string(),
            NodeType::Expression { r#type } => r#type.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub r#type: NodeType,
}

impl Node {
    pub fn token_literal(&self) -> String {
        match self.r#type {
            NodeType::Statement { r#type: _ } => self.r#type.token_literal(),
            NodeType::Expression { r#type: _ } => self.r#type.token_literal(),
        }
    }
}

impl ToString for Node {
    fn to_string(&self) -> String {
        self.r#type.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Node>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements[0].token_literal();
        } else {
            return "".to_string();
        }
    }
}

impl ToString for Program {
    fn to_string(&self) -> String {
        let mut out = String::from("");
        for stm in &self.statements {
            out = out + &stm.to_string();
        }
        return out;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::token_types::TokenTypes;
    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Node {
                r#type: NodeType::Statement {
                    r#type: {
                        StatementType::Let {
                            token: Token::new(TokenTypes::LET, "let"),
                            name: Identifier {
                                token: Token::new(TokenTypes::IDENT, "myVar"),
                                value: "myVar".to_string(),
                            },
                            value: Box::from(ExpressionType::Identifier {
                                identifier: Identifier {
                                    token: Token::new(TokenTypes::IDENT, "anotherVar"),
                                    value: "anotherVar".to_string(),
                                },
                            }),
                        }
                    },
                },
            }],
        };
        assert_eq!(program.to_string(), "let myVar = anotherVar;".to_string());
    }
}
