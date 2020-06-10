use crate::token::token::Token;
use std::ops::Deref;
// use crate::token::token_types::TokenTypes;

#[derive(Debug, Clone)]
pub struct Identifier {
  pub token: Token,
  pub value: String
}

// impl Deref for Identifier {
//   type Target = Identifier;
//   fn deref(&self) -> &Self::Target {
//     &self.0
//   }
// }

impl Identifier {
  pub fn token_literal(&self) -> String {
    return self.token.literal.clone();
  }
}

#[derive(Debug, Clone)]
pub enum StatementType {
  Let { token: Token, value: ExpressionType, name: Identifier },
  None,
}

impl StatementType {
  pub fn token_literal(&self) -> String {
    match &self {
      StatementType::Let { token, value: _, name: _ } => {
        return token.clone().literal;
      },
      _ => "".to_string()
    }
  }
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
  None
}

impl ExpressionType {
  pub fn token_literal(&self) -> String {
    "".to_string()
  }
}

#[derive(Debug, Clone)]
pub enum NodeType {
  Statement { r#type: StatementType },
  Expression { r#type: ExpressionType }
}

impl NodeType {
  pub fn token_literal(&self) -> String {
    match &self {
      NodeType::Statement { r#type: stm_type } => {
        return stm_type.token_literal();
      },
      NodeType::Expression { r#type: expr_type } => {
        return expr_type.token_literal();
      }
    }
  }
}

#[derive(Debug, Clone)]
pub struct Node {
  pub r#type: NodeType
}

impl Node {
  pub fn token_literal(&self) -> String {
    match self.r#type {
      NodeType::Statement { r#type: _ } => self.r#type.token_literal(),
      NodeType::Expression { r#type: _ } => self.r#type.token_literal()
    }
  }
}

pub struct Program {
  pub statements: Vec<Node>
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