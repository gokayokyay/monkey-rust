use crate::ast::ast::{ Program, Node, NodeType, StatementType, Identifier };
use crate::token::token_types::TokenTypes;
use crate::token::token::Token;
use crate::lexer::lexer::Lexer;

pub struct Parser {
  pub lexer: Lexer,
  pub current_token: Token,
  pub peek_token: Token,
  pub errors: Vec<String>
}

impl Parser {
  pub fn new(l: Lexer) -> Parser {
    let mut p = Parser {
      lexer: l,
      current_token: Token::new(TokenTypes::ILLEGAL, ""),
      peek_token: Token::new(TokenTypes::ILLEGAL, ""),
      errors: vec!()
    };
    p.next_token();
    p.next_token();
    p
  }
  pub fn next_token(&mut self) {
    self.current_token = self.peek_token.clone();
    self.peek_token = self.lexer.next_token();
  }
  pub fn current_token_is(&mut self, t: TokenTypes) -> bool {
    self.current_token.token_type == t
  }
  pub fn peek_token_is(&mut self, t: TokenTypes) -> bool {
    self.peek_token.token_type == t
  }
  pub fn expect_peek(&mut self, t: TokenTypes) -> bool {
    if self.peek_token_is(t) {
      self.next_token();
      return true;
    } else {
      self.peek_error(t);
      return false;
    }
  }
  pub fn peek_error(&mut self, token_type: TokenTypes) {
    let msg = String::from(format!("Expected next token to be: {:?}, got {:?} instead", token_type, self.peek_token.token_type));
    self.errors.push(msg);
  }
  pub fn parse_program(&mut self) -> Program {
    let mut p = Program {
      statements: [].to_vec()
    };

    while self.current_token.token_type != TokenTypes::EOF {
      let stmt = self.parse_statement();
      match stmt {
        Some(x) => p.statements.push(x),
        None => {

        }
      };
      self.next_token();
    };

    p
  }
  pub fn parse_statement(&mut self) -> Option<Node> {
    match self.current_token.token_type {
      TokenTypes::LET => self.parse_let_statement(),
      TokenTypes::RETURN => self.parse_return_statement(),
      _ => self.parse_expression_statement()
    }
  }
  pub fn parse_let_statement(&mut self) -> Option<Node> {
    let mut stmt = Node {
      token: self.peek_token.clone(),
      node_type: NodeType::Statement { statement_type: StatementType::Let },
      value: "".to_string(),
      name: Identifier::new(Token::new(TokenTypes::ILLEGAL, ""), "".to_string()),
    };

    if !self.expect_peek(TokenTypes::IDENT) {
      return None;
    }
    stmt.name = Identifier::new(self.current_token.clone(), self.current_token.literal.clone());
    stmt.value = self.current_token.literal.clone();
    
    if !self.expect_peek(TokenTypes::ASSIGN) {
      return None;
    }
    while !self.current_token_is(TokenTypes::SEMICOLON) {
      self.next_token()
    }
    return Some(stmt);
  }
  pub fn parse_return_statement(&mut self) -> Option<Node> {
    let stmt = Node {
      token: self.current_token.clone(),
      name: Identifier::new(Token::new(TokenTypes::ILLEGAL, ""), "".to_string()),
      node_type: NodeType::Statement { statement_type: StatementType::Return },
      value: "".to_string()
    };
    self.next_token();
    while !self.current_token_is(TokenTypes::SEMICOLON) {
      self.next_token();
    }
    return Some(stmt);
  }
  pub fn parse_expression_statement(&mut self) -> Option<Node> {
    let stmt = Node {
      token: self.current_token.clone(),
      name: Identifier::new(Token::new(TokenTypes::ILLEGAL, ""), "".to_string()),
      node_type: NodeType::Statement { statement_type: StatementType::Expression },
      value: "".to_string()
    };
    
  }
}


#[cfg(test)]
mod tests {
  use super::*;
  fn check_parser_errors(p: Parser) {
    if p.errors.len() == 0 { return; }
    println!("Parser has {} errors.", p.errors.len());
    for error in p.errors {
      println!("Parser error: {}", error);
    }
    panic!();
  }
  #[test]
  fn test_let_statements() {
    let input = r#"
    let x = 5;
    let y = 10;
    let foobar = 123123;
    "#;
    let l = Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();
    check_parser_errors(parser);

    assert_eq!(program.statements.len(), 3);

    struct Expectation {
      expected_identifier: String
    };
    let expectations = [
      Expectation { expected_identifier: "x".to_string() },
      Expectation { expected_identifier: "y".to_string() },
      Expectation { expected_identifier: "foobar".to_string() },
    ];
    for (i, statement) in expectations.iter().enumerate() {
      let state = &program.statements[i];
      assert_ne!(test_let_statement(state, &statement.expected_identifier), false); 
    }
  }
  fn test_let_statement(statement: &Node, name: &str) -> bool {
    println!("{:?}", statement);
    println!("{:?}", name);
    let a = match &statement.node_type {
      NodeType::Expression { expression_type: _ } => false,
      NodeType::Statement { statement_type: stm_type } => {
        match stm_type {
          StatementType::Let => true,
          _ => false
        }
      }
    };
    if !a { return false; }
    if statement.value != name {
      return false;
    }
    if statement.token_literal() != name {
      return false;
    }
    return true;
  }
  #[test]
  fn test_return_statements() {
    let input = r#"
    return 5;
    return 10;
    return 1232313;
    "#;
    let l = Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();
    check_parser_errors(parser);

    assert_eq!(program.statements.len(), 3);
    for statement in program.statements {
      let _ = match &statement.node_type {
        NodeType::Expression { expression_type: _ } => panic!("statement is not Statement!"),
        NodeType::Statement { statement_type: stm_type } => {
          match stm_type {
            StatementType::Return => true,
            _ => panic!("statement's type is not Return!")
          }
        }
      };
      assert_eq!(statement.token_literal(), "return");
    }
  }
  #[test]
  fn test_identifier_expression() {
    let input = r#"
    foobar;
    "#;
    let l = Lexer::new(input);
    let mut parser = Parser::new(l);
    let program = parser.parse_program();
    check_parser_errors(parser);

    assert_eq!(program.statements.len(), 1);
    let _ = match &program.statements[0].node_type {
      NodeType::Statement { statement_type } => {
        match statement_type {
          StatementType::Expression => true,
          _ => false
        }
      },
      _ => false
    };
    assert_eq!(program.statements[0].name.value, "foobar");
    assert_eq!(program.statements[0].name.token_literal(), "foobar");
  }
}