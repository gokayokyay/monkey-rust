use crate::ast::ast::*;
use crate::lexer::lexer::Lexer;
use crate::token::token::Token;
use crate::token::token_types::TokenTypes;

pub enum PRECEDENCES {
  LOWEST = 1,
  EQUALS = 2,      // ==
  LESSGREATER = 3, // > or <
  SUM = 4,         // +
  PRODUCT = 5,     // *
  PREFIX = 6,      // -X or !X
  CALL = 7,        // myFunction(X)
}

pub struct Parser {
    pub lexer: Lexer,
    pub current_token: Token,
    pub peek_token: Token,
    pub errors: Vec<String>
}

impl Parser {
    pub fn new(l: Lexer) -> Self {
        let mut p = Parser {
            lexer: l,
            current_token: Token::new(TokenTypes::ILLEGAL, ""),
            peek_token: Token::new(TokenTypes::ILLEGAL, ""),
            errors: vec!()
        };
        p.next_token();
        p.next_token();
        return p;
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
            // self.peek_error(t);
            return false;
        }
    }
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.current_token.token_type != TokenTypes::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        return program;
    }
    pub fn parse_statement(&mut self) -> Option<Node> {
        match self.current_token.token_type {
            TokenTypes::LET => self.parse_let_statement(),
            TokenTypes::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    pub fn parse_let_statement(&mut self) -> Option<Node> {
        let mut stmt = Node {
            r#type: NodeType::Statement {
                r#type: StatementType::Let {
                    token: self.current_token.clone(),
                    value: ExpressionType::None,
                    name: Identifier {
                        token: Token::new(TokenTypes::ILLEGAL, ""),
                        value: "".to_string(),
                    },
                },
            },
        };

        if !self.expect_peek(TokenTypes::IDENT) {
            return None;
        }

        match &mut stmt.r#type {
            NodeType::Statement { r#type: x } => {
                println!("{:?}", x);
                match x {
                    StatementType::Let {
                        token: _,
                        value: _,
                        name: x_name,
                    } => {
                        // *x_name = Identifier {
                        //     token: self.current_token.clone(),
                        //     value: self.current_token.literal.clone(),
                        // };
                        std::mem::swap(
                            x_name,
                            &mut Identifier {
                                token: self.current_token.clone(),
                                value: self.current_token.literal.clone(),
                            },
                        );
                    }
                    _ => return None,
                }
            }
            _ => return None,
        };

        if !self.expect_peek(TokenTypes::ASSIGN) {
            return None;
        }

        while !self.current_token_is(TokenTypes::SEMICOLON) {
            self.next_token();
        }

        return Some(stmt);
    }
    pub fn parse_return_statement(&mut self) -> Option<Node> {
        let stmt = Node {
            r#type: NodeType::Statement {
                r#type: StatementType::Return {
                    token: self.current_token.clone(),
                    return_value: ExpressionType::None,
                },
            },
        };
        self.next_token();
        while !self.current_token_is(TokenTypes::SEMICOLON) {
            self.next_token();
        }
        return Some(stmt);
    }
    pub fn parse_expression_statement(&mut self) -> Option<Node> {
      let stmt = Node {
        r#type: NodeType::Statement {
          r#type: StatementType::Expression {
            token: self.current_token.clone(),
            expression: self.parse_expression(PRECEDENCES::LOWEST).unwrap()
          }
        }
      };
      if self.peek_token_is(TokenTypes::SEMICOLON) {
        self.next_token();
      }
      return Some(stmt);
    }
    pub fn parse_expression(&mut self, _precedence: PRECEDENCES) -> Option<ExpressionType> {
      let prefix = self.parse_prefix(self.current_token.token_type);
      prefix
    }
    pub fn parse_prefix(&mut self, token_type: TokenTypes) -> Option<ExpressionType> {
      match token_type {
        TokenTypes::IDENT => Some(self.parse_identifier()),
        _ => None
      }
    }
    pub fn parse_identifier(&mut self) -> ExpressionType {
      return ExpressionType::Identifier {
        identifier: Identifier {
          token: self.current_token.clone(),
          value: self.current_token.literal.clone()
        }
      }
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
            expected_identifier: String,
        };
        let expectations = [
            Expectation {
                expected_identifier: "x".to_string(),
            },
            Expectation {
                expected_identifier: "y".to_string(),
            },
            Expectation {
                expected_identifier: "foobar".to_string(),
            },
        ];
        for (i, statement) in expectations.iter().enumerate() {
            let state = &program.statements[i];
            assert_ne!(
                test_let_statement(state, &statement.expected_identifier),
                false
            );
        }
    }
    fn test_let_statement(statement: &Node, name: &str) -> bool {
        if statement.token_literal() != "let" {
            return false;
        }
        let _ = match &statement.r#type {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Let {
                    token: _,
                    name: n_name,
                    value: _,
                } => {
                    if n_name.value != name {
                        return false;
                    }
                    if n_name.token_literal() != name {
                        return false;
                    }
                    return true;
                }
                _ => false,
            },
            _ => false,
        };
        return true;
    }
    #[test]
    fn test_return_statement() {
        let input = r#"
        return 5;
        return 10;
        return 123123;
        "#;
        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        check_parser_errors(parser);

        assert_eq!(program.statements.len(), 3);

        for stmt in program.statements {
            match &stmt.r#type {
                NodeType::Statement {
                    r#type: statement_type,
                } => {
                    match statement_type {
                        StatementType::Return { .. } => {
                            assert_eq!(stmt.token_literal(), "return");
                            continue;
                        }
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            };
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
      let stmt = &program.statements[0];
      println!("{:?}", stmt.r#type);
      match &stmt.r#type {
        NodeType::Statement { r#type } => {
          match r#type {
            StatementType::Expression { expression, .. } => {
              match expression {
                ExpressionType::Identifier { identifier } => {
                  assert_eq!(identifier.value, "foobar");
                  assert_eq!(identifier.token_literal(), "foobar");
                },
                _ => panic!()
              }
            },
            _ => panic!()
          }
        },
        _ => panic!()
      }
    }
}
