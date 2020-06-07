use crate::ast::ast;
use crate::lexer::lexer;
use crate::token::token;
use crate::token::token_types::TokenTypes;

enum Precedences {
    LOWEST = 1,
    EQUALS = 2,      // ==
    LESSGREATER = 3, // > or <
    SUM = 4,         // +
    PRODUCT = 5,     // *
    PREFIX = 6,      // -X or !X
    CALL = 7,        // myFunction(X)
}

pub struct Parser {
    l: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(l: lexer::Lexer) -> Parser {
        let mut p = Parser {
            l: l,
            cur_token: token::Token::new(TokenTypes::ILLEGAL, ""),
            peek_token: token::Token::new(TokenTypes::ILLEGAL, ""),
            errors: vec![],
        };
        // let clone_parser = p.clone();
        p.next_token();
        p.next_token();
        p
    }
    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }
    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_token.token_type {
            TokenTypes::LET => Some(self.parse_let_statement()),
            TokenTypes::RETURN => Some(self.parse_return_statement()),
            _ => self.parse_expression_statement(),
        }
    }
    fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { statements: vec![] };
        while self.cur_token.token_type != TokenTypes::EOF {
            let statement = self.parse_statement();
            if let Some(stm) = statement {
                program.statements.push(stm);
            }
            self.next_token();
        }
        return program;
    }
    fn parse_expression(&mut self, precedence: Precedences) -> Option<ast::Expression> {
        println!("{:?}", self.cur_token.token_type);
        match self.cur_token.token_type {
            TokenTypes::IDENT => Some(self.parse_identifier()),
            _ => None,
        }
    }
    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let mut state = ast::Statement::new_empty();
        if let Some(e) = self.parse_expression(Precedences::LOWEST) {
            state.expression_val = e;
            state.statement_type = ast::StatementType::Expression;
        } else {
            return None;
        }
        if self.peek_token_is(TokenTypes::SEMICOLON) {
            self.next_token();
        }
        return Some(state);
    }
    fn parse_identifier(&mut self) -> ast::Expression {
        return ast::Expression {
            token: self.cur_token.clone(),
        };
    }
    fn parse_let_statement(&mut self) -> ast::Statement {
        let mut state = ast::Statement::new_empty();
        if self.expect_peek(TokenTypes::IDENT) {
            return ast::Statement::new_empty();
        }
        state.name = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        if self.expect_peek(TokenTypes::ASSIGN) {
            return ast::Statement::new_empty();
        }
        while self.cur_token_is(TokenTypes::SEMICOLON) {
            self.next_token();
        }
        return state;
    }
    fn parse_return_statement(&mut self) -> ast::Statement {
        let mut state = ast::Statement::new_empty();
        state.statement_type = ast::StatementType::Return;
        state.token = self.cur_token.clone();
        self.next_token();
        while self.cur_token_is(TokenTypes::SEMICOLON) {
            self.next_token();
        }
        return state;
    }
    fn cur_token_is(&mut self, t: TokenTypes) -> bool {
        return self.cur_token.token_type == t;
    }
    fn peek_token_is(&mut self, t: TokenTypes) -> bool {
        return self.peek_token.token_type == t;
    }
    fn expect_peek(&mut self, t: TokenTypes) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }
    fn peek_error(&mut self, t: TokenTypes) {
        let msg = format!(
            "Expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.token_type
        );
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_parser_errors(p: Parser) {
        let errors = p.errors;
        if errors.len() == 0 {
            return;
        }
        println!("Parser has errors");
        for error in errors {
            println!("Parser error: {}", error);
        }
        panic!();
    }

    #[test]
    fn test_let_statements() {
        let input = r#"
      let x = 5;
      let y = 10;
      let foobar = 838383;
    "#;
        let l = lexer::Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(program.statements.len(), 3);
        struct Expection {
            expected_identifier: String,
        };
        let expections: Vec<Expection> = vec![
            Expection {
                expected_identifier: String::from("x"),
            },
            Expection {
                expected_identifier: String::from("y"),
            },
            Expection {
                expected_identifier: String::from("foobar"),
            },
        ];

        for (i, expect) in expections.iter().enumerate() {
            let statements = program.statements.clone();
            let stmt = &statements[i];
            if !test_let_statement(stmt.clone(), expect.expected_identifier.clone()) {
                return;
            }
        }
    }

    fn test_let_statement(s: ast::Statement, name: String) -> bool {
        use crate::ast::ast::Node;
        if s.token_literal() != "let" {
            return false;
        }
        if s.statement_type != ast::StatementType::Let {
            return false;
        }
        if s.name.value != name {
            return false;
        }
        if s.token_literal() != name {
            return false;
        }
        return true;
    }

    #[test]
    fn test_return_statements() {
        use crate::ast::ast::Node;
        let input = r#"
        return 5;
        return 10;
        return 123123;
        "#;
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        assert_eq!(program.statements.len(), 3);

        for (_, statement) in program.statements.iter().enumerate() {
            assert_eq!(statement.statement_type, ast::StatementType::Return);
            assert_eq!(statement.token_literal(), "return");
        }
    }
    #[test]
    fn test_identifier_expression() {
        use crate::ast::ast::Node;
        let input = "foobar;";
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        assert_eq!(program.statements.len(), 1);
        let statement = &program.statements[0];
        println!("{:?}", statement);
        assert_eq!(statement.statement_type, ast::StatementType::Expression);
        let ident = &statement.expression_val;
        assert_eq!(ident.token.literal, "foobar");
        assert_eq!(ident.token_literal(), "foobar");
    }
}
