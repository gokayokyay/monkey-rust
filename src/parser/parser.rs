use crate::ast::ast::*;
use crate::lexer::lexer::Lexer;
use crate::token::token::Token;
use crate::token::token_types::TokenTypes;
use std::collections::HashMap;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum PRECEDENCES {
    LOWEST = 1,
    EQUALS = 2,      // ==
    LESSGREATER = 3, // > or <
    SUM = 4,         // +
    PRODUCT = 5,     // *
    PREFIX = 6,      // -X or !X
    CALL = 7,        // myFunction(X)
}

pub fn get_precedence_map() -> HashMap<TokenTypes, PRECEDENCES> {
    let mut precedences_map: HashMap<TokenTypes, PRECEDENCES> = HashMap::new();
    precedences_map.insert(TokenTypes::EQ, PRECEDENCES::EQUALS);
    precedences_map.insert(TokenTypes::NOT_EQ, PRECEDENCES::EQUALS);
    precedences_map.insert(TokenTypes::LT, PRECEDENCES::LESSGREATER);
    precedences_map.insert(TokenTypes::GT, PRECEDENCES::LESSGREATER);
    precedences_map.insert(TokenTypes::PLUS, PRECEDENCES::SUM);
    precedences_map.insert(TokenTypes::MINUS, PRECEDENCES::SUM);
    precedences_map.insert(TokenTypes::SLASH, PRECEDENCES::PRODUCT);
    precedences_map.insert(TokenTypes::ASTERISK, PRECEDENCES::PRODUCT);
    precedences_map
}

pub struct Parser {
    pub lexer: Lexer,
    pub current_token: Token,
    pub peek_token: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(l: Lexer) -> Self {
        let mut p = Parser {
            lexer: l,
            current_token: Token::new(TokenTypes::ILLEGAL, ""),
            peek_token: Token::new(TokenTypes::ILLEGAL, ""),
            errors: vec![],
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
    pub fn peek_precedence(&mut self) -> PRECEDENCES {
        return match get_precedence_map().get(&self.peek_token.token_type) {
            Some(p) => p.clone(),
            None => PRECEDENCES::LOWEST,
        };
    }
    pub fn current_precedence(&mut self) -> PRECEDENCES {
        return match get_precedence_map().get(&self.current_token.token_type) {
            Some(p) => p.clone(),
            None => PRECEDENCES::LOWEST,
        };
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
                    expression: self.parse_expression(PRECEDENCES::LOWEST).unwrap(),
                },
            },
        };
        if self.peek_token_is(TokenTypes::SEMICOLON) {
            self.next_token();
        }
        return Some(stmt);
    }
    pub fn parse_expression(&mut self, precedence: PRECEDENCES) -> Option<ExpressionType> {
        let mut left_expr = self.parse_prefix(self.current_token.token_type);
        while !self.peek_token_is(TokenTypes::SEMICOLON) && precedence < self.peek_precedence() {
            self.next_token();
            if let Some(x) = left_expr.clone() {
                left_expr = Some(self.parse_infix_expression(x));
            }
        }

        return left_expr;
    }
    pub fn parse_prefix(&mut self, token_type: TokenTypes) -> Option<ExpressionType> {
        match token_type {
            TokenTypes::IDENT => Some(self.parse_identifier()),
            TokenTypes::INT => Some(self.parse_integer_literal()),
            TokenTypes::BANG | TokenTypes::MINUS => Some(self.parse_prefix_expression()),
            TokenTypes::TRUE | TokenTypes::FALSE => Some(self.parse_boolean()),
            _ => None,
        }
    }
    pub fn parse_identifier(&mut self) -> ExpressionType {
        return ExpressionType::Identifier {
            identifier: Identifier {
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            },
        };
    }
    pub fn parse_integer_literal(&mut self) -> ExpressionType {
        let val = self.current_token.literal.clone();
        return ExpressionType::Integer {
            token: self.current_token.clone(),
            value: val.parse().unwrap(),
        };
    }
    pub fn parse_prefix_expression(&mut self) -> ExpressionType {
        let mut expr = ExpressionType::Prefix {
            token: self.current_token.clone(),
            operator: self.current_token.literal.clone(),
            right: Box::from(ExpressionType::None),
        };

        self.next_token();

        let _ = match &mut expr {
            ExpressionType::Prefix { right, .. } => {
                *right = Box::from(self.parse_expression(PRECEDENCES::PREFIX).unwrap());
                false
            }
            _ => false,
        };

        return expr;
    }
    pub fn parse_infix_expression(&mut self, left: ExpressionType) -> ExpressionType {
        let mut expr = ExpressionType::Infix {
            left: Box::from(left),
            operator: self.current_token.literal.clone(),
            right: Box::from(ExpressionType::None),
            token: self.current_token.clone(),
        };

        let precedence = self.current_precedence();
        self.next_token();
        let _ = match &mut expr {
            ExpressionType::Infix { right, .. } => std::mem::swap(
                right,
                &mut Box::from(self.parse_expression(precedence).unwrap()),
            ),
            _ => panic!(),
        };

        return expr;
    }
    pub fn parse_boolean(&mut self) -> ExpressionType {
        return ExpressionType::Boolean {
            token: self.current_token.clone(),
            value: self.current_token_is(TokenTypes::TRUE),
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn check_parser_errors(p: Parser) {
        if p.errors.len() == 0 {
            return;
        }
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

        match &stmt.r#type {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Expression { expression, .. } => match expression {
                    ExpressionType::Identifier { identifier } => {
                        assert_eq!(identifier.value, "foobar");
                        assert_eq!(identifier.token_literal(), "foobar");
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    #[test]
    fn test_integer_literal_expressions() {
        let input = r#"
      5;
      "#;
        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        check_parser_errors(parser);
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];
        match &stmt.r#type {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Expression { expression, .. } => match expression {
                    ExpressionType::Integer { value, .. } => {
                        assert_eq!(*value, 5);
                        assert_eq!(expression.token_literal(), "5");
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    #[test]
    fn test_parsing_prefix_expressions_int() {
        use std::ops::Deref;
        struct PrefixTest {
            input: String,
            operator: String,
            integer_value: i64,
        };
        let prefix_tests = vec![
            PrefixTest {
                input: "!5".to_string(),
                operator: "!".to_string(),
                integer_value: 5,
            },
            PrefixTest {
                input: "!5".to_string(),
                operator: "!".to_string(),
                integer_value: 5,
            },
        ];
        for prefix in prefix_tests {
            let l = Lexer::new(&prefix.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            match &stmt.r#type {
                NodeType::Statement { r#type } => match r#type {
                    StatementType::Expression { expression, .. } => match expression {
                        ExpressionType::Prefix {
                            operator, right, ..
                        } => {
                            assert_eq!(operator, &prefix.operator);
                            assert_eq!(
                                test_integer_literal(
                                    (*right.deref()).clone(),
                                    prefix.integer_value
                                ),
                                true
                            );
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }
    #[test]
    fn test_parsing_prefix_expressions_bool() {
        use std::ops::Deref;
        struct PrefixTest {
            input: String,
            operator: String,
            value: bool,
        };
        let prefix_tests = vec![
            PrefixTest {
                input: "!true".to_string(),
                operator: "!".to_string(),
                value: true,
            },
            PrefixTest {
                input: "!false".to_string(),
                operator: "!".to_string(),
                value: false,
            },
        ];
        for prefix in prefix_tests {
            let l = Lexer::new(&prefix.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            match &stmt.r#type {
                NodeType::Statement { r#type } => match r#type {
                    StatementType::Expression { expression, .. } => match expression {
                        ExpressionType::Prefix {
                            operator, right, ..
                        } => {
                            assert_eq!(operator, &prefix.operator);
                            assert_eq!(
                                test_boolean_literal((*right.deref()).clone(), prefix.value),
                                true
                            );
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }
    fn test_integer_literal(il: ExpressionType, value: i64) -> bool {
        match il {
            ExpressionType::Integer {
                value: real_val, ..
            } => {
                assert_eq!(real_val, value);
                assert_eq!(il.token_literal(), format!("{}", value));
            }
            _ => return false,
        }
        return true;
    }
    #[test]
    fn test_parsing_infix_expressions_int() {
        use std::ops::Deref;
        struct InfixTest {
            input: String,
            left_value: i64,
            operator: String,
            right_value: i64,
        };
        let infix_tests = vec![
            InfixTest {
                input: "5 + 5;".to_string(),
                left_value: 5,
                operator: "+".to_string(),
                right_value: 5,
            },
            InfixTest {
                input: "5 - 5;".to_string(),
                left_value: 5,
                operator: "-".to_string(),
                right_value: 5,
            },
            InfixTest {
                input: "5 * 5;".to_string(),
                left_value: 5,
                operator: "*".to_string(),
                right_value: 5,
            },
            InfixTest {
                input: "5 / 5;".to_string(),
                left_value: 5,
                operator: "/".to_string(),
                right_value: 5,
            },
            InfixTest {
                input: "5 > 5;".to_string(),
                left_value: 5,
                operator: ">".to_string(),
                right_value: 5,
            },
            InfixTest {
                input: "5 < 5;".to_string(),
                left_value: 5,
                operator: "<".to_string(),
                right_value: 5,
            },
            InfixTest {
                input: "5 == 5;".to_string(),
                left_value: 5,
                operator: "==".to_string(),
                right_value: 5,
            },
            InfixTest {
                input: "5 != 5;".to_string(),
                left_value: 5,
                operator: "!=".to_string(),
                right_value: 5,
            },
        ];
        for infix in infix_tests {
            let l = Lexer::new(&infix.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            match &stmt.r#type {
                NodeType::Statement { r#type } => match r#type {
                    StatementType::Expression { expression, .. } => match expression {
                        ExpressionType::Infix {
                            left,
                            operator,
                            right,
                            ..
                        } => {
                            assert_eq!(operator, &infix.operator);
                            assert_eq!(
                                test_integer_literal((*right.deref()).clone(), infix.right_value),
                                true
                            );
                            assert_eq!(
                                test_integer_literal((*left.deref()).clone(), infix.left_value),
                                true
                            );
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }
    #[test]
    fn test_parsing_infix_expressions_bool() {
        use std::ops::Deref;
        struct InfixTest {
            input: String,
            left_value: bool,
            operator: String,
            right_value: bool,
        };
        let infix_tests = vec![
            InfixTest {
                input: "true == true;".to_string(),
                left_value: true,
                operator: "==".to_string(),
                right_value: true,
            },
            InfixTest {
                input: "true != false;".to_string(),
                left_value: true,
                operator: "!=".to_string(),
                right_value: false,
            },
            InfixTest {
                input: "false == false;".to_string(),
                left_value: false,
                operator: "==".to_string(),
                right_value: false,
            },
        ];
        for infix in infix_tests {
            let l = Lexer::new(&infix.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            match &stmt.r#type {
                NodeType::Statement { r#type } => match r#type {
                    StatementType::Expression { expression, .. } => match expression {
                        ExpressionType::Infix {
                            left,
                            operator,
                            right,
                            ..
                        } => {
                            assert_eq!(operator, &infix.operator);
                            assert_eq!(
                                test_boolean_literal((*right.deref()).clone(), infix.right_value),
                                true
                            );
                            assert_eq!(
                                test_boolean_literal((*left.deref()).clone(), infix.left_value),
                                true
                            );
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }
    #[test]
    fn test_operator_precedence_parsing() {
        struct OperatorPrecedenceTest {
            input: String,
            expected: String,
        };
        let tests = vec![
            OperatorPrecedenceTest {
                input: "-a * b".to_string(),
                expected: "((-a) * b)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "!-a".to_string(),
                expected: "(!(-a))".to_string(),
            },
            OperatorPrecedenceTest {
                input: "a + b + c".to_string(),
                expected: "((a + b) + c)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "a + b - c".to_string(),
                expected: "((a + b) - c)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "a * b / c".to_string(),
                expected: "((a * b) / c)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "a + b / c".to_string(),
                expected: "(a + (b / c))".to_string(),
            },
            OperatorPrecedenceTest {
                input: "a + b * c + d / e - f".to_string(),
                expected: "(((a + (b * c)) + (d / e)) - f)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "3 + 4; -5 * 5".to_string(),
                expected: "(3 + 4)((-5) * 5)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "5 > 4 == 3 < 4".to_string(),
                expected: "((5 > 4) == (3 < 4))".to_string(),
            },
            OperatorPrecedenceTest {
                input: "5 < 4 != 3 > 4".to_string(),
                expected: "((5 < 4) != (3 > 4))".to_string(),
            },
            OperatorPrecedenceTest {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
            },
            OperatorPrecedenceTest {
                input: "true".to_string(),
                expected: "true".to_string(),
            },
            OperatorPrecedenceTest {
                input: "false".to_string(),
                expected: "false".to_string(),
            },
            OperatorPrecedenceTest {
                input: "3 > 5 == false".to_string(),
                expected: "((3 > 5) == false)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "3 < 5 == true".to_string(),
                expected: "((3 < 5) == true)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
            },
        ];
        for prec in tests {
            let l = Lexer::new(&prec.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let actual = program.to_string();
            assert_eq!(actual, prec.expected);
        }
    }
    fn test_identifier(expr: ExpressionType, val: String) -> bool {
        match expr {
            ExpressionType::Identifier { identifier } => {
                assert_eq!(val, identifier.value);
                assert_eq!(val, identifier.token_literal());
            }
            _ => panic!(),
        }
        return true;
    }

    fn test_literal_expression(expr: ExpressionType, expected: ExpressionType) -> bool {
        match expr.clone() {
            ExpressionType::Identifier { .. } => {
                if let ExpressionType::Identifier {
                    identifier: expected_identifier,
                } = expected
                {
                    return test_identifier(expr, expected_identifier.value);
                }
                return false;
            }
            ExpressionType::Integer { .. } => {
                if let ExpressionType::Integer { value, .. } = expected {
                    return test_integer_literal(expr, value);
                }
                return false;
            }
            ExpressionType::Boolean { .. } => {
                if let ExpressionType::Boolean { value, .. } = expected {
                    return test_boolean_literal(expr, value);
                }
                return false;
            }
            _ => return false,
        }
    }
    fn test_infix_expression(
        expr: ExpressionType,
        left: ExpressionType,
        operator: String,
        right: ExpressionType,
    ) -> bool {
        match expr {
            ExpressionType::Infix {
                left: expected_left,
                right: expected_right,
                operator: expected_operator,
                ..
            } => {
                test_literal_expression(*expected_left, left);
                test_literal_expression(*expected_right, right);
                assert_eq!(operator, expected_operator);
                return false;
            }
            _ => return false,
        }
    }
    #[test]
    fn test_boolean_expressions() {
        let input = r#"
        true;
        "#;
        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        check_parser_errors(parser);
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];
        match &stmt.r#type {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Expression { expression, .. } => match expression {
                    ExpressionType::Boolean { value, .. } => {
                        assert_eq!(value, &true);
                        assert_eq!(expression.token_literal(), "true");
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    fn test_boolean_literal(expr: ExpressionType, val: bool) -> bool {
        match expr {
            ExpressionType::Boolean { value, .. } => {
                assert_eq!(value, val);
                assert_eq!(expr.token_literal(), val.to_string())
            }
            _ => panic!(),
        }
        return true;
    }
}
