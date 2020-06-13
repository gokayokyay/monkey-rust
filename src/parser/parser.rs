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
    precedences_map.insert(TokenTypes::LPAREN, PRECEDENCES::CALL);
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
                    value: Box::from(ExpressionType::None),
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
        let cur_tok = self.current_token.clone();

        if !self.expect_peek(TokenTypes::ASSIGN) {
            return None;
        }
        self.next_token();
        match &mut stmt.r#type {
            NodeType::Statement { r#type: x } => {
                match x {
                    StatementType::Let {
                        token: _,
                        value: x_value,
                        name: x_name,
                    } => {
                        // *x_name = Identifier {
                        //     token: self.current_token.clone(),
                        //     value: self.current_token.literal.clone(),
                        // };
                        std::mem::swap(
                            x_name,
                            &mut Identifier {
                                token: cur_tok.clone(),
                                value: cur_tok.literal,
                            },
                        );
                        *x_value = Box::from(self.parse_expression(PRECEDENCES::LOWEST).unwrap());
                    }
                    _ => return None,
                }
            }
            _ => return None,
        };

        while !self.current_token_is(TokenTypes::SEMICOLON) {
            self.next_token();
        }

        return Some(stmt);
    }
    pub fn parse_return_statement(&mut self) -> Option<Node> {
        let cur_tok = self.current_token.clone();
        self.next_token();
        let stmt = Node {
            r#type: NodeType::Statement {
                r#type: StatementType::Return {
                    token: cur_tok,
                    return_value: Box::from(self.parse_expression(PRECEDENCES::LOWEST).unwrap()),
                },
            },
        };
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
                    expression: Box::from(self.parse_expression(PRECEDENCES::LOWEST).unwrap()),
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
                left_expr = Some(self.parse_infix(x));
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
            TokenTypes::LPAREN => self.parse_grouped_expression(),
            TokenTypes::IF => self.parse_if_expression(),
            TokenTypes::FUNCTION => self.parse_function_literal(),
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
    pub fn parse_infix(&mut self, left: ExpressionType) -> ExpressionType {
        match self.current_token.clone().token_type {
            TokenTypes::LPAREN => self.parse_call_expression(left),
            _ => self.parse_infix_expression(left),
        }
    }
    pub fn parse_boolean(&mut self) -> ExpressionType {
        return ExpressionType::Boolean {
            token: self.current_token.clone(),
            value: self.current_token_is(TokenTypes::TRUE),
        };
    }
    pub fn parse_grouped_expression(&mut self) -> Option<ExpressionType> {
        self.next_token();
        let exp = self.parse_expression(PRECEDENCES::LOWEST);
        if !self.expect_peek(TokenTypes::RPAREN) {
            return None;
        }
        return exp;
    }
    pub fn parse_if_expression(&mut self) -> Option<ExpressionType> {
        let cur_tok = self.current_token.clone();
        if !self.expect_peek(TokenTypes::LPAREN) {
            return None;
        }
        self.next_token();
        let condition = self.parse_expression(PRECEDENCES::LOWEST).unwrap();
        if !self.expect_peek(TokenTypes::RPAREN) || !self.expect_peek(TokenTypes::LBRACE) {
            return None;
        }
        let consequence = self.parse_block_statement();
        let alternative;
        if self.peek_token_is(TokenTypes::ELSE) {
            self.next_token();
            if !self.expect_peek(TokenTypes::LBRACE) {
                return None;
            }
            alternative = Option::from(self.parse_block_statement());
        } else {
            alternative = None;
        }
        let expr = ExpressionType::If {
            token: cur_tok,
            condition: Box::from(condition),
            consequence: consequence,
            alternative: alternative,
        };
        return Some(expr);
    }
    pub fn parse_block_statement(&mut self) -> StatementType {
        let mut block = StatementType::Block {
            token: self.current_token.clone(),
            statements: vec![],
        };
        let mut block_statements = vec![];
        self.next_token();
        while !self.current_token_is(TokenTypes::RBRACE) && !self.current_token_is(TokenTypes::EOF)
        {
            let stm = self.parse_statement();
            if let Some(x) = stm {
                match x.r#type {
                    NodeType::Statement { r#type: y } => {
                        block_statements.push(Box::from(y));
                    }
                    _ => (),
                }
            }
            self.next_token();
        }
        match &mut block {
            StatementType::Block { statements, .. } => {
                *statements = block_statements;
            }
            _ => panic!(),
        };
        return block;
    }
    pub fn parse_function_literal(&mut self) -> Option<ExpressionType> {
        let cur_tok = self.current_token.clone();
        if !self.expect_peek(TokenTypes::LPAREN) {
            return None;
        };
        let parameters = self.parse_function_parameters();
        if !self.expect_peek(TokenTypes::LBRACE) {
            return None;
        };
        let body = self.parse_block_statement();
        return Some(ExpressionType::FunctionLiteral {
            token: cur_tok,
            body: Box::from(body),
            parameters,
        });
    }
    pub fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers: Vec<Identifier> = vec![];
        if self.peek_token_is(TokenTypes::RPAREN) {
            self.next_token();
            return identifiers;
        }
        self.next_token();
        let ident = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };
        identifiers.push(ident);
        while self.peek_token_is(TokenTypes::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Identifier {
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            };
            identifiers.push(ident);
        }
        if !self.expect_peek(TokenTypes::RPAREN) {
            panic!();
        }
        return identifiers;
    }
    pub fn parse_call_expression(&mut self, function: ExpressionType) -> ExpressionType {
        let cur_tok = self.current_token.clone();
        let expr = ExpressionType::CallExpression {
            function: Box::new(function),
            token: cur_tok,
            arguments: self.parse_call_arguments(),
        };
        return expr;
    }
    pub fn parse_call_arguments(&mut self) -> Vec<ExpressionType> {
        let mut args = vec![];
        if self.peek_token_is(TokenTypes::RPAREN) {
            self.next_token();
            return args;
        }
        self.next_token();
        args.push(self.parse_expression(PRECEDENCES::LOWEST).unwrap());
        while self.peek_token_is(TokenTypes::COMMA) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(PRECEDENCES::LOWEST).unwrap());
        }
        if !self.expect_peek(TokenTypes::RPAREN) {
            panic!();
        }
        return args;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Deref;
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
        println!("{:?}", statement);
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
                StatementType::Expression { expression, .. } => match expression.deref() {
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
                StatementType::Expression { expression, .. } => match expression.deref() {
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
                    StatementType::Expression { expression, .. } => match expression.deref() {
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
                    StatementType::Expression { expression, .. } => match expression.deref() {
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
                    StatementType::Expression { expression, .. } => match expression.deref() {
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
                    StatementType::Expression { expression, .. } => match expression.deref() {
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
            OperatorPrecedenceTest {
                input: "1 + (2 + 3) + 4".to_string(),
                expected: "((1 + (2 + 3)) + 4)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "(5 + 5) * 2".to_string(),
                expected: "((5 + 5) * 2)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "2 / (5 + 5)".to_string(),
                expected: "(2 / (5 + 5))".to_string(),
            },
            OperatorPrecedenceTest {
                input: "-(5 + 5)".to_string(),
                expected: "(-(5 + 5))".to_string(),
            },
            OperatorPrecedenceTest {
                input: "!(true == true)".to_string(),
                expected: "(!(true == true))".to_string(),
            },
            OperatorPrecedenceTest {
                input: "a + add(b * c) + d".to_string(),
                expected: "((a + add((b * c))) + d)".to_string(),
            },
            OperatorPrecedenceTest {
                input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))".to_string(),
                expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))".to_string(),
            },
            OperatorPrecedenceTest {
                input: "add(a + b + c * d / f + g)".to_string(),
                expected: "add((((a + b) + ((c * d) / f)) + g))".to_string(),
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
        println!("{:?}", expr);
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
                return true;
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
                StatementType::Expression { expression, .. } => match expression.deref() {
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

    #[test]
    fn test_if_expression() {
        let input = r#"
        if (x < y) { x }
        "#;
        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        check_parser_errors(parser);
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        match &stmt.r#type {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Expression { expression, .. } => match expression.deref() {
                    ExpressionType::If {
                        condition,
                        consequence,
                        alternative,
                        ..
                    } => {
                        let x_token = Token::new(TokenTypes::IDENT, "x");
                        let y_token = Token::new(TokenTypes::IDENT, "y");
                        let x_expr = ExpressionType::Identifier {
                            identifier: Identifier {
                                token: x_token,
                                value: "x".to_string(),
                            },
                        };
                        let y_expr = ExpressionType::Identifier {
                            identifier: Identifier {
                                token: y_token,
                                value: "y".to_string(),
                            },
                        };
                        assert_eq!(
                            test_infix_expression(
                                *condition.clone(),
                                x_expr,
                                "<".to_string(),
                                y_expr
                            ),
                            true
                        );
                        if let StatementType::Block {
                            statements: cons_statements,
                            ..
                        } = consequence
                        {
                            println!("{:?}", consequence);
                            assert_eq!(cons_statements.len(), 1);
                            if let StatementType::Expression {
                                expression: cons_stm_0_expr,
                                ..
                            } = &*cons_statements[0]
                            {
                                assert_eq!(
                                    test_identifier(
                                        cons_stm_0_expr.deref().clone(),
                                        "x".to_string()
                                    ),
                                    true
                                )
                            } else {
                                panic!();
                            }
                        } else {
                            panic!();
                        }
                        if let Some(_) = alternative {
                            panic!()
                        }
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    #[test]
    fn test_function_literal_parsing() {
        let input = r#"
            fn(x, y) { x + y }
        "#;
        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        check_parser_errors(parser);
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        match stmt.r#type.clone() {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Expression { expression, .. } => match expression.deref() {
                    ExpressionType::FunctionLiteral {
                        parameters, body, ..
                    } => {
                        let x_ident = Identifier {
                            token: Token::new(TokenTypes::IDENT, "x"),
                            value: "x".to_string(),
                        };
                        let y_ident = Identifier {
                            token: Token::new(TokenTypes::IDENT, "y"),
                            value: "y".to_string(),
                        };
                        assert_eq!(parameters.len(), 2);
                        test_literal_expression(
                            ExpressionType::Identifier {
                                identifier: x_ident,
                            },
                            ExpressionType::Identifier {
                                identifier: parameters[0].clone(),
                            },
                        );
                        test_literal_expression(
                            ExpressionType::Identifier {
                                identifier: y_ident,
                            },
                            ExpressionType::Identifier {
                                identifier: parameters[1].clone(),
                            },
                        );
                        match body.deref() {
                            StatementType::Block { statements, .. } => {
                                assert_eq!(statements.len(), 1);
                                let x = &statements[0];
                                match x.deref() {
                                    StatementType::Expression {
                                        expression: expr_last,
                                        ..
                                    } => test_infix_expression(
                                        expr_last.deref().clone(),
                                        ExpressionType::Identifier {
                                            identifier: Identifier {
                                                value: "x".to_string(),
                                                token: Token::new(TokenTypes::IDENT, "x"),
                                            },
                                        },
                                        "+".to_string(),
                                        ExpressionType::Identifier {
                                            identifier: Identifier {
                                                value: "y".to_string(),
                                                token: Token::new(TokenTypes::IDENT, "y"),
                                            },
                                        },
                                    ),
                                    _ => panic!(),
                                }
                            }
                            _ => panic!(),
                        };
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    #[test]
    fn test_call_expression_parsing() {
        let input = r#"
            add(1, 2 * 3, 4 + 5);
        "#;
        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        check_parser_errors(parser);
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        match stmt.r#type.clone() {
            NodeType::Statement { r#type: stm_type } => {
                match stm_type {
                    StatementType::Expression { expression, .. } => {
                        match expression.deref() {
                            ExpressionType::CallExpression {
                                function,
                                arguments,
                                ..
                            } => {
                                // let add_ident = Identifier {
                                //     token: Token::new(TokenTypes::IDENT, "add"),
                                //     value: "add".to_string(),
                                // };
                                test_identifier(function.deref().clone(), "add".to_string());
                                assert_eq!(arguments.len(), 3);
                                test_literal_expression(
                                    arguments[0].clone(),
                                    ExpressionType::Integer {
                                        token: Token::new(TokenTypes::INT, "1"),
                                        value: 1,
                                    },
                                );
                                test_infix_expression(
                                    arguments[1].clone(),
                                    ExpressionType::Integer {
                                        token: Token::new(TokenTypes::INT, "2"),
                                        value: 2,
                                    },
                                    "*".to_string(),
                                    ExpressionType::Integer {
                                        token: Token::new(TokenTypes::INT, "3"),
                                        value: 3,
                                    },
                                );
                                test_infix_expression(
                                    arguments[2].clone(),
                                    ExpressionType::Integer {
                                        token: Token::new(TokenTypes::INT, "4"),
                                        value: 4,
                                    },
                                    "+".to_string(),
                                    ExpressionType::Integer {
                                        token: Token::new(TokenTypes::INT, "5"),
                                        value: 5,
                                    },
                                );
                            }
                            _ => panic!(),
                        }
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }
    #[test]
    fn test_let_statement_expressions() {
        let input = r#"
        let x = 5;
        let y = true;
        let foobar = y;
        "#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let mut program = p.parse_program();
        assert_eq!(program.statements.len(), 3);
        let first = program.statements.remove(0);
        assert_eq!(test_let_statement(&first, "x"), true);
        match first.r#type {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Let { value, .. } => {
                    println!("{:?}", value);
                    match value.deref() {
                        ExpressionType::Integer { .. } => {
                            assert_eq!(
                                test_literal_expression(
                                    value.deref().clone(),
                                    ExpressionType::Integer {
                                        token: Token::new(TokenTypes::INT, "5"),
                                        value: 5,
                                    },
                                ),
                                true
                            );
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
        let second = program.statements.remove(0);
        assert_eq!(test_let_statement(&second, "y"), true);
        match second.r#type {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Let { value, .. } => {
                    println!("{:?}", value);
                    match value.deref() {
                        ExpressionType::Boolean { .. } => {
                            assert_eq!(
                                test_literal_expression(
                                    value.deref().clone(),
                                    ExpressionType::Boolean {
                                        token: Token::new(TokenTypes::TRUE, "true"),
                                        value: true
                                    }
                                ),
                                true
                            );
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
        let third = program.statements.remove(0);
        assert_eq!(test_let_statement(&third, "foobar"), true);
        match third.r#type {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Let { value, .. } => {
                    println!("{:?}", value);
                    match value.deref() {
                        ExpressionType::Identifier { .. } => {
                            assert_eq!(
                                test_literal_expression(
                                    value.deref().clone(),
                                    ExpressionType::Identifier {
                                        identifier: Identifier {
                                            token: Token::new(TokenTypes::IDENT, "y"),
                                            value: "y".to_string()
                                        }
                                    }
                                ),
                                true
                            );
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
}
