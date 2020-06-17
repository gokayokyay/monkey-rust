use crate::ast::ast::{ExpressionType, Node, NodeType, Program, StatementType};
use crate::lexer::lexer::Lexer;
use crate::object::object::{Object, STATIC_FALSE_OBJECT, STATIC_NULL_OBJECT, STATIC_TRUE_OBJECT};
use crate::parser::parser::Parser;
use std::ops::Deref;

pub struct Evaluator {}

impl Evaluator {
    pub fn eval_program(&mut self, program: Program) -> Object {
        return self.eval_statements(program.statements);
    }
    pub fn eval_statements(&mut self, statements: Vec<Node>) -> Object {
        let mut result = Object::Null;
        for stmt in statements {
            result = self.eval_statement(stmt);
        }
        return result;
    }
    pub fn eval_statement(&mut self, statement: Node) -> Object {
        match statement.r#type {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Expression { expression, .. } => {
                    self.eval_expression(expression.deref().clone())
                }
                _ => Object::Null,
            },
            _ => Object::Null,
        }
    }
    pub fn eval_expression(&mut self, expression: ExpressionType) -> Object {
        match expression {
            ExpressionType::Integer { value, .. } => {
                return Object::Integer { value: value };
            }
            ExpressionType::Boolean { value, .. } => match value {
                true => STATIC_TRUE_OBJECT,
                false => STATIC_FALSE_OBJECT,
            },
            ExpressionType::Prefix {
                operator, right, ..
            } => {
                let obj_right = self.eval_expression(*right);
                return self.eval_prefix_expression(operator, obj_right);
            }
            ExpressionType::Infix {
                left,
                right,
                operator,
                ..
            } => {
                let obj_left = self.eval_expression(*left);
                let obj_right = self.eval_expression(*right);
                return self.eval_infix_expression(operator, obj_left, obj_right);
            }
            _ => STATIC_NULL_OBJECT,
        }
    }
    pub fn eval_prefix_expression(&mut self, operator: String, right: Object) -> Object {
        let operator = operator;
        match operator.as_str() {
            "!" => {
                return self.eval_bang_operator_expression(right);
            }
            "-" => {
                return self.eval_minus_prefix_operator_expression(right);
            }
            _ => STATIC_NULL_OBJECT,
        }
    }
    pub fn eval_bang_operator_expression(&mut self, right: Object) -> Object {
        match right {
            Object::Boolean { value } => match value {
                true => return STATIC_FALSE_OBJECT,
                false => return STATIC_TRUE_OBJECT,
            },
            Object::Null => return STATIC_TRUE_OBJECT,
            _ => return STATIC_FALSE_OBJECT,
        }
    }
    pub fn eval_minus_prefix_operator_expression(&mut self, right: Object) -> Object {
        match right {
            Object::Integer { value } => {
                return Object::Integer { value: -value };
            }
            _ => return STATIC_NULL_OBJECT,
        }
    }
    pub fn eval_infix_expression(
        &mut self,
        operator: String,
        left: Object,
        right: Object,
    ) -> Object {
        let mut is_integer = (false, false);
        let mut is_bool = (false, false);
        match left {
            Object::Integer { .. } => is_integer = (true, is_integer.1),
            Object::Boolean { .. } => is_bool = (true, is_bool.1),
            _ => return STATIC_NULL_OBJECT,
        };
        match right {
            Object::Integer { .. } => is_integer = (is_integer.0, true),
            Object::Boolean { .. } => is_bool = (is_bool.0, true),
            _ => return STATIC_NULL_OBJECT,
        };
        if is_integer == (true, true) {
            return self.eval_integer_infix_expression(operator, left, right);
        } else if is_bool == (true, true) {
            match operator.as_ref() {
                "==" => {
                    return Object::Boolean {
                        value: left == right,
                    }
                }
                "!=" => {
                    return Object::Boolean {
                        value: left != right,
                    }
                }
                _ => STATIC_NULL_OBJECT,
            };
        }
        return STATIC_NULL_OBJECT;
    }
    pub fn eval_integer_infix_expression(
        &mut self,
        operator: String,
        left: Object,
        right: Object,
    ) -> Object {
        let left_val = match left {
            Object::Integer { value } => value,
            _ => return STATIC_NULL_OBJECT,
        };
        let right_val = match right {
            Object::Integer { value } => value,
            _ => return STATIC_NULL_OBJECT,
        };
        match operator.as_ref() {
            "+" => {
                return Object::Integer {
                    value: left_val + right_val,
                };
            }
            "-" => {
                return Object::Integer {
                    value: left_val - right_val,
                };
            }
            "*" => {
                return Object::Integer {
                    value: left_val * right_val,
                };
            }
            "/" => {
                return Object::Integer {
                    value: left_val / right_val,
                };
            }
            "<" => {
                if left_val < right_val {
                    return STATIC_TRUE_OBJECT;
                } else {
                    return STATIC_FALSE_OBJECT;
                }
            }
            ">" => {
                if left_val > right_val {
                    return STATIC_TRUE_OBJECT;
                } else {
                    return STATIC_FALSE_OBJECT;
                }
            }
            "==" => {
                if left_val == right_val {
                    return STATIC_TRUE_OBJECT;
                } else {
                    return STATIC_FALSE_OBJECT;
                }
            }
            "!=" => {
                if left_val != right_val {
                    return STATIC_TRUE_OBJECT;
                } else {
                    return STATIC_FALSE_OBJECT;
                }
            }
            _ => return STATIC_NULL_OBJECT,
        };
    }
}

mod tests {
    use super::*;

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut evaluator = Evaluator {};
        return evaluator.eval_program(program);
    }
    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for test in tests.iter() {
            let eval = test_eval(test.0);
            test_integer_object(eval, test.1);
        }
    }
    fn test_integer_object(obj: Object, expected: i64) {
        match obj {
            Object::Integer { value } => {
                assert_eq!(value, expected);
            }
            _ => panic!(),
        }
    }
    #[test]
    fn test_eval_boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for t in &tests {
            let eval = test_eval(t.0);
            test_boolean_object(eval, t.1);
        }
    }
    fn test_boolean_object(obj: Object, expected: bool) {
        match obj {
            Object::Boolean { value } => {
                assert_eq!(value, expected);
            }
            _ => panic!(),
        }
    }
    #[test]
    fn test_bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!true", true),
        ];
        for t in &tests {
            let eval = test_eval(t.0);
            test_boolean_object(eval, t.1);
        }
    }
}