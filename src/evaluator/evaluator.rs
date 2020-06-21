use crate::ast::ast::{ExpressionType, Node, NodeType, Program, StatementType};
use crate::lexer::lexer::Lexer;
use crate::object::object::{
    Environment, Object, STATIC_FALSE_OBJECT, STATIC_NULL_OBJECT, STATIC_TRUE_OBJECT,
};
use crate::parser::parser::Parser;
use std::ops::Deref;

pub struct Evaluator {
    pub env: Environment,
}

impl Evaluator {
    pub fn eval_program(&mut self, program: Program) -> Object {
        return self.eval_statements(program.statements);
    }
    pub fn eval_statements(&mut self, statements: Vec<Node>) -> Object {
        let mut result = Object::Null;
        for stmt in &statements {
            result = self.eval_statement(stmt.clone());
            if let Object::Return { value } = result {
                return value.deref().clone();
            } else if let Object::Error { .. } = result {
                return result;
            }
        }
        return result;
    }
    pub fn eval_statement(&mut self, statement: Node) -> Object {
        match statement.r#type {
            NodeType::Statement { r#type } => match r#type {
                StatementType::Expression { expression, .. } => {
                    self.eval_expression(expression.deref().clone())
                }
                StatementType::Block { statements, .. } => {
                    let statements = statements.deref().clone().to_vec();
                    let statements = statements
                        .iter()
                        .map(|x| {
                            let node = Node {
                                r#type: NodeType::Statement {
                                    r#type: x.deref().clone(),
                                },
                            };
                            return node;
                        })
                        .collect::<Vec<_>>();
                    for stmt in &statements.clone() {
                        let result = self.eval_statement(stmt.clone());
                        if let Object::Return { .. } = result {
                            return result;
                        } else if let Object::Error { .. } = result {
                            return result;
                        }
                    }
                    self.eval_statements(statements)
                }
                StatementType::Return { return_value, .. } => {
                    let val = self.eval_expression(return_value.deref().clone());
                    if self.is_error(&val) {
                        return val;
                    }
                    return Object::Return {
                        value: Box::from(val),
                    };
                }
                StatementType::Let { value, name, .. } => {
                    let val = self.eval_expression(value.deref().clone());
                    if self.is_error(&val) {
                        return val;
                    }
                    return self.env.set(name.value, val.clone());
                }
                _ => Object::Null,
            },
            _ => Object::Null, // NodeType::Expression { r#type } => self.eval_expression(r#type),
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
                if self.is_error(&obj_right) {
                    return obj_right;
                }
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
                if self.is_error(&obj_left) {
                    return obj_left;
                } else if self.is_error(&obj_right) {
                    return obj_right;
                }
                return self.eval_infix_expression(operator, obj_left, obj_right);
            }
            ExpressionType::If { .. } => {
                return self.eval_if_expression(expression);
            }
            ExpressionType::Identifier { identifier } => {
                if let Some(x) = self.env.get(identifier.value.clone()) {
                    return x.clone();
                } else {
                    return Object::new_error(format!(
                        "identifier not found: {}",
                        identifier.value
                    ));
                }
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
            _ => {
                return Object::new_error(format!(
                    "unknown operator {}{}",
                    operator,
                    right.type_string()
                ))
            }
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
            _ => return Object::new_error(format!("unknown operator: -{}", right.type_string())),
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
                _ => {
                    return Object::new_error(format!(
                        "unknown operator: {} {} {}",
                        left.type_string(),
                        operator,
                        right.type_string()
                    ))
                }
            };
        } else if left.type_string() != right.type_string() {
            return Object::new_error(format!(
                "type mismatch: {} {} {}",
                left.type_string(),
                operator,
                right.type_string()
            ));
        } else {
            return Object::new_error(format!(
                "unknown operator: {} {} {}",
                left.type_string(),
                operator,
                right.type_string()
            ));
        }
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
            _ => {
                return Object::new_error(format!(
                    "unknown operator: {} {} {}",
                    left.type_string(),
                    operator,
                    right.type_string()
                ))
            }
        };
    }
    pub fn eval_if_expression(&mut self, ie: ExpressionType) -> Object {
        let cond;
        let cons;
        let alt;
        match ie {
            ExpressionType::If {
                ref consequence,
                ref alternative,
                ref condition,
                ..
            } => {
                cons = Node {
                    r#type: NodeType::Statement {
                        r#type: consequence.clone(),
                    },
                };
                alt = alternative.clone();
                cond = self.eval_expression(condition.deref().clone());
            }
            _ => panic!(),
        };
        if self.is_error(&cond) {
            return cond;
        }
        if self.is_truthy(cond) {
            return self.eval_statement(cons);
        } else if let Some(a) = alt {
            return self.eval_statement(Node {
                r#type: NodeType::Statement { r#type: a.clone() },
            });
        } else {
            return Object::Null;
        }
    }
    pub fn is_truthy(&mut self, obj: Object) -> bool {
        match obj {
            Object::Null => false,
            Object::Boolean { value } => value,
            _ => true,
        }
    }
    pub fn is_error(&mut self, obj: &Object) -> bool {
        return obj.type_string() == "ERROR";
    }
}

#[allow(dead_code)]
mod tests {
    use super::*;

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let env = Environment::new();
        let mut evaluator = Evaluator { env };
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
    #[test]
    fn test_if_else_expressions() {
        let tests = [
            ("if (true) { 10 }", 10),
            ("if (1) { 10 }", 10),
            ("if (1 < 2) { 10 }", 10),
            ("if (1 > 2) { 10 } else { 20 }", 20),
            ("if (1 < 2) { 10 } else { 20 }", 10),
        ];
        let null_tests = [
            ("if (1 > 2) { 10 }", STATIC_NULL_OBJECT),
            ("if (false) { 10 }", STATIC_NULL_OBJECT),
        ];
        for t in &tests {
            let eval = test_eval(t.0);
            test_integer_object(eval, t.1);
        }
        for n in &null_tests {
            let eval = test_eval(n.0);
            test_null_object(eval, n.1.clone());
        }
    }
    fn test_null_object(obj: Object, null: Object) {
        assert_eq!(obj, null);
    }
    #[test]
    fn test_return_statements() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) { return 10 ;} return 1; }", 10),
        ];
        for t in &tests {
            let eval = test_eval(t.0);
            test_integer_object(eval, t.1);
        }
    }
    #[test]
    fn test_error_handling() {
        let tests = [
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                    return true + false;
                    }
                    return 1;
                    }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];
        for t in &tests {
            let eval = test_eval(t.0);
            match eval {
                Object::Error { message } => {
                    assert_eq!(message, t.1);
                }
                _ => panic!(),
            }
        }
    }
    #[test]
    fn test_let_statements() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];
        for t in &tests {
            test_integer_object(test_eval(t.0), t.1);
        }
    }
}
