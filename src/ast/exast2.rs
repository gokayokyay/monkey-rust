use crate::token::token;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node + StatementClone {
    fn statement_node(&mut self);
}

pub trait StatementClone {
  fn clone_box(&self) -> Box<dyn Statement>;
}

pub trait Expression: Node + Clone {
    fn expression_node(&mut self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

#[derive(Clone)]
pub struct LetStatement<T: Expression> {
    token: token::Token,
    name: Identifier,
    value: T,
}
#[derive(Clone)]
pub struct Identifier {
    token: token::Token,
    value: String,
}

impl<T> StatementClone for T
where T: 'static + Statement + Clone {
  fn clone_box(&self) -> Box<dyn Statement> {
    Box::new(self.clone())
  }
}

impl Clone for Box<dyn Statement> {
  fn clone(&self) -> Self {
    self.clone_box()
  }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}

impl<T: Expression> Node for LetStatement<T> {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl<T: 'static + Expression> Statement for LetStatement<T> {
    fn statement_node(&mut self) {}
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Expression for Identifier {
    fn expression_node(&mut self) {}
}
