use crate::token::token_types::TokenTypes;
use crate::token::token_types::lookup_ident;
use crate::token::token::Token;
use crate::lexer::utils;

#[derive(Debug)]
pub struct Lexer {
  input: String,
  position: i64,
  read_position: i64,
  ch: char
}

impl Lexer {
  pub fn new(input: &str) -> Lexer {
    let mut lexer = Lexer {
      input: String::from(input),
      position: 0,
      read_position: 0,
      ch: '\0'
    };
    lexer.read_char();
    lexer
  }
  pub fn read_char(&mut self) {
    if self.read_position >= (self.input.chars().count() as i64) {
      self.ch = '\0';
    } else {
      self.ch = self.input.as_bytes()[self.read_position as usize] as char;
    }
    self.position = self.read_position;
    self.read_position += 1;
  }
  pub fn next_token(&mut self) -> Token {
    self.skip_whitespace();
    let mut is_symbol = true;
    let tok = match self.ch as char {
      '=' => Token { token_type: TokenTypes::ASSIGN, literal: self.ch.to_string() },
      ';' => Token { token_type: TokenTypes::SEMICOLON, literal: self.ch.to_string() },
      '(' => Token { token_type: TokenTypes::LPAREN, literal: self.ch.to_string() },
      ')' => Token { token_type: TokenTypes::RPAREN, literal: self.ch.to_string() },
      ',' => Token { token_type: TokenTypes::COMMA, literal: self.ch.to_string() },
      '+' => Token { token_type: TokenTypes::PLUS, literal: self.ch.to_string() },
      '-' => Token { token_type: TokenTypes::MINUS, literal: self.ch.to_string() },
      '!' => Token { token_type: TokenTypes::BANG, literal: self.ch.to_string() },
      '*' => Token { token_type: TokenTypes::ASTERISK, literal: self.ch.to_string() },
      '/' => Token { token_type: TokenTypes::SLASH, literal: self.ch.to_string() },
      '<' => Token { token_type: TokenTypes::LT, literal: self.ch.to_string() },
      '>' => Token { token_type: TokenTypes::GT, literal: self.ch.to_string() },
      '{' => Token { token_type: TokenTypes::LBRACE, literal: self.ch.to_string() },
      '}' => Token { token_type: TokenTypes::RBRACE, literal: self.ch.to_string() },
      '\0' => Token { token_type: TokenTypes::EOF, literal: String::from("") },
      _ => {
        is_symbol = false;
        if utils::is_letter(&self.ch) {
          let id = self.read_identifier();
          Token { token_type: lookup_ident(id.clone()), literal: id }
        } else if utils::is_digit(&self.ch) {
          Token { token_type: TokenTypes::INT, literal: self.read_number() }
        } else {
          Token { token_type: TokenTypes::ILLEGAL, literal: "".to_string() }
        }
      }
    };
    if is_symbol {
      self.read_char();
    }
    tok
  }
  fn read_number(&mut self) -> String {
    let pos = self.position;
    while utils::is_digit(&self.ch) {
      self.read_char();
    }
    (self.input[(pos as usize)..(self.position as usize)]).to_string()
  }
  fn read_identifier(&mut self) -> String {
    let pos = self.position;
    while utils::is_letter(&self.ch) {
      self.read_char();
    }
    (self.input[(pos as usize)..(self.position as usize)]).to_string()
  }
  fn skip_whitespace(&mut self) {
    let space_array = [' ', '\t', '\n', '\r'];
    while space_array.contains(&self.ch) {
      self.read_char()
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn test_next_token() {
    let input = r#"
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
      x + y;
    };
    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;
    "#;
    let types: [(TokenTypes, &str); 49] = [
      (TokenTypes::LET, "let"),
      (TokenTypes::IDENT, "five"),
      (TokenTypes::ASSIGN, "="),
      (TokenTypes::INT, "5"),
      (TokenTypes::SEMICOLON, ";"),
      (TokenTypes::LET, "let"),
      (TokenTypes::IDENT, "ten"),
      (TokenTypes::ASSIGN, "="),
      (TokenTypes::INT, "10"),
      (TokenTypes::SEMICOLON, ";"),
      (TokenTypes::LET, "let"),
      (TokenTypes::IDENT, "add"),
      (TokenTypes::ASSIGN, "="),
      (TokenTypes::FUNCTION, "fn"),
      (TokenTypes::LPAREN, "("),
      (TokenTypes::IDENT, "x"),
      (TokenTypes::COMMA, ","),
      (TokenTypes::IDENT, "y"),
      (TokenTypes::RPAREN, ")"),
      (TokenTypes::LBRACE, "{"),
      (TokenTypes::IDENT, "x"),
      (TokenTypes::PLUS, "+"),
      (TokenTypes::IDENT, "y"),
      (TokenTypes::SEMICOLON, ";"),
      (TokenTypes::RBRACE, "}"),
      (TokenTypes::SEMICOLON, ";"),
      (TokenTypes::LET, "let"),
      (TokenTypes::IDENT, "result"),
      (TokenTypes::ASSIGN, "="),
      (TokenTypes::IDENT, "add"),
      (TokenTypes::LPAREN, "("),
      (TokenTypes::IDENT, "five"),
      (TokenTypes::COMMA, ","),
      (TokenTypes::IDENT, "ten"),
      (TokenTypes::RPAREN, ")"),
      (TokenTypes::SEMICOLON, ";"),
      (TokenTypes::BANG, "!"),
      (TokenTypes::MINUS, "-"),
      (TokenTypes::SLASH, "/"),
      (TokenTypes::ASTERISK, "*"),
      (TokenTypes::INT, "5"),
      (TokenTypes::SEMICOLON, ";"),
      (TokenTypes::INT, "5"),
      (TokenTypes::LT, "<"),
      (TokenTypes::INT, "10"),
      (TokenTypes::GT, ">"),
      (TokenTypes::INT, "5"),
      (TokenTypes::SEMICOLON, ";"),
      (TokenTypes::EOF, "\0"),
    ];
    let mut lex = Lexer::new(input);
    for tokens in types.iter() {
      let tok = lex.next_token();
      assert_eq!(tok.token_type, tokens.0);
    }
  }
}