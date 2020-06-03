// pub static ILLEGAL: &str = "ILLEGAL";
// pub static EOF: &str = "EOF";
// // Identifiers + literals
// pub static IDENT: &str = "IDENT"; // add, foobar, x, y...
// pub static INT: &str = "INT"; // 1, 23123, 1242141
// // Operators
// pub static ASSIGN: &str = "=";
// pub static PLUS: &str = "+";
// // Delimiters
// pub static COMMA: &str = ",";
// pub static SEMICOLON: &str = ";";

// pub static LPAREN: &str = "(";
// pub static RPAREN: &str = ")";
// pub static LBRACE: &str = "{";
// pub static RBRACE: &str = "}";

// // Keywords
// pub static FUNCTION: &str = "FUNCTION";
// pub static LET: &str = "LET";
// // pub static : &str = "";
use std::collections::HashMap;
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenTypes {
  ILLEGAL,
  EOF,
  IDENT,
  INT,
  // Operators
  ASSIGN, 
  PLUS,
  MINUS,
  BANG,
  ASTERISK,
  SLASH,
  LT,
  GT,

  COMMA,
  SEMICOLON, 
  LPAREN, 
  RPAREN,
  LBRACE,
  RBRACE,
  
  // Keywords
  FUNCTION,
  LET,
  TRUE,
  FALSE,
  IF,
  ELSE,
  RETURN,
  EQ,
  NOT_EQ
}

pub fn keywords<'a>() -> std::collections::HashMap<&'a str, TokenTypes> {
  let mut map = HashMap::new();
  map.insert("fn", TokenTypes::FUNCTION);
  map.insert("let", TokenTypes::LET);
  map.insert("true", TokenTypes::TRUE);
  map.insert("false", TokenTypes::FALSE);
  map.insert("if", TokenTypes::IF);
  map.insert("else", TokenTypes::ELSE);
  map.insert("return", TokenTypes::RETURN);
  map
}

pub fn lookup_ident(ident: String) -> TokenTypes {
  let kw_map = keywords();
  let ident = ident.as_str();
  if kw_map.contains_key(ident) {
    return kw_map[ident];
  } else {
    return TokenTypes::IDENT;
  }
}