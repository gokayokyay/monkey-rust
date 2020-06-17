#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer { value: i64 },
    Boolean { value: bool },
    Null,
}

impl Object {
    pub fn inspect(&mut self) -> String {
        match self {
            Self::Integer { value } => value.to_string(),
            Self::Boolean { value } => value.to_string(),
            Self::Null => "null".to_string(),
        }
    }
    pub fn type_string(&self) -> &str {
        match self {
            Self::Integer { .. } => "INTEGER",
            Self::Boolean { .. } => "BOOLEAN",
            Self::Null => "NULL",
        }
    }
}

pub const STATIC_TRUE_OBJECT: Object = Object::Boolean { value: true };
pub const STATIC_FALSE_OBJECT: Object = Object::Boolean { value: false };
pub const STATIC_NULL_OBJECT: Object = Object::Null;
