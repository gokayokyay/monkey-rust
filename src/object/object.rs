use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer { value: i64 },
    Boolean { value: bool },
    Return { value: Box<Object> },
    Error { message: String },
    Null,
}

impl Object {
    pub fn inspect(&mut self) -> String {
        match self {
            Self::Integer { value } => value.to_string(),
            Self::Boolean { value } => value.to_string(),
            Self::Return { value } => (*value.inspect()).to_string(),
            Self::Error { message } => message.to_string(),
            Self::Null => "null".to_string(),
        }
    }
    pub fn type_string(&self) -> &str {
        match self {
            Self::Integer { .. } => "INTEGER",
            Self::Boolean { .. } => "BOOLEAN",
            Self::Return { .. } => "RETURN_VALUE",
            Self::Error { .. } => "ERROR",
            Self::Null => "NULL",
        }
    }
    pub fn new_error(format: String) -> Self {
        return Self::Error { message: format };
    }
}

pub const STATIC_TRUE_OBJECT: Object = Object::Boolean { value: true };
pub const STATIC_FALSE_OBJECT: Object = Object::Boolean { value: false };
pub const STATIC_NULL_OBJECT: Object = Object::Null;

#[derive(Clone, Debug)]
pub struct Environment {
    pub store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        let s = Environment {
            store: HashMap::new(),
        };
        return s;
    }
    pub fn get(&mut self, name: String) -> Option<&Object> {
        let o = self.store.get(&name);
        return o;
    }
    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        return value;
    }
}
