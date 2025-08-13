use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Type {
    Type(u32),
    Function(Box<Type>, Box<Type>),
}
