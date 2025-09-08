use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Type(u32),
    Function(Box<Type>, Box<Type>),
}

impl Type {
    pub fn final_type(&self) -> u32 {
        match self {
            Self::Type(x) => *x,
            Self::Function(_, b) => b.final_type(),
        }
    }
    
    pub fn is_possible(&self, test: &Self) -> bool {
        *self == *test || match self {
            Self::Type(_) => false,
            Self::Function(_, output) => output.is_possible(test)
        }
    }

    pub fn apply_type(self, arg: Self) -> Option<Self> {
        match self {
            Self::Type(u32) => None,
            Self::Function(x, y) => {
                if arg == *x { Some(*y) } else { None }
            }
        }
    }
    pub fn arg_types(self) -> Vec<Type> {
        let mut args = Vec::new();
        let mut current_type = self;
        loop {
            match current_type {
                Type::Function(a,b) => {
                    args.push(*a);
                    current_type = *b;
                },
                Type::Type(_) => return args
            }
        }
    }
}

