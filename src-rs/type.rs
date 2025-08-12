use std::{collections::HashMap}

enum Type {
    Type(u32),
    Function(Box<Type>, Box<Type>)
}

fn parse_type<T: Iterator<Item = String>>(
    mut strings: T,
    table: &HashMap<String, u32>,
) -> Option<(Type, T)> {
    match strings.next()?.as_str() {
        "fn" => {
            let (arg1, leftover1) = parse_type(strings, table)?;
            let (arg2, leftover2) = parse_type(leftover1, table)?;
            Some((Type::Function(Box::new(arg1), Box::new(arg2)), leftover2))
        },
        word => {
            let index = table.get(word)?.clone();
            Some((Type::Type(index), strings))
        }
    }
}

