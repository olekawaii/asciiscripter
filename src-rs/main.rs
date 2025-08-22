use std::{collections::HashMap, env, fmt, fs, rc::Rc};
// save unchanging in map, only leave variables in expression!

mod parse;
mod runtime;
mod r#type;

use crate::{
    parse::{SyntaxTree, build_syntax_tree, extract_signiture, parse_data, tokenize},
    runtime::{Expression, ExpressionCache, Id},
};

fn main() {
    let string: Vec<String> = "match f a b
  cons a b to lambda a b match b
    a b to d
  nil to just six"
        .lines()
        .map(str::to_string)
        .collect();
    // dbg!(build_syntax_tree(tokenize(string).unwrap()));
    let string: Vec<String> = "data bool contains\n  true fn int int int\n  false"
        .lines()
        .map(str::to_string)
        .collect();
    let mut tokens = tokenize(string).unwrap();
    let mut map: HashMap<String, u32> = HashMap::from([(String::from("int"), 1)]);
    extract_signiture(&mut tokens);
    dbg!(parse_data(tokens, &map));
}

fn test() {
    let mut initial = Expression::Tree {
        root: Id::LambdaArg(5),
        arguments: Vec::from([Expression::Tree {
            root: Id::DataConstructor(7),
            arguments: Vec::new(),
        }]),
    };
    initial.substitute(
        Id::LambdaArg(5),
        Rc::new(Expression::Tree {
            root: Id::DataConstructor(7),
            arguments: Vec::new(),
        }),
    );
    let h = ExpressionCache {
        expressions: Vec::new(),
    };
    initial.simplify(&h);
    dbg!(initial);
}
