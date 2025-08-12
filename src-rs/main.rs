use std::{collections::HashMap, env, fmt, fs, rc::Rc};
// save unchanging in map, only leave variables in expression!

mod runtime;
mod parse;

use crate::{
    runtime::{Expression, Id, ExpressionCache},
    parse::{SyntaxTree, tokenize, build_syntax_tree}
};


fn main() {
    let string: Vec<String> = 
"match f a b
  cons a b to lambda a b match b
    a b to d
  nil to just six"
        .lines()
        .map(str::to_string)
        .collect();
    dbg!(build_syntax_tree(tokenize(string).unwrap()));
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

    let h = ExpressionCache { expressions: Vec::new() };
    initial.simplify(&h);

    dbg!(initial);
}
