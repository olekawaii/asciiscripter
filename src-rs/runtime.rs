use std::{collections::HashMap, fmt, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Id {
    Thunk(Rc<Expression>),
    LambdaArg(u32),
    Variable(usize),
    DataConstructor(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Tree {
        root: Id,
        arguments: Vec<Expression>,
    },
    Match {
        pattern: Box<Expression>,
        branches: HashMap<u32, (Vec<u32>, Expression)>,
    },
    Lambda {
        id: u32,
        body: Box<Expression>,
    },
}

impl Expression {
    // rewrite expression until it starts with either a lambda or a data constructor
    pub fn simplify(&mut self, definitions: &ExpressionCache) {
        match self {
            Expression::Tree { root, arguments } => {
                let mut output = match root {
                    Id::DataConstructor(_) => return (),
                    Id::Thunk(exp) => (**exp).clone(),
                    Id::Variable(index) => definitions.get(*index),
                    Id::LambdaArg(_) => unreachable!()
                };
                for i in arguments {
                    output.simplify(definitions);
                    match output {
                        Expression::Lambda { id, mut body } => {
                            body.substitute(Id::LambdaArg(id.clone()), Rc::new(i.clone()));
                            output = *body;
                        }
                        Expression::Tree { mut arguments, root } => {
                            arguments.push(i.clone());
                            output = Expression::Tree {
                                root,
                                arguments,
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                output.simplify(definitions);
                *self = output;
            }
            Expression::Match { pattern, branches } => {
                pattern.simplify(definitions);
                match *(*pattern).clone() {
                    Expression::Tree { root, arguments } => {
                        let (vec, mut output) = match root {
                            Id::DataConstructor(root) => branches.get(&root).unwrap().clone(),
                            _ => todo!(),
                        };
                        *self = output;
                        for (id, expression) in vec.into_iter().zip(arguments.into_iter()) {
                            self.substitute(Id::LambdaArg(id), Rc::new(expression));
                        }
                        self.simplify(definitions);
                    }
                    _ => todo!(),
                }
            }
            _ => (),
        }
    }

    // substitute every instance of an id with an expression
    pub fn substitute(&mut self, key: Id, new_expression: Rc<Expression>) {
        match self {
            Expression::Lambda { body, .. } => body.substitute(key, new_expression),
            Expression::Tree { root, arguments } => {
                for mut i in arguments.iter_mut() {
                    i.substitute(key.clone(), Rc::clone(&new_expression));
                }
                if *root == key {
                    *root = Id::Thunk(new_expression);
                };
            }
            Expression::Match { pattern, branches } => {
                for (_, (_, i)) in branches.iter_mut() {
                    i.substitute(key.clone(), Rc::clone(&new_expression));
                }
                pattern.substitute(key, Rc::clone(&new_expression));
            }
        }
    }

    // evaluate an expression strictly, leavivg only a tree of data constructors.
    // can't be called on functions
    pub fn evaluate_strictly(&mut self, definitions: &ExpressionCache) {
        self.simplify(definitions);
        match self {
            Expression::Tree {
                root: Id::DataConstructor(_),
                arguments,
            } => {
                for i in arguments.iter_mut() {
                    i.evaluate_strictly(definitions);
                }
            }
            Expression::Lambda { .. } => panic!("attempted to evaluate a function"),
            _ => unreachable!(),
        }
    }
}

pub struct ExpressionCache {
    pub expressions: Vec<Expression>,
}

impl ExpressionCache {
    fn get(&self, index: usize) -> Expression {
        self.expressions[index].clone()
    }
}
