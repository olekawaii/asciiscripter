use std::{collections::HashMap, fmt, rc::Rc};
use std::sync::Mutex;

// instead of Rc<Expression> use Rc<Mutex<Expression>> and first one to use it modifies it

#[derive(Debug, Clone)]
pub enum Id {
    Thunk(Rc<Mutex<Expression>>),
    LambdaArg(u32),
    Variable(usize),
    DataConstructor(u32),
}

#[derive(Debug, Clone)]
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

pub static mut counter: usize = 0;

impl Expression {
    // TODO make simplify non-recursive so it doesn't blow the stack
    // this means adding an attempt_to_simplify finction and calling it in a loop
    // for match expressions only simplify what's in the pattern, with increasing depth
    // for trees if there are arguments, just add them to the root arguments, otherwise
    // substitute them in the root

    // rewrite expression until it starts with either a lambda or a data constructor
    pub fn simplify(&mut self, definitions: &ExpressionCache) {
        unsafe {
            counter += 1;
        }
        match self {
            Expression::Tree { root, arguments } => {
                let mut output = match root {
                    Id::DataConstructor(_) => return (),
                    Id::Thunk(exp) => {
                        let mut inside = (*exp).lock().unwrap();
                        (*inside).simplify(definitions);
                        inside.clone()
                    }
                    Id::Variable(index) => definitions.get(*index),
                    Id::LambdaArg(_) => unreachable!(),
                };
                for i in arguments {
                    output.simplify(definitions);
                    match output {
                        Expression::Lambda { id, mut body } => {
                            body.substitute(id.clone(), Rc::new(Mutex::new(i.clone())));
                            output = *body;
                        }
                        Expression::Tree {
                            mut arguments,
                            root,
                        } => {
                            arguments.push(i.clone());
                            output = Expression::Tree { root, arguments }
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
                            Id::DataConstructor(root) => match branches.get(&root) {
                                None => {
                                    dbg!(&root); 
                                    dbg!(&pattern);
                                    dbg!(&branches);
                                    panic!("unmatched pattern");
                                },
                                Some(x) => x.clone(),
                            }
                            _ => todo!(),
                        };
                        *self = output;
                        for (id, expression) in vec.into_iter().zip(arguments.into_iter()) {
                            self.substitute(id, {
                                if let Expression::Tree {root: Id::Thunk(ref x), arguments: ref a} = expression {
                                    if a.len() == 0 {
                                        //dbg!("skipped a clone!");
                                        Rc::clone(&x)
                                    } else {
                                        Rc::new(Mutex::new(expression))
                                    }
                                } else {
                                    Rc::new(Mutex::new(expression))
                                }
                            })
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
    pub fn substitute(&mut self, key: u32, new_expression: Rc<Mutex<Expression>>) {
        match self {
            Expression::Lambda { body, .. } => body.substitute(key, new_expression),
            Expression::Tree { root, arguments } => {
                for mut i in arguments.iter_mut() {
                    i.substitute(key.clone(), Rc::clone(&new_expression));
                }
                //if *root == key {
                //    *root = Id::Thunk(new_expression);
                //};
                if matches!(root, Id::LambdaArg(x) if *x == key) {
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
