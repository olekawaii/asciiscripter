// thorn - a pure lazy functional programming language
// Copyright (C) 2025  Oleksiy Buell <olekawaii@proton.me>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use crate::error::{Error, ErrorType, Mark};
use std::sync::{Arc, Mutex};
use std::thread;
use std::{collections::HashMap, fmt};

#[derive(Debug)]
enum RuntimeError {
    EvaluatedUndefined,
    // EvaluatedBottom,
}

impl ErrorType for RuntimeError {
    fn gist(&self) -> &'static str {
        match self {
            Self::EvaluatedUndefined => "entered undefined code",
            // Self::EvaluatedBottom => "evaluated a bottom _|_",
        }
    }

    fn phase(&self) -> &'static str {
        "runtime"
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EvaluatedUndefined => write!(f, "attempted to evaluate an undefined expression"),
            // Self::EvaluatedBottom => write!(f, "attempted to evaluate a bottom expression"),
        }
    }
}

fn build_thunk(mut input: Expression) -> Arc<Mutex<Expression>> {
    match &mut input {
        Expression::Tree {root: Id::Thunk(x), arguments} if arguments.len() == 0 => {
            std::mem::take(x)
        }
        _ => {
            optimize_expression(&mut input);
            Arc::new(Mutex::new(input))
        }
    }
}

pub fn optimize_expression(input: &mut Expression) {
    if matches!(&input, Expression::Tree { arguments, .. } if arguments.len() == 0) {
        //matches!(&input, Expression::Lambda {..}) {
        return ();
    }
    match input {
        Expression::Tree { root, arguments } => {
            arguments.iter_mut().for_each(optimize_expression);
            if !matches!(root, Id::DataConstructor(_)) {
                *input = Expression::Tree {
                    root: Id::Thunk(Arc::new(Mutex::new(std::mem::take(input)))),
                    arguments: Vec::new(),
                }
            }
        }
        // // no point in optimizing anything since the simplified output will be optimized later
        // Expression::Match {branches, ..} => {
        //     for (pattern, expression) in branches.iter_mut() {
        //         if let Pattern::Dropped = pattern {
        //             optimize_expression(expression)
        //         }
        //     }
        // },
        Expression::Lambda {
            pattern: Pattern::Dropped,
            body,
        } => optimize_expression(body),
        Expression::Undefined { .. } => (),
        _ => (),
    }
}

#[derive(Debug, Clone)]
pub enum Id {
    Thunk(Arc<Mutex<Expression>>),
    LambdaArg(u32),
    Variable(usize), // only used during parsing
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
        branches: Vec<(Pattern, Expression)>,
    },
    Lambda {
        pattern: Pattern,
        body: Box<Expression>,
    },
    Undefined {
        mark: Mark,
    },
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Dropped,
    Captured(u32),
    DataConstructor(u32, Vec<Pattern>),
}

fn matches_expression(pattern: &Pattern, matched: &mut Expression) -> bool {
    match pattern {
        Pattern::Dropped => true,
        Pattern::Captured(_) => true,
        Pattern::DataConstructor(data_constructor, patterns) => {
            matched.simplify_owned();
            match matched {
                Expression::Tree {
                    root: Id::DataConstructor(id),
                    arguments,
                } => {
                    if id == data_constructor {
                        for (pattern, arg) in patterns.iter().zip(arguments.iter_mut()) {
                            if !matches_expression(pattern, arg) {
                                return false;
                            }
                        }
                        true
                    } else {
                        false
                    }
                }
                _ => unreachable!(),
            }
        }
    }
}

fn match_on_expression(pattern: &Pattern, matched: Expression) -> HashMap<u32, Expression> {
    let mut output = HashMap::new();
    match_on_expression_helper(&mut output, pattern, matched);
    output
}

fn match_on_expression_helper(
    output: &mut HashMap<u32, Expression>,
    pattern: &Pattern,
    mut matched: Expression,
) {
    match pattern {
        Pattern::Dropped => (),
        Pattern::Captured(id) => {
            output.insert(*id, matched);
        }
        Pattern::DataConstructor(data_constructor, patterns) => {
            matched.simplify_owned();
            match matched {
                Expression::Tree {
                    root: Id::DataConstructor(id),
                    arguments,
                } => {
                    if id == *data_constructor {
                        for (pattern, arg) in patterns.iter().zip(arguments.into_iter()) {
                            match_on_expression_helper(output, pattern, arg)
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
    }
}

impl Default for Expression {
    fn default() -> Self {
        Self::Tree {
            root: Id::LambdaArg(67),
            arguments: Vec::new(),
        }
    }
}

//pub static mut COUNTER: usize = 0;
//pub static THREADS_USED: Mutex<usize> = Mutex::new(0);

impl Expression {
    fn is_simplified(&self) -> bool {
        matches!(self, Expression::Lambda { .. })
            || matches!(
                self,
                Expression::Tree {
                    root: Id::DataConstructor(_),
                    ..
                }
            )
    }
    // rewrite expression until it starts with either a lambda or a data constructor
    pub fn simplify_owned(&mut self) {
        while !self.is_simplified() {
            match std::mem::take(self) {
                Expression::Undefined { mark } => {
                    let error = Error {
                        error_type: Box::new(RuntimeError::EvaluatedUndefined),
                        mark: mark,
                    };
                    eprintln!("{error}");
                    std::process::exit(1);
                }
                Expression::Match {
                    mut pattern,
                    branches,
                } => {
                    let mut found = false;
                    for (pat, mut new_expression) in branches.into_iter() {
                        if matches_expression(&pat, &mut *pattern) {
                            let map = match_on_expression(&pat, *pattern);
                            map.into_iter().for_each(|(id, exp)| {
                                new_expression.substitute(id, build_thunk(exp))
                            });
                            *self = new_expression;
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        panic!("partial pattern")
                    };
                }
                Expression::Tree { root, arguments } => {
                    *self = match root {
                        Id::Thunk(exp) => match Arc::try_unwrap(exp) {
                            Ok(x) => {
                                let mut inner = x.into_inner().unwrap();
                                inner.simplify_owned();
                                optimize_expression(&mut inner);
                                inner
                            }
                            Err(x) => {
                                let mut inside = match (*x).try_lock() {
                                    Ok(x) => x,
                                    Err(_) => {
                                        eprintln!(
                                            "\x1b[91merror: \x1b[0mattempted to evaluate a bottom _|_"
                                        );
                                        std::process::exit(1);
                                    }
                                };
                                if !inside.is_simplified() {
                                    inside.simplify_owned();
                                    optimize_expression(&mut *inside);
                                }
                                inside.clone()
                            }
                        },
                        _ => unreachable!(),
                    };
                    for mut i in arguments {
                        if !self.is_simplified() {
                            self.simplify_owned()
                        }
                        match self {
                            Expression::Tree { arguments, .. } => arguments.push(i),
                            Expression::Lambda { pattern, body } => {
                                if matches_expression(&pattern, &mut i) {
                                    let map = match_on_expression(&pattern, i);
                                    for (id, expression) in map.into_iter() {
                                        body.substitute(id, build_thunk(expression));
                                    }
                                    *self = std::mem::take(&mut *body);
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
        if let Expression::Tree { arguments, .. } = self {
            arguments.iter_mut().for_each(optimize_expression);
        }
    }

    // substitute every instance of a LambdaArg with an Thunk
    pub fn substitute(&mut self, key: u32, new_expression: Arc<Mutex<Expression>>) {
        match self {
            Expression::Lambda { body, .. } => body.substitute(key, new_expression),
            Expression::Tree { root, arguments } => {
                arguments
                    .iter_mut()
                    .for_each(|i| i.substitute(key, Arc::clone(&new_expression)));
                if matches!(root, Id::LambdaArg(x) if *x == key) {
                    *root = Id::Thunk(new_expression);
                };
            }
            Expression::Match { pattern, branches } => {
                branches
                    .iter_mut()
                    .for_each(|(_, i)| i.substitute(key, Arc::clone(&new_expression)));
                pattern.substitute(key, Arc::clone(&new_expression));
            }
            Expression::Undefined { .. } => (),
        }
    }

    // the multithreaded evaluation function doesn't work currently because I'm using Mutexes to detect
    // bottoms. Should eventually switch those to state machines.

    //    // evaluate an expression strictly, leavivg only a tree of data constructors.
    //    // can't be called on functions
    //    pub fn evaluate_strictly(mut self, definitions: &ExpressionCache) -> Self {
    //        self.simplify_owned(definitions);
    //        match self {
    //            Expression::Tree {
    //                root,
    //                mut arguments,
    //            } => {
    //                enum HandleOrValue<T> {
    //                    Handle(T),
    //                    Value(Expression)
    //                }
    //                let mut handles = Vec::new();
    //                for i in arguments {
    //                    if matches!(
    //                        &i,
    //                        Expression::Tree {root: Id::DataConstructor(_), arguments} if arguments.len() == 0
    //                    ) {
    //                        handles.push(HandleOrValue::Value(i));
    //                    } else if matches!(
    //                        &i,
    //                        Expression::Tree {root: Id::DataConstructor(_), arguments}
    //                    ) {
    //                        handles.push(HandleOrValue::Value(i.evaluate_strictly(definitions)));
    //                    } else {
    //                        let r = definitions;
    //                        //dbg!("spawned thread");
    //                        let builder = thread::Builder::new().stack_size(1 * 1024 * 1024);
    //                        handles.push(
    //                            HandleOrValue::Handle(builder.spawn( move || i.evaluate_strictly(r)).unwrap())
    //                        )
    //                    }
    //                }
    //                let mut arguments = Vec::new();
    //                for i in handles.into_iter() {
    //                    arguments.push(match i {
    //                        HandleOrValue::Handle(i) => {
    //                            i.join().unwrap()
    //                        },
    //                        HandleOrValue::Value(i) => {
    //
    //                            i
    //                        }
    //                    });
    //                }
    //                Expression::Tree { root, arguments }
    //            }
    //            Expression::Lambda { .. } => panic!("attempted to evaluate a function"),
    //            _ => unreachable!(),
    //        }
    //    }
    //}

    // evaluates an currently not used    

    // pub fn evaluate_strictly(&mut self) {
    //     let mut to_evaluate: Vec<&mut Expression> = vec![self];
    //     while let Some(x) = to_evaluate.pop() {
    //         x.simplify_owned();
    //         match x {
    //             Expression::Tree { root, arguments } => {
    //                 arguments.iter_mut().for_each(|ptr| to_evaluate.push(ptr))
    //             }
    //             Expression::Lambda { .. } => panic!("attempted to evaluate a function"),
    //             _ => unreachable!(),
    //         }
    //     }
    // }

    // The print function is a combination of evaluate_strictly and 
    // convert_to_file. It exists to eat much less memory while evaluating large 
    // structures. It's also able to print stuff like infinity (succ succ ...)
    // without eating memory at all.
    
    pub fn print(self, names: &HashMap<u32, String>) {
        let mut to_evaluate: Vec<Expression> = vec![self];
        while let Some(mut x) = to_evaluate.pop() {
            x.simplify_owned();
            match x {
                Expression::Tree { root, arguments } => {
                    arguments.into_iter().rev().for_each(|ptr| to_evaluate.push(ptr));
                    let word = match root {
                        Id::DataConstructor(x) => names.get(&x).unwrap(), // unwrap should be safe
                        _ => unreachable!(),
                    };
                    print!("{word} ")

                }
                Expression::Lambda { .. } => panic!("attempted to evaluate a function"),
                _ => unreachable!(),
            }
        }
        print!("\n")
    }
}
