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

static COUNTER: Mutex<u32> = Mutex::new(0);

use crate::error::{Error, ErrorType, Mark};
use std::sync::{Arc, Mutex};
// use std::thread;
use std::{collections::HashMap};

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
    actually_optimize(&mut input);
    match &mut input {
        Expression::Tree {root: Id::Thunk(x), arguments, ..} if arguments.is_empty() => {
            //dbg!(&x);
            std::mem::take(x)
        }
        _ => Arc::new(Mutex::new(input)),
    }
}

fn actually_optimize(input: &mut Expression) {
    match input {
        Expression::Tree { root, arguments} => {
            arguments.iter_mut().for_each(optimize_expression);
        }
        Expression::Lambda { pattern: Pattern::Dropped, body } => optimize_expression(body),
        Expression::Undefined { .. } => (),
        _ => (),
    }
}


pub fn optimize_expression(input: &mut Expression) {
    if matches!(&input, Expression::Tree { arguments, .. } if arguments.is_empty()) {
        return
    }
    match input {
        Expression::Tree { root, arguments} => {
            arguments.iter_mut().for_each(optimize_expression);
            if !matches!(root, Id::DataConstructor(_)) {
                *input = Expression::Tree {
                    root: Id::Thunk(Arc::new(Mutex::new(std::mem::take(input)))),
                    arguments: Vec::new(),
                }
            }
        }
        // // no point in optimizing anything since the simplified output will be optimized later
        //Expression::Match {branches, pattern} => {
        //    optimize_expression(pattern);
        //    for (pattern, expression) in branches.iter_mut() {
        //        if let Pattern::Dropped = pattern {
        //            optimize_expression(expression)
        //        }
        //    }
        //},
        Expression::Lambda { pattern: Pattern::Dropped, body } => optimize_expression(body),
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
        //is_optimized: bool,
    },
    Match {
        matched_on: Box<Expression>,
        branches: Vec<(Pattern, Expression)>,
    },
    Lambda {
        pattern: Pattern,
        body: Box<Expression>,
    },
    Undefined {
        mark: Mark,
    },
    // Thunk(Arc<Mutex<Expression>>),
    // LambdaArg(u32)
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
                Expression::Tree { root: Id::DataConstructor(id), arguments, .. } => {
                    id == data_constructor && {
                        for (pattern, arg) in patterns.iter().zip(arguments.iter_mut()) {
                            if !matches_expression(pattern, arg) {
                                return false;
                            }
                        }
                        true
                    }
                }
                _ => unreachable!(),
            }
        }
    }
}

fn match_on_expression(pattern: &Pattern, matched: Expression) -> HashMap<u32, Arc<Mutex<Expression>>> {
    let mut output = HashMap::new();
    match_on_expression_helper(&mut output, pattern, matched);
    output
}

fn match_on_expression_helper(
    output: &mut HashMap<u32, Arc<Mutex<Expression>>>,
    pattern: &Pattern,
    mut matched: Expression,
) {
    match pattern {
        Pattern::Dropped => (),
        Pattern::Captured(id) => {
            output.insert(*id, build_thunk(matched));
        }
        Pattern::DataConstructor(data_constructor, patterns) => {
            matched.simplify_owned();
            match matched {
                Expression::Tree { root: Id::DataConstructor(id), arguments, .. } => {
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
            //is_optimized: true
        }
    }
}

impl Expression {
    fn is_simplified(&self) -> bool {
        matches!(
            self, 
            Expression::Lambda { .. } | Expression::Tree {root: Id::DataConstructor(_), ..}
        )
    }

    // rewrite expression until it starts with a data constructor or a lambda
    
    pub fn simplify_owned(&mut self) {
        while !self.is_simplified() {
            match std::mem::take(self) {
                Expression::Tree { root, arguments, .. } => {
                    *self = match root {
                        Id::Thunk(exp) => match Arc::try_unwrap(exp) {
                            Ok(x) => x.into_inner().unwrap(), // safe unwrap
                            Err(x) => {
                                let Ok(mut inner) = (*x).try_lock() else {
                                    eprintln!(
                                        "\x1b[91mruntime error: \x1b[0mattempted to evaluate a bottom _|_"
                                    );
                                    std::process::exit(1);
                                };
                                if !inner.is_simplified() {
                                    inner.simplify_owned();
                                    actually_optimize(&mut inner);
                                }
                                inner.clone()
                            }
                        },
                        _ => unreachable!(),
                    };
                    for i in arguments {
                        self.simplify_owned();
                        match self {
                            Expression::Tree { arguments, .. } => arguments.push(i),
                            Expression::Lambda { pattern, body } => {
                                // assume the pattern matches
                                let map = match_on_expression(pattern, i);
                                body.substitute(&map);
                                *self = std::mem::take(&mut *body);
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                Expression::Match {
                    mut matched_on,
                    branches,
                } => {
                    let mut found = false;
                    for (pat, mut new_expression) in branches.into_iter() {
                        if matches_expression(&pat, &mut matched_on) {
                            let map = match_on_expression(&pat, *matched_on);
                            new_expression.substitute(&map);
                            *self = new_expression;
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        panic!("partial pattern")
                    };
                }
                Expression::Undefined { mark } => {
                    let error = Error {
                        error_type: Box::new(RuntimeError::EvaluatedUndefined),
                        mark,
                    };
                    eprintln!("{error}");
                    std::process::exit(1);
                }
                _ => unreachable!(),
            }
        }
    }

    // Substitute every instance of a LambdaArg with a Thunk. To be used with
    // patternmatching (match_on_expression) output
      
    pub fn substitute(&mut self, map: &HashMap<u32, Arc<Mutex<Expression>>>) {
        match self {
            Expression::Lambda { body, .. } => body.substitute(map),
            Expression::Tree { root, arguments, .. } => {
                arguments
                    .iter_mut()
                    .for_each(|i| i.substitute(map));
                if let Id::LambdaArg(x) = root && let Some(new_expression) = map.get(x) {
                    *root = Id::Thunk(Arc::clone(new_expression));
                }
            }
            Expression::Match { matched_on, branches } => {
                branches
                    .iter_mut()
                    .for_each(|(_, i)| i.substitute(map));
                matched_on.substitute(map);
            }
            Expression::Undefined { .. } => (),
        }
    }

    // Evaluates an expression, leaving only a tree of data constructors. 
    // Currently unused

    //pub fn evaluate_strictly(&mut self) {
    //    let mut to_evaluate: Vec<&mut Expression> = vec![self];
    //    while let Some(x) = to_evaluate.pop() {
    //        x.simplify_owned();
    //        match x {
    //            Expression::Tree { root, arguments, .. } => {
    //                arguments.iter_mut().for_each(|ptr| to_evaluate.push(ptr))
    //            }
    //            Expression::Lambda { .. } => panic!("attempted to evaluate a function"),
    //            _ => unreachable!(),
    //        }
    //    }
    //}

    // The print function is a combination of evaluate_strictly and 
    // convert_to_file. It exists to eat much less memory while evaluating large 
    // structures. It's also able to print stuff like infinity (succ succ ...)
    // without eating memory at all.
    
    pub fn print(mut self, names: &mut HashMap<u32, String>) {
        let mut cache = HashMap::new();
        let mut to_evaluate: Vec<Expression> = vec![self];
        while let Some(mut x) = to_evaluate.pop() {
            x.simplify_owned();
            match x {
                Expression::Tree { root, arguments, .. } => {
                    arguments.into_iter().rev().for_each(|x| to_evaluate.push(x));
                    let Id::DataConstructor(id) = root else { unreachable!() };
                    let word = match cache.get(&id) {
                        Some(x) => x,
                        None => {
                            let name = names.remove(&id).unwrap();
                            cache.insert(id, name);
                            cache.get(&id).unwrap()
                        }
                    };
                    //let word = names.get(&id).unwrap(); // should be safe
                    print!("{word} ")

                }
                Expression::Lambda { .. } => panic!("attempted to print a function"),
                _ => unreachable!(),
            }
        }
        println!();
    }
}
