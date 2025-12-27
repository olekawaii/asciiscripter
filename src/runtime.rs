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
use std::io;
use std::io::Write;
use std::{collections::HashMap};

#[derive(Debug, Clone)]
pub enum Expression {
    Tree {
        root:        Box<Expression>,
        arguments:   Vec<Expression>,
    },
    Match {
        matched_on:  Box<Expression>,
        branches:    Vec<(Arc<Pattern>, Expression)>,
    },
    Lambda {
        pattern:     Arc<Pattern>,
        body:        Box<Expression>,
    },
    Undefined(Box<Mark>),
    Thunk(Arc<Mutex<Expression>>),
    LocalVarPlaceholder(u32),
    DataConstructor(u32),
    Variable(usize), // only used during parsing
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
            matched.simplify();
            match matched {
                Expression::Tree { root, arguments, .. } => {
                    let Expression::DataConstructor(id) = **root else { unreachable!() };
                    id == *data_constructor && {
                        for (pattern, arg) in patterns.iter().zip(arguments.iter_mut()) {
                            if !matches_expression(pattern, arg) {
                                return false;
                            }
                        }
                        true
                    }
                }
                Expression::DataConstructor(id) => id == data_constructor,
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
    output:       &mut HashMap<u32, Expression>,
    pattern:      &Pattern,
    mut matched:  Expression,
) {
    match pattern {
        Pattern::Dropped => (),
        Pattern::Captured(id) => {
            output.insert(*id, build_thunk(matched));
        }
        Pattern::DataConstructor(data_constructor, patterns) => {
            matched.simplify();
            match matched {
                Expression::Tree { root, arguments, .. } => {
                    let Expression::DataConstructor(id) = *root else { unreachable!() };
                    if id == *data_constructor {
                        for (pattern, arg) in patterns.iter().zip(arguments.into_iter()) {
                            match_on_expression_helper(output, pattern, arg)
                        }
                    }
                }
                Expression::DataConstructor(_) => (),
                _ => unreachable!(),
            }
        }
    }
}

impl Default for Expression {
    fn default() -> Self {
        Self::LocalVarPlaceholder(67)
    }
}

impl Expression {
    fn is_simplified(&self) -> bool {
        matches!(self, Expression::Lambda { .. } | Expression::DataConstructor(_)) || 
        matches!(self, Expression::Tree {root, ..} if matches!(&**root, Expression::DataConstructor(_)))
    }

    // rewrite expression until it starts with a data constructor or a lambda
    
    pub fn simplify(&mut self) {
        while !self.is_simplified() {
            match std::mem::take(self) {
                Expression::Thunk(exp) => match Arc::try_unwrap(exp) {
                    Ok(x) => *self = x.into_inner().unwrap(), // safe unwrap
                    Err(x) => {
                        let Ok(mut inner) = (*x).try_lock() else {
                            eprintln!( "\x1b[91mruntime error: \x1b[0mattempted to evaluate a bottom _|_");
                            std::process::exit(1);
                        };
                        if !inner.is_simplified() {
                            inner.simplify();
                            actually_optimize(&mut inner);
                        }
                        *self = inner.clone()
                    }
                }
                Expression::Tree { root, arguments, .. } => {
                    *self = *root;
                    for mut i in arguments {
                        self.simplify();
                        match self {
                            Expression::Tree { arguments, .. } => arguments.push(i),
                            Expression::Lambda { pattern, body } => {
                                // assume the pattern matches
                                if !matches_expression(pattern, &mut i) {
                                    panic!()
                                }
                                let map = match_on_expression(pattern, i);
                                body.substitute(&map);
                                *self = std::mem::take(&mut *body);
                            }
                            Expression::DataConstructor(_) => {
                                *self = Expression::Tree {
                                    root: Box::new(std::mem::take(self)),
                                    arguments: vec![i]
                                }
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
                Expression::Undefined(mark) => {
                    let error = Error {
                        error_type: Box::new(RuntimeError::EvaluatedUndefined),
                        mark: (*mark).clone(),
                    };
                    eprintln!("{error}");
                    std::process::exit(1);
                }
                _ => unreachable!()
            }
        }
    }

    // Substitute every instance of a LocalVarPlaceholder with a Thunk. To be used with
    // patternmatching (match_on_expression) output
      
    pub fn substitute(&mut self, map: &HashMap<u32, Expression>) {
        match self {
            Expression::Lambda { body, .. } => body.substitute(map),
            Expression::LocalVarPlaceholder(id) => {
                if let Some(new_expression) = map.get(id) {
                    *self = new_expression.clone()
                }
            }
            Expression::Tree { root, arguments, .. } => {
                root.substitute(map);
                arguments
                    .iter_mut()
                    .for_each(|i| i.substitute(map));
            }
            Expression::Match { matched_on, branches } => {
                branches
                    .iter_mut()
                    .for_each(|(_, i)| i.substitute(map));
                matched_on.substitute(map);
            }
            Expression::Undefined { .. } | Expression::Thunk(_) | Expression::DataConstructor(_) => (),
            Expression::Variable(_) => unreachable!()
        }
    }

    // Evaluates an expression, leaving only a tree of data constructors. 
    // Currently unused

    //pub fn evaluate_strictly(&mut self) {
    //    let mut to_evaluate: Vec<&mut Expression> = vec![self];
    //    while let Some(x) = to_evaluate.pop() {
    //        x.simplify();
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
    
    pub fn print(self, names: &mut HashMap<u32, String>) {
        let mut stdout = io::stdout().lock();
        let mut cache = HashMap::new();
        let mut to_evaluate: Vec<Expression> = vec![self];
        while let Some(mut x) = to_evaluate.pop() {
            x.simplify();
            match x {
                Expression::Tree { root, arguments, .. } => {
                    arguments.into_iter().rev().for_each(|x| to_evaluate.push(x));
                    to_evaluate.push(*root);
                }
                Expression::DataConstructor(id) => {
                    let word = match cache.get(&id) {
                        Some(x) => x,
                        None => {
                            let name = names.remove(&id).unwrap();
                            cache.insert(id, name);
                            cache.get(&id).unwrap() // safe
                        }
                    };
                    stdout.write_all(word.as_bytes()).expect("");
                    stdout.write_all(b" ").expect("");
                }
                Expression::Lambda { .. } => stdout.write_all(b"<lambda> ").unwrap(),
                _ => unreachable!(),
            }
        }
        stdout.write_all(b"\n").expect("");
    }
}

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

fn build_thunk(mut input: Expression) -> Expression {
    actually_optimize(&mut input);
    if let Expression::Tree { root, arguments } = &mut input && arguments.is_empty() {
        input = std::mem::take(root)
    }
    match &mut input {
        Expression::Tree { .. } | Expression::Match { .. } => Expression::Thunk(Arc::new(Mutex::new(input))),
        _ => input
    }
}

fn actually_optimize(input: &mut Expression) {
    match input {
        Expression::Tree { arguments, .. } => {
            arguments.iter_mut().for_each(optimize_expression);
        }
        Expression::Lambda { pattern, body } if matches!(**pattern, Pattern::Dropped) => optimize_expression(body),
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
            if !matches!(&**root, Expression::DataConstructor(_)) {
                *input = Expression::Thunk(Arc::new(Mutex::new(std::mem::take(input))))
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
        Expression::Lambda { pattern, body } if matches!(**pattern, Pattern::Dropped) => optimize_expression(body),
        Expression::Undefined { .. } => (),
        _ => (),
    }
}
