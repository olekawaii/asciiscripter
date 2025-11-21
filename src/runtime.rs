/* thorn - a pure lazy functional programming language
 * Copyright (C) 2025  Oleksiy Buell <olekawaii@proton.me>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 */

use std::{collections::HashMap, fmt};
use std::sync::{Mutex, Arc};
use std::thread;
use crate::error::{Error, Mark, ErrorType};

#[derive(Debug)]
enum RuntimeError {
    EvaluatedUndefined,
    EvaluatedBottom,
}

impl ErrorType for RuntimeError {
    fn gist(&self) -> &'static str {
        match self {
            Self::EvaluatedUndefined => "entered undefined code",
            Self::EvaluatedBottom    => "evaluated a bottom _|_"
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
            Self::EvaluatedBottom    => write!(f, "attempted to evaluate a bottom expression")
        }
    }
}

fn build_thunk(mut input: Expression) -> Arc<Mutex<Expression>> {
    optimize_expression(&mut input);
    Arc::new(Mutex::new(input))
}

pub fn optimize_expression(input: &mut Expression) {
    if matches!(&input, Expression::Tree {root: Id::DataConstructor(_), arguments} if arguments.len() == 0) ||
       matches!(&input, Expression::Tree {root: Id::Thunk(_), arguments} if arguments.len() == 0) {
       //matches!(&input, Expression::Lambda {..}) {
        return ()
    }
    match input {
        Expression::Tree {root, arguments} => {
            arguments.iter_mut().for_each(optimize_expression);
            if !matches!(root, Id::DataConstructor(_)) {
                *input = Expression::Tree {
                    root: Id::Thunk(Arc::new(Mutex::new(Expression::Tree {
                        root: root.clone(),
                        arguments: std::mem::take(arguments)}
                    ))),
                    arguments: Vec::new()
                }
            }
        }
        //Expression::Match {branches, ..} => {
        //    // no point in optimizing anything since the simplified output will be optimized later
        //    for (pattern, expression) in branches.iter_mut() {
        //        if let Pattern::Dropped = pattern {
        //            optimize_expression(expression)
        //        }
        //    }
        //},
        Expression::Lambda {id: Pattern::Dropped, body} => optimize_expression(body),
        Expression::Undefined {..} => (),
        _ => ()
    }
}

#[derive(Debug, Clone)]
pub enum Id {
    Thunk(Arc<Mutex<Expression>>),
    LambdaArg(u32),
    Variable(usize),       // only used during parsing
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
        branches: Vec<(Pattern, Expression)> 
    },
    Lambda {
        id: Pattern,
        body: Box<Expression>,
    },
    Undefined {mark: Mark},
}

// switch Captured(u32) to Captured(Vec<*Id>) to avoid the traversals

#[derive(Debug, Clone)]
pub enum Pattern {
    Dropped,
    Captured(u32),
    DataConstructor(u32, Vec<Pattern>)
}

fn matches_expression(
    pattern: &Pattern, 
    matched: &mut Expression,
) -> bool {
    match pattern { 
        Pattern::Dropped => true,
        Pattern::Captured(id) => true,
        Pattern::DataConstructor(data_constructor, patterns) => {
            matched.simplify_owned();
            match matched {
                Expression::Tree {root: Id::DataConstructor(id), arguments} => {
                    if id == data_constructor {
                        for (pattern, arg) in patterns.iter().zip(arguments.iter_mut()) {
                            if !matches_expression( pattern, arg,) { return false }
                        } 
                        true
                    } else {
                        false
                    }
                }
                _ => unreachable!()
            }
        }
    }
}

fn match_on_expression(
    pattern: &Pattern, 
    matched: Expression,
) -> HashMap<u32, Expression> {
    let mut output = HashMap::new();
    match_on_expression_helper(
        &mut output,
        pattern,
        matched,
    );
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
            output.insert(*id, matched.clone());
        }
        Pattern::DataConstructor(data_constructor, patterns) => {
            matched.simplify_owned();
            match matched {
                Expression::Tree {root: Id::DataConstructor(id), arguments} => {
                    if id == *data_constructor {
                        for (pattern, arg) in patterns.iter().zip(arguments.into_iter()) {
                            match_on_expression_helper(
                                output,
                                pattern,
                                arg,
                            )
                        } 
                    }
                }
                _ => unreachable!()
            }
        }
    }
}

impl Default for Expression {
    fn default() -> Self {
        Self::Tree {
            root: Id::LambdaArg(67),
            arguments: Vec::new()
        }
    }
}

//pub static mut COUNTER: usize = 0;
//pub static THREADS_USED: Mutex<usize> = Mutex::new(0);

impl Expression {
    fn is_simplified(&self) -> bool {
        matches!(self, Expression::Lambda {..}) ||
        matches!(self, Expression::Tree {root: Id::DataConstructor(_), arguments} if arguments.len() == 0)
    }
    // TODO make simplify non-recursive so it doesn't blow the stack
    // this means adding an attempt_to_simplify finction and calling it in a loop
    // for match expressions only simplify what's in the pattern, with increasing depth
    // for trees if there are arguments, just add them to the root arguments, otherwise
    // substitute them in the root

    // rewrite expression until it starts with either a lambda or a data constructor
    pub fn simplify_owned(&mut self) {
        //unsafe {
        //    COUNTER += 1;
        //}
        if matches!(self, Expression::Tree {root: Id::DataConstructor(_), ..}) || 
           matches!(self, Expression::Lambda {..} ) {
            return ()
        }
        match std::mem::take(self) {
            Expression::Tree { root, arguments } => {
                let mut output: Expression = match root {
                    Id::Thunk(exp) => {
                        match Arc::try_unwrap(exp) {
                            Ok(x) => {
                                let mut inner = x.into_inner().unwrap();
                                inner.simplify_owned();
                                if let Expression::Tree {arguments, ..} = &mut inner {
                                    arguments.iter_mut().for_each(optimize_expression);
                                }
                                inner
                            }
                            Err(x) => {
                                let mut inside = match (*x).try_lock() {
                                    Ok(x) => x,
                                    Err(_) => {
                                        eprintln!("\x1b[91merror: \x1b[0mattempted to evaluate a bottom");
                                        std::process::exit(1);
                                    }
                                };
                                if !matches!(
                                    &*inside, 
                                    Expression::Tree {root: Id::DataConstructor(_), ..} | Expression::Lambda {..}
                                ) {
                                    inside.simplify_owned();
                                    if let Expression::Tree {arguments, ..} = &mut *inside {
                                        arguments.iter_mut().for_each(optimize_expression);
                                    }
                                }
                                inside.clone()

                            }
                        }
                    }
                    _ => unreachable!(),
                };
                for mut i in arguments {
                    if let Expression::Tree {arguments, ..} = &mut output {
                        arguments.push(i);
                    } else if let Expression::Lambda { id, mut body } = output {
                        if matches_expression(&id, &mut i) {
                            let map = match_on_expression(&id, i);
                            for (id, expression) in map.into_iter() {
                                body.substitute(id, build_thunk(expression));
                            }
                            output = *body;
                        } else {
                            panic!("wuuwwuwuwu")
                        }
                    }
                    if !matches!(
                        &output, 
                        Expression::Tree {root: Id::DataConstructor(_), ..}
                    ) {
                        output.simplify_owned();
                    }
                }
                //output.simplify(definitions);
                if let Expression::Tree {arguments, ..} = &mut output {
                    arguments.iter_mut().for_each(optimize_expression);
                }
                *self = output;
            }
            Expression::Match { mut pattern, mut branches } => {
                let mut found = false;
                for (pat, mut new_expression) in branches.into_iter() {
                    if matches_expression(&pat, &mut *pattern) {
                        let map = match_on_expression(&pat, *pattern);
                        for (id, expression) in map.into_iter() {
                            new_expression.substitute(id, build_thunk(expression));
                        }
                        *self = new_expression;
                        found = true;
                        break
                    }
                }
                if !found {panic!("partial pattern")};
                self.simplify_owned();
                if let Expression::Tree {arguments, ..} = self {
                    arguments.iter_mut().for_each(optimize_expression);
                }
            }
            Expression::Undefined { mark } => {
                let error = Error {
                    error_type: Box::new(RuntimeError::EvaluatedUndefined),
                    mark: mark,
                };
                eprintln!("{error}");
                std::process::exit(1);
            }
            x => unreachable!(),
        }
    }

    // substitute every instance of an id with an expression
    pub fn substitute(&mut self, key: u32, new_expression: Arc<Mutex<Expression>>) {
        match self {
            Expression::Lambda { body, .. } => body.substitute(key, new_expression),
            Expression::Tree { root, arguments } => {
                arguments.iter_mut().for_each(|i| i.substitute(key, Arc::clone(&new_expression)));
                if matches!(root, Id::LambdaArg(x) if *x == key) {
                    *root = Id::Thunk(new_expression);
                };

            }
            Expression::Match { pattern, branches } => {
                branches.iter_mut().for_each(|(_, i)| i.substitute(key, Arc::clone(&new_expression)));
                pattern.substitute(key, Arc::clone(&new_expression));
            }
            Expression::Undefined {..} => ()
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

    pub fn evaluate_strictly(&mut self) {
        self.simplify_owned();
        match self {
            Expression::Tree { root, arguments, } => {
                arguments.iter_mut().for_each(|x| x.evaluate_strictly())
            }
            Expression::Lambda { .. } => panic!("attempted to evaluate a function"),
            _ => unreachable!(),
        }
    }
}
