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
}

impl ErrorType for RuntimeError {
    fn gist(&self) -> &'static str {
        match self {
            Self::EvaluatedUndefined => "entered undefined code"
        }
    }
    
    fn phase(&self) -> &'static str {
        "runtime"
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EvaluatedUndefined => write!(f, "attempted to evaluate an undefined expression")
        }
    }
}

// instead of Arc<Expression> use Arc<Mutex<Expression>> and first one to use it modifies it

// build_mutex takes a lambdaarg and makes every branch in a tree a mutex, or pattern in match, 
fn build_mutex(mut input: Expression) -> Arc<Mutex<Expression>> {
    //optimize_expression(&mut input);
    //dbg!(&input);
    //panic!();
    Arc::new(Mutex::new(input))
    //Arc::new(Mutex::new(input))
}

//fn optimize_expression(input: &mut Expression) {
//    if matches!(&input, Expression::Tree {root: Id::DataConstructor(_), arguments} if arguments.len() == 0) ||
//       matches!(&input, Expression::Tree {root: Id::Thunk(_), arguments} if arguments.len() == 0) {
//       //matches!(&input, Expression::Lambda {..}) {
//        return ()
//    }
//    match input {
//        Expression::Tree {root, arguments} => {
//            //let mut new_arguments = Vec::new();
//            for i in arguments.iter_mut() {
//                optimize_expression(i)
//            }
//            *input = Expression::Tree {
//                root: Id::Thunk(Arc::new(Mutex::new(Expression::Tree {root: root.clone(), arguments: std::mem::take(arguments)}))),
//                arguments: Vec::new()
//            }
//        }
//        Expression::Match {pattern, branches} => {
//            //pattern = Box::new(Expression::Tree {root: Id::Thunk(build_mutex(*pattern)), arguments: Vec::new()});
//            for (_, (vec, expression)) in branches.iter_mut() {
//                if vec.iter().all(|x| x.is_none()) {
//                    optimize_expression(expression);
//                    //dbg!("s");
//                }
//            }
//
//            optimize_expression(&mut(*pattern))
//        }
//        Expression::Lambda {id: None, body} => optimize_expression(body),
//        l@Expression::Lambda {..} => {},
//        Expression::Undefined => ()
//    }
//}

//fn optimize_expression(input: &mut Expression) {
//    if matches!(&input, Expression::Tree {root: Id::DataConstructor(_), arguments} if arguments.len() == 0) ||
//       matches!(&input, Expression::Tree {root: Id::Thunk(_), arguments} if arguments.len() == 0) {
//       //matches!(&input, Expression::Lambda {..}) {
//        return ()
//    }
//    match input {
//        Expression::Tree {root, arguments} => {
//            //let mut new_arguments = Vec::new();
//            //for i in arguments.iter_mut() {
//            //    optimize_expression(i)
//            //}
//            *input = Expression::Tree {
//                root: Id::Thunk(Arc::new(Mutex::new(Expression::Tree {root: root.clone(), arguments: std::mem::take(arguments)}))),
//                arguments: Vec::new()
//            }
//        }
//        Expression::Match {pattern, branches} => {
//            for (_, (vec, expression)) in branches.iter_mut() {
//                if vec.iter().all(|x| x.is_none()) {
//                    optimize_expression(expression);
//                    //dbg!("s");
//                }
//            }
//
//            optimize_expression(&mut(*pattern))
//            //if matches!(**pattern, Expression::Tree { root: Id::Thunk(_), ..}) {
//            //    *pattern = Box::new(Expression::Tree {
//            //        root: Id::Thunk(Arc::new(Mutex::new(std::mem::take(pattern)))),
//            //        arguments: Vec::new()
//            //    })
//            //}
//        }
//        //Expression::Lambda {id: None, body} => optimize_expression(body),
//        l@Expression::Lambda {..} => {},
//        Expression::Undefined {..} => ()
//    }
//}

#[derive(Debug, Clone)]
pub enum Id {
    Thunk(Arc<Mutex<Expression>>),
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
        branches: Vec<(Pattern, Expression)> //HashMap<u32, (Vec<Option<u32>>, Expression)>,
    },
    Lambda {
        //id: Option<u32>,
        id: Pattern,
        body: Box<Expression>,
    },
    Undefined {mark: Mark},
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Dropped,
    Captured(u32),
    DataConstructor(u32, Vec<Pattern>)
}

fn match_on_expression(
    pattern: &Pattern, 
    matched: &mut Expression,
    global_vars: Arc<ExpressionCache>
) -> Option<HashMap<u32, Expression>> {
    let mut output = HashMap::new();
    match_on_expression_helper(
        &mut output,
        pattern,
        matched,
        global_vars
    )?;
    Some(output)
}


fn match_on_expression_helper(
    output: &mut HashMap<u32, Expression>,
    pattern: &Pattern, 
    matched: &mut Expression,
    global_vars: Arc<ExpressionCache>
) -> Option<()> {
    match pattern { 
        Pattern::Dropped => Some(()),
        Pattern::Captured(id) => {
            output.insert(*id, matched.clone());
            Some(())
        }
        Pattern::DataConstructor(data_constructor, patterns) => {
            matched.simplify_owned(Arc::clone(&global_vars));
            match matched {
                Expression::Tree {root: Id::DataConstructor(id), arguments} => {
                    if id == data_constructor {
                        for (pattern, arg) in patterns.iter().zip(arguments.iter_mut()) {
                            match_on_expression_helper(
                                output,
                                pattern,
                                arg,
                                Arc::clone(&global_vars)
                            )?
                        } 
                        Some(())
                    } else {
                        None
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

pub static mut COUNTER: usize = 0;
pub static THREADS_USED: Mutex<usize> = Mutex::new(0);

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
    pub fn simplify_owned(&mut self, definitions: Arc<ExpressionCache>) {
        unsafe {
            COUNTER += 1;
        }
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
                                inner.simplify_owned(Arc::clone(&definitions));
                                inner
                            }
                            Err(x) => {
                                let mut inside = (*x).lock().unwrap();
                                if !matches!(
                                    &*inside, 
                                    Expression::Tree {root: Id::DataConstructor(_), ..} | Expression::Lambda {..}
                                ) {
                                    inside.simplify_owned(Arc::clone(&definitions));
                                }
                                inside.clone()
                            }
                        }
                    }
                    Id::Variable(index) => {
                        let mut var = definitions.get(index);
                        //if !matches!(
                        //    &var, 
                        //    Expression::Tree {root: Id::DataConstructor(_), ..} | Expression::Lambda {..}
                        //) {
                        var.simplify_owned(Arc::clone(&definitions));
                        var
                    }
                    Id::LambdaArg(a) => {
                        dbg!(a);
                        unreachable!()
                    }
                    _ => unreachable!(),
                };
                for mut i in arguments {
                    if let Expression::Tree {arguments, ..} = &mut output {
                        arguments.push(i);
                    } else if let Expression::Lambda { id, mut body } = output {
                        let map = match_on_expression(&id, &mut i, Arc::clone(&definitions)).unwrap();
                        for (id, expression) in map.into_iter() {
                            body.substitute(id, build_mutex(expression));
                        }
                        output = *body;
                    }
                    if !matches!(
                        &output, 
                        Expression::Tree {root: Id::DataConstructor(_), ..}
                    ) {
                        output.simplify_owned(Arc::clone(&definitions));
                    }
                }
                //output.simplify(definitions);
                *self = output;
            }
            Expression::Match { mut pattern, mut branches } => {
                let mut found = false;
                for (pat, mut new_expression) in branches.into_iter() {
                    if let Some(map) = match_on_expression(&pat, &mut pattern, Arc::clone(&definitions)) {
                        for (id, expression) in map.into_iter() {
                            new_expression.substitute(id, build_mutex(expression));
                        }
                        *self = new_expression;
                        found = true;
                        break
                    }
                }
                if !found {panic!("partial pattern")};
                self.simplify_owned(Arc::clone(&definitions));
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
                for i in arguments.iter_mut() {
                    i.substitute(key, Arc::clone(&new_expression));
                }
                if matches!(root, Id::LambdaArg(x) if *x == key) {
                    *root = Id::Thunk(new_expression);
                };

            }
            Expression::Match { pattern, branches } => {
                for (_, i) in branches.iter_mut() {
                    i.substitute(key, Arc::clone(&new_expression));
                }
                pattern.substitute(key, Arc::clone(&new_expression));
            }
            Expression::Undefined {..} => ()
        }
    }

    // evaluate an expression strictly, leavivg only a tree of data constructors.
    // can't be called on functions
    pub fn evaluate_strictly(mut self, definitions: Arc<ExpressionCache>) -> Self {
        self.simplify_owned(Arc::clone(&definitions));
        match self {
            Expression::Tree {
                root,
                mut arguments,
            } => {
                enum HandleOrValue<T> {
                    Handle(T),
                    Value(Expression)
                }
                let mut handles = Vec::new();
                for i in arguments {
                    if matches!(
                        &i, 
                        Expression::Tree {root: Id::DataConstructor(_), arguments} if arguments.len() == 0 
                    ) {
                        handles.push(HandleOrValue::Value(i));
                    } else if matches!(
                        &i, 
                        Expression::Tree {root: Id::DataConstructor(_), arguments}
                    ) {
                        handles.push(HandleOrValue::Value(i.evaluate_strictly(Arc::clone(&definitions))));
                    } else {
                        let r = Arc::clone(&definitions);
                        //dbg!("spawned thread");
                        let builder = thread::Builder::new().stack_size(4 * 1024 * 1024);
                        handles.push(
                            HandleOrValue::Handle(builder.spawn( move || i.evaluate_strictly(r)).unwrap())
                        )
                    }
                }
                let mut arguments = Vec::new();
                for i in handles.into_iter() {
                    arguments.push(match i {
                        HandleOrValue::Handle(i) => {
                            i.join().unwrap()
                        },
                        HandleOrValue::Value(i) => {

                            i
                        }
                    });
                }
                Expression::Tree { root, arguments }
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
