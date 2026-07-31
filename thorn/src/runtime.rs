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

use crate::error::{ Error, ErrorType, Mark, Marked, make_error };
use std::{
    io::{ Write, BufWriter },
    process::{Command, Stdio},
    sync::Arc,
    collections::HashMap,
    cell::RefCell,
};

use crate::parse::Type;

// pretty bad

#[derive(Debug, Clone)]
pub enum Expression {
    Tree {
        root:        Box<Expression>,
        arguments:   Box<[Expression]>,
    },
    Match {
        matched_on:  Box<Expression>,
        branches:    Box<[(Arc<Marked<Pattern>>, Expression)]>,
    },
    Lambda {
        pattern:     Arc<Marked<Pattern>>,
        body:        Box<Expression>,
    },
    Thunk {
        value:       Arc<RefCell<Expression>>,
        mark:        Option<Arc<Mark>>,  // mark in case the value is a bottom
    },
    Undefined(Box<Mark>),
    LocalVarPlaceholder(u32),
    DataConstructor(u32),
    // used for Expression::default(). If encountered, 
    // it's a compiler bug somewhere
    CompilerBug
}

impl Default for Expression {
    fn default() -> Self {
        Self::CompilerBug
    }
}

#[derive(Debug)]
enum RuntimeError {
    EvaluatedUndefined,
    EvaluatedBottom,
    UnmatchedPattern(Expression, Vec<String>),
}

impl ErrorType for RuntimeError {
    fn gist(&self) -> &'static str {
        match self {
            Self::UnmatchedPattern { .. }  => "value did not match patterns",
            Self::EvaluatedUndefined       => "entered undefined code",
            Self::EvaluatedBottom          => "evaluated a bottom _|_",
        }
    }

    fn phase(&self) -> &'static str {
        "RUNTIME"
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EvaluatedUndefined => write!(f, "evaluated an undefined expression"),
            Self::UnmatchedPattern(expr, names) => write!(
                f, 
                "a value did not match the patterns\nthe value was \x1b[97m{}\x1b[0m",
                debug_print(expr, names)
            ),
            Self::EvaluatedBottom => write!(
                f, 
                "attempted to evaluate a bottom expression.\n\
                    this expression will never return a value"
            )
        }
    }
}

fn debug_print(expr: &Expression, names: &Vec<String>) -> String {
    match expr {
        Expression::Tree { root, arguments } => {
            let mut s = format!("{}", debug_print(root, names));
            arguments.iter().for_each(|x| {
                let a = debug_print(x, names);
                s.push_str(&a);
            });
            s
        }
        Expression::DataConstructor(x) => format!(" {}", names[*x as usize]),
        _ => " _".to_string(),
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Dropped,
    Captured(u32),
    DataConstructor(u32, Vec<Pattern>),
    Bound(u32, Box<Pattern>),
    Either(Box<(Pattern, Pattern)>)
}

fn matches_expression(
    pattern: &Pattern, 
    matched: &mut Expression, 
    names: &Vec<String>
) -> bool {
    match pattern {
        Pattern::Dropped => true,
        Pattern::Captured(_) => true,
        Pattern::Bound(_, pat) => matches_expression(pat, matched, names),
        Pattern::DataConstructor(data_constructor, patterns) => {
            matched.simplify(names);
            match matched {
                Expression::Tree { root, arguments, .. } => {
                    let Expression::DataConstructor(id) = **root else { unreachable!() };
                    id == *data_constructor && {
                        for (pattern, arg) in patterns.iter().zip(arguments.iter_mut()) {
                            if !matches_expression(pattern, arg, names) {
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
        Pattern::Either(x) => {
            let (fst, snd) = &**x;
            matches_expression(fst, matched, names) || matches_expression(snd, matched, names)
        }
    }
}

fn match_on_expression(
    pattern: &Pattern, 
    matched: Expression, 
    names: &Vec<String>
) -> Vec<(u32, Expression)> {
    let mut output = Vec::new();
    match_on_expression_helper(&mut output, pattern, matched, names);
    output
}

fn match_on_expression_helper(
    output:       &mut Vec<(u32, Expression)>,
    pattern:      &Pattern,
    mut matched:  Expression,
    names:        &Vec<String>
) {
    match pattern {
        Pattern::Dropped => (),
        Pattern::Bound(id, pat) => {
            let thunk = build_thunk(matched);
            output.push((*id,thunk.clone()));
            match_on_expression_helper(output, &**pat, thunk, names);
        }
        Pattern::Captured(id) => {
            output.push((*id, build_thunk(matched)));
        }
        Pattern::DataConstructor(data_constructor, patterns) => {
            matched.simplify(names);
            match matched {
                Expression::Tree { root, arguments, .. } => {
                    let Expression::DataConstructor(id) = *root else { unreachable!() };
                    if id == *data_constructor {
                        for (pattern, arg) in patterns.iter().zip(arguments.into_iter()) {
                            match_on_expression_helper(output, pattern, arg, names)
                        }
                    }
                }
                Expression::DataConstructor(_) => (),
                _ => unreachable!(),
            }
        }
        Pattern::Either(x) => {
            let pat: &Pattern;
            let (first, second) = &**x;
            if matches_expression(first, &mut matched, names) {
                pat = first;
            } else {
                pat = second;
            }
            match_on_expression_helper(output, pat, matched, names);
        }
    }
}

impl Expression {
    #[inline]
    fn is_simplified(&self) -> bool {
        matches!(self, Expression::Tree {root, ..} 
            if matches!(&**root, Expression::DataConstructor(_))) ||
        matches!(self, Expression::Lambda { .. } | Expression::DataConstructor(_))
    }

    pub fn simplify(&mut self, names: &Vec<String>) {
        if self.is_simplified() { return }
        let mut is_simplified = false;
        'uwu: while !is_simplified {
            match std::mem::take(self) {
                Expression::Thunk {value: exp, mark} => match Arc::try_unwrap(exp) {
                    Ok(x) => *self = x.into_inner(),
                    Err(x) => {
                        let Ok(mut inner) = (*x).try_borrow_mut() else {
                            let error = make_error(RuntimeError::EvaluatedBottom, (*mark.unwrap()).clone());
                            eprintln!("{error}");
                            std::process::exit(1);
                        };
                        inner.simplify(names);
                        *self = inner.clone();
                        return
                    }
                }
                Expression::Tree { root, arguments } => {
                    let mut args = arguments.into_iter();
                    *self = *root;
                    while let Some(mut i) = args.next() {
                        self.simplify(names);
                        match self {
                            Expression::Tree { arguments, .. } => {
                                let mut new_args = Vec::with_capacity(arguments.len() + args.len() + 1);
                                new_args.extend(std::mem::take(arguments).into_iter());
                                new_args.push(i);
                                new_args.extend(args);
                                *arguments = new_args.into();
                                break 'uwu;
                            }
                            Expression::Lambda { pattern, body } => {
                                if !matches_expression(&pattern.value, &mut i, names) {
                                    let error = Error {
                                        error_type: Box::new(RuntimeError::UnmatchedPattern(
                                            std::mem::take(&mut i),
                                            names.clone()
                                        )),
                                        mark: pattern.mark.clone(),
                                        note: None,
                                    };
                                    eprintln!("{error}");
                                    std::process::exit(1);
                                }
                                let map = match_on_expression(&pattern.value, i, names);
                                body.substitute(&map);
                                *self = std::mem::take(&mut *body);
                            }
                            Expression::DataConstructor(_) => {
                                let mut new_args = Vec::with_capacity(args.len() + 1);
                                new_args.push(i);
                                new_args.extend(args);
                                *self = Expression::Tree {
                                    root: Box::new(std::mem::take(self)),
                                    arguments: new_args.into(),
                                };
                                break 'uwu
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                Expression::Match { mut matched_on, branches } => {
                    let mut blame = None;
                    'aaa: loop {   // goto
                        for (pat, mut new_expression) in branches.into_iter() {
                            if blame.is_none() {
                                blame = Some(pat.mark.clone());
                            }
                            if matches_expression(&(pat.value), &mut matched_on, names) {
                                let map = match_on_expression(&pat.value, *matched_on, names);
                                new_expression.substitute(&map);
                                *self = new_expression;
                                break 'aaa;
                            }
                        }
                        let error = Error {
                            error_type: Box::new(RuntimeError::UnmatchedPattern(
                                std::mem::take(&mut matched_on),
                                names.clone()
                            )),
                            mark: blame.unwrap(),
                            note: None,
                        };
                        eprintln!("{error}");
                        std::process::exit(1);
                    }
                }
                Expression::Undefined(mark) => {
                    let error = Error {
                        error_type: Box::new(RuntimeError::EvaluatedUndefined),
                        mark: (*mark).clone(),
                        note: None,
                    };
                    eprintln!("{error}");
                    std::process::exit(1);
                }
                x => {dbg!(x); unreachable!();}
            }
            is_simplified = self.is_simplified();
        }
        optimize_branches(self);
    }

    // Substitute every instance of a LocalVarPlaceholder with a Thunk. To be used with
    // patternmatching (match_on_expression) output
      
    pub fn substitute(&mut self, map: &Vec<(u32, Expression)>) {
        match self {
            Expression::Lambda { body, .. } => body.substitute(map),
            Expression::LocalVarPlaceholder(id) => {
                map
                    .iter()
                    .find(|(k, _)| k == id)
                    .map(|(_, v)| *self = v.clone());
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
            Expression::Undefined { .. } | Expression::Thunk { .. } | Expression::DataConstructor(_) => (),
            Expression::CompilerBug => unreachable!()
        }
    }

    // The print function is a combination of evaluate_strictly and 
    // convert_to_file. It exists to eat much less memory while evaluating large 
    // structures. It's also able to print some infinitely large structures
    // without eating memory at all.
    
    pub fn print(self, names: &Vec<String>, tp: &Type) {
        let mut more = Command::new("more")
            .arg("-n")
            .arg("3")
            .stdin(Stdio::piped())
            .spawn()
            .unwrap();
        let stdin = more.stdin.take().unwrap();
        let mut writer = BufWriter::new(stdin);
        write!(writer, "the {}", tp.show()).unwrap();
        let mut to_evaluate: Vec<Expression> = vec![self];
        while let Some(mut x) = to_evaluate.pop() {
            x.simplify(names);
            match x {
                Expression::Tree { root, arguments, .. } => {
                    to_evaluate.extend(arguments.into_iter().rev());
                    to_evaluate.push(*root);
                }
                Expression::DataConstructor(id) => {
                    let word = &names[id as usize];
                    if let Err(_) = write!(writer, "{word} ") { 
                        return
                    };
                }
                Expression::Lambda { .. } => {
                    if let Err(_) = write!(writer, "<lambda> ") {
                        return
                    };
                }
                _ => unreachable!(),
            }
        }
        if let Err(_) = write!(writer, "\n") {
            return
        }
        drop(writer);
        more.wait().unwrap();
    }
}

fn build_thunk(mut input: Expression) -> Expression {
    optimize_expression(&mut input);
    match &mut input {
        Expression::Tree { .. } | Expression::Match { .. } | Expression::Lambda { .. } =>
            Expression::Thunk {
                value: Arc::new(RefCell::new(input)),
                mark: None,
            },
        _ => input
    }
}

fn optimize_branches(input: &mut Expression) {
    match input {
        Expression::Tree { arguments, .. } => {
            arguments.iter_mut().for_each(optimize_expression);
        }
        Expression::Lambda { pattern, body } if matches!(pattern.value, Pattern::Dropped) => 
            optimize_expression(body),
        _ => (),
    }
}

pub fn optimize_expression(input: &mut Expression) {
    match input {
        Expression::Tree { root: _, arguments} => {
            arguments.iter_mut().for_each(optimize_expression);
            *input = Expression::Thunk {
                value: Arc::new(RefCell::new(std::mem::take(input))),
                mark: None,
            }
        }
        Expression::Match { .. } => {
            // no point in optimizing anything since the simplified output will 
            // be optimized later
            *input = Expression::Thunk {
                value: Arc::new(RefCell::new(std::mem::take(input))),
                mark: None,
            }
        },
        Expression::Lambda { pattern, body } if matches!(pattern.value, Pattern::Dropped) => 
            optimize_expression(body),
        Expression::Undefined { .. } => (),
        _ => (),
    }
}
