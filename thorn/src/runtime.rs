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

use crate::error::{Error, ErrorType, Mark, Marked, make_error};
use std::{
    cell::RefCell,
    io::{BufWriter, Write},
    process::{Command, Stdio},
    rc::Rc,
};

use crate::parse::Type;

type MarkIndex = u16;
type PatternIndex = u16;

// pretty bad

#[derive(Debug, Clone, Default)]
pub enum Expression {
    Tree {
        root: Box<Expression>,
        arguments: Box<[Expression]>,
    },
    Match {
        matched_on: Box<Expression>,
        mark: MarkIndex,
        branches: Box<[(PatternIndex, Expression)]>,
    },
    Lambda {
        pattern: PatternIndex,
        mark: MarkIndex,
        body: Box<Expression>,
    },
    Thunk {
        value: Rc<RefCell<Expression>>,
        mark: Option<MarkIndex>, // mark in case the value is a bottom
    },
    Undefined(MarkIndex),
    LocalVarPlaceholder(u32),
    DataConstructor(u32),
    // used for Expression::default(). If encountered,
    // it's a compiler bug somewhere
    #[default]
    CompilerBug,
}

#[allow(unused)]
fn debug_expression(expr: &Expression, names: &Vec<String>, patterns: &Vec<Pattern>) {
    debug_expression_helper(expr, 0, names, patterns);
}

#[allow(unused)]
fn debug_expression_helper(
    expr: &Expression,
    level: usize,
    names: &Vec<String>,
    patterns: &Vec<Pattern>,
) {
    let mut out = "    ".repeat(level);
    match expr {
        Expression::Tree { root, arguments } => {
            debug_expression_helper(root, level, names, patterns);
            for i in arguments.iter() {
                debug_expression_helper(i, level + 1, names, patterns);
            }
        }
        Expression::Lambda { pattern, body, .. } => {
            out.push_str("lambda");
            eprintln!("{}", &out);
            debug_pattern(*pattern, level + 1, names, patterns);
            debug_expression_helper(body, level + 1, names, patterns);
        }
        Expression::Match {
            matched_on,
            mark,
            branches,
        } => {
            out.push_str("match");
            eprintln!("{}", &out);
            debug_expression_helper(matched_on, level + 1, names, patterns);
            out = "    ".repeat(level + 1);
            out.push_str("case");
            for (pattern, expr) in branches {
                eprintln!("{}", &out);
                debug_pattern(*pattern, level + 2, names, patterns);
                debug_expression_helper(expr, level + 2, names, patterns);
            }
        }
        Expression::Thunk { .. } => {
            out.push_str("<thunk>");
            eprintln!("{}", &out);
        }
        Expression::Undefined { .. } => {
            out.push_str("undefined");
            eprintln!("{}", &out);
        }
        Expression::LocalVarPlaceholder(n) => {
            let s = n.to_string();
            out.push_str("local ");
            out.push_str(&s);
            eprintln!("{}", &out);
        }
        Expression::DataConstructor(n) => {
            out.push_str(&names[*n as usize]);
            eprintln!("{}", &out);
        }
        Expression::CompilerBug => {
            eprintln!("f");
            panic!();
        }
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
            Self::UnmatchedPattern { .. } => "does not cover all patterns",
            Self::EvaluatedUndefined => "entered undefined code",
            Self::EvaluatedBottom => "evaluated a bottom _|_",
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
            ),
        }
    }
}

#[allow(unused)]
fn debug_print(expr: &Expression, names: &Vec<String>) -> String {
    match expr {
        Expression::Tree { root, arguments } => {
            let mut s = debug_print(root, names);
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
    DataConstructor(u32, Vec<PatternIndex>),
    Bound(u32, PatternIndex),
    Either(Vec<PatternIndex>),
}

#[allow(unused)]
fn debug_pattern(
    pattern: PatternIndex,
    level: usize,
    names: &Vec<String>,
    patterns: &Vec<Pattern>,
) {
    let mut out = "    ".repeat(level);
    match &patterns[pattern as usize] {
        Pattern::Dropped => {
            out.push('_');
            eprintln!("{}", out);
        }
        Pattern::Captured(n) => {
            let s = n.to_string();
            out.push_str("local ");
            out.push_str(&s);
            eprintln!("{}", out);
        }
        Pattern::DataConstructor(n, v) => {
            out.push_str(&names[*n as usize]);
            eprintln!("{}", out);
            for i in v.iter() {
                debug_pattern(*i, level + 1, names, patterns);
            }
        }
        Pattern::Either(v) => {
            out.push_str("either");
            eprintln!("{}", out);
            for i in v.iter() {
                debug_pattern(*i, level + 1, names, patterns);
            }
        }
        Pattern::Bound(n, v) => {
            out.push_str("bind local ");
            let s = n.to_string();
            out.push_str(&s);
            eprintln!("{}", out);
            debug_pattern(*v, level + 1, names, patterns);
        }
    }
}

fn matches_expression(
    pattern: PatternIndex,
    matched: &mut Expression,
    names: &Vec<String>,
    marks: &Vec<Mark>,
    patterns: &Vec<Pattern>,
) -> bool {
    match &patterns[pattern as usize] {
        Pattern::Dropped => true,
        Pattern::Captured(_) => true,
        Pattern::Bound(_, pat) => matches_expression(*pat, matched, names, marks, patterns),
        Pattern::DataConstructor(data_constructor, arg_patterns) => {
            matched.simplify(names, marks, patterns);
            match matched {
                Expression::Tree {
                    root, arguments, ..
                } => {
                    let Expression::DataConstructor(id) = **root else {
                        unreachable!()
                    };
                    id == *data_constructor && {
                        for (pattern, arg) in arg_patterns.iter().zip(arguments.iter_mut()) {
                            if !matches_expression(*pattern, arg, names, marks, patterns) {
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
        Pattern::Either(x) => x
            .iter()
            .any(|x| matches_expression(*x, matched, names, marks, patterns)),
    }
}

fn match_on_expression(
    pattern: PatternIndex,
    matched: Expression,
    names: &Vec<String>,
    marks: &Vec<Mark>,
    patterns: &Vec<Pattern>,
) -> Vec<(u32, Expression)> {
    let mut output = Vec::new();
    match_on_expression_helper(&mut output, pattern, matched, names, marks, patterns);
    output
}

fn match_on_expression_helper(
    output: &mut Vec<(u32, Expression)>,
    pattern: PatternIndex,
    mut matched: Expression,
    names: &Vec<String>,
    marks: &Vec<Mark>,
    patterns: &Vec<Pattern>,
) {
    match &patterns[pattern as usize] {
        Pattern::Dropped => (),
        Pattern::Bound(id, pat) => {
            let thunk = build_thunk(matched);
            output.push((*id, thunk.clone()));
            match_on_expression_helper(output, *pat, thunk, names, marks, patterns);
        }
        Pattern::Captured(id) => {
            output.push((*id, build_thunk(matched)));
        }
        Pattern::DataConstructor(data_constructor, arg_patterns) => {
            matched.simplify(names, marks, patterns);
            match matched {
                Expression::Tree {
                    root, arguments, ..
                } => {
                    let Expression::DataConstructor(id) = *root else {
                        unreachable!()
                    };
                    if id == *data_constructor {
                        for (pattern, arg) in arg_patterns.iter().zip(arguments) {
                            match_on_expression_helper(
                                output, *pattern, arg, names, marks, patterns,
                            )
                        }
                    }
                }
                Expression::DataConstructor(_) => (),
                _ => unreachable!(),
            }
        }
        Pattern::Either(x) => {
            let pat = x
                .iter()
                .find(|x| matches_expression(**x, &mut matched, names, marks, patterns))
                .unwrap();
            match_on_expression_helper(output, *pat, matched, names, marks, patterns);
        }
    }
}

impl Expression {
    #[inline]
    fn is_simplified(&self) -> bool {
        matches!(self, Expression::Tree {root, ..}
            if matches!(&**root, Expression::DataConstructor(_)))
            || matches!(
                self,
                Expression::Lambda { .. } | Expression::DataConstructor(_)
            )
    }

    pub fn simplify(&mut self, names: &Vec<String>, marks: &Vec<Mark>, patterns: &Vec<Pattern>) {
        if self.is_simplified() {
            return;
        }
        let mut is_simplified = false;
        'uwu: while !is_simplified {
            // debug_expression(&self, names);
            match std::mem::take(self) {
                Expression::Thunk { value: exp, mark } => match Rc::try_unwrap(exp) {
                    Ok(x) => *self = x.into_inner(),
                    Err(x) => {
                        let Ok(mut inner) = (*x).try_borrow_mut() else {
                            let error = make_error(
                                RuntimeError::EvaluatedBottom,
                                marks[mark.unwrap() as usize].clone(),
                            );
                            eprintln!("{error}");
                            std::process::exit(1);
                        };
                        inner.simplify(names, marks, patterns);
                        *self = inner.clone();
                        return;
                    }
                },
                Expression::Tree { root, arguments } => {
                    let mut args = arguments.into_iter();
                    *self = *root;
                    while let Some(mut i) = args.next() {
                        self.simplify(names, marks, patterns);
                        match self {
                            Expression::Tree { arguments, .. } => {
                                let mut new_args =
                                    Vec::with_capacity(arguments.len() + args.len() + 1);
                                new_args.extend(std::mem::take(arguments));
                                new_args.push(i);
                                new_args.extend(args);
                                *arguments = new_args.into();
                                break 'uwu;
                            }
                            Expression::Lambda {
                                pattern,
                                mark,
                                body,
                            } => {
                                if !matches_expression(*pattern, &mut i, names, marks, patterns) {
                                    let error = Error {
                                        error_type: Box::new(RuntimeError::UnmatchedPattern(
                                            std::mem::take(&mut i),
                                            names.clone(),
                                        )),
                                        mark: marks[*mark as usize].clone(),
                                        note: None,
                                    };
                                    eprintln!("{error}");
                                    std::process::exit(1);
                                }
                                let map = match_on_expression(*pattern, i, names, marks, patterns);
                                // map of thunks!
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
                                break 'uwu;
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                Expression::Match {
                    mut matched_on,
                    mark,
                    branches,
                } => {
                    'error: {
                        // goto
                        for (pat, mut new_expression) in branches.into_iter() {
                            if matches_expression(pat, &mut matched_on, names, marks, patterns) {
                                let map =
                                    match_on_expression(pat, *matched_on, names, marks, patterns);
                                new_expression.substitute(&map);
                                *self = new_expression;
                                break 'error;
                            }
                        }
                        let error = Error {
                            error_type: Box::new(RuntimeError::UnmatchedPattern(
                                std::mem::take(&mut matched_on),
                                names.clone(),
                            )),
                            mark: marks[mark as usize].clone(),
                            note: None,
                        };
                        eprintln!("{error}");
                        std::process::exit(1);
                    }
                }
                Expression::Undefined(mark) => {
                    let error = Error {
                        error_type: Box::new(RuntimeError::EvaluatedUndefined),
                        mark: marks[mark as usize].clone(),
                        note: None,
                    };
                    eprintln!("{error}");
                    std::process::exit(1);
                }
                x => {
                    dbg!(x);
                    unreachable!();
                }
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
                if let Some((_, v)) = map.iter().find(|(k, _)| k == id) {
                    *self = v.clone();
                }
            }
            Expression::Tree {
                root, arguments, ..
            } => {
                root.substitute(map);
                arguments.iter_mut().for_each(|i| i.substitute(map));
            }
            Expression::Match {
                matched_on,
                mark,
                branches,
            } => {
                branches.iter_mut().for_each(|(_, i)| i.substitute(map));
                matched_on.substitute(map);
            }
            Expression::Undefined { .. }
            | Expression::Thunk { .. }
            | Expression::DataConstructor(_) => (),
            Expression::CompilerBug => unreachable!(),
        }
    }

    // The print function is a combination of evaluate_strictly and
    // convert_to_file. It exists to eat much less memory while evaluating large
    // structures. It's also able to print some infinitely large structures
    // without eating memory at all.

    pub fn print(self, names: &Vec<String>, tp: &Type, marks: &Vec<Mark>, patterns: &Vec<Pattern>) {
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
            x.simplify(names, marks, patterns);
            match x {
                Expression::Tree {
                    root, arguments, ..
                } => {
                    to_evaluate.extend(arguments.into_iter().rev());
                    to_evaluate.push(*root);
                }
                Expression::DataConstructor(id) => {
                    let word = &names[id as usize];
                    if write!(writer, "{word} ").is_err() {
                        more.wait().unwrap();
                        return;
                    };
                }
                Expression::Lambda { .. } => {
                    if write!(writer, "<lambda> ").is_err() {
                        more.wait().unwrap();
                        return;
                    };
                }
                _ => unreachable!(),
            }
        }
        if writeln!(writer).is_err() {
            more.wait().unwrap();
            return;
        }
        drop(writer);
        more.wait().unwrap();
    }
}

fn build_thunk(mut input: Expression) -> Expression {
    optimize_expression(&mut input);
    match &mut input {
        Expression::Tree { .. } | Expression::Match { .. } | Expression::Lambda { .. } => {
            Expression::Thunk {
                value: Rc::new(RefCell::new(input)),
                mark: None,
            }
        }
        _ => input,
    }
}

fn optimize_branches(input: &mut Expression) {
    match input {
        Expression::Tree { arguments, .. } => {
            arguments.iter_mut().for_each(optimize_expression);
        }
        Expression::Lambda { pattern, body, .. } if *pattern == 0 => optimize_expression(body),
        _ => (),
    }
}

pub fn optimize_expression(input: &mut Expression) {
    match input {
        Expression::Tree { root: _, arguments } => {
            arguments.iter_mut().for_each(optimize_expression);
            *input = Expression::Thunk {
                value: Rc::new(RefCell::new(std::mem::take(input))),
                mark: None,
            }
        }
        Expression::Match { .. } => {
            // no point in optimizing anything since the simplified output will
            // be optimized later
            *input = Expression::Thunk {
                value: Rc::new(RefCell::new(std::mem::take(input))),
                mark: None,
            }
        }
        Expression::Lambda { pattern, body, .. } if *pattern == 0 => optimize_expression(body),
        Expression::Undefined { .. } => (),
        _ => (),
    }
}
