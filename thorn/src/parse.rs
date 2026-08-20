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

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::rc::Rc;
use std::sync::Mutex; // TODO use for passing in type parse_expression

use crate::error::{DEBUG_INFO, Error, ErrorType, Mark, Marked, Result, get_file_name, make_error};
use crate::runtime::{Expression, Pattern};
use crate::tokens::*;

pub static TYPES: Mutex<Option<GlobalTypes>> = Mutex::new(None);

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum CompilationError {
    CannotInferType,
    TypeNotInScope(String),
    NotUsed,
    EitherMismatch,
    ExpectedMoreArguments,
    NotInScope(String, Option<u16>),
    TypeMismatch(Type, Option<Type>),
    // BadTypeInference(Type, Type),
    BadFile(String),
    MultipleDeclarations(u16),
    // TypeAnnotationNeeded,
    RedundentPattern,
}

impl ErrorType for CompilationError {
    fn gist(&self) -> &'static str {
        match self {
            Self::CannotInferType => "can't infer type",
            // Self::TypeAnnotationNeeded    => "type annotation needed",
            // Self::BadTypeInference(_, _)  => "of unexpected type",
            Self::NotUsed => "local variable never used",
            Self::EitherMismatch => "mismatch between branches",
            Self::MultipleDeclarations(_) => "multiple declarations",
            //Self::PartialPattern => "not all patterns covered",
            Self::RedundentPattern => "redundent pattern",
            Self::ExpectedMoreArguments => "expected more arguments",
            Self::NotInScope(_, _) => "not in scope",
            Self::TypeNotInScope(_) => "type not in scope",
            Self::TypeMismatch(_, _) => "of unexpected type",
            Self::BadFile(_) => "couldn't find file",
        }
    }

    fn phase(&self) -> &'static str {
        "COMPILATION"
    }
}

impl std::fmt::Display for CompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Self::BadTypeInference(expected, got) => write!(
            //     f,
            //     "expected a value of type \x1b[97m{}\x1b[90m\nbut the given type is \x1b[97m{}\x1b[90m",
            //     expected.show(),
            //     got.show()
            // ),
            // Self::TypeAnnotationNeeded => write!(f, "consider adding a type annotation with the \x1b[97mthe\x1b[90m keyword"),
            Self::EitherMismatch => write!(
                f,
                "the two patterns in the \x1b[97meither\x1b[90m pattern must have
the same variables and of the same type"
            ),
            Self::MultipleDeclarations(s) => write!(
                f,
                "name already used in \x1b[97m{}\x1b[90m",
                get_file_name(*s)
            ),
            Self::NotUsed => write!(
                f,
                "consider prepending it with an \x1b[97m_\x1b[90m to drop the value"
            ),
            Self::RedundentPattern => write!(
                f,
                "this branch will never be reached because the
branch above is a wildcard that matches everything"
            ),
            Self::BadFile(s) => write!(f, "unable to find \x1b[97m{s}\x1b[90m in this project"),
            Self::NotInScope(x, hint) => write!(
                f,
                "variable \x1b[97m{x}\x1b[90m not in scope{}",
                match hint {
                    None => String::new(),
                    Some(name) => {
                        let file_name = get_file_name(*name);
                        format!(
                            ",
however it's defined in {}
consider including it with \x1b[97minclude {}\x1b[90m",
                            get_file_name(*name),
                            extract_file_name(&file_name)
                        )
                    }
                }
            ),
            Self::TypeNotInScope(x) => write!(f, "type \x1b[97m{x}\x1b[90m not in scope"),
            Self::TypeMismatch(tp1, tp2) => write!(
                f,
                "expected a value of type \x1b[97m{}\x1b[90mhowever this value can never evaluate to it{}",
                tp1.show(),
                if let Some(tp2) = tp2 {
                    format!(".\nit is of type \x1b[97m{}\x1b[90m", tp2.show())
                } else {
                    String::new()
                }
            ),
            _ => write!(f, "todo"),
        }
    }
}

pub struct LoadedExpressions {
    pub info_table: HashMap<String, GlobalVarData>,
    pub expressions: Vec<Expression>,
    pub var_names: Vec<String>,
    pub patterns: Vec<Pattern>,
    pub marks: Vec<Mark>,
}

type GlobalTypes = HashMap<String, GlobalTypeData>;

#[derive(Default, Clone)]
pub struct LocalVars<'a> {
    pub vars: Vec<(&'a str, Type, &'a Mark)>,
}

impl<'a> LocalVars<'a> {
    fn get_name(&'a self, key: &str) -> Option<(u32, &'a str, &'a Type, &'a Mark)> {
        let (n, (a, b, c)) = self
            .vars
            .iter()
            .enumerate()
            .rev()
            .find(|(_, (s, _, _))| *s == key)?;
        Some((n as u32, a, b, c))
    }
}

pub type Generics = Vec<(String, usize)>;

#[derive(Clone)]
pub struct GlobalVarData {
    pub mark: u16,
    pub var_type: Type,
    pub generics: Generics,
    pub id: Id,
}

#[derive(Hash, Clone)]
pub struct GlobalTypeData {
    pub mark: Mark,
    pub kind: Kind,
    pub generics: Generics,
    pub id: usize,
}

#[derive(Clone)]
pub enum Id {
    Variable(usize),
    Constructor(usize),
}

type Globals = HashMap<String, GlobalVarData>;

pub fn get_everything() -> Result<LoadedExpressions> {
    let output = std::process::Command::new("sh")
        .arg("-c")
        .arg("find -L . -maxdepth 5 | grep '\\.th$'")
        .output()
        .unwrap();
    let files = String::from_utf8_lossy(&output.stdout);
    let files: Vec<String> = files.split_whitespace().map(|x| x.to_string()).collect();
    {
        let mut ptr = DEBUG_INFO.lock().unwrap();
        ptr.files = files.clone();
    }
    uwu(&files)
}

struct Dependencies {
    files: HashMap<u16, HashSet<u16>>, // filename, its file dependencies
}

impl Dependencies {
    fn available_files(&mut self, file: u16) -> HashSet<u16> {
        let mut visited_files: HashSet<u16> = HashSet::from([file]);
        let mut to_visit: Vec<u16> = vec![file];
        while let Some(x) = to_visit.pop() {
            self.files
                .get(&x)
                .unwrap()
                .difference(&visited_files)
                .copied()
                .collect::<Vec<u16>>()
                .into_iter()
                .for_each(|i| {
                    to_visit.push(i);
                    visited_files.insert(i);
                });
        }
        visited_files
    }
}

fn get_includes(
    blocks: &mut Vec<Block>,
    file_names: &HashMap<String, (&str, u16)>,
) -> Result<HashSet<u16>> {
    let elem = BlockTraversal::new(&blocks[0]);
    let mut output = HashSet::new();
    if let Ok((_include_mark, mut bt)) = elem.expect_keyword(Keyword::Include) {
        while let Ok((x, mark, bt_)) = bt.expect_word() {
            bt = bt_;
            let Some((_, key)) = file_names.get(x) else {
                return Err(make_error(
                    CompilationError::BadFile(x.to_string()),
                    mark.clone(),
                ));
            };
            output.insert(*key);
        }
        blocks.remove(0);
    }
    Ok(output)
}

fn extract_file_name(file_name: &str) -> String {
    file_name
        .chars()
        .rev()
        .skip(3)
        .take_while(|&x| x != '/')
        .collect::<String>()
        .chars()
        .rev()
        .collect()
}

pub fn kind_from_generics(count: u32) -> Kind {
    if count == 0 {
        Kind::Type
    } else {
        Kind::Fn(
            Box::new(Kind::Type),
            Box::new(kind_from_generics(count - 1)),
        )
    }
}

fn uwu(files: &[String]) -> Result<LoadedExpressions> {
    let mut loaded = LoadedExpressions {
        info_table: HashMap::new(),
        expressions: Vec::new(),
        var_names: Vec::new(),
        patterns: vec![Pattern::Dropped],
        marks: Vec::new(),
    };
    let mut file_names: HashMap<String, (&str, u16)> = HashMap::new();
    for (count, i) in files.iter().enumerate() {
        file_names.insert(extract_file_name(i), (i, count as u16));
    }
    // Step 1: find all the types
    // we need types before we can parse data constructors and
    // variable types so we'll do those later
    let mut type_bodies: Vec<BlockTraversal> = Vec::new();
    let mut var_bodies: Vec<BlockTraversal> = Vec::new();
    let mut type_table: HashMap<String, GlobalTypeData> = HashMap::new();
    type_table.insert(
        String::from("fn"),
        GlobalTypeData {
            mark: Mark::default(),
            kind: Kind::Fn(Box::new(Kind::Type), Box::new(Kind::Type)),
            id: 0,
            generics: Vec::new(),
        },
    );
    let mut var_table: HashMap<String, (Mark, usize, Generics)> = HashMap::new();
    let mut file_tokens: Vec<(&str, Vec<Block>)> = Vec::new();
    let mut dependencies: Dependencies = Dependencies {
        files: HashMap::new(),
    };
    let mut file_strings: Vec<String> = Vec::new();
    for file_name in files.iter() {
        let contents = read_to_string(file_name).unwrap();
        file_strings.push(contents)
    }
    for (index, file_name) in files.iter().enumerate() {
        let contents = &file_strings[index];
        let mut blocks = tokenize_file(contents, index as u16)?;
        let deps = get_includes(&mut blocks, &file_names)?;
        dependencies.files.insert(index as u16, deps);
        file_tokens.push((file_name, blocks));
    }
    for (_file_name, blocks) in file_tokens.iter() {
        for block in blocks.iter() {
            let bt = BlockTraversal::new(block);
            let (
                NameAndGenerics {
                    name,
                    mark,
                    generics,
                    kind,
                },
                bt,
            ) = extract_name_and_generics(bt)?;
            match kind {
                BlockKind::Variable => {
                    if let Some(x) =
                        var_table.insert(name, (mark.clone(), var_bodies.len(), generics))
                    {
                        return Err(make_error(
                            CompilationError::MultipleDeclarations(x.0.file),
                            mark,
                        ));
                    }
                    var_bodies.push(bt);
                }
                BlockKind::Type => {
                    if let Some(x) = type_table.get(&name) {
                        return Err(make_error(
                            CompilationError::MultipleDeclarations(x.mark.file),
                            mark,
                        ));
                    }
                    type_table.insert(
                        name,
                        GlobalTypeData {
                            mark: mark.clone(),
                            id: type_table.len(),
                            kind: kind_from_generics(generics.len() as u32),
                            generics,
                        },
                    );
                    type_bodies.push(bt);
                }
            }
        }
    }
    {
        let mut ptr = TYPES.lock().unwrap();
        *ptr = Some(type_table.clone());
    }
    // Step 2: parse the types of constructors and variables
    for (name, (mark, index, generics)) in var_table.into_iter() {
        loaded.marks.push(mark);
        let (var_type, bt) = parse_the(var_bodies[index], &generics)?;
        loaded.info_table.insert(
            name,
            GlobalVarData {
                var_type,
                mark: loaded.marks.len() as u16 - 1,
                id: Id::Variable(index),
                generics,
            },
        );
        var_bodies[index] = bt;
    }
    for (
        _name,
        GlobalTypeData {
            mark: _,
            id: index,
            generics,
            kind: _,
        },
    ) in type_table.into_iter()
    {
        if index != 0 {
            let branches = parse_data(type_bodies[index - 1], index as u32, &generics)?;
            for (name, tp, mark) in branches.into_iter() {
                loaded.marks.push(mark.clone());
                if let Some(x) = loaded.info_table.insert(
                    name,
                    GlobalVarData {
                        mark: loaded.marks.len() as u16 - 1,
                        id: Id::Constructor(loaded.info_table.len()),
                        generics: generics.clone(),
                        var_type: tp,
                    },
                ) {
                    return Err(make_error(
                        CompilationError::MultipleDeclarations(loaded.marks[x.mark as usize].file),
                        mark,
                    ));
                };
            }
        }
    }
    for _ in 0..loaded.info_table.len() {
        loaded.expressions.push(Expression::default());
    }
    for (_, GlobalVarData { mark, id, .. }) in loaded.info_table.iter() {
        match id {
            Id::Constructor(n) => loaded.expressions[*n] = Expression::DataConstructor(*n as u32),
            Id::Variable(n) => {
                loaded.expressions[*n] = Expression::Thunk {
                    value: Rc::new(RefCell::new(Expression::default())),
                    mark: Some(*mark),
                }
            }
        }
    }
    for _ in 0..loaded.info_table.len() {
        loaded.var_names.push(String::new());
    }

    for (
        name,
        GlobalVarData {
            mark,
            var_type,
            id,
            generics,
        },
    ) in loaded.info_table.iter()
    {
        let (Id::Variable(index) | Id::Constructor(index)) = id;
        loaded.var_names[*index] = name.clone();
    }
    // step 3: parse the expressions
    for index in 0..loaded.var_names.len() {
        let GlobalVarData {
            mark,
            var_type,
            id,
            generics,
        } = loaded.info_table.get(&loaded.var_names[index]).unwrap();
        let t = var_type.clone();
        let g = generics.clone();
        let Id::Variable(index) = id.clone() else {
            continue;
        };
        let mut temp_local_vars = LocalVars { vars: Vec::new() };
        let available_files = dependencies.available_files(loaded.marks[*mark as usize].file);
        let (expression, bt) = parse_expression(
            &mut loaded,
            &available_files,
            &t,
            var_bodies[index],
            &mut temp_local_vars,
            &g,
        )?;
        BlockTraversal::expect_end_option(bt)?;
        {
            let Expression::Thunk { value: ref x, .. } = loaded.expressions[index] else {
                unreachable!()
            };
            let Ok(mut inner) = (*x).try_borrow_mut() else {
                unreachable!()
            };
            *inner = expression;
        }
    }
    Ok(loaded)
}

fn parse_pattern<'a>(
    local_vars: &mut LocalVars<'a>,
    expected_type: &Type,
    bt: BlockTraversal<'a>,
    global_vars: &Globals,
    patterns: &mut Vec<Pattern>,
) -> Result<(u16, Mark, Option<BlockTraversal<'a>>)> {
    let mark = bt.next_token_in_line()?.1.clone();
    let (pattern, bt) = parse_pattern_helper(local_vars, expected_type, bt, global_vars, patterns)?;
    Ok((pattern, mark, bt))
}

// asserts that the two maps are equal and makes their ids equivelent

fn make_maps_equal(
    either_mark: &Mark,
    fst: &LocalVars,
    snd: &LocalVars,
    pattern: u16,
    patterns: &mut Vec<Pattern>,
) -> Result<()> {
    let mut ret = Vec::new();
    if fst.vars.len() != snd.vars.len() {
        return Err(make_error(
            CompilationError::EitherMismatch,
            either_mark.clone(),
        ));
    };
    for (id, (name, tp, mark)) in fst.vars.iter().enumerate() {
        if let Some((new_id, _, new_tp, _new_mark)) = snd.get_name(name)
            && new_tp == tp
        {
            ret.push((new_id, id as u32))
        } else {
            return Err(make_error(
                CompilationError::EitherMismatch,
                (*mark).clone(),
            ));
        }
    }
    substitute_pattern(pattern, patterns, &ret);
    Ok(())
}

fn change_pattern_ids(f: impl Fn(u32) -> u32 + Clone, pattern: u16, patterns: &mut Vec<Pattern>) {
    match &mut patterns[pattern as usize] {
        Pattern::Dropped => (),
        Pattern::Captured(n) => {
            *n = f(*n);
        }
        Pattern::DataConstructor(_, v) | Pattern::Either(v) => v
            .clone()
            .into_iter()
            .for_each(|i| change_pattern_ids(f.clone(), i, patterns)),
        Pattern::Bound(n, v) => {
            *n = f(*n);
            change_pattern_ids(f.clone(), *v, patterns);
        }
    }
}

fn substitute_pattern(pattern: u16, patterns: &mut Vec<Pattern>, to_replace: &[(u32, u32)]) {
    change_pattern_ids(
        |x| {
            if let Some((_, new)) = to_replace.iter().find(|(a, _)| *a == x) {
                *new
            } else {
                x
            }
        },
        pattern,
        patterns,
    );
}

fn increment_pattern_vars(pattern: u16, patterns: &mut Vec<Pattern>, inc: u32) {
    change_pattern_ids(|x| x + inc, pattern, patterns);
}

fn parse_pattern_helper<'a>(
    local_vars: &mut LocalVars<'a>,
    expected_type: &Type,
    bt: BlockTraversal<'a>,
    global_vars: &Globals,
    patterns: &mut Vec<Pattern>,
) -> Result<(u16, Option<BlockTraversal<'a>>)> {
    let (token, root_mark, bt) = bt.next_token_in_line()?;
    match token {
        Token::Keyword(Keyword::Bind) => {
            let (name, mark, bt) = bt.expect_word()?;
            assert!(!name.starts_with('_')); // TODO
            let number_of_local_copy = local_vars.vars.len() as u32;
            local_vars.vars.push((name, expected_type.clone(), mark));
            let (pat, bt) =
                parse_pattern_helper(local_vars, expected_type, bt, global_vars, patterns)?;
            let pat = Pattern::Bound(number_of_local_copy, pat);
            patterns.push(pat);
            Ok((patterns.len() as u16 - 1, bt))
        }
        Token::Keyword(Keyword::Either) => {
            let mut ret_bt: Option<BlockTraversal> = Some(bt);
            let mut either_patterns: Vec<(u16, LocalVars)> = Vec::new();
            if let NextOutput::IndentedBlocks(branches) = bt.next()? {
                ret_bt = None;
                assert!(branches.len() >= 2);
                for branch in branches.iter().map(BlockTraversal::new) {
                    let mut vars = LocalVars { vars: Vec::new() };
                    let (pat, bt) = parse_pattern_helper(
                        &mut vars,
                        expected_type,
                        branch,
                        global_vars,
                        patterns,
                    )?;
                    BlockTraversal::expect_end_option(bt)?;
                    either_patterns.push((pat, vars));
                }
            } else {
                let mut temp_bt = bt;
                for i in 0..2 {
                    let mut vars = LocalVars { vars: Vec::new() };
                    let (pat, bt) = parse_pattern_helper(
                        &mut vars,
                        expected_type,
                        temp_bt,
                        global_vars,
                        patterns,
                    )?;
                    ret_bt = bt;
                    either_patterns.push((pat, vars));
                    if i == 1 {
                        break;
                    } else {
                        let mark = temp_bt.next_token_in_line()?.1;
                        temp_bt = BlockTraversal::expect_no_indents(bt, mark)?;
                    }
                }
            }
            let old_vars_len = local_vars.vars.len() as u32;
            let old_vars = either_patterns[0].1.clone();
            for pattern in either_patterns.iter_mut().skip(1) {
                let new_vars = std::mem::take(&mut pattern.1);
                make_maps_equal(root_mark, &old_vars, &new_vars, pattern.0, patterns)?;
                if old_vars_len != 0 {
                    increment_pattern_vars(pattern.0, patterns, old_vars_len)
                }
            }
            if old_vars_len != 0 {
                increment_pattern_vars(either_patterns[0].0, patterns, old_vars_len)
            }
            local_vars
                .vars
                .extend(std::mem::take(&mut either_patterns[0].1.vars));
            let pat = Pattern::Either(either_patterns.into_iter().map(|x| x.0).collect());
            patterns.push(pat);
            Ok((patterns.len() as u16 - 1, ret_bt))
        }
        Token::Keyword(_) => Err(make_error(ParseError::UnexpectedKeyword, root_mark.clone())),
        Token::Word(name) if name.starts_with('_') => Ok((0, Some(bt))),
        Token::Word(name) => {
            if let Some(GlobalVarData {
                id: Id::Constructor(index),
                var_type: tp,
                generics: _new_generics,
                ..
            }) = global_vars.get(name)
                && let Type::Type {
                    type_constructor: tc1,
                    ..
                } = tp.final_type()
                && let Type::Type {
                    type_constructor: tc2,
                    ..
                } = expected_type
                && *tc1 == *tc2
            {
                let mut constructor_patterns = Vec::new();
                let ts = get_type_from_constructor(tp.clone(), expected_type.clone(), root_mark)?
                    .arg_types();
                let mut ret_bt = Some(bt);
                if !ts.is_empty() {
                    match bt.next_expecting_count(ts.len())? {
                        NextOutput::Token(_, mark, _) => {
                            let mut temp_bt = Some(bt);
                            for t in ts {
                                let (pat, bt) = parse_pattern_helper(
                                    local_vars,
                                    &t,
                                    BlockTraversal::expect_no_indents(temp_bt, mark)?,
                                    global_vars,
                                    patterns,
                                )?;
                                temp_bt = bt;
                                constructor_patterns.push(pat);
                            }
                            ret_bt = temp_bt;
                        }
                        NextOutput::IndentedBlocks(v) => {
                            ret_bt = None;
                            for (t, branch) in ts.iter().zip(v.iter().map(BlockTraversal::new)) {
                                let (pat, bt) = parse_pattern_helper(
                                    local_vars,
                                    t,
                                    branch,
                                    global_vars,
                                    patterns,
                                )?;
                                BlockTraversal::expect_end_option(bt)?;
                                constructor_patterns.push(pat);
                            }
                        }
                    }
                }
                let pat = Pattern::DataConstructor(*index as u32, constructor_patterns);
                patterns.push(pat);
                Ok((patterns.len() as u16 - 1, ret_bt))
            } else {
                let id = local_vars.vars.len() as u32;
                local_vars
                    .vars
                    .push((name, expected_type.clone(), root_mark));
                let pat = Pattern::Captured(id);
                patterns.push(pat);
                Ok((patterns.len() as u16 - 1, Some(bt)))
            }
        }
    }
}

fn is_used(expression: &Expression, id: u32) -> bool {
    match expression {
        Expression::Undefined { .. } => false, // debatable
        Expression::Lambda { body, .. } => is_used(body, id),
        Expression::Tree { root, arguments } => {
            if is_used(root, id) {
                return true;
            }
            for i in arguments.iter() {
                if is_used(i, id) {
                    return true;
                }
            }
            false
        }
        Expression::LocalVarPlaceholder(x) => *x == id,
        Expression::Match {
            matched_on,
            mark,
            branches,
        } => {
            if is_used(matched_on, id) {
                return true;
            }
            for (_pattern, expr) in branches.iter() {
                if is_used(expr, id) {
                    return true;
                }
            }
            false
        }
        _ => false,
    }
}

fn replace_types(t: &mut Type, to_replace: &Vec<(usize, Type)>) {
    match t {
        Type::Generic(a) => {
            if let Some((_, new)) = to_replace.iter().find(|(x, _)| x == a) {
                *t = new.clone()
            }
        }
        Type::Function(a, b) => {
            replace_types(&mut *a, to_replace);
            replace_types(&mut *b, to_replace);
        }
        Type::Type { arguments, .. } => arguments
            .iter_mut()
            .for_each(|x| replace_types(x, to_replace)),
    }
}

fn lookup_global_vars<'a>(
    name: &str,
    mark: &Mark,
    file_dependencies: &HashSet<u16>,
    global_vars: &'a Globals,
    marks: &Vec<Mark>,
) -> Result<&'a GlobalVarData> {
    let Some(var) = global_vars.get(name) else {
        return Err(make_error(
            CompilationError::NotInScope(name.into(), None),
            mark.clone(),
        ));
    };
    let file = marks[var.mark as usize].file;
    if !file_dependencies.contains(&file) {
        return Err(make_error(
            CompilationError::NotInScope(name.into(), Some(file)),
            mark.clone(),
        ));
    }
    Ok(var)
}

//  Because we always know the final type of
//  whatever we're trying to parse and because
//  the data constructor prevents any further
//  function application, it's trivial to figure
//  out the types of its arguments based on the
//  final type.

fn get_type_from_constructor(data: Type, expected: Type, mark: &Mark) -> Result<Type> {
    let mut ret = data.clone();
    let Type::Type {
        type_constructor: tc1,
        arguments: args1,
    }: Type = data.final_type().clone()
    else {
        dbg!(data.final_type());
        unreachable!()
    };
    let Type::Type {
        type_constructor: tc2,
        arguments: args2,
    }: Type = expected.final_type().clone()
    else {
        unreachable!()
    };
    if tc1 != tc2 {
        return Err(make_error(
            CompilationError::TypeMismatch(expected, Some(data)),
            mark.clone(),
        ));
    }
    let mut to_replace: Vec<(usize, Type)> = Vec::new();
    args1.into_iter().zip(args2).for_each(|(t1, t2)| {
        if let Type::Generic(a) = t1 {
            // TODO probably broken
            to_replace.push((a, t2))
        }
    });
    if !to_replace.is_empty() {
        replace_types(&mut ret, &to_replace)
    }
    Ok(ret)
}

fn has_generics(t: &Type) -> bool {
    match t {
        Type::Type { arguments, .. } => arguments.iter().any(has_generics),
        Type::Function(t1, t2) => has_generics(t1) || has_generics(t2),
        Type::Generic(_) => true,
    }
}

fn build_table_of_unknown_generics(t: &Type) -> Vec<(usize, Option<Type>)> {
    let mut map = Vec::new();
    build_table_of_unknown_generics_helper(t, &mut map);
    map
}

fn build_table_of_unknown_generics_helper(t: &Type, map: &mut Vec<(usize, Option<Type>)>) {
    match t {
        Type::Type { arguments, .. } => arguments
            .iter()
            .for_each(|x| build_table_of_unknown_generics_helper(x, map)),
        Type::Function(t1, t2) => {
            build_table_of_unknown_generics_helper(t1, map);
            build_table_of_unknown_generics_helper(t2, map);
        }
        Type::Generic(n) => {
            if map.iter().find(|(a, _)| a == n).is_none() {
                map.push((*n, None));
            }
        }
    }
}

fn substitute_back_in(t: &mut Type, map: &Vec<(usize, Option<Type>)>) {
    match t {
        Type::Type { arguments, .. } => arguments
            .iter_mut()
            .for_each(|x| substitute_back_in(x, map)),
        Type::Function(t1, t2) => {
            substitute_back_in(&mut (*t1), map);
            substitute_back_in(&mut (*t2), map);
        }
        Type::Generic(n) => *t = map.iter().find(|(x, _)| x == n).unwrap().1.clone().unwrap(),
    }
}

// this function take the name of the variable
// and the tokens and returns its type, and
// possibly the number of arguments

fn still_has_unknown_generics(generics: &[(usize, Option<Type>)]) -> bool {
    generics.iter().map(|x| &x.1).any(|x| x.is_none())
}

fn figure_out_type<'a>(
    expected_type: Option<&Type>,
    bt: BlockTraversal<'a>,
    name: &str,
    mark: &Mark,
    global_vars: &Globals,
    local_vars: &LocalVars,
    generics: &Generics,
) -> Result<(Type, Option<u32>)> {
    let contains_unknown_generics: bool;
    let mut output: Type;
    if let Some((_id, _, tp, _)) = local_vars.get_name(name) {
        output = tp.clone();
        contains_unknown_generics = false;
    } else if let Some(GlobalVarData { var_type, .. }) = global_vars.get(name) {
        output = var_type.clone();
        contains_unknown_generics = has_generics(var_type);
    } else {
        return Err(make_error(
            CompilationError::NotInScope(name.to_string(), None),
            mark.clone(),
        ));
    };
    if !contains_unknown_generics {
        let arg_count = if bt.expect_end().is_ok() {
            Some(0)
        } else {
            None
        };
        // the arg counter is only useful when we don't
        // know the block's type. If we do, we can skip
        // the rest and avoid the token clone
        return Ok((output, arg_count));
    }
    // map containing the unknown generics mapped to found types
    let mut unknown_generics: Vec<(usize, Option<Type>)> = build_table_of_unknown_generics(&output);
    'success: {
        if let Some(Type::Type {
            type_constructor: c1,
            arguments: a1,
        }) = expected_type.map(|x| x.final_type())
            && let Type::Type {
                type_constructor: c2,
                arguments: a2,
            } = output.final_type()
        {
            assert_eq!(c1, c2);
            for (a, b) in a1.iter().zip(a2.iter()) {
                merge_types_borrowed(b, a, &mut unknown_generics);
            }
            if !still_has_unknown_generics(&unknown_generics) {
                let arg_count = if bt.expect_end().is_ok() {
                    Some(0)
                } else {
                    None
                };

                substitute_back_in(&mut output, &unknown_generics);
                return Ok((output, arg_count));
            }
        }
        let t_args = output.clone().arg_types();
        let mut t_args = t_args.into_iter();
        let mut number_args_used = Some(0);
        let mut counter = {
            match expected_type {
                None => -1,
                Some(x) => (output.clone().arg_types().len() - x.clone().arg_types().len()) as i32,
            }
        };
        let mut bt = bt;
        loop {
            if bt.reached_end_of_line() {
                counter = -1; // everything else is fair game
                break;
            }
            match counter {
                -1 => (), // why?
                0 => {
                    number_args_used = None;
                    break;
                }
                _ => counter -= 1,
            }
            let t = match t_args.next() {
                Some(x) => x,
                None => {
                    break;
                }
            };
            number_args_used = number_args_used.map(|x| x + 1);
            let (value, var_mark, bt_) = bt.next_token_in_line().unwrap();
            bt = bt_;
            let var_name = match value {
                Token::Word(word) => word,
                _ => {
                    return Err(make_error(
                        CompilationError::CannotInferType,
                        var_mark.clone(),
                    ));
                }
            };
            let has_unknown_generics: bool;
            let got_t = match local_vars.get_name(var_name) {
                Some((_id, _, t, _)) => {
                    has_unknown_generics = false;
                    t.clone()
                }
                None => match global_vars.get(var_name) {
                    Some(GlobalVarData { var_type, .. }) => {
                        has_unknown_generics = has_generics(var_type);
                        var_type.clone()
                    }
                    None => {
                        return Err(make_error(
                            CompilationError::NotInScope(var_name.to_string(), None),
                            var_mark.clone(),
                        ));
                    }
                },
            };
            if has_unknown_generics {
                // not much i can do here
                if matches!(got_t, Type::Type { .. }) {
                } else if got_t.is_function() && matches!(got_t.final_type(), Type::Type { .. }) {
                    let expected_arg_count = t.clone().arg_types().len();
                    let got_arg_count = got_t.clone().arg_types().len();
                    if expected_arg_count != got_arg_count {
                        break 'success;
                    }
                } else {
                    break 'success;
                }
                continue;
            }
            if got_t.is_function() {
                // might be slightly wrong
                let expected_arg_count = t.clone().arg_types().len();
                let got_arg_count = got_t.clone().arg_types().len();
                if expected_arg_count != got_arg_count {
                    break 'success;
                }
            }
            merge_types(&t, got_t, &mut unknown_generics);
        }
        let mut total_number_args = number_args_used;
        if let Ok(NextOutput::IndentedBlocks(branches)) = bt.next()
            && counter == -1
        {
            total_number_args = number_args_used.map(|x| x + branches.len() as u32);
            let zipped = t_args.zip(branches.iter().map(BlockTraversal::new));
            for (tp, bt) in zipped {
                if !still_has_unknown_generics(&unknown_generics) {
                    break;
                }
                number_args_used = number_args_used.map(|x| x + 1);
                if !has_generics(&tp) {
                    continue;
                }
                let (token, mark, leftover) = bt.next_token_in_line().unwrap(); // safe
                let got_t = match token {
                    Token::Keyword(Keyword::The) => parse_type(leftover, generics)?.0,
                    Token::Word(w) => {
                        match figure_out_type(
                            None,
                            leftover,
                            w,
                            mark,
                            global_vars,
                            local_vars,
                            generics,
                        ) {
                            Ok((var_t, Some(arg_count))) => cut_off_n(&var_t, arg_count).clone(),
                            _ => {
                                // TODO pattern match on error
                                continue;
                            }
                        }
                    }
                    _ => continue, // add error to vec
                };
                // check_compatable(tp, got_t)?;
                merge_types(&tp, got_t, &mut unknown_generics);
            }
        }
        if let Some(t) = expected_type
            && let Some(n) = number_args_used
            && n == total_number_args.unwrap()
        {
            let temp_t: &Type = cut_off_n(&output, n);
            merge_types_borrowed(temp_t, t, &mut unknown_generics);
        }
        if still_has_unknown_generics(&unknown_generics) {
            return Err(make_error(CompilationError::CannotInferType, mark.clone()));
        }
        substitute_back_in(&mut output, &unknown_generics);
        return Ok((output, number_args_used));
    }
    if !still_has_unknown_generics(&unknown_generics) {
        substitute_back_in(&mut output, &unknown_generics);
        return Ok((output, None));
    }
    Err(make_error(CompilationError::CannotInferType, mark.clone()))
}

fn merge_types(unknown: &Type, new: Type, output: &mut Vec<(usize, Option<Type>)>) {
    if let Type::Generic(x) = unknown {
        output.iter_mut().find(|(a, _)| a == x).unwrap().1 = Some(new);
        return;
    }
    match (unknown, new) {
        (Type::Function(a1, b1), Type::Function(a2, b2)) => {
            merge_types(a1, *a2, output);
            merge_types(b1, *b2, output);
        }
        (
            Type::Type {
                arguments: args1, ..
            },
            Type::Type {
                arguments: args2, ..
            },
        ) => {
            args1
                .iter()
                .zip(args2)
                .for_each(|(a, b)| merge_types(a, b, output));
        }
        (a, b) => {
            dbg!(a);
            dbg!(b);
            todo!()
        }
    }
}

fn merge_types_borrowed(unknown: &Type, new: &Type, output: &mut Vec<(usize, Option<Type>)>) {
    if let Type::Generic(x) = unknown {
        output.iter_mut().find(|(a, _)| a == x).unwrap().1 = Some(new.clone());
        return;
    }
    match (unknown, new) {
        (Type::Function(a1, b1), Type::Function(a2, b2)) => {
            merge_types_borrowed(a1, a2, output);
            merge_types_borrowed(b1, b2, output);
        }
        (
            Type::Type {
                arguments: args1, ..
            },
            Type::Type {
                arguments: args2, ..
            },
        ) => {
            args1
                .iter()
                .zip(args2.iter())
                .for_each(|(a, b)| merge_types_borrowed(a, b, output));
        }
        (a, b) => {
            dbg!(a);
            dbg!(b);
            todo!()
        }
    }
}

pub fn parse_expression<'a>(
    loaded: &mut LoadedExpressions,
    file_dependencies: &HashSet<u16>,
    expected_type: &Type,
    bt: BlockTraversal<'a>,
    local_vars: &mut LocalVars<'a>,
    generics: &Generics,
) -> Result<(Expression, Option<BlockTraversal<'a>>)> {
    let (token, mark, next_bt) = bt.next_token_in_line_fallthrough()?;
    match token {
        Token::Keyword(Keyword::The) => {
            let (tp, bt) = parse_the(bt, generics)?;
            parse_expression(loaded, file_dependencies, &tp, bt, local_vars, generics)
        }
        Token::Keyword(Keyword::Undefined) => {
            BlockTraversal::expect_end(next_bt)?;
            loaded.marks.push(mark.clone());
            Ok((Expression::Undefined(loaded.marks.len() as u16 - 1), None))
        }
        Token::Keyword(Keyword::Lambda) => {
            let Type::Function(a, b) = expected_type else {
                return Err(make_error(
                    CompilationError::TypeMismatch(expected_type.clone(), None),
                    mark.clone(),
                ));
            };
            let mut new_local_vars = local_vars.clone();
            let local_vars = &mut new_local_vars;
            let (pattern, _pattern_mark, expr) =
                parse_lambda_case(loaded, file_dependencies, a, b, bt, local_vars, generics)?;
            loaded.marks.push(mark.clone());
            Ok((
                Expression::Lambda {
                    pattern,
                    mark: loaded.marks.len() as u16 - 1,
                    body: Box::new(expr),
                },
                None,
            ))
        }
        Token::Keyword(Keyword::Match) => {
            let mut must_end = false;
            let (mut matched_on_bt, branches_bt) = match next_bt.next()? {
                NextOutput::Token { .. } => {
                    must_end = true;
                    (
                        next_bt,
                        next_bt
                            .get_indented_blocks()
                            .into_iter()
                            .map(BlockTraversal::new)
                            .collect::<Vec<BlockTraversal>>(),
                    )
                }
                NextOutput::IndentedBlocks(v) => (
                    BlockTraversal::new(&v[0]),
                    v.into_iter().skip(1).map(BlockTraversal::new).collect(),
                ),
            };
            let tp: Type = {
                let (value, mark, bt) = matched_on_bt.next_token_in_line()?;
                match value {
                    Token::Keyword(Keyword::The) => {
                        let (tp, body_bt) = parse_the(matched_on_bt, generics)?;
                        matched_on_bt = body_bt;
                        tp
                    }
                    Token::Word(first_name) => {
                        let (root_type, _) = figure_out_type(
                            None,
                            bt,
                            first_name,
                            mark,
                            &loaded.info_table,
                            local_vars,
                            generics,
                        )?;
                        root_type.final_type().clone()
                    }
                    Token::Keyword(_) => {
                        return Err(make_error(ParseError::UnexpectedKeyword, mark.clone()));
                    }
                }
            };
            let (matched_on_expr, leftover_bt) = parse_expression(
                loaded,
                file_dependencies,
                &tp,
                matched_on_bt,
                local_vars,
                generics,
            )?;
            if must_end {
                BlockTraversal::expect_no_indents(leftover_bt, mark)?;
            } else {
                BlockTraversal::expect_end_option(leftover_bt)?;
            }
            let mut branches: Vec<(u16, Expression)> = Vec::with_capacity(branches_bt.len());
            let mut encountered_wildcard = false;
            for branch_bt in branches_bt {
                let mut new_local_vars = local_vars.clone();
                let local_vars = &mut new_local_vars;
                let (case_mark, _) = branch_bt.expect_keyword(Keyword::Case)?;
                if encountered_wildcard {
                    return Err(make_error(
                        CompilationError::RedundentPattern,
                        case_mark.clone(),
                    ));
                }
                let (pattern, pattern_mark, expr) = parse_lambda_case(
                    loaded,
                    file_dependencies,
                    &tp,
                    expected_type,
                    branch_bt,
                    local_vars,
                    generics,
                )?;
                if matches!(
                    loaded.patterns[pattern as usize],
                    Pattern::Dropped | Pattern::Captured(_)
                ) {
                    encountered_wildcard = true;
                }
                branches.push((pattern, expr));
            }
            loaded.marks.push(mark.clone());
            Ok((
                Expression::Match {
                    matched_on: Box::new(matched_on_expr),
                    mark: loaded.marks.len() as u16 - 1,
                    branches: branches.into(),
                },
                None,
            ))
        }
        Token::Word(name) => {
            // peek if the next token is a newline
            let (root_id, root_type) = if let Some((a, _, b, _)) = local_vars.get_name(name) {
                (Expression::LocalVarPlaceholder(a), b.clone())
            } else {
                let GlobalVarData { id: a, .. } = lookup_global_vars(
                    name,
                    mark,
                    file_dependencies,
                    &loaded.info_table,
                    &loaded.marks,
                )?;
                let (Id::Variable(a) | Id::Constructor(a)) = a;
                let (t, _) = figure_out_type(
                    Some(expected_type),
                    next_bt,
                    name,
                    mark,
                    &loaded.info_table,
                    local_vars,
                    generics,
                )?;
                (loaded.expressions[*a].clone(), t)
            };
            if !root_type.is_possible(expected_type) {
                return Err(make_error(
                    CompilationError::TypeMismatch(expected_type.clone(), Some(root_type.clone())),
                    mark.clone(),
                ));
            }
            let mut output_args = Vec::new();
            let mut current_type = root_type.to_owned();
            let mut ret_bt = Some(next_bt);
            while current_type != *expected_type {
                let bt = BlockTraversal::expect_no_indents(ret_bt, mark)?;
                match bt.next()? {
                    NextOutput::IndentedBlocks(v) => {
                        let mut arg_groups = v.into_iter().map(BlockTraversal::new);
                        while current_type != *expected_type {
                            let Some(current_bt) = arg_groups.next() else {
                                return Err(make_error(
                                    CompilationError::ExpectedMoreArguments,
                                    mark.clone(),
                                ));
                            };
                            let (next_type, leftover) = match current_type {
                                Type::Function(a, b) => (*a, *b),
                                Type::Type { .. } | Type::Generic(_) => unreachable!(),
                            };
                            current_type = leftover;
                            let (next_arg, bt) = parse_expression(
                                loaded,
                                file_dependencies,
                                &next_type,
                                current_bt,
                                local_vars,
                                generics,
                            )?;
                            BlockTraversal::expect_end_option(bt)?;
                            output_args.push(next_arg);
                        }
                        if let Some(trailing_line) = arg_groups.next() {
                            trailing_line.expect_end()?;
                        }
                        ret_bt = None;
                        break;
                    }
                    NextOutput::Token { .. } => {
                        let (next_type, leftover) = match current_type {
                            Type::Function(a, b) => (*a, *b),
                            Type::Type { .. } | Type::Generic(_) => unreachable!(),
                        };
                        current_type = leftover;
                        let (next_arg, new_bt) = parse_expression(
                            loaded,
                            file_dependencies,
                            &next_type,
                            bt,
                            local_vars,
                            generics,
                        )?;
                        output_args.push(next_arg);
                        ret_bt = new_bt
                    }
                }
            }
            Ok(if output_args.is_empty() {
                (root_id, ret_bt)
            } else {
                (
                    Expression::Tree {
                        root: Box::new(root_id),
                        arguments: output_args.into(),
                    },
                    ret_bt,
                )
            })
        }
        _ => {
            debug_print_block(bt, 0);
            todo!();
        }
    }
}

// lambda and case have the same syntax

pub fn parse_lambda_case<'a>(
    loaded: &mut LoadedExpressions,
    file_dependencies: &HashSet<u16>,
    pattern_type: &Type,
    block_type: &Type,
    bt: BlockTraversal<'a>,
    local_vars: &mut LocalVars<'a>,
    generics: &Generics,
) -> Result<(u16, &'a Mark, Expression)> {
    let original_local_var_count = local_vars.vars.len();
    let (lambda_mark, bt) = bt
        .expect_keyword(Keyword::Lambda)
        .or(bt.expect_keyword(Keyword::Case))?;
    let (pattern, body_bt) = match bt.next_expecting_count(2)? {
        NextOutput::IndentedBlocks(v) => {
            let (pattern, _mark, leftover_bt) = parse_pattern(
                local_vars,
                pattern_type,
                BlockTraversal::new(&v[0]),
                &loaded.info_table,
                &mut loaded.patterns,
            )?;
            BlockTraversal::expect_end_option(leftover_bt)?;
            (pattern, BlockTraversal::new(&v[1]))
        }
        NextOutput::Token { .. } => {
            let (pattern, _mark, body_bt) = parse_pattern(
                local_vars,
                pattern_type,
                bt,
                &loaded.info_table,
                &mut loaded.patterns,
            )?;
            (
                pattern,
                BlockTraversal::expect_no_indents(body_bt, lambda_mark)?,
            )
        }
    };
    let (body, bt) = parse_expression(
        loaded,
        file_dependencies,
        block_type,
        body_bt,
        local_vars,
        generics,
    )?;
    BlockTraversal::expect_end_option(bt)?;
    for (id, (_name, _tp, mark)) in local_vars
        .vars
        .iter()
        .enumerate()
        .skip(original_local_var_count)
    {
        if !is_used(&body, id as u32) {
            return Err(make_error(CompilationError::NotUsed, (*mark).clone()));
        }
    }
    Ok((pattern, lambda_mark, body))
}

#[derive(Hash, PartialEq, Eq)]
pub enum BlockKind {
    Type,
    Variable,
}

pub struct NameAndGenerics {
    pub name: String,
    pub mark: Mark,
    pub generics: Generics,
    pub kind: BlockKind,
}

pub fn extract_name_and_generics<'a>(
    bt: BlockTraversal<'a>,
) -> Result<(NameAndGenerics, BlockTraversal<'a>)> {
    let mut generics: Generics = Vec::new();
    let (name, mark, kind, bt) = extract_name_and_genericsl_helper(bt, &mut generics)?;
    fn extract_name_and_genericsl_helper<'a>(
        bt: BlockTraversal<'a>,
        generics: &mut Generics,
    ) -> Result<(String, Mark, BlockKind, BlockTraversal<'a>)> {
        let (token, mark1, mut bt) = bt.next_token_in_line_fallthrough()?;
        match token {
            Token::Keyword(Keyword::ForAll) => {
                while let Ok((name, _name_mark, bt_)) = bt.expect_word() {
                    bt = bt_;
                    let index = generics.len() + 1;
                    generics.push((name.to_string(), index));
                }
                extract_name_and_genericsl_helper(bt, generics)
            }
            Token::Keyword(Keyword::Let) => {
                let (name, name_mark, bt) = bt.expect_word()?;
                let (_be_mark, bt) = bt.expect_keyword(Keyword::Be)?;
                Ok((name.to_string(), name_mark.clone(), BlockKind::Variable, bt))
            }
            Token::Keyword(Keyword::Type) => {
                let (name, name_mark, bt) = bt.expect_word()?;
                let (_be_mark, bt) = bt.expect_keyword(Keyword::Contains)?;
                Ok((name.to_string(), name_mark.clone(), BlockKind::Type, bt))
            }
            _ => Err(Error {
                mark: mark1.clone(),
                error_type: Box::new(ParseError::UnexpectedKeyword),
                note: Some(String::from(
                    "\x1b[90mexpected one of \x1b[97mlet type forall\x1b[90m",
                )),
            }),
        }
    }
    Ok((
        NameAndGenerics {
            name: name.to_string(),
            mark: mark.clone(),
            generics,
            kind,
        },
        bt,
    ))
}

pub fn parse_the<'a>(
    bt: BlockTraversal<'a>,
    generics: &Vec<(String, usize)>,
) -> Result<(Type, BlockTraversal<'a>)> {
    let (the_mark, bt) = bt.expect_keyword(Keyword::The)?;
    let (tp, body) = match bt.next_expecting_count(2)? {
        NextOutput::IndentedBlocks(v) => {
            let (tp, leftover) = parse_type(BlockTraversal::new(&v[0]), generics)?;
            BlockTraversal::expect_end_option(leftover).unwrap();
            (tp, BlockTraversal::new(&v[1]))
        }
        NextOutput::Token { .. } => {
            let (tp, leftover) = parse_type(bt, generics)?;
            (tp, BlockTraversal::expect_no_indents(leftover, the_mark)?)
        }
    };
    Ok((tp, body))
}

pub fn parse_type<'a>(
    bt: BlockTraversal<'a>,
    generics: &Vec<(String, usize)>,
) -> Result<(Type, Option<BlockTraversal<'a>>)> {
    parse_type_helper(bt, generics, Kind::Type)
}

fn parse_type_helper<'a>(
    bt: BlockTraversal<'a>,
    generics: &Vec<(String, usize)>,
    mut _kind: Kind, // TODO
) -> Result<(Type, Option<BlockTraversal<'a>>)> {
    let (word, mark, leftover_bt) = bt.expect_word()?;
    let is_fn = word == "fn";
    let mut kind = Kind::Fn(
        Kind::Type.into(),
        Kind::Fn(Kind::Type.into(), Kind::Type.into()).into(),
    );
    let mut index = None;
    if !is_fn {
        if let Some(t) = get_from_generics(word, generics) {
            return Ok((t, Some(leftover_bt)));
        }
        {
            let ptr = TYPES.lock().unwrap();
            index = Some(
                ptr.as_ref()
                    .unwrap()
                    .get(word)
                    .ok_or(Error {
                        mark: mark.clone(),
                        error_type: Box::new(CompilationError::TypeNotInScope(word.to_string())),
                        note: None,
                    })?
                    .clone(),
            );
        }
        kind = index.as_ref().unwrap().kind.clone();
    }
    let mut arguments = Vec::new();
    let mut bt = leftover_bt;
    let mut ret_bt = Some(bt);
    while let Kind::Fn(_, x) = kind {
        kind = *x;
        match bt.next()? {
            NextOutput::Token(_, mark, _) => {
                let (tp, bt_) = parse_type_helper(bt, generics, Kind::Type)?;
                arguments.push(tp);
                match BlockTraversal::expect_no_indents(bt_, mark) {
                    Err(e) if !matches!(kind, Kind::Type) => return Err(e),
                    Err(_) => {
                        ret_bt = None;
                        break;
                    }
                    Ok(bt_) => {
                        ret_bt = Some(bt_);
                        bt = bt_;
                    }
                }
            }
            NextOutput::IndentedBlocks(v) => {
                let mut count = 1;
                while let Kind::Fn(_, x) = kind {
                    count += 1;
                    kind = *x;
                }
                assert_eq!(count, v.len());
                for bt in v.iter().map(BlockTraversal::new) {
                    let (tp, leftover) = parse_type_helper(bt, generics, Kind::Type)?;
                    BlockTraversal::expect_end_option(leftover)?;
                    arguments.push(tp);
                }
                ret_bt = None;
                break;
            }
        }
    }
    let ret = if is_fn {
        Type::Function(
            Box::new(arguments[0].clone()),
            Box::new(arguments[1].clone()),
        )
    } else {
        Type::Type {
            type_constructor: index.unwrap().id as u32,
            arguments,
        }
    };
    Ok((ret, ret_bt))
}

fn get_from_generics(name: &str, generics: &Generics) -> Option<Type> {
    generics
        .iter()
        .find(|(generic_name, _)| name == generic_name)
        .map(|(_, index)| Type::Generic(*index))
}

pub fn parse_data(
    bt: BlockTraversal,
    parent_type: u32,
    generics: &Generics,
) -> Result<Vec<(String, Type, Mark)>> {
    let mut output = Vec::new();
    match bt.next()? {
        NextOutput::IndentedBlocks(v) => {
            for branch in v.iter().map(BlockTraversal::new) {
                let (constructor_name, constructor_mark, bt) = branch.expect_word()?;
                let mut arg_types: Vec<Type> = Vec::new();
                match bt.next() {
                    Ok(NextOutput::IndentedBlocks(ts)) => {
                        for type_block in ts.iter().map(BlockTraversal::new) {
                            let (t, leftover_bt) = parse_type(type_block, generics)?;
                            BlockTraversal::expect_end_option(leftover_bt)?;
                            arg_types.push(t);
                        }
                    }
                    _ => {
                        let mut temp_bt = bt;
                        while matches!(temp_bt.next(), Ok(NextOutput::Token { .. })) {
                            let (t, leftover_bt) = parse_type(temp_bt, generics)?;
                            arg_types.push(t);
                            temp_bt =
                                BlockTraversal::expect_no_indents(leftover_bt, constructor_mark)?;
                        }
                        temp_bt.expect_end()?;
                    }
                }
                let mut args = arg_types.into_iter();
                output.push((
                    constructor_name.to_string(),
                    build_type(
                        &mut args,
                        Type::Type {
                            type_constructor: parent_type,
                            arguments: generics_to_type(generics),
                        },
                    ),
                    constructor_mark.clone(),
                ));
            }
        }
        NextOutput::Token { .. } => {
            debug_print_block(bt, 0);
            todo!();
        }
    }
    Ok(output)
}

fn generics_to_type(generics: &Generics) -> Vec<Type> {
    generics.iter().map(|(_, id)| Type::Generic(*id)).collect()
}

pub fn build_type(input: &mut impl Iterator<Item = Type>, result: Type) -> Type {
    match input.next() {
        None => result,
        Some(x) => Type::Function(Box::new(x), Box::new(build_type(input, result))),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Type {
        type_constructor: u32,
        arguments: Vec<Type>,
    },
    Function(Box<Type>, Box<Type>),
    Generic(usize),
}

impl Type {
    pub fn final_type(&self) -> &Type {
        match self {
            Self::Function(_, b) => b.final_type(),
            _ => self,
        }
    }

    pub fn is_possible(&self, test: &Self) -> bool {
        *self == *test
            || match self {
                Self::Type { .. } | Self::Generic(_) => false,
                Self::Function(_, output) => output.is_possible(test),
            }
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function { .. })
    }

    pub fn arg_types(self) -> Vec<Type> {
        let mut args = Vec::new();
        let mut current_type = self;
        while let Type::Function(a, b) = current_type {
            args.push(*a);
            current_type = *b;
        }
        args
    }
    pub fn show(&self) -> String {
        fn helper(tp: &Type, types: &HashMap<u32, String>, output: &mut String) {
            match tp {
                Type::Type {
                    type_constructor: a,
                    arguments,
                } => {
                    let name = types.get(a).unwrap(); // safe
                    output.push_str(name);
                    output.push(' ');
                    arguments.iter().for_each(|x| {
                        let a = x.show();
                        output.push_str(&a);
                    })
                }
                Type::Function(a, b) => {
                    output.push_str("fn ");
                    helper(a, types, output);
                    helper(b, types, output);
                }
                Type::Generic(a) => {
                    let s = a.to_string();
                    output.push('g');
                    output.push_str(&s);
                    output.push(' ');
                }
            }
        }
        let mut output = String::new();
        let mut new_map = HashMap::new();
        {
            let ptr = TYPES.lock().unwrap();
            ptr.as_ref()
                .unwrap()
                .clone()
                .into_iter()
                .for_each(|(k, w)| {
                    new_map.insert(w.id as u32, k);
                });
        }
        helper(self, &new_map, &mut output);
        output
    }
}

fn cut_off_n(mut tp: &Type, n: u32) -> &Type {
    // should be result
    for _ in 0..n {
        match tp {
            Type::Function(_, b) => tp = b,
            _ => todo!(),
        }
    }
    tp
}

#[derive(Debug, Hash, Clone)]
pub enum Kind {
    Type,
    Fn(Box<Kind>, Box<Kind>),
}
