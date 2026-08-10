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

use std::collections::{
    HashMap,
    HashSet,
};
use std::fs::read_to_string;
use std::sync::Mutex;
use std::rc::Rc;
use std::cell::RefCell;
use std::borrow::Cow; // TODO use for passing in type parse_expression

use crate::error::{make_error, Result, Error, ErrorType, Mark, Marked, DEBUG_INFO, get_file_name};
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
    NotInScope(String, Option<u32>),
    TypeMismatch(Type, Option<Type>),
    // BadTypeInference(Type, Type),
    BadFile(String),
    MultipleDeclarations(u32),
    // TypeAnnotationNeeded,
    RedundentPattern
}

impl ErrorType for CompilationError {
    fn gist(&self) -> &'static str {
        match self {
            Self::CannotInferType         => "can't infer type",
            // Self::TypeAnnotationNeeded    => "type annotation needed",
            // Self::BadTypeInference(_, _)  => "of unexpected type",
            Self::NotUsed                 => "local variable never used",
            Self::EitherMismatch          => "mismatch between branches",
            Self::MultipleDeclarations(_) => "multiple declarations",
            //Self::PartialPattern => "not all patterns covered",
            Self::RedundentPattern        => "redundent pattern",
            Self::ExpectedMoreArguments   => "expected more arguments",
            Self::NotInScope(_,_)         => "not in scope",
            Self::TypeNotInScope(_)       => "type not in scope",
            Self::TypeMismatch(_, _)      => "of unexpected type",
            Self::BadFile(_)              => "couldn't find file"
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
            Self::EitherMismatch => write!(f, "the two patterns in the \x1b[97meither\x1b[90m pattern must have
the same variables and of the same type"),
            Self::MultipleDeclarations(s) => write!(f, "name already used in \x1b[97m{s}\x1b[90m"),
            Self::NotUsed => write!(f, "consider prepending it with an \x1b[97m_\x1b[90m to drop the value"),
            Self::RedundentPattern => write!(f,
"this branch will never be reached because the
branch above is a wildcard that matches everything"),
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
extract_file_name(&file_name))
                    }
                }
            ),
            Self::TypeNotInScope(x) => write!(f, "type \x1b[97m{x}\x1b[90m not in scope"),
            Self::TypeMismatch(tp1, tp2) => write!(
                f,
                "expected a value of type \x1b[97m{}\x1b[90mhowever this value can never evaluate to it{}",
                tp1.show(),
                if let Some(tp2) = tp2 {
                    format!(
                        ".\nit is of type \x1b[97m{}\x1b[90m",
                        tp2.show()
                    )
                } else {
                    String::new()
                }
            ),
            _ => write!(f, "todo")
        }
    }
}


// TODO use this

struct LoadedExpressions {
    expressions: Vec<Expression>,
    info_table:  HashMap<String, GlobalVarData>,
    var_names:   Vec<String>,
    patterns:    Vec<Pattern>,
}

type GlobalTypes = HashMap<String, GlobalTypeData>;

type LocalVars = HashMap<String, (u32, Type, Mark)>;
pub type Generics = Vec<(String, usize)>;

#[derive(Clone)]
pub struct GlobalVarData {
    pub mark: Mark,
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

pub fn get_everything() -> Result<(Vec<Expression>, Vec<String>, Globals)> {
    let output = std::process::Command::new("sh")
        .arg("-c")
        .arg("find -L . -maxdepth 5 | grep '\\.th$'")
        .output()
        .unwrap();
    let files = String::from_utf8_lossy(&output.stdout);
    let files: Vec<String> = files
        .split_whitespace()
        .map(|x| x.to_string())
        .collect();
    {
        let mut ptr = DEBUG_INFO.lock().unwrap();
        ptr.files = files.clone();
    }
    let (vars, vars_dummy) = new_uwu(&files)?;
    let mut map = Vec::with_capacity(vars_dummy.len());
    for _ in 0..vars_dummy.len() {
        map.push(String::new());
    }
    for (name, GlobalVarData {id, ..}) in vars_dummy.iter() {
        let id_index = match id {
            Id::Variable(a)    => *a,
            Id::Constructor(a) => *a,
        };
        map[id_index] = name.clone();
    }
    Ok((vars, map, vars_dummy))
}

// 1. find  all types
// 2. parse all types
// 3. parse all values

struct Dependencies {
    files: HashMap<u32, HashSet<u32>>      // filename, its file dependencies
}

impl Dependencies {
    fn available_files(&mut self, file: u32) -> HashSet<u32> {
        let mut visited_files: HashSet<u32> = HashSet::from([file]);
        let mut to_visit: Vec<u32> = vec![file];
        while let Some(x) = to_visit.pop() {
            self.files
                .get(&x)
                .unwrap()
                .difference(&visited_files)
                .copied()
                .collect::<Vec<u32>>()
                .into_iter()
                .for_each(|i| {
                    to_visit.push(i);
                    visited_files.insert(i);
                });
        }
        visited_files
    }
}

fn new_get_includes(
    blocks: &mut Vec<Block>,
    file_names: &HashMap<String, (&str, u32)>
) -> Result<HashSet<u32>> {
    let elem = BlockTraversal::new(&blocks[0]);
    let mut output = HashSet::new();
    if let Ok((_include_mark, mut bt)) = elem.expect_keyword(Keyword::Include) {
        while let Ok((x, mark, bt_)) = bt.expect_word() {
            bt = bt_;
            let Some((_, key)) = file_names.get(x) else {
                return Err(make_error(CompilationError::BadFile(x.to_string()), mark.clone()))
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
        Kind::Fn(Box::new(Kind::Type), Box::new(kind_from_generics(count - 1)))
    }
}

fn new_uwu(files: &[String]) -> Result<(Vec<Expression>, Globals)> {
    let mut file_names: HashMap<String, (&str, u32)> = HashMap::new();
    for (count, i) in files.iter().enumerate() {
        file_names.insert(extract_file_name(i), (i, count as u32));
    }
    let mut final_var_table: HashMap<String, GlobalVarData> = HashMap::new();

    // Step 1: find all the types

    // we need types before we can parse data constructors and
    // variable types so we'll do those later

    let mut type_bodies: Vec<BlockTraversal> = Vec::new();
    let mut var_bodies: Vec<BlockTraversal> = Vec::new();

    let mut type_table: HashMap<String, GlobalTypeData> = HashMap::new();
    type_table.insert(String::from("fn"), GlobalTypeData {
        mark: Mark::default(),
        kind: Kind::Fn(Box::new(Kind::Type), Box::new(Kind::Type)),
        id: 0,
        generics: Vec::new(),
    });
    let mut var_table: HashMap<String, (Mark, usize, Generics)> = HashMap::new();

    let mut file_tokens: Vec<(&str, Vec<Block>)> = Vec::new();
    let mut dependencies: Dependencies = Dependencies { files: HashMap::new() };

    for (index, file_name) in files.iter().enumerate() {
        let contents = read_to_string(file_name).unwrap();
        let mut blocks = new_tokenize_file(contents, index as u32)?;
        let deps = new_get_includes(&mut blocks, &file_names)?;
        dependencies.files.insert(index as u32, deps);
        file_tokens.push((file_name, blocks));
    }

    for (_file_name, blocks) in file_tokens.iter() {
        for block in blocks.iter() {
            let bt = BlockTraversal::new(block);
            let (NameAndGenerics {name, mark, generics, kind}, bt) = new_extract_name_and_generics(bt)?;
            match kind {
                BlockKind::Variable => {
                    if let Some(x) = var_table.insert(name, (mark.clone(), var_bodies.len(), generics)) {
                        return Err(make_error(
                            CompilationError::MultipleDeclarations(x.0.file),
                            mark
                        ))
                    }
                    var_bodies.push(bt);
                }
                BlockKind::Type => {
                    if let Some(x) = type_table.get(&name) {
                        return Err(make_error(
                            CompilationError::MultipleDeclarations(x.mark.file),
                            mark
                        ))
                    }
                    type_table.insert(name, GlobalTypeData {
                        mark: mark.clone(),
                        id: type_table.len(),
                        kind: kind_from_generics(generics.len() as u32),
                        generics,
                    });
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
        let (var_type, bt) = parse_the(var_bodies[index], &generics)?;
        final_var_table.insert(name, GlobalVarData {
            var_type,
            mark,
            id: Id::Variable(index),
            generics,
        });
        var_bodies[index] = bt;
    }

    for (_name, GlobalTypeData {mark: _, id: index, generics, kind: _ }) in type_table.into_iter() {
        if index != 0 {
            let branches = new_parse_data(
                type_bodies[index - 1],
                index as u32,
                &generics
            )?;
            for (name, tp, mark) in branches.into_iter() {
                if let Some(x) = final_var_table.insert(name, GlobalVarData {
                    mark: mark.clone(),
                    id: Id::Constructor(final_var_table.len()),
                    generics: generics.clone(),
                    var_type: tp,
                }) {
                    return Err(make_error(
                        CompilationError::MultipleDeclarations(x.mark.file),
                        mark
                    ))
                };
            }
        }
    }

    let mut expressions: Vec<Expression> = Vec::with_capacity(final_var_table.len());
    for _ in 0..final_var_table.len() {
        expressions.push(Expression::default());
    }
    for (_, GlobalVarData {mark, id, ..}) in final_var_table.iter() {
        match id {
            Id::Constructor(n) => expressions[*n] = Expression::DataConstructor(*n as u32),
            Id::Variable(n) => expressions[*n] = Expression::Thunk {
                value: Rc::new(RefCell::new(Expression::default())),
                mark: Some(Rc::new(mark.clone()))
            }
        }
    }

    // step 3: parse the expressions
    let temp_local_vars = HashMap::new();
    for (_, GlobalVarData {mark, var_type, id, generics}) in final_var_table.iter() {
        let available_files = dependencies.available_files(mark.file);
        let Id::Variable(index) = id else { continue };
        let (expression, bt) = new_parse_expression(
            &mut expressions,
            &available_files,
            var_type,
            var_bodies[*index],
            &temp_local_vars,
            0,
            &final_var_table,
            generics,
        )?;
        BlockTraversal::expect_end_option(bt)?;
        {
            let Expression::Thunk { value: ref x, .. } = expressions[*index] else { unreachable!() };
            let Ok(mut inner) = (*x).try_borrow_mut() else { unreachable!() };
            *inner = expression;
        }
    }
    Ok((expressions, final_var_table))
}

fn new_parse_pattern<'a>(
    mut number_of_local:  u32,
    expected_type:        &Type,
    bt:                   BlockTraversal<'a>,
    global_vars:          &Globals,
) -> Result<(Pattern, Mark, LocalVars, u32, Option<BlockTraversal<'a>>)> {
    let mut output = HashMap::new();
    let mark = bt.next_token_in_line()?.1.clone();
    let (pattern, bt) = new_parse_pattern_helper(&mut number_of_local, expected_type, &mut output, bt, global_vars)?;
    Ok((pattern, mark, output, number_of_local, bt))
}

// asserts that the two maps are equal and makes their ids equivelent

fn assert_maps_equal(either_mark: &Mark, fst: &LocalVars, snd: &LocalVars) -> Result<()> {
    if fst.len() != snd.len() {
        return Err(make_error(CompilationError::EitherMismatch, either_mark.clone()));
    };
    for (name, (_, tp, mark)) in fst.iter() {
        if let Some(x) = snd.get(name) && x.1 == *tp {
            // x.0 = *id;
        } else {
            return Err(make_error(CompilationError::EitherMismatch, mark.clone()));
        }
    }
    Ok(())
}

fn new_parse_pattern_helper<'a>(
    number_of_local: &mut u32,
    expected_type: &Type,
    output: &mut LocalVars,
    bt: BlockTraversal<'a>,
    global_vars: &Globals,
) -> Result<(Pattern, Option<BlockTraversal<'a>>)> {
    let (token, root_mark, bt) = bt.next_token_in_line()?;
    match token {
        OwnedToken::Keyword(Keyword::Bind) => {
            let (name, mark, bt) = bt.expect_word()?;
            assert!(!name.starts_with('_')); // TODO
            *number_of_local += 1;
            let number_of_local_copy = *number_of_local;
            output.insert(name.to_string(), (*number_of_local, expected_type.clone(), mark.clone()));
            let (pat, bt) = new_parse_pattern_helper(number_of_local, expected_type, output, bt, global_vars)?;
            Ok((Pattern::Bound(number_of_local_copy, Box::new(pat)), bt))
        }
        OwnedToken::Keyword(Keyword::Either) => {
            let mut ret_bt: Option<BlockTraversal> = Some(bt);
            let mut patterns: Vec<(Pattern, LocalVars)> = Vec::new();
            if let NextOutput::IndentedBlocks(branches) = bt.next()? {
                ret_bt = None;
                assert!(branches.len() >= 2);
                for branch in branches.into_iter() {
                    let mut num = *number_of_local;
                    let mut vars = HashMap::new();
                    let (pat, bt) = new_parse_pattern_helper(&mut num, expected_type, &mut vars, branch, global_vars)?;
                    BlockTraversal::expect_end_option(bt)?;
                    patterns.push((pat, vars));
                }
            } else {
                let mut temp_bt = bt;
                for i in 0..2 {
                    let mut num = *number_of_local;
                    let mut vars = HashMap::new();
                    let (pat, bt) = new_parse_pattern_helper(&mut num, expected_type, &mut vars, temp_bt, global_vars)?;
                    ret_bt = bt;
                    patterns.push((pat, vars));
                    if i == 1 { break }
                    else {
                        let mark = temp_bt.next_token_in_line()?.1;
                        temp_bt  = BlockTraversal::expect_no_indents(bt, mark)?;
                    }
                }
            }
            for i in 1..patterns.len() {
                assert_maps_equal(root_mark, &patterns[0].1, &patterns[i].1)?;
            }
            *number_of_local = patterns[0].1.len() as u32; // TODO bugged
            output.extend(patterns[0].1.clone());
            Ok((Pattern::Either(patterns.into_iter().map(|x| x.0).collect()), ret_bt))
        }
        OwnedToken::Keyword(_) => Err(make_error(ParseError::UnexpectedKeyword, root_mark.clone())),
        OwnedToken::Word(name) if name.starts_with('_') => Ok((Pattern::Dropped, Some(bt))),
        OwnedToken::Word(name) => {
            if
                let Some(GlobalVarData { id: Id::Constructor(index), var_type: tp, generics: _new_generics, ..  }) = global_vars.get(name) &&
                let Type::Type {type_constructor: tc1, ..} = tp.final_type() &&
                let Type::Type {type_constructor: tc2, ..} = expected_type &&
                *tc1 == *tc2
            {
                let mut patterns = Vec::new();
                let ts = get_type_from_constructor(tp.clone(), expected_type.clone(), root_mark)?.arg_types();
                let ret_bt;
                match bt.next()? {
                    NextOutput::Token(_, mark, _) => {
                        let mut temp_bt = Some(bt);
                        for t in ts {
                            let (pat, bt) = new_parse_pattern_helper(
                                number_of_local,
                                &t,
                                output,
                                BlockTraversal::expect_no_indents(temp_bt, mark)?,
                                global_vars
                            )?;
                            temp_bt = bt;
                            patterns.push(pat);
                        }
                        ret_bt = temp_bt;
                    }
                    NextOutput::IndentedBlocks(v) => {
                        ret_bt = None;
                        assert_eq!(v.len(), ts.len());
                        for (t, branch) in ts.iter().zip(v.iter()) {
                            let (pat, bt) = new_parse_pattern_helper(number_of_local, t, output, *branch, global_vars)?;
                            BlockTraversal::expect_end_option(bt)?;
                            patterns.push(pat);
                        }
                    }
                }
                Ok((Pattern::DataConstructor(*index as u32, patterns), ret_bt))
            } else {
                *number_of_local += 1;
                output.insert(name.to_string(), (*number_of_local, expected_type.clone(), root_mark.clone()));
                Ok((Pattern::Captured(*number_of_local), Some(bt)))
            }
        }
    }
}

fn is_used(expression: &Expression, id: u32) -> bool {
    match expression {
        Expression::Undefined { .. } => false, // debatable
        Expression::Lambda { body, .. } => is_used(body, id),
        Expression::Tree {root, arguments} => {
            if is_used(root, id) { return true }
            for i in arguments.iter() {
                if is_used(i, id) {
                    return true
                }
            }
            false
        }
        Expression::LocalVarPlaceholder(x) => *x == id,
        Expression::Match { matched_on, branches } => {
            if is_used(matched_on, id) {
                return true
            }
            for (_pattern, expr) in branches.iter() {
                if is_used(expr, id) {
                    return true
                }
            }
            false
        }
        _ => false
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
        Type::Type { arguments, .. } => {
            arguments.iter_mut().for_each(|x| replace_types(x, to_replace))
        },
    }
}

pub fn new_tokenize_file(input: String, file_index: u32) -> Result<Vec<Block>> {
    let mut output: Vec<Block> = Vec::new();
    let mut current_block: Vec<(usize, &str)> = Vec::new();
    let file_lines: Vec<String> = input.lines().map(|x| x.trim_end().to_string()).collect();
    let mut file_lines = file_lines.iter().map(|x| x.as_str()).enumerate();
    while let Some((line_number, string)) = file_lines.next() {
        if string.is_empty() || string.split_whitespace().next().unwrap() == "--" { // safe unwrap
            continue
        }
        current_block.push((line_number, string));
        'good_lines: loop {
            match file_lines.next() {
                None | Some((_, "")) => {
                    let block = tokenize_block(current_block.clone(), file_index, 0)?;
                    output.push(block);
                    current_block = Vec::new();
                    break 'good_lines;
                }
                Some((line_number, string)) => {
                    current_block.push((line_number, string));
                }
            }
        }
    }
    Ok(output)
}

fn lookup_global_vars<'a>(
    name:               &str,
    mark:               &Mark,
    file_dependencies:  &HashSet<u32>,
    global_vars:        &'a Globals,
) -> Result<&'a GlobalVarData> {
    let Some(var) = global_vars.get(name) else {
        return Err(make_error(CompilationError::NotInScope(name.into(), None), mark.clone()));
    };
    if !file_dependencies.contains(&var.mark.file) {
        return Err(make_error(
            CompilationError::NotInScope(name.into(), Some(var.mark.file)),
            mark.clone()
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
    let Type::Type {type_constructor: tc1, arguments: args1}: Type = data.final_type().clone()
    else {
        dbg!(data.final_type());
        unreachable!()
    };
    let Type::Type {type_constructor: tc2, arguments: args2}: Type = expected.final_type().clone()
    else {unreachable!()};
    if tc1 != tc2 { return Err(make_error(
        CompilationError::TypeMismatch(expected, Some(data)),
        mark.clone()
    ))}
    let mut to_replace: Vec<(usize, Type)> = Vec::new();
    args1
        .into_iter()
        .zip(args2)
        .for_each(|(t1, t2)| if let Type::Generic(a) = t1 { // TODO probably broken
            to_replace.push((a, t2))
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

fn build_table_of_unknown_generics(t: &Type) -> HashMap<usize, Option<Type>> {
    let mut map = HashMap::new();
    build_table_of_unknown_generics_helper(t, &mut map);
    map
}

fn build_table_of_unknown_generics_helper(t: &Type, map: &mut HashMap<usize, Option<Type>>) {
    match t {
        Type::Type { arguments, .. } => arguments
            .iter()
            .for_each(|x| build_table_of_unknown_generics_helper(x, map)),
        Type::Function(t1, t2) => {
            build_table_of_unknown_generics_helper(t1, map);
            build_table_of_unknown_generics_helper(t2, map);
        }
        Type::Generic(n) => {
            map.insert(*n, None);
        }
    }
}

fn substitute_back_in(t: &mut Type, map: &HashMap<usize, Option<Type>>) {
    match t {
        Type::Type { arguments, .. } => arguments
            .iter_mut()
            .for_each(|x| substitute_back_in(x, map)),
        Type::Function(t1, t2) => {
            substitute_back_in(&mut (*t1), map);
            substitute_back_in(&mut (*t2), map);
        }
        Type::Generic(n) => {
            *t = map.get(n).unwrap().as_ref().unwrap().clone()
        }
    }
}

// this function take the name of the variable
// and the tokens and returns its type, and
// possibly the number of arguments

fn new_figure_out_type<'a>(
    expected_type:        Option<&Type>,
    bt:                   BlockTraversal<'a>,
    name:                 &str,
    mark:                 &Mark,
    global_vars:          &Globals,
    local_vars:           &HashMap<String, (u32, Type, Mark)>,
    generics:             &Generics
)                      -> Result<(Type, Option<u32>)> {
    let contains_unknown_generics: bool;
    let mut output: Type;
    if let Some((_, tp, _)) = local_vars.get(name) {
        output = tp.clone();
        contains_unknown_generics = false;
    }
    else if let Some(GlobalVarData { var_type, .. }) = global_vars.get(name) {
        output = var_type.clone();
        contains_unknown_generics = has_generics(var_type);
    } else {
        return Err(make_error(
            CompilationError::NotInScope(name.to_string(), None),
            mark.clone()
        ));
    } ;
    if !contains_unknown_generics {
        let arg_count = if bt.expect_end().is_ok() { Some(0) } else { None };
        // the arg counter is only useful when we don't
        // know the block's type. If we do, we can skip
        // the rest and avoid the token clone
        return Ok((output,  arg_count))
    }
    // map containing the unknown generics mapped to found types
    let mut unknown_generics: HashMap<usize, Option<Type>> = build_table_of_unknown_generics(&output);
    'success: {
        if
            let Some(Type::Type { type_constructor: c1, arguments: a1 }) = expected_type.map(|x| x.final_type()) &&
            let Type::Type { type_constructor: c2, arguments: a2 } = output.final_type()
        {
            assert_eq!(c1, c2);
            for (a, b) in a1.iter().zip(a2.iter()) {
                merge_types(b, a, &mut unknown_generics);
            }
            if unknown_generics.values().all(|x| x.is_some()) {
                let arg_count = if bt.expect_end().is_ok() { Some(0) } else { None };

                substitute_back_in(&mut output, &unknown_generics);
                return Ok((output, arg_count))
            }
        }
        let t_args = output.clone().arg_types();
        let mut t_args = t_args.into_iter();
        let mut number_args_used = Some(0);
        let mut counter = {
            match expected_type {
                None => -1,
                Some(x) =>  (output.clone().arg_types().len() - x.clone().arg_types().len()) as i32,
            }
        };
        let mut bt = bt;
        loop {
            if bt.reached_end_of_line() {
                counter = -1; // everything else is fair game
                break
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
                OwnedToken::Word(word) => word,
                _ => {
                    return Err(make_error(CompilationError::CannotInferType, var_mark.clone()))
                }
            };
            let has_unknown_generics: bool;
            let got_t = match local_vars.get(var_name) {
                Some((_, t, _)) => {
                    has_unknown_generics = false;
                    t.clone()
                }
                None => match global_vars.get(var_name) {
                    Some(GlobalVarData { var_type, ..}) => {
                        has_unknown_generics = has_generics(var_type);
                        var_type.clone()
                    }
                    None => {
                        return Err(make_error(
                            CompilationError::NotInScope(var_name.clone(), None),
                            var_mark.clone()
                        ));
                    }
                }
            };
            if has_unknown_generics {
                // not much i can do here
                if matches!(got_t, Type::Type { .. }) {
                }
                else if got_t.is_function() && matches!(got_t.final_type(), Type::Type { .. }) {
                    let expected_arg_count = t.clone().arg_types().len();
                    let got_arg_count = got_t.clone().arg_types().len();
                    if expected_arg_count != got_arg_count {
                        break 'success;
                    }
                }
                else {
                    break 'success;
                }
                continue
            }
            if got_t.is_function() {
                // might be slightly wrong
                let expected_arg_count = t.clone().arg_types().len();
                let got_arg_count = got_t.clone().arg_types().len();
                if expected_arg_count != got_arg_count {
                    break 'success;
                }
            }
            merge_types(&t, &got_t, &mut unknown_generics);
        }
        let mut total_number_args = number_args_used;
        if let Ok(NextOutput::IndentedBlocks(branches)) = bt.next() && counter == -1 {
            total_number_args = number_args_used.map(|x| x + branches.len() as u32);
            let zipped = t_args.zip(branches);
            for (tp, bt) in zipped {
                number_args_used = number_args_used.map(|x| x + 1);
                if !has_generics(&tp) {
                    continue
                }
                let (token, mark, leftover) = bt.next_token_in_line().unwrap(); // safe
                let got_t = match token {
                    OwnedToken::Keyword(Keyword::The) => new_parse_type(leftover, generics)?.0,
                    OwnedToken::Word(w) => {
                        match new_figure_out_type(
                            None,
                            leftover,
                            w,
                            mark,
                            global_vars,
                            local_vars,
                            generics
                        ) {
                            Ok((var_t, Some(arg_count))) => {
                                cut_off_n(&var_t, arg_count).clone()
                            }
                            _ => {
                                // TODO pattern match on error
                                continue
                            }
                        }
                    }
                    _ => continue, // add error to vec
                };
                // check_compatable(tp, got_t)?;
                merge_types(&tp, &got_t, &mut unknown_generics);
            }
        }
        if
            let Some(t) = expected_type    &&
            let Some(n) = number_args_used &&
            n == total_number_args.unwrap()
        {
            let temp_t: &Type = cut_off_n(&output, n);
            merge_types(temp_t, t, &mut unknown_generics);
        }
        if unknown_generics.values().any(|x| x.is_none()) {
            return Err(make_error(CompilationError::CannotInferType, mark.clone()))
        }
        substitute_back_in(&mut output, &unknown_generics);
        return Ok((output, number_args_used))
    }
    if unknown_generics.values().all(|x| x.is_some()) {
        substitute_back_in(&mut output, &unknown_generics);
        return Ok((output, None))
    }
    Err(make_error(CompilationError::CannotInferType, mark.clone()))
}

fn merge_types(unknown: &Type, new: &Type, output: &mut HashMap<usize, Option<Type>>) {
    match (unknown, new) {
        (Type::Generic(x), _) => {
            output.insert(*x, Some(new.clone()));
        }
        (Type::Function(a1,b1), Type::Function(a2,b2)) => {
            merge_types(a1, a2, output);
            merge_types(b1, b2, output);
        }
        (Type::Type { arguments: args1, .. }, Type::Type { arguments: args2, .. } ) => {
            args1
                .iter()
                .zip(args2.iter())
                .for_each(|(a, b)| merge_types(a, b, output));
        }
        (a, b) => {
            dbg!(a);
            dbg!(b);
            todo!()
        }
    }
}

pub fn new_parse_expression<'a>(
    expressions:           &mut Vec<Expression>,
    file_dependencies:     &HashSet<u32>,
    expected_type:         &Type,
    bt:                    BlockTraversal<'a>,
    local_vars:            &LocalVars,
    local_vars_count:      u32,
    global_vars:           &Globals,
    generics:              &Generics,
) -> Result<(Expression, Option<BlockTraversal<'a>>)> {
    // assert_eq!(local_vars.len(), local_vars_count as usize); // TODO figure out why this fails
    let (token, mark, next_bt) = bt.next_token_in_line_fallthrough()?;
    match token {
        OwnedToken::Keyword(Keyword::The) => {
            let (tp, bt) = parse_the(bt, generics)?;
            new_parse_expression(
                expressions,
                file_dependencies,
                &tp,
                bt,
                local_vars,
                local_vars_count,
                global_vars,
                generics,
            )
        }
        OwnedToken::Keyword(Keyword::Undefined) => {
            BlockTraversal::expect_end(next_bt)?;
            Ok((Expression::Undefined(Box::new(mark.clone())), None))
        }
        OwnedToken::Keyword(Keyword::Lambda) => {
            let Type::Function(a, b) = expected_type else {
                return Err(make_error(
                    CompilationError::TypeMismatch(expected_type.clone(), None),
                    mark.clone()
                ))
            };
            let (pattern, _pattern_mark, expr) = parse_lambda_case(
                expressions,
                file_dependencies,
                a,
                b,
                bt,
                local_vars,
                local_vars_count,
                global_vars,
                generics
            )?;
            Ok((
                Expression::Lambda {
                    pattern: Rc::new(Marked::<Pattern> {
                        value: pattern,
                        mark: mark.clone()
                    }),
                    body: Box::new(expr),
                },
                None
            ))
        }
        OwnedToken::Keyword(Keyword::Match) => {
            let matched_on_block;
            let (mut matched_on_bt, branches_bt) = match next_bt.next()? {
                NextOutput::Token { .. } => {
                    let line = next_bt.block.line_tokens
                        .clone()
                        .into_iter()
                        .skip(next_bt.word)
                        .collect();
                    matched_on_block = Block {
                        line_tokens: line,
                        line_end_mark: next_bt.block.line_end_mark.clone(),
                        indented_blocks_beneath: Vec::new(),
                    };
                    (BlockTraversal::new(&matched_on_block), next_bt.get_indented_blocks())
                }
                NextOutput::IndentedBlocks(v) => {
                    (v[0], v.into_iter().skip(1).collect())
                }
            };
            let tp: Type = {
                let (value, mark, bt) = matched_on_bt.next_token_in_line()?;
                match value {
                    OwnedToken::Keyword(Keyword::The) => {
                        let (tp, body_bt) = parse_the(matched_on_bt, generics)?;
                        matched_on_bt = body_bt;
                        tp
                    }
                    OwnedToken::Word(first_name) => {
                        let (root_type, _) = new_figure_out_type(
                            None,
                            bt,
                            first_name,
                            mark,
                            global_vars,
                            local_vars,
                            generics
                        )?;
                        root_type.final_type().clone()
                    }
                    OwnedToken::Keyword(_) => {
                        return Err(make_error(ParseError::UnexpectedKeyword, mark.clone()))
                    }
                }
            };
            let (matched_on_expr, leftover_bt) = new_parse_expression(
                expressions,
                file_dependencies,
                &tp,
                matched_on_bt,
                local_vars,
                local_vars_count,
                global_vars,
                generics,
            )?;
            BlockTraversal::expect_end_option(leftover_bt)?;
            let mut branches: Vec<(Rc<Marked<Pattern>>, Expression)> = Vec::with_capacity(branches_bt.len());
            let mut encountered_wildcard = false;
            for branch_bt in branches_bt {
                let (case_mark, _) = branch_bt.expect_keyword(Keyword::Case)?;
                if encountered_wildcard {
                    return Err(make_error(CompilationError::RedundentPattern, case_mark.clone()))
                }
                let (pattern, pattern_mark, expr) = parse_lambda_case(
                    expressions,
                    file_dependencies,
                    &tp,
                    expected_type,
                    branch_bt,
                    local_vars,
                    local_vars_count,
                    global_vars,
                    generics
                )?;
                if matches!(pattern, Pattern::Dropped | Pattern::Captured(_)) {
                    encountered_wildcard = true;
                }
                branches.push((
                    Rc::new(Marked::<Pattern> {
                        value: pattern,
                        mark: pattern_mark.clone(),
                    }),
                    expr
                ));
            }
            Ok((
                Expression::Match {
                    matched_on: Box::new(matched_on_expr),
                    branches: branches.into(),
                },
                None
            ))
        }
        OwnedToken::Word(name) => {
            // peek if the next token is a newline
            let (root_id, root_type) = if let Some((a, b, _)) = local_vars.get(name) {
                (Expression::LocalVarPlaceholder(*a), b.clone())
            } else {
                let GlobalVarData { id: a, .. } = lookup_global_vars(
                    name,
                    mark,
                    file_dependencies,
                    global_vars,
                )?;
                let (Id::Variable(a) | Id::Constructor(a)) = a;
                let (t, _) = new_figure_out_type(
                    Some(expected_type),
                    next_bt,
                    name,
                    mark,
                    global_vars,
                    local_vars,
                    generics
                )?;
                (expressions[*a].clone(), t)
            };
            if !root_type.is_possible(expected_type) {
                return Err(make_error(
                    CompilationError::TypeMismatch( expected_type.clone(), Some(root_type.clone())),
                    mark.clone()
                ))
            }
            let mut output_args = Vec::new();
            let mut current_type = root_type.to_owned();
            let mut ret_bt = Some(next_bt);
            while current_type != *expected_type {
                let bt = BlockTraversal::expect_no_indents(ret_bt, mark)?;
                match bt.next()? {
                    NextOutput::IndentedBlocks(v) => {
                        let mut arg_groups = v.into_iter();
                        while current_type != *expected_type {
                            let Some(current_bt) = arg_groups.next() else {
                                return Err(make_error(CompilationError::ExpectedMoreArguments, mark.clone()))
                            };
                            let (next_type, leftover) = match current_type {
                                Type::Function(a, b) => (*a, *b),
                                Type::Type { .. } | Type::Generic(_) => unreachable!(),
                            };
                            current_type = leftover;
                            let (next_arg, bt) = new_parse_expression(
                                expressions,
                                file_dependencies,
                                &next_type,
                                current_bt,
                                local_vars,
                                local_vars_count,
                                global_vars,
                                generics,
                            )?;
                            BlockTraversal::expect_end_option(bt)?;
                            output_args.push(next_arg);
                        }
                        if let Some(trailing_line) = arg_groups.next() {
                            trailing_line.expect_end()?;
                        }
                        ret_bt = None;
                        break
                    }
                    NextOutput::Token { .. } => {
                        let (next_type, leftover) = match current_type {
                            Type::Function(a, b) => (*a, *b),
                            Type::Type { .. } | Type::Generic(_) => unreachable!(),
                        };
                        current_type = leftover;
                        let (next_arg, new_bt) = new_parse_expression(
                            expressions,
                            file_dependencies,
                            &next_type,
                            bt,
                            local_vars,
                            local_vars_count,
                            global_vars,
                            generics,
                        )?;
                        output_args.push(next_arg);
                        ret_bt = new_bt
                    }
                }
            }
            Ok(
                if output_args.is_empty() {
                    (root_id, ret_bt)
                } else {
                    (Expression::Tree {
                        root: Box::new(root_id),
                        arguments: output_args.into(),
                    }, ret_bt)
                }
            )
        }
        _ => {
            debug_print_block(bt, 0);
            todo!();
        }
    }
}

// lambda and case have the same syntax

pub fn parse_lambda_case<'a>(
    expressions:           &mut Vec<Expression>,
    file_dependencies:     &HashSet<u32>,
    pattern_type:          &Type,
    block_type:            &Type,
    bt:                    BlockTraversal<'a>,
    local_vars:            &LocalVars,
    local_vars_count:      u32,
    global_vars:           &Globals,
    generics:              &Generics,
) -> Result<(Pattern, &'a Mark, Expression)> {
    let (lambda_mark, bt) = bt.expect_keyword(Keyword::Lambda).or(bt.expect_keyword(Keyword::Case))?;
    let (pattern, local_vars_new, local_vars_count_new, body_bt) = match bt.next()? {
        NextOutput::IndentedBlocks(v) => {
            assert!(v.len() == 2);
            let (pattern, _mark, local_vars_new, local_vars_count_new, leftover_bt) =
                new_parse_pattern(local_vars_count, pattern_type, v[0], global_vars)?;
            BlockTraversal::expect_end_option(leftover_bt)?;
            (pattern, local_vars_new, local_vars_count_new, v[1])
        }
        NextOutput::Token { .. } => {
            let (pattern, _mark, local_vars_new, local_vars_count_new, body_bt) =
                new_parse_pattern(local_vars_count, pattern_type, bt, global_vars)?;
            (
                pattern,
                local_vars_new,
                local_vars_count_new,
                BlockTraversal::expect_no_indents(body_bt, lambda_mark)?
            )
        }
    };
    let mut local_vars: &LocalVars = local_vars;
    let mut new_locals;
    if !local_vars_new.is_empty() {
        new_locals = local_vars.clone();
        local_vars_new
            .clone()
            .into_iter()
            .for_each(|(k, v)| { new_locals.insert(k, v); });
        local_vars = &new_locals;
    }
    let (body, bt) = new_parse_expression(
        expressions,
        file_dependencies,
        block_type,
        body_bt,
        local_vars,
        local_vars_count_new,
        global_vars,
        generics,
    )?;
    BlockTraversal::expect_end_option(bt)?;
    for (_, (id, _tp, mark)) in local_vars_new.into_iter() {
        if !is_used(&body, id) {
            return Err(make_error(CompilationError::NotUsed, mark))
        }
    }
    Ok((pattern, lambda_mark, body))
}

#[derive(Hash, PartialEq, Eq)]
pub enum BlockKind {
    Type,
    Variable
}

pub struct NameAndGenerics {
    pub name: String,
    pub mark: Mark,
    pub generics: Generics,
    pub kind: BlockKind
}

pub fn new_extract_name_and_generics<'a>(bt: BlockTraversal<'a>) -> Result<(NameAndGenerics, BlockTraversal<'a>)> {
    let mut generics: Generics = Vec::new();
    let (name, mark, kind, bt) = extract_name_and_genericsl_helper(bt, &mut generics)?;
    fn extract_name_and_genericsl_helper<'a>(
        bt: BlockTraversal<'a>,
        generics: &mut Generics
    ) -> Result<(String, Mark, BlockKind, BlockTraversal<'a>)> {
        let (token, mark1, mut bt) = bt.next_token_in_line_fallthrough()?;
        match token {
            OwnedToken::Keyword(Keyword::ForAll) => {
                while let Ok((name, _name_mark, bt_)) = bt.expect_word() {
                    bt = bt_;
                    let index = generics.len() + 1;
                    generics.push((name.to_string(), index));
                }
                extract_name_and_genericsl_helper(bt, generics)
            }
            OwnedToken::Keyword(Keyword::Let) => {
                let (name, name_mark, bt) = bt.expect_word()?;
                let (_be_mark, bt) = bt.expect_keyword(Keyword::Be)?;
                Ok((name.to_string(), name_mark.clone(), BlockKind::Variable, bt))
            }
            OwnedToken::Keyword(Keyword::Type) => {
                let (name, name_mark, bt) = bt.expect_word()?;
                let (_be_mark, bt) = bt.expect_keyword(Keyword::Contains)?;
                Ok((name.to_string(), name_mark.clone(), BlockKind::Type, bt))
            }
            _ => Err(Error {
                mark: mark1.clone(),
                error_type: Box::new(ParseError::UnexpectedKeyword),
                note: Some(String::from("\x1b[90mexpected one of \x1b[97mdefine forall type\x1b[90m")),
            }),
        }
    }
    Ok((NameAndGenerics { name: name.to_string(), mark: mark.clone(), generics, kind }, bt))
}

pub fn parse_the<'a>(bt: BlockTraversal<'a>, generics: &Vec<(String, usize)>) -> Result<(Type, BlockTraversal<'a>)> {
    let (the_mark, bt) = bt.expect_keyword(Keyword::The)?;
    let (tp, body) = match bt.next()? {
        NextOutput::IndentedBlocks(v) => {
            assert_eq!(v.len(), 2);
            let (tp, leftover) = new_parse_type(v[0], generics)?;
            BlockTraversal::expect_end_option(leftover).unwrap();
            (tp, v[1])
        }
        NextOutput::Token { .. } => {
            let (tp, leftover) = new_parse_type(bt, generics)?;
            (tp, BlockTraversal::expect_no_indents(leftover, the_mark)?)
        }
    };
    Ok((tp, body))
}

pub fn new_parse_type<'a> (bt: BlockTraversal<'a>, generics: &Vec<(String, usize)>)
    -> Result<(Type, Option<BlockTraversal<'a>>)>
{
    new_parse_type_helper(bt, generics, Kind::Type)
}

fn new_parse_type_helper<'a>(
    bt:       BlockTraversal<'a>,
    generics: &Vec<(String, usize)>,
    mut _kind: Kind // TODO
) -> Result<(Type, Option<BlockTraversal<'a>>)> {
    let (word, mark, leftover_bt) = bt.expect_word()?;
    let is_fn = word == "fn";
    let mut kind = Kind::Fn(
        Kind::Type.into(),
        Kind::Fn(Kind::Type.into(), Kind::Type.into()).into()
    );
    let mut index = None;
    if !is_fn {
        if let Some(t) = get_from_generics(word, generics) {
            return Ok((t, Some(leftover_bt)))
        }
        {
            let ptr = TYPES.lock().unwrap();
            index = Some(ptr.as_ref().unwrap().get(word).ok_or(Error {
                mark: mark.clone(),
                error_type: Box::new(CompilationError::TypeNotInScope(word.to_string())),
                note: None,
            })?.clone());
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
                let (tp, bt_) = new_parse_type_helper(bt, generics, Kind::Type)?;
                arguments.push(tp);
                match BlockTraversal::expect_no_indents(bt_, mark) {
                    Err(e) if !matches!(kind, Kind::Type) => {
                        return Err(e)
                    }
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
                for bt in v {
                    let (tp, leftover) = new_parse_type_helper(bt, generics, Kind::Type)?;
                    BlockTraversal::expect_end_option(leftover)?;
                    arguments.push(tp);
                }
                ret_bt = None;
                break;
            }
        }
    }
    let ret = if is_fn {
        Type::Function(Box::new(arguments[0].clone()), Box::new(arguments[1].clone()))
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

pub fn new_parse_data(
    bt: BlockTraversal,
    parent_type: u32,
    generics: &Generics,
) -> Result<Vec<(String, Type, Mark)>> {
    let mut output = Vec::new();
    match bt.next()? {
        NextOutput::IndentedBlocks(v) => {
            for branch in v {
                let (constructor_name, constructor_mark, bt) = branch.expect_word()?;
                let mut arg_types: Vec<Type> = Vec::new();
                match bt.next() {
                    Ok(NextOutput::IndentedBlocks(ts)) => {
                        for type_block in ts {
                            let (t, leftover_bt) = new_parse_type(type_block, generics)?;
                            BlockTraversal::expect_end_option(leftover_bt)?;
                            arg_types.push(t);
                        }
                    }
                    _ => {
                        let mut temp_bt = bt;
                        while matches!(temp_bt.next(), Ok(NextOutput::Token {..})) {
                            let (t, leftover_bt) = new_parse_type(temp_bt, generics)?;
                            arg_types.push(t);
                            temp_bt = BlockTraversal::expect_no_indents(leftover_bt, constructor_mark)?;
                        }
                        temp_bt.expect_end()?;
                    }
                }
                let mut args = arg_types.into_iter();
                output.push((constructor_name.to_string(), build_type(
                    &mut args,
                    Type::Type { type_constructor: parent_type, arguments: generics_to_type(generics) }),
                    constructor_mark.clone()
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
        arguments: Vec<Type>
    },
    Function(Box<Type>, Box<Type>),
    Generic(usize)
}

impl Type {
    pub fn final_type(&self) -> &Type {
        match self {
            Self::Function(_, b) => b.final_type(),
            _ => self
        }
    }

    pub fn is_possible(&self, test: &Self) -> bool {
        *self == *test || match self {
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
                Type::Type { type_constructor: a, arguments } => {
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
            ptr.as_ref().unwrap().clone().into_iter().for_each(|(k, w)| { new_map.insert(w.id as u32, k); });
        }
        helper(self, &new_map, &mut output);
        output
    }
}

fn cut_off_n(mut tp: &Type, n: u32) -> &Type {  // should be result
    for _ in 0..n {
        match tp {
            Type::Function(_, b) => {
                tp = b
            }
            _ => todo!(),
        }
    }
    tp
}

#[derive(Debug, Hash, Clone)]
pub enum Kind {
    Type,
    Fn(Box<Kind>, Box<Kind>)
}
