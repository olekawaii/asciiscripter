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

use crate::error::{Error, ErrorType, Index, Mark};
use crate::runtime::{Expression, Pattern};
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::sync::{Mutex, Arc};

use crate::runtime::optimize_expression;

const INDENTATION: u32 = 4;

type Result<T> = std::result::Result<T, Error>;

type LocalVars = HashMap<String, (u32, Type, Mark)>;
type GlobalVars = HashMap<String, (usize, Type, bool)>;

static TYPES: Mutex<Option<HashMap<String, u32>>> = Mutex::new(None);

pub fn parse_file( file_name: Marked<String>,) -> Result<(Vec<Expression>, GlobalVars)> {
    let mut temp_vec = HashSet::new();
    let blocks = get_tokens(file_name, &mut temp_vec)?;
    let (type_blocks, values): (Vec<(Signiture, TokenStream)>, _) = blocks
        .into_iter()
        .map(|mut x| {
            let signiture = extract_signiture(&mut x)?;
            Ok((signiture, x))
        })
        .collect::<Result<Vec<(Signiture, TokenStream)>>>()?
        .into_iter()
        .partition(|(x, _)| matches!(x, Signiture::Type(_,_)));
    let mut types: HashMap<String, u32> = HashMap::new();
    let mut data_blocks: Vec<(u32, TokenStream)> = Vec::new();
    for (number_of_types, (signiture, tokens)) in type_blocks.into_iter().enumerate() {
        let number_of_types = number_of_types as u32;
        let Signiture::Type(name, name_mark) = signiture else { unreachable!() };
        if types.insert(name, number_of_types).is_some() {
            return Err(make_error(CompilationError::MultipleDeclorations, name_mark))
        }
        data_blocks.push((number_of_types, tokens));
    }
    {
        let mut ptr = TYPES.lock().unwrap();
        *ptr = Some(types);
    }
    let mut number_of_values = 0;
    let mut global_vars_dummy: GlobalVars = HashMap::new();
    let mut global_vars: Vec<Expression> = Vec::with_capacity(data_blocks.len());
    let mut constructors: HashMap<u32, HashSet<usize>> = HashMap::new();
    for (tp, tokens) in data_blocks.into_iter() {
        let mut set = HashSet::new();
        for (num, (name, constructor_tp, name_mark)) in parse_data(tokens, tp)?.into_iter().enumerate() {
            set.insert(number_of_values);
            if global_vars_dummy.insert(name, (number_of_values, constructor_tp, true)).is_some() {
                return Err(make_error(CompilationError::MultipleDeclorations, name_mark))
            }
            number_of_values += 1;
            global_vars.push(Expression::DataConstructor(num as u32));
        }
        constructors.insert(tp, set);
    }
    let mut vals: Vec<(Type, TokenStream)> = Vec::with_capacity(values.len());
    for (signiture, tokens) in values.into_iter() {
        let Signiture::Value(name, name_mark, mut type_tokens) = signiture else { unreachable!() };
        let tp = parse_type(&mut type_tokens)?;
        if global_vars_dummy.insert(name, (number_of_values, tp.clone(), false)).is_some() {
            return Err(make_error(CompilationError::MultipleDeclorations, name_mark))
        }
        vals.push((tp, tokens));
        number_of_values += 1;
    }
    for (tp, mut tokens) in vals.into_iter() {
        global_vars.push({
            let mut e = parse_expression(
                tp, 
                &mut 
                tokens, 
                HashMap::new(), 
                0, 
                &global_vars_dummy, 
                &constructors
            )?;
            optimize_expression(&mut e);
            e
        })
    }
    Ok((global_vars, global_vars_dummy))
}

fn parse_pattern(
    mut number_of_local: u32,
    expected_type: &Type,
    tokens: &mut TokenStream,
    global_vars: &GlobalVars,
) -> Result<(Pattern, Mark, LocalVars, u32)> {
    let mut output = HashMap::new();
    let mut mark = tokens.peek().unwrap().mark.clone();
    mark.word_index = Index::Art(0);
    let pattern = parse_pattern_helper(
        &mut number_of_local,
        expected_type,
        &mut output,
        tokens,
        global_vars,
    )?;
    Ok((pattern, mark, output, number_of_local))
}

fn parse_pattern_helper(
    number_of_local: &mut u32,
    expected_type: &Type,
    output: &mut LocalVars,
    tokens: &mut TokenStream,
    global_vars: &GlobalVars,
) -> Result<Pattern> {
    let (name, mark) = next_word(tokens)?.destructure();
    if name.starts_with('_') {
        return Ok(Pattern::Dropped)
    }
    if let Some((id, tp, is_constructor)) = global_vars.get(&name)
        && !expected_type.is_a_function()
        && *is_constructor
        && Type::Type(tp.final_type()) == *expected_type {
            let mut patterns = Vec::new();
            for t in tp.clone().arg_types() {
                patterns.push(parse_pattern_helper(
                    number_of_local,
                    &t,
                    output,
                    tokens,
                    global_vars,
                )?)
            }
            Ok(Pattern::DataConstructor(*id as u32, patterns))
    } else {
        *number_of_local += 1;
        output.insert(name, (*number_of_local, expected_type.clone(), mark));
        Ok(Pattern::Captured(*number_of_local))
    }
}

fn is_used(expression: &Expression, id: u32) -> bool {
    match expression {
        Expression::Undefined { .. } => true,
        Expression::Lambda { body, .. } => is_used(&*body, id),
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
            if is_used(&*matched_on, id) { 
                return true 
            }
            for (_pattern, expr) in branches.iter() {
                if is_used(expr, id) {
                    return true
                }
            }
            false
        }
        Expression::Thunk(_) | Expression::DataConstructor(_) | Expression::Variable(_) => false
    }
}
 
pub fn get_tokens(file: Marked<String>, done: &mut HashSet<String>) -> Result<Vec<TokenStream>> {
    let (file, mark) = file.destructure();
    if done.contains(&file) { 
        return Ok(Vec::new()) 
    }
    let Ok(file_contents) = read_to_string(&file) else {
        return Err(make_error(CompilationError::BadFile(file), mark))
    };
    eprintln!("including \x1b[95m{file}\x1b[0m");
    done.insert(file.clone());
    let mut output = tokenize_file(file_contents, &Arc::new(file))?;
    while matches!(output[0].peek().unwrap().value, Token::NewLine(_)) {
        output[0].next();
    }
    if matches!(
        output[0].peek().unwrap().value,
        Token::Keyword(Keyword::Include)
    ) {
        let mut imports = output.remove(0);
        imports.next(); // remove the import keyword
        while let Ok(i) = next_word(&mut imports) {
            let (mut x, mark) = i.destructure();
            x.push_str(".th");
            let mut f = get_tokens(
                Marked::<String> {
                    value: x,
                    mark,
                }, 
                done
            )?;
            output.append(&mut f)
        }
    }
    Ok(output)
}

pub fn tokenize_file(input: String, file_name: &Arc<String>) -> Result<Vec<TokenStream>> {
    let file_lines: Arc<Vec<String>> =
        Arc::new(input.lines().map(|x| x.trim_end().to_string()).collect());
    let mut output: Vec<TokenStream> = Vec::new();
    let mut current_block: Vec<(usize, String)> = Vec::new();
    let mut file = file_lines.iter().map(|x| x.as_str()).enumerate();
    while let Some((line_number, string)) = file.next() {
        if string.is_empty() || string.split_whitespace().next().unwrap() == "--" { // safe unwrap
            continue
        }
        current_block.push((line_number, string.to_string()));
        'good_lines: loop {
            match file.next() {
                None | Some((_, "")) => {
                    output.push(tokenize(
                        current_block,
                        Arc::clone(file_name),
                        Arc::clone(&file_lines),
                    )?);
                    current_block = Vec::new();
                    break 'good_lines;
                }
                Some((line_number, string)) => {
                    current_block.push((line_number, string.to_string()));
                }
            }
        }
    }
    Ok(output)
}

pub fn parse_roman_numeral(numeral: &str) -> Option<u32> {
    let mut numerals: Vec<(&str, u32)> = vec![
        ("i", 1),
        ("iv", 4),
        ("v", 5),
        ("ix", 9),
        ("x", 10),
        ("xl", 40),
        ("l", 50),
        ("xc", 90),
        ("c", 100),
    ];
    let mut starting_index = 0;
    let mut consecutive_times = 0;
    let mut output = 0;
    // unwrap is safe assuming numerals vec is not empty
    let mut tuple = numerals.pop().unwrap();
    loop {
        let pattern = tuple.0;
        let value = tuple.1;
        let pattern_len = pattern.len();
        let numeral_len = numeral.len() - starting_index;
        if numeral_len == 0 {
            return Some(output)
        } else if numeral_len < pattern_len
            || &numeral[starting_index..starting_index + pattern_len] != pattern
        {
            tuple = numerals.pop()?;
            consecutive_times = 0;
        } else {
            output += value;
            starting_index += pattern_len;
            if consecutive_times != 0 && (pattern_len > 1 || consecutive_times > 3) {
                return None;
            };
            let skips = match value.to_string().chars().next().unwrap() {
                '1' => 0,
                '4' | '5' => 1,
                '9' => 3,
                _ => unreachable!(),
            };
            for _ in 0..skips {
                numerals.pop();
            }
            consecutive_times += 1;
        }
    }
}

pub fn parse_expression(
    expected_type:         Type,
    tokens:                &mut TokenStream,
    mut local_vars:        LocalVars,
    local_vars_count:      u32,
    global_vars:           &GlobalVars,
    constructors:          &HashMap<u32, HashSet<usize>>
) -> Result<Expression> {
    let (token, keyword_mark) = next_non_newline(tokens)?.destructure();
    match token {
        Token::EndOfBlock | Token::NewLine(_) => unreachable!(),
        Token::Keyword(k) => match k {
            Keyword::Undefined => Ok(Expression::Undefined(Box::new(keyword_mark))),
            Keyword::Lambda => {
                match expected_type {
                    Type::Type(_) => Err(make_error(
                        CompilationError::TypeMismatch(expected_type, None), 
                        keyword_mark
                    )),
                    Type::Function(a, b) => {
                        let (pattern, _mark, local_vars_new, local_vars_count) = parse_pattern(
                            local_vars_count, 
                            &a, 
                            tokens, 
                            global_vars
                        )?;
                        //validate_patterns(vec![(pattern.clone(), mark)], constructors, global_vars, keyword_mark)?;
                        local_vars_new
                            .clone()
                            .into_iter()
                            .for_each(|(k, v)| { local_vars.insert(k, v); });
                        let body = parse_expression(
                            *b, 
                            tokens, 
                            local_vars, 
                            local_vars_count, 
                            global_vars, 
                            constructors
                        )?;
                        for (_, (id, _tp, mark)) in local_vars_new.into_iter() {
                            if !is_used(&body, id) {
                                return Err(make_error(CompilationError::NotUsed, mark))
                            }
                        }
                        Ok(Expression::Lambda {
                            pattern: Arc::new(pattern),
                            body: Box::new(body),
                        })
                    }
                }
            }
            Keyword::Match => {
                remove_leading_newlines(tokens);
                let (value, mark) = tokens.peek().unwrap().clone().destructure();
                let tp = match value {
                    Token::Keyword(Keyword::OfType) => {
                        tokens.next(); // safe
                        parse_type(tokens)?
                    }
                    Token::Word(first_name) => {
                        let root_type = if let Some((_, b, _)) = local_vars.get(&first_name) { b } 
                        else if let Some((_, b, _)) = global_vars.get(&first_name) { b } 
                        else { return Err(make_error(CompilationError::NotInScope, mark.clone())) };
                        Type::Type(root_type.final_type())
                    }
                    Token::Keyword(_) => return Err(make_error(CompilationError::UnexpectedKeyword, mark)),
                    Token::EndOfBlock => return Err(make_error(CompilationError::UnexpectedEnd,     mark)),
                    Token::NewLine(_) => unreachable!(),
                };
                let mut matched_on_tokens = Vec::new();
                let mut unmatched_matches = 0;
                loop {
                    let token = match next_token(tokens) {
                        Ok(x) => x,
                        Err(mut e) => {
                            e.error_type = Box::new(CompilationError::KeywordNotFound(Keyword::With));
                            return Err(e)
                        }
                    };
                    match &token.value {
                        Token::Keyword(Keyword::With) if unmatched_matches == 0 => break,
                        Token::Keyword(Keyword::With) => {
                            unmatched_matches -= 1;
                            // technically should never happen
                            if unmatched_matches < 0 {
                                return Err(make_error(CompilationError::UnexpectedKeyword, token.mark))
                            }
                            matched_on_tokens.push(token);
                        }
                        Token::Keyword(Keyword::Match) => {
                            unmatched_matches += 1;
                            matched_on_tokens.push(token);
                        }
                        _ => matched_on_tokens.push(token),
                    }
                }
                let mut matched_on_tokens = new_tokenstream(matched_on_tokens);
                let matched_on = parse_expression(
                    tp.clone(), 
                    &mut matched_on_tokens, 
                    local_vars.clone(), 
                    local_vars_count, 
                    global_vars, 
                    constructors
                )?;
                let (token, _mark) = next_token(tokens)?.destructure();
                let Token::NewLine(indentation) = token else { panic!("no newline") };
                let mut branch_tokens = get_with_indentation(tokens, indentation);
                let mut branches: Vec<(Arc<Pattern>, Expression)> = Vec::with_capacity(branch_tokens.len());
                let mut patterns = Vec::new();
                for branch in branch_tokens.iter_mut() {
                    let (pattern, mark, local_vars_new, local_vars_count) = parse_pattern(
                        local_vars_count, 
                        &tp, 
                        branch, 
                        global_vars
                    )?;
                    let mut loc = local_vars.clone();
                    local_vars_new.clone().into_iter().for_each(|(k, v)| { loc.insert(k, v); });
                    expect_keyword(branch, Keyword::To)?;
                    let body = parse_expression(
                        expected_type.clone(),
                        branch, 
                        loc,
                        local_vars_count, 
                        global_vars, 
                        constructors
                    )?;
                    for (_, (id, _tp, mark)) in local_vars_new.into_iter() {
                        if !is_used(&body, id) {
                            return Err(make_error(CompilationError::NotUsed, mark))
                        }
                    }
                    patterns.push((pattern.clone(), mark));
                    branches.push((Arc::new(pattern), body))
                }
                //validate_patterns(patterns, constructors, global_vars, keyword_mark)?;
                Ok(Expression::Match {
                    matched_on: Box::new(matched_on),
                    branches,
                })
            }
            _ => Err(make_error(CompilationError::UnexpectedKeyword, keyword_mark))
        }
        Token::Word(name) => {
            // peek if the next token is a newline
            let (root_id, root_type) = if let Some((a, b, _)) = local_vars.get(&name) {
                (Expression::LocalVarPlaceholder(*a), b)
            } else if let Some((a, b, c)) = global_vars.get(&name) {
                (
                    if *c {
                        Expression::DataConstructor(*a as u32)
                    } else {
                        Expression::Variable(*a)
                    },
                    b,
                )
            } else {
                return Err(make_error(CompilationError::NotInScope, keyword_mark))
            };
            if !root_type.is_possible(&expected_type) {
                return Err(make_error(
                    CompilationError::TypeMismatch( expected_type, Some(root_type.clone())), 
                    keyword_mark
                ))
            }
            let mut output_args = Vec::new();
            let mut current_type = root_type.to_owned();
            while current_type != expected_type {
                match tokens.peek().unwrap().value {
                    Token::NewLine(indentation) => {
                        let mut arg_groups = get_with_indentation(tokens, indentation).into_iter();
                        while current_type != expected_type {
                            let Some(mut current_tokens) = arg_groups.next() else {
                                return Err(make_error(CompilationError::ExpectedMoreArguments, keyword_mark))
                            };
                            let (next_type, leftover) = match current_type {
                                Type::Function(a, b) => (*a, *b),
                                Type::Type(_) => unreachable!(),
                            };
                            current_type = leftover;
                            let next_arg = parse_expression(
                                next_type,
                                &mut current_tokens,
                                local_vars.clone(),
                                local_vars_count,
                                global_vars,
                                constructors
                            )?;
                            output_args.push(next_arg);
                        }
                        break
                    }
                    _ => {
                        let (next_type, leftover) = match current_type {
                            Type::Function(a, b) => (*a, *b),
                            Type::Type(_) => unreachable!(),
                        };
                        current_type = leftover;
                        let next_arg = parse_expression(
                            next_type,
                            tokens,
                            local_vars.clone(),
                            local_vars_count,
                            global_vars,
                            constructors
                        )?;
                        output_args.push(next_arg);
                    }
                }
            }

            Ok(
                if output_args.is_empty() {
                    root_id
                } else {
                    Expression::Tree {
                        root: Box::new(root_id),
                        arguments: output_args,
                    }
                }
            )
        }
    }
}

pub fn parse_art(
    width: usize,
    height: usize,
    text: Vec<Vec<Marked<char>>>,
    mark: Mark, // mark of the art keyword
) -> Result<Vec<Cells>> {
    let number_of_lines = text.len();
    if number_of_lines % height != 0 {
        return Err(Error {
            error_type: Box::new(CompilationError::BadArtHeight {
                height,
                got: number_of_lines,
            }),
            mark,
        });
    }
    for line in text.iter() {
        let length = line.len();
        if length % (width * 2) != 0 {
            return Err(Error {
                error_type: Box::new(CompilationError::BadArtLength { width, got: length }),
                mark: line[length - 1].mark.clone(),
            });
        }
    }
    let mut output: Vec<Cells> = Vec::new();
    let mut current_starting_line = 0;
    let mut current_starting_char = 0;
    loop {
        let mut current_map = HashMap::new();
        for x in 0..width {
            for y in 0..height {
                let line = y + current_starting_line;
                let art_char = text[line][x + current_starting_char].clone();
                let color_char = text[line][x + current_starting_char + width].clone();
                current_map.insert((x as u32, (height - y - 1) as u32), (art_char, color_char));
            }
        }
        output.push(current_map);
        current_starting_char += width * 2;
        if current_starting_char + 1 >= text[current_starting_line].len() {
            current_starting_char = 0;
            current_starting_line += height;
            if current_starting_line + 1 > text.len() {
                return Ok(output);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Include,
    Lambda,
    Match,
    With,
    Define,
    OfType,
    As,
    To,
    Type,
    Contains,
    Undefined,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Include => write!(f, "include"),
            Keyword::Lambda => write!(f, "lambda"),
            Keyword::Match => write!(f, "match"),
            Keyword::With => write!(f, "with"),
            Keyword::Define => write!(f, "define"),
            Keyword::OfType => write!(f, "of_type"),
            Keyword::As => write!(f, "as"),
            Keyword::To => write!(f, "to"),
            Keyword::Type => write!(f, "type"),
            Keyword::Contains => write!(f, "contains"),
            Keyword::Undefined => write!(f, "undefined"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Marked<T> {
    pub value: T,
    pub mark: Mark,
}

impl<T> Marked<T> {
    pub fn destructure(self) -> (T, Mark) {
        (self.value, self.mark)
    }
}

#[derive(Debug, Clone)]
pub enum CompilationError {
    //RedundantPattern,
    //PartialPattern,
    BadIndentation,
    InvalidColor,
    NotUsed,
    ExpectedMoreArguments,
    Custom(String),
    InvalidName,
    NotInScope,
    TypeMismatch(Type, Option<Type>),
    ExpectedRoman,
    UnexpectedKeyword,
    ExpectedKeyword(Keyword),
    //TrailingCharacters,
    BadArtLength { width: usize, got: usize },
    BadArtHeight { height: usize, got: usize },
    BadFile(String),
    UnexpectedEnd,
    ArtMissingArgs,
    TranspOnChar,
    ColorOnSpace,
    MultipleDeclorations,
    KeywordNotFound(Keyword),
}

impl ErrorType for CompilationError {
    fn gist(&self) -> &'static str {
        match self {
            Self::KeywordNotFound(_) => "looking for keyword but reached the end",
            Self::BadIndentation => "indentation not divisible by four",
            Self::NotUsed => "local variable never used",
            Self::InvalidColor => "invalid color",
            Self::MultipleDeclorations => "multiple declorations",
            //Self::PartialPattern => "not all patterns covered",
            //Self::RedundantPattern => "redundent pattern",
            Self::ColorOnSpace => "can only be used with non-spaces",
            Self::TranspOnChar => "can only be used with spaces",
            Self::ArtMissingArgs => "art expected more arguments",
            Self::ExpectedMoreArguments => "expected more arguments",
            Self::UnexpectedEnd => "unexpected end",
            Self::ExpectedKeyword(_) => "expected a keyword",
            Self::Custom(_) => "",
            Self::InvalidName => "invalid name",
            Self::NotInScope => "not in scope",
            Self::TypeMismatch(_, _) => "of unexpected type",
            Self::ExpectedRoman => "expected a roman numeral",
            Self::UnexpectedKeyword => "unexpected keyword",
            //Self::TrailingCharacters => "trailing characters",
            Self::BadArtLength { .. } => "line length not divisible by 2*width",
            Self::BadArtHeight { .. } => "number of lines not divisible by height",
            Self::BadFile(_) => "couldn't find file"
        }
    }

    fn phase(&self) -> &'static str {
        "compilation"
    }
}

impl std::fmt::Display for CompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::KeywordNotFound(k) => write!(
                f, 
                "expected to find the \x1b[97m{k}\x1b[90m keyword but reached the end"
            ),
            Self::NotUsed => write!(f, "consider prepending it with an '_' to drop the value"),
            //Self::RedundantPattern => write!(f, "this branch will never be reached"),
            Self::ColorOnSpace => write!(f, "colors can not be used on spaces. instead use . or |"),
            Self::TranspOnChar => write!(f, "colors . and | can only be used with spaces to mark transparency"),
            Self::UnexpectedEnd => write!(f, "unexpected end to expression"),
            Self::ExpectedKeyword(k) => write!(f, "expected the keyword '{}'", k),
            Self::BadFile(s) => write!(f, "unable to find {s} in this directory"),
            Self::Custom(s) => write!(f, "{s}"),
            Self::InvalidName => write!(f, "invalid keyword or variable name"),
            Self::NotInScope => write!(f, "variable not in scope"),
            Self::BadArtLength { width, got } => write!(
                f,
                "expected line length to be divisible hy {}, but it has {got} chars",
                width * 2,
            ),
            Self::BadArtHeight { height, got } => write!(
                f,
                "expected number of lines to be divisible hy {height}, but it has {got} lines",
            ),
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
    NewLine(u32),
    Word(String),
    EndOfBlock
}

pub type TokenStream = std::iter::Peekable<std::vec::IntoIter<Marked<Token>>>;

fn new_tokenstream(mut tokens: Vec<Marked<Token>>) -> TokenStream {
    let mut last_mark = tokens[tokens.len() - 1].mark.clone();
    if let Index::Expression(index) = last_mark.word_index {
        last_mark.word_index = Index::EndOfWord(index);
    }
    tokens.push(Marked::<Token> {
        value: Token::EndOfBlock,
        mark: last_mark,
    });
    tokens.into_iter().peekable()
}

fn remove_leading_newlines(tokens: &mut TokenStream) {
    while let Some(Marked::<Token> { value: Token::NewLine(_), .. }) = tokens.peek() {
        tokens.next();
    }
}

fn expect_keyword(tokens: &mut TokenStream, keyword: Keyword) -> Result<()> {
    let (value, mark) = next_non_newline(tokens)?.destructure();
    match value {
        Token::Keyword(k) if k == keyword => Ok(()),
        _ => Err(make_error(CompilationError::ExpectedKeyword(keyword), mark))
    }
}

fn collect_until(tokens: &mut TokenStream, keyword: Keyword) -> Result<TokenStream> {
    let mut output = Vec::new();
    loop {
        let token = match next_token(tokens) {
            Ok(x) => x,
            Err(mut e) => {
                e.error_type = Box::new(CompilationError::KeywordNotFound(keyword));
                return Err(e)
            }
        };
        if matches!(&token.value, Token::Keyword(k) if *k == keyword) {
            break
        }
        output.push(token);
    }
    Ok(new_tokenstream(output))
}

fn next_non_newline(input: &mut TokenStream) -> Result<Marked<Token>> {
    remove_leading_newlines(input);
    next_token(input)
}

fn next_token(input: &mut TokenStream) -> Result<Marked<Token>> {
    let output = input.next().unwrap();
    if let Token::EndOfBlock = output.value {
        return Err(Error {
            error_type: Box::new(CompilationError::UnexpectedEnd), 
            mark: output.mark
        })
    }
    Ok(output)
}

fn next_word (tokens: &mut TokenStream) -> Result<Marked<String>> {
    let (token, mark) = next_non_newline(tokens)?.destructure();
    let Token::Word(word) = token else {
        return Err(make_error(CompilationError::UnexpectedKeyword, mark))
    };
    Ok(Marked::<String> { value: word, mark })
}

fn get_with_indentation(input: &mut TokenStream, indentation: u32) -> Vec<TokenStream> {
    let mut output: Vec<TokenStream> = Vec::new();
    let mut current: Vec<Marked<Token>> = Vec::new();
    while let Ok(token) = next_token(input) {
        if matches!(&token.value, Token::NewLine(i) if *i == indentation) {
            if !current.is_empty() {
                output.push(new_tokenstream(current));
                current = Vec::new();
            }
        } else {
            current.push(token)
        }
    }
    output.push(new_tokenstream(current));
    output
}

pub fn tokenize(
    input: Vec<(usize, String)>,
    file_name: Arc<String>,
    file: Arc<Vec<String>>,
) -> Result<TokenStream> {
    let keywords: HashMap<&str, Keyword> = HashMap::from([
        ( "match",     Keyword::Match     ),
        ( "define",    Keyword::Define    ),
        ( "of_type",   Keyword::OfType    ),
        ( "as",        Keyword::As        ),
        ( "with",      Keyword::With      ),
        ( "to",        Keyword::To        ),
        ( "lambda",    Keyword::Lambda    ),
        ( "type",      Keyword::Type      ),
        ( "contains",  Keyword::Contains  ),
        ( "include",   Keyword::Include   ),
        ( "...",       Keyword::Undefined ),
    ]);
    let mut output = Vec::new();
    let mut current_indentation: Option<u32> = None;
    let mut block = input.into_iter();
    'lines: while let Some((line_number, line)) = block.next() {
        let indentation = indentation_length(&line);
        if indentation % INDENTATION != 0 {
            return Err(make_error(CompilationError::BadIndentation, Mark {
                file_name: Arc::clone(&file_name),
                file: Arc::clone(&file),
                line: line_number,
                block: None,
                word_index: Index::Art(indentation as usize - 1),
            }))
        }
        if current_indentation.is_none() {
            output.push(Marked::<Token> {
                mark: Mark {
                    file_name: Arc::clone(&file_name),
                    file: Arc::clone(&file),
                    line: line_number,
                    block: None,
                    word_index: Index::EndOfLine,
                },
                value: Token::NewLine(indentation),
            });
            current_indentation = Some(indentation)
        }
        let mut words = line.split_whitespace();
        let mut word_index: usize = 0;
        'words: while let Some(word) = words.next() {
            let mark: Mark = Mark {
                file_name: Arc::clone(&file_name),
                file: Arc::clone(&file),
                line: line_number,
                block: None,
                word_index: Index::Expression(word_index),
            };
            match word {
                "--" => break 'words,
                "art" => {
                    let Some(x) = words.next() else { return Err(make_error(
                        CompilationError::ArtMissingArgs,
                        Mark { word_index: Index::EndOfWord(word_index), ..mark }
                    ))};
                    let Some(x) = parse_roman_numeral(x) else { return Err(make_error(
                        CompilationError::ExpectedRoman, 
                        Mark { word_index: Index::Expression(word_index + 1), ..mark }
                    ))};
                    let Some(y) = words.next() else { return Err(make_error(
                        CompilationError::ArtMissingArgs,
                        Mark { word_index: Index::EndOfWord(word_index), ..mark }
                    ))};
                    let Some(y) = parse_roman_numeral(y) else { return Err(make_error(
                        CompilationError::ExpectedRoman,
                        Mark { word_index: Index::Expression(word_index + 2), ..mark }
                    ))};
                    let art: Vec<(usize, Vec<(usize, char)>)> = block
                        .map(|(index, line)| (index, line.chars().enumerate().collect()))
                        .collect();
                    let mut new_output = Vec::new();
                    for (line_index, line) in art.into_iter() {
                        let mut temp = Vec::new();
                        for (char_index, character) in line.into_iter() {
                            let marked_char = Marked::<char> {
                                value: character,
                                mark: Mark {
                                    line: line_index,
                                    word_index: Index::Art(char_index),
                                    ..mark.clone()
                                },
                            };
                            temp.push(marked_char);
                        }
                        new_output.push(temp);
                    }
                    let aaa = parse_art(x as usize, y as usize, new_output, mark.clone())?;
                    build_tokens_from_art(mark, aaa)?.for_each(|x| output.push(x));
                    break 'lines;
                }
                other => output.push(Marked::<Token> {
                    mark: mark.clone(),
                    value: match keywords.get(&other) {
                        Some(keyword) => Token::Keyword(*keyword),
                        None => {
                            if !other.chars().all(|x| x.is_lowercase() || x == '_') {
                                return Err(Error {
                                    mark,
                                    error_type: Box::new(CompilationError::InvalidName),
                                });
                            }
                            Token::Word(other.to_string())
                        }
                    },
                }),
            }
            word_index += 1;
            current_indentation = None;
        }
    }
    Ok(new_tokenstream(output))
}

fn build_token(name: &'static str, mark: &Mark) -> Marked<Token> {
    Marked::<Token> {
        value: Token::Word(name.to_string()),
        mark: mark.clone(),
    }
}

type Cells = HashMap<(u32, u32), (Marked<char>, Marked<char>)>;

fn build_tokens_from_art(
    mark: Mark,
    input: Vec<Cells>,
) -> Result<TokenStream> {
    let mut output = Vec::new();
    for i in input.into_iter() {
        output.push(build_token("prepend", &mark));
        for ((x, y), (c1, c2)) in i.into_iter() {
            let c1_char = c1.value;
            let c2_char = c2.value;
            if c1_char == ' ' && c2_char == '.' {
                continue;
            }
            output.push(build_token("insert_frame", &mark));
            output.push(build_token("positive", &mark));
            for _ in 0..x {
                output.push(build_token("succ", &mark));
            }
            output.push(build_token("one", &mark));
            output.push(build_token("positive", &mark));
            for _ in 0..y {
                output.push(build_token("succ", &mark));
            }
            output.push(build_token("one", &mark));
            match (c1_char, c2_char) {
                (_, ' ') => return Err(make_error(CompilationError::InvalidColor, c2.mark)),
                (' ', '|') => {
                    output.push(Marked::<Token> {
                        mark: c1.mark,
                        value: Token::Word("space".to_string()),
                    });
                }
                (_, '.') | (_, '|') => return Err(make_error(CompilationError::TranspOnChar, c2.mark)),
                (c1_char, '$') => {
                    output.push(Marked::<Token> {
                        mark: c1.mark,
                        value: Token::Word(c1_char.to_string()),
                    });
                }
                (c1_char, c2_char) => {
                    output.push(Marked::<Token> {
                        mark: mark.clone(),
                        value: Token::Word("char".to_string()),
                    });
                    let character = match c1_char {
                        '!' => "bang",
                        '"' => "double_quotes",
                        '#' => "pound",
                        '$' => "dollar",
                        '%' => "percent",
                        '&' => "ampersand",
                        '\'' => "single_quote",
                        '(' => "open_paranthesis",
                        ')' => "close_paranthesis",
                        '*' => "asterisk",
                        '+' => "plus",
                        ',' => "comma",
                        '-' => "hyphen",
                        '.' => "period",
                        '/' => "slash",
                        '0' => "digit_zero",
                        '1' => "digit_one",
                        '2' => "digit_two",
                        '3' => "digit_three",
                        '4' => "digit_four",
                        '5' => "digit_five",
                        '6' => "digit_six",
                        '7' => "digit_seven",
                        '8' => "digit_eight",
                        '9' => "digit_nine",
                        ':' => "colon",
                        ';' => "semicolon",
                        '<' => "less_than",
                        '=' => "equals",
                        '>' => "greater_than",
                        '?' => "question_mark",
                        '@' => "at_sign",
                        'A' => "uppercase_a",
                        'B' => "uppercase_b",
                        'C' => "uppercase_c",
                        'D' => "uppercase_d",
                        'E' => "uppercase_e",
                        'F' => "uppercase_f",
                        'G' => "uppercase_g",
                        'H' => "uppercase_h",
                        'I' => "uppercase_i",
                        'J' => "uppercase_j",
                        'K' => "uppercase_k",
                        'L' => "uppercase_l",
                        'M' => "uppercase_m",
                        'N' => "uppercase_n",
                        'O' => "uppercase_o",
                        'P' => "uppercase_p",
                        'Q' => "uppercase_q",
                        'R' => "uppercase_r",
                        'S' => "uppercase_s",
                        'T' => "uppercase_t",
                        'U' => "uppercase_u",
                        'V' => "uppercase_v",
                        'W' => "uppercase_w",
                        'X' => "uppercase_x",
                        'Y' => "uppercase_y",
                        'Z' => "uppercase_z",
                        '[' => "opening_bracket",
                        '\\' => "backslash",
                        ']' => "closing_bracket",
                        '^' => "caret",
                        '_' => "underscore",
                        '`' => "grave_accent",
                        'a' => "lowercase_a",
                        'b' => "lowercase_b",
                        'c' => "lowercase_c",
                        'd' => "lowercase_d",
                        'e' => "lowercase_e",
                        'f' => "lowercase_f",
                        'g' => "lowercase_g",
                        'h' => "lowercase_h",
                        'i' => "lowercase_i",
                        'j' => "lowercase_j",
                        'k' => "lowercase_k",
                        'l' => "lowercase_l",
                        'm' => "lowercase_m",
                        'n' => "lowercase_n",
                        'o' => "lowercase_o",
                        'p' => "lowercase_p",
                        'q' => "lowercase_q",
                        'r' => "lowercase_r",
                        's' => "lowercase_s",
                        't' => "lowercase_t",
                        'u' => "lowercase_u",
                        'v' => "lowercase_v",
                        'w' => "lowercase_w",
                        'x' => "lowercase_x",
                        'y' => "lowercase_y",
                        'z' => "lowercase_z",
                        '{' => "opening_brace",
                        '|' => "vertical_bar",
                        '}' => "closing_brace",
                        '~' => "tilde",
                        ' ' => {
                            return Err(Error {
                                error_type: Box::new(CompilationError::ColorOnSpace),
                                mark: c2.mark,
                            });
                        }
                        _ => panic!("bad char"),
                    }
                    .to_string();
                    output.push(Marked::<Token> {
                        mark: mark.clone(),
                        value: Token::Word(character),
                    });
                    let color = match c2_char {
                        '0' => Ok("black"),
                        '1' => Ok("red"),
                        '2' => Ok("green"),
                        '3' => Ok("yellow"),
                        '4' => Ok("blue"),
                        '5' => Ok("magenta"),
                        '6' => Ok("cyan"),
                        '7' => Ok("white"),
                        '8' => Ok("orange"),
                        '9' => Ok("purple"),
                        x => Err(x),
                    };
                    match color {
                        Ok(x) => output.push(build_token(x, &mark)),
                        Err(x) => output.push(Marked::<Token> {
                            mark: c2.mark,
                            value: Token::Word(String::from(x)),
                        }),
                    };
                }
            }
        }
        output.push(Marked::<Token> {
            mark: mark.clone(),
            value: Token::Word("new_frame".to_string()),
        });
    }
    let mut i = output.len() - 1;
    loop {
        match output.get_mut(i).unwrap().value {
            Token::Word(ref mut x) if x == "prepend" => {
                *x = "single".to_owned();
                break;
            }
            _ => i -= 1
        }
    }
    Ok(new_tokenstream(output))
}

fn indentation_length(input: &str) -> u32 {
    let mut counter = 0;
    let mut chars = input.chars();
    while let Some(' ') = chars.next() {
        counter += 1
    }
    counter
}

#[derive(Debug)]
pub enum Signiture {
    Value(String, Mark, TokenStream),
    Type(String, Mark),
}

pub fn extract_signiture(input: &mut TokenStream) -> Result<Signiture> {
    let (token, mark1) = next_non_newline(input)?.destructure();
    match token {
        Token::Keyword(Keyword::Define) => {
            let (name, name_mark) = next_word(input)?.destructure();
            expect_keyword(input, Keyword::OfType)?;
            let type_strings = collect_until(input, Keyword::As)?;
            Ok(Signiture::Value(name, name_mark, type_strings))
        }
        Token::Keyword(Keyword::Type) => {
            let (name, name_mark) = next_word(input)?.destructure();
            expect_keyword(input, Keyword::Contains)?;
            Ok(Signiture::Type(name, name_mark))
        }
        x => Err(Error {
            mark: mark1,
            error_type: Box::new(CompilationError::Custom(format!(
                "expected 'type' or 'define' but found {:?}",
                x
            ))),
        }),
    }
}

fn make_error(error: CompilationError, mark: Mark) -> Error {
    Error {
        mark,
        error_type: Box::new(error)
    }
}

pub fn parse_type(tokens: &mut TokenStream) -> Result<Type> {
    let (word, mark) = next_word(tokens)?.destructure();
    match word.as_str() {
        "fn" => {
            let arg1 = parse_type(tokens)?;
            let arg2 = parse_type(tokens)?;
            Ok(Type::Function(Box::new(arg1), Box::new(arg2)))
        }
        word => {
            let ptr = TYPES.lock().unwrap();
            let index = ptr.as_ref().unwrap().get(word).ok_or(Error {
                mark,
                error_type: Box::new(CompilationError::NotInScope),
            })?;
            Ok(Type::Type(*index))
        }
    }
}

pub fn parse_data(
    mut tokens: TokenStream,
    parent_type: u32,
) -> Result<Vec<(String, Type, Mark)>> {
    let mut output = Vec::new();
    //dbg!("hi");
    for mut i in get_with_indentation(&mut tokens, INDENTATION).into_iter() {
        let (name, name_mark) = next_word(&mut i)?.destructure();
        let mut arg_types: Vec<Type> = Vec::new();
        while !matches!(i.peek().unwrap().value, Token::EndOfBlock) {
            arg_types.push(parse_type(&mut i)?)
        }
        let mut args = arg_types.into_iter();
        output.push((name, build_type(&mut args, Type::Type(parent_type)), name_mark));
    }
    Ok(output)
}

pub fn build_type(input: &mut impl Iterator<Item = Type>, result: Type) -> Type {
    match input.next() {
        None => result,
        Some(x) => Type::Function(Box::new(x), Box::new(build_type(input, result))),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Type(u32),
    Function(Box<Type>, Box<Type>),
}

impl Type {
    pub fn is_a_function(&self) -> bool {
        match self {
            Self::Type(_) => false,
            Self::Function(_, _) => true,
        }
    }

    pub fn final_type(&self) -> u32 {
        match self {
            Self::Type(x) => *x,
            Self::Function(_, b) => b.final_type(),
        }
    }

    pub fn is_possible(&self, test: &Self) -> bool {
        *self == *test
            || match self {
                Self::Type(_) => false,
                Self::Function(_, output) => output.is_possible(test),
            }
    }

    //pub fn apply_type(self, arg: Self) -> Option<Self> {
    //    match self {
    //        Self::Type(u32) => None,
    //        Self::Function(x, y) => {
    //            if arg == *x {
    //                Some(*y)
    //            } else {
    //                None
    //            }
    //        }
    //    }
    //}

    pub fn arg_types(self) -> Vec<Type> {
        let mut args = Vec::new();
        let mut current_type = self;
        loop {
            match current_type {
                Type::Function(a, b) => {
                    args.push(*a);
                    current_type = *b;
                }
                Type::Type(_) => return args,
            }
        }
    }
    pub fn show(&self) -> String {
        fn helper(tp: &Type, types: &HashMap<&u32, &String>, output: &mut String) {
            match tp {
                Type::Type(a) => {
                    let name = types.get(a).unwrap(); // safe
                    output.push_str(name);
                    output.push(' ')
                }
                Type::Function(a, b) => {
                    output.push_str("fn ");
                    helper(a, types, output);
                    helper(b, types, output);
                }
            }
        }
        let mut output = String::new();
        let mut new_map = HashMap::new();
        let ptr = TYPES.lock().unwrap();
        ptr.as_ref().unwrap().iter().for_each(|(k, w)| { new_map.insert(w, k); });
        helper(self, &new_map, &mut output);
        output
    }
}
