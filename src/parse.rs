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
use crate::runtime::{Expression, Id, Pattern};
use crate::r#type::Type;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::sync::Arc;

const INDENTATION: u32 = 4;

type Result<T> = std::result::Result<T, Error>;

fn parse_pattern(
    mut number_of_local: u32,
    expected_type: &Type,
    tokens: &mut TokenStream,
    global_vars: &HashMap<String, (usize, Type, bool)>,
) -> Result<(Pattern, HashMap<String, (u32, Type)>, u32)> {
    let mut output = HashMap::new();
    let pattern = parse_pattern_helper(
        &mut number_of_local,
        expected_type,
        &mut output,
        tokens,
        global_vars,
    )?;
    Ok((pattern, output, number_of_local))
}

fn parse_pattern_helper(
    number_of_local: &mut u32,
    expected_type: &Type,
    output: &mut HashMap<String, (u32, Type)>,
    tokens: &mut TokenStream,
    global_vars: &HashMap<String, (usize, Type, bool)>,
) -> Result<Pattern> {
    let (name, mark) = next_word(tokens)?.destructure();
    if name.chars().next().unwrap() /* safe */ == '_' {
        return Ok(Pattern::Dropped);
    }
    if let Some((id, tp, is_constructor)) = global_vars.get(&name) {
        if {
            !expected_type.is_a_function()
                && *is_constructor
                && Type::Type(tp.final_type()) == *expected_type
        } {
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
            return Ok(Pattern::DataConstructor(*id as u32, patterns));
        } else {
            *number_of_local = *number_of_local + 1;
            output.insert(name, (*number_of_local, expected_type.clone()));
            return Ok(Pattern::Captured(*number_of_local));
        }
    } else {
        *number_of_local = *number_of_local + 1;
        output.insert(name, (*number_of_local, expected_type.clone()));
        return Ok(Pattern::Captured(*number_of_local));
    }
}

pub fn get_tokens(file: Marked<String>, done: &mut Vec<String>) -> Result<Vec<TokenStream>> {
    let (file, mark) = file.destructure();
    if done.contains(&file) { return Ok(Vec::new()) }
    let file_contents = match read_to_string(&file) {
        Ok(x) => x,
        Err(_) => return Err(make_error(CompilationError::BadFile(file), mark))
    };
    eprintln!("including \x1b[95m{file}\x1b[0m");
    done.push(file.clone());
    let mut output = tokenize_file(file_contents, &Arc::new(file))?;
    //dbg!(output.remove(0));
    while matches!(output[0].peek().unwrap().value, Token::NewLine(_)) {
        output[0].next();
    }
    //dbg!(matches!(output[0].peek().unwrap().value, Token::Keyword(Keyword::Include)));
    //dbg!(&output);
    if matches!(
        output[0].peek().unwrap().value,
        Token::Keyword(Keyword::Include)
    ) {
        let mut imports = output.remove(0);
        //dbg!(&output);
        //dbg!(&output);
        imports.next(); // remove the import keyword
        let mut imports = imports.into_iter();
        //dbg!(&imports);
        //dbg!(next_non_newline(&mut imports));
        while let Ok(i) = next_non_newline(&mut imports) {
            match i.value {
                Token::Word(mut x) => {
                    x.push_str(".th");
                    let mut f = get_tokens(
                        Marked::<String> {
                            value: x,
                            mark: i.mark.clone()
                        }, 
                        done
                    )?;
                    output.append(&mut f)
                }
                _ => todo!(),
            }
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
        if string.len() == 0 || string.split_whitespace().next().unwrap() == "--" {
            // safe unwrap
            continue;
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

pub fn parse_roman_numeral(mut numeral: &str) -> Option<u32> {
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
            return Some(output);
        };
        if numeral_len < pattern_len
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
    mut local_vars:        HashMap<String, (u32, Type)>,
    mut local_vars_count:  u32,
    global_vars:           &HashMap<String, (usize, Type, bool)>,
    types:                 &HashMap<String, u32>,
) -> Result<Expression> {
    let (token, mark) = next_non_newline(tokens)?.destructure();
    match token {
        Token::EndOfBlock | Token::NewLine(_) => unreachable!(),
        Token::Keyword(k) => match k {
            Keyword::Undefined => return Ok(Expression::Undefined { mark }),
            Keyword::Lambda => {
                match expected_type {
                    Type::Type(_) => Err(make_error(CompilationError::TypeMismatch, mark)),
                    Type::Function(a, b) => {
                        let (pattern, local_vars_new, local_vars_count) = parse_pattern(
                            local_vars_count, 
                            &*a, 
                            tokens, 
                            global_vars
                        )?;
                        local_vars_new.into_iter().for_each(|(k, v)| { local_vars.insert(k, v); });
                        let body = parse_expression(*b, tokens, local_vars, local_vars_count, global_vars, types)?;
                        Ok(Expression::Lambda {
                            pattern,
                            body: Box::new(body),
                        })
                    }
                }
            }
            Keyword::Match => {
                let mut indent: u32;
                while let Some(Marked::<Token> { value: Token::NewLine(_), .. }) = tokens.peek() {
                    tokens.next();
                }
                let (value, mark) = tokens.peek().unwrap().clone().destructure();
                let mut tp;
                match value {
                    Token::Keyword(Keyword::Of_type) => {
                        tokens.next(); // safe
                        tp = parse_type(tokens, types)?;
                    }
                    Token::Word(first_name) => {
                        let (_, root_type) = if let Some((a, b)) = local_vars.get(&first_name) {
                            (Id::LambdaArg(*a), b)
                        } else if let Some((a, b, c)) = global_vars.get(&first_name) {(
                            if *c { Id::DataConstructor(*a as u32) } else { Id::Variable(*a) },
                            b,
                        )} else {
                            return Err(make_error(CompilationError::NotInScope, mark.clone()))
                        };
                        tp = Type::Type(root_type.final_type())
                    }
                    _ => return Err(make_error(CompilationError::Empty, mark))
                }
                let mut matched_on_tokens = Vec::new();
                let mut unmatched_matches = 0;
                loop {
                    let token = next_token(tokens)?;
                    match &token.value {
                        Token::Keyword(Keyword::With) => {
                            if unmatched_matches == 0 {
                                break;
                            } else {
                                unmatched_matches -= 1;
                                matched_on_tokens.push(token);
                                // check if it's negative
                            }
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
                    types
                )?;
                let (token, mark) = next_token(tokens)?.destructure();
                let indentation = if let Token::NewLine(num) = token { num } else { panic!("no newline") };
                let mut branch_tokens = get_with_indentation(tokens, indentation);
                let mut branches: Vec<(Pattern, Expression)> = Vec::new();
                for branch in branch_tokens.iter_mut() {
                    let (pattern, local_vars_new, local_vars_count) = parse_pattern(
                        local_vars_count, 
                        &tp, 
                        branch, 
                        global_vars
                    )?;
                    local_vars_new.into_iter().for_each(|(k, v)| { local_vars.insert(k, v); });
                    expect_keyword(branch, Keyword::To)?;
                    let body = parse_expression(
                        expected_type.clone(),
                        branch, 
                        local_vars.clone(), 
                        local_vars_count, 
                        global_vars, 
                        types
                    )?;
                    branches.push((pattern, body))
                }
                Ok(Expression::Match {
                    pattern: Box::new(matched_on),
                    branches,
                })
            }
            _ => Err(make_error(CompilationError::UnexpectedKeyword, mark))
        }
        Token::Word(name) => {
            // peek if the next token is a newline
            let (root_id, root_type) = if let Some((a, b)) = local_vars.get(&name) {
                (Id::LambdaArg(*a), b)
            } else if let Some((a, b, c)) = global_vars.get(&name) {
                (
                    if *c {
                        Id::DataConstructor(*a as u32)
                    } else {
                        Id::Variable(*a)
                    },
                    b,
                )
            } else {
                return Err(make_error(CompilationError::NotInScope, mark))
            };
            if !root_type.is_possible(&expected_type) {
                return Err(make_error(CompilationError::TypeMismatch, mark))
            }
            let mut output_args = Vec::new();
            let mut current_type = root_type.to_owned();
            while current_type != expected_type {
                match tokens.peek().unwrap().value {
                    Token::NewLine(indentation) => {
                        let mut arg_groups = get_with_indentation(tokens, indentation).into_iter();
                        while current_type != expected_type {
                            let mut current_tokens = arg_groups.next().expect("fffffffffffff");
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
                                types
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
                            types
                        )?;
                        output_args.push(next_arg);
                    }
                }
            }
            return Ok(Expression::Tree {
                root: root_id,
                arguments: output_args,
            })
        }
        _ => todo!()
    }
}

pub fn parse_art(
    width: usize,
    height: usize,
    text: Vec<Vec<Marked<char>>>,
    mark: Mark, // mark of the art keyword
) -> Result<Vec<HashMap<(u32, u32), (Marked<char>, Marked<char>)>>> {
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
    let mut output: Vec<HashMap<(u32, u32), (Marked<char>, Marked<char>)>> = Vec::new();
    let mut current_index = 0;
    let mut current_starting_line = 0;
    let mut current_starting_char = 0;
    loop {
        let mut current_map = HashMap::new();
        for x in 0..width as usize {
            for y in 0..height as usize {
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Include,
    Lambda,
    Match,
    With,
    Define,
    Of_type,
    As,
    To,
    Data,
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
            Keyword::Of_type => write!(f, "of_type"),
            Keyword::As => write!(f, "as"),
            Keyword::To => write!(f, "to"),
            Keyword::Data => write!(f, "data"),
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
    Empty,
    Custom(String),
    UnexpectedClosingComment,
    UnexpectedOpeningComment(usize),
    UnclosedComment,
    InvalidName,
    NotInScope,
    TypeMismatch,
    ExpectedRoman,
    UnexpectedKeyword,
    ExpectedKeyword(Keyword),
    TrailingCharacters,
    BadArtLength { width: usize, got: usize },
    BadArtHeight { height: usize, got: usize },
    BadFile(String),
    UnexpectedEnd
}

impl ErrorType for CompilationError {
    fn gist(&self) -> &'static str {
        match self {
            Self::UnexpectedEnd => "unexpected end",
            Self::ExpectedKeyword(_) => "expected a keyword",
            Self::Custom(_) => "",
            Self::Empty => "",
            Self::UnexpectedClosingComment => "unexpected delimiter",
            Self::UnexpectedOpeningComment(_) => "unmatched delimiter",
            Self::UnclosedComment => "unclosed comment",
            Self::InvalidName => "invalid name",
            Self::NotInScope => "not in scope",
            Self::TypeMismatch => "unexpected type",
            Self::ExpectedRoman => "expected a roman numeral",
            Self::UnexpectedKeyword => "unexpected keyword",
            Self::TrailingCharacters => "trailing characters",
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
            Self::UnexpectedEnd => write!(f, "unexpected end to expression"),
            Self::ExpectedKeyword(k) => write!(f, "expected the keyword '{}'", k),
            Self::BadFile(s) => write!(f, "unable to find {s} in this directory"),
            Self::Custom(s) => write!(f, "{s}"),
            Self::Empty => write!(f, "empty error message"),
            Self::UnexpectedClosingComment => write!(f, "unexpected closing delimiter"),
            Self::UnexpectedOpeningComment(line) => write!(
                f,
                "unexpected opening delimiter.
expected a closing delimiter for '---' on line {}
",
                line + 1
            ),
            Self::UnclosedComment => write!(f, "unclosed multi-line comment"),
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
            _ => write!(f, "todo"),
        }
    }
}

#[derive(Debug, Clone)]
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


fn expect_keyword(tokens: &mut TokenStream, keyword: Keyword) -> Result<()> {
    let (value, mark) = next_non_newline(tokens)?.destructure();
    match value {
        Token::Keyword(k) if k == keyword => Ok(()),
        _ => Err(make_error(CompilationError::ExpectedKeyword(keyword), mark))
    }
}


fn next_non_newline(input: &mut TokenStream) -> Result<Marked<Token>> {
    loop {
        //dbg!(&input.peek().unwrap().value);
        let current = next_token(input)?;
        if !matches!(current.value, Token::NewLine(_)) {
            return Ok(current);
        }
    }
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
    let (value, mark) = next_non_newline(tokens)?.destructure();
    match value {
        Token::Word(w) => Ok(Marked::<String> { value: w, mark }),
        _ => Err(make_error(CompilationError::UnexpectedKeyword, mark))
    }
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
    let final_line: usize = input[input.len() - 1].0;
    let keywords: HashMap<&str, Keyword> = HashMap::from([
        ("match", Keyword::Match),
        ("define", Keyword::Define),
        ("of_type", Keyword::Of_type),
        ("as", Keyword::As),
        ("with", Keyword::With),
        ("to", Keyword::To),
        ("lambda", Keyword::Lambda),
        ("data", Keyword::Data),
        ("contains", Keyword::Contains),
        ("include", Keyword::Include),
        ("...", Keyword::Undefined),
    ]);
    let mut output = Vec::new();
    let mut current_indentation: Option<u32> = None;
    let mut block = input.into_iter();
    'lines: while let Some((line_number, line)) = block.next() {
        let indentation = indentation_length(&line);
        if current_indentation == None {
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
                    let x = match words.next() {
                        None => {
                            return Err(Error {
                                mark: Mark {
                                    word_index: Index::EndOfWord(word_index),
                                    ..mark
                                },
                                error_type: Box::new(CompilationError::Empty),
                            });
                        }
                        Some(x) => match parse_roman_numeral(x) {
                            None => {
                                return Err(Error {
                                    mark: Mark {
                                        word_index: Index::Expression(word_index + 1),
                                        ..mark
                                    },
                                    error_type: Box::new(CompilationError::ExpectedRoman),
                                });
                            }
                            Some(x) => x,
                        },
                    };
                    let y = match words.next() {
                        None => {
                            return Err(Error {
                                mark: Mark {
                                    word_index: Index::EndOfWord(word_index),
                                    ..mark
                                },
                                error_type: Box::new(CompilationError::Empty),
                            });
                        }
                        Some(x) => match parse_roman_numeral(x) {
                            None => {
                                return Err(Error {
                                    mark: Mark {
                                        word_index: Index::Expression(word_index + 2),
                                        ..mark
                                    },
                                    error_type: Box::new(CompilationError::ExpectedRoman),
                                });
                            }
                            Some(x) => x,
                        },
                    };
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
                    for i in build_tokens_from_art(mark, aaa)? {
                        output.push(i);
                    }
                    break 'lines;
                }
                other => output.push(Marked::<Token> {
                    mark: mark.clone(),
                    value: match keywords.get(&other) {
                        Some(keyword) => Token::Keyword(keyword.clone()),
                        None => {
                            if !other.chars().all(|x| x.is_lowercase() || x == '_') {
                                return Err(Error {
                                    mark: mark,
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

fn build_tokens_from_art(
    mark: Mark,
    input: Vec<HashMap<(u32, u32), (Marked<char>, Marked<char>)>>,
) -> Result<TokenStream> {
    let mut output = Vec::new();
    for i in input.into_iter() {
        output.push(build_token("cons_video", &mark));
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
                (' ', '|') => {
                    output.push(Marked::<Token> {
                        mark: c1.mark,
                        value: Token::Word("space".to_string()),
                    });
                }
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
                                error_type: Box::new(CompilationError::Empty),
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
    output.push(Marked::<Token> {
        mark: mark.clone(),
        value: Token::Word("nil_video".to_string()),
    });
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
    Value(String, TokenStream),
    Type(String),
}

fn convert_to_after(mut mark: Mark) -> Option<Mark> {
    match mark.word_index {
        Index::Expression(index) => mark.word_index = Index::EndOfWord(index),
        _ => return None,
    }
    Some(mark)
}

pub fn extract_signiture(input: &mut TokenStream) -> Result<Signiture> {
    let (token, mark1) = next_non_newline(input)?.destructure();
    match token {
        Token::Keyword(Keyword::Define) => {
            let (token, mark2) = next_non_newline(input)?.destructure();
            match token {
                Token::Word(name) => {
                    let Marked::<Token> {
                        mark: mark3,
                        value: token,
                    } = next_non_newline(input)?;
                    if !matches!(token, Token::Keyword(Keyword::Of_type)) {
                        return Err(make_error(CompilationError::Empty, mark3))
                    }
                    let mut type_strings: Vec<Marked<Token>> = Vec::new();
                    loop {
                        let w = next_non_newline(input)?;
                        match &w.value {
                            Token::Word(_) => type_strings.push(w),
                            Token::Keyword(Keyword::As) => break,
                            _ => return Err(make_error(CompilationError::Empty, w.mark))
                        }
                    }
                    Ok(Signiture::Value(name, type_strings.into_iter().peekable()))
                }
                _ => panic!("extract"),
            }
        }
        Token::Keyword(Keyword::Data) => {
            let (token, root_mark) = next_non_newline(input)?.destructure();
            match token {
                Token::Word(name) => {
                    expect_keyword(input, Keyword::Contains)?;
                    Ok(Signiture::Type(name))
                }
                _ => panic!("expected a name"),
            }
        }
        x => Err(Error {
            mark: mark1,
            error_type: Box::new(CompilationError::Custom(format!(
                "expected 'data' or 'define' but found {:?}",
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

pub fn parse_type(tokens: &mut TokenStream, table: &HashMap<String, u32>) -> Result<Type> {
    let (word, mark) = next_word(tokens)?.destructure();
    //let Marked::<Token> { mark, value: token } = next_non_newline(tokens).expect("kkkk");
    match word.as_str() {
        "fn" => {
            let arg1 = parse_type(tokens, table)?;
            let arg2 = parse_type(tokens, table)?;
            Ok((Type::Function(Box::new(arg1), Box::new(arg2))))
        }
        word => {
            let index = table.get(word).ok_or(Error {
                mark,
                error_type: Box::new(CompilationError::NotInScope),
            })?;
            Ok(Type::Type(*index))
        }
    }
}

pub fn parse_data(
    mut tokens: TokenStream,
    types: &HashMap<String, u32>,
    parent_type: u32,
) -> Result<Vec<(String, Type)>> {
    let mut output = Vec::new();
    //dbg!("hi");
    for mut i in get_with_indentation(&mut tokens, INDENTATION).into_iter() {
        let (name, root_mark) = next_word(&mut i)?.destructure();
        let mut arg_types: Vec<Type> = Vec::new();
        while !matches!(i.peek().unwrap().value, Token::EndOfBlock) {
            arg_types.push(parse_type(&mut i, types)?)
        }
        let mut args = arg_types.into_iter();
        output.push((name, build_type(&mut args, Type::Type(parent_type))));
    }
    Ok(output)
}

pub fn build_type(input: &mut impl Iterator<Item = Type>, result: Type) -> Type {
    match input.next() {
        None => result,
        Some(x) => Type::Function(Box::new(x), Box::new(build_type(input, result))),
    }
}
