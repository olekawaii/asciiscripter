use crate::runtime::{Expression, Id};
use crate::r#type::Type;
use std::{collections::HashMap, rc::Rc};
use std::fs::read_to_string;
type Result<T> = std::result::Result<T, Error>;

pub fn get_tokens(file: String, done: &mut Vec<String>) -> Result<Vec<TokenStream>> {
    dbg!(&file);
    if done.contains(&file) {
        return Ok(Vec::new())
    } else {
        done.push(file.clone());
        let mut output = tokenize_file(read_to_string(&file).unwrap(), &Rc::new(file))?;
        //dbg!(output.remove(0));
        while matches!(output[0].peek().unwrap().value, Token::NewLine(_)) {
            output[0].next();
        }
        //dbg!(matches!(output[0].peek().unwrap().value, Token::Keyword(Keyword::Include)));
        //dbg!(&output);
        if matches!(output[0].peek().unwrap().value, Token::Keyword(Keyword::Include)) {
            let mut imports = output.remove(0);
            //dbg!(&output);
            imports.next(); // remove the import keyword
            let mut imports = imports.into_iter();
            while let Some(i) = next_non_newline(&mut imports) {
                match i.value {
                    Token::Variable(ValueToken::Value(mut x)) => {
                        x.push_str(".ascr");
                        let mut f = get_tokens(x, done)?;
                        output.append(&mut f)
                    }
                    _ => todo!()
                }
            }
        }
        Ok(output)
    }
}

pub fn tokenize_file(input: String, file_name: &Rc<String>) -> Result<Vec<TokenStream>> {
    let file_lines: Rc<Vec<String>> =
        Rc::new(input.lines().map(|x| x.trim_end().to_string()).collect());
    let mut output: Vec<TokenStream> = Vec::new();
    let mut current_block: Vec<(usize, String)> = Vec::new();
    let mut file = remove_multiline_comments(
        file_lines.iter().map(|x| x.as_str()).enumerate(),
        file_name,
        &file_lines
    )?.into_iter();
    while let Some((line_number, string)) = file.next() {
        if string.len() == 0 || string.split_whitespace().next().unwrap() == "--" {  // safe unwrap
            continue
        } 
        current_block.push((line_number, string.to_string()));
        'good_lines: loop {
            match file.next() {
                None | Some((_, "")) => {
                    output.push(tokenize(
                        current_block,
                        Rc::clone(file_name),
                        Rc::clone(&file_lines),
                    )?);
                    current_block = Vec::new();
                    break 'good_lines
                }
                Some((line_number, string)) => {
                    current_block.push((line_number, string.to_string()));
                }
            }
        }
    }
    Ok(output)
}

fn remove_multiline_comments<'a>(
    mut input: impl Iterator<Item = (usize, &'a str)>,
    file_name: &Rc<String>,
    file: &Rc<Vec<String>>
) -> Result<Vec<(usize, &'a str)>> {
    let mut output = Vec::new();
    loop {
        match input.next() {
            None => return Ok(output),
            Some((line, string)) => match string {
                "---" => 'multi_line_comment: loop {
                    match input.next() {
                        None => return Err(Error {
                            mark: Mark {
                                file_name: Rc::clone(file_name),
                                file: Rc::clone(file),
                                line,
                                block: None,
                                word_index: Index::Expression(0),
                            },
                            error_message: ErrorType::UnclosedComment
                        }),
                        Some((line2, x)) => match x {
                            "<o>" => break 'multi_line_comment,
                            "---" => return Err(Error {
                                mark: Mark {
                                    file_name: Rc::clone(file_name),
                                    file: Rc::clone(file),
                                    line: line2,
                                    block: None,
                                    word_index: Index::Expression(0),
                                },
                                error_message: ErrorType::UnexpectedOpeningComment(line)
                            }),
                            _ => ()
                        }
                    }
                },
                "<o>" => return Err(Error {
                    mark: Mark {
                        file_name: Rc::clone(file_name),
                        file: Rc::clone(file),
                        line,
                        block: None,
                        word_index: Index::Expression(0),
                    },
                    error_message: ErrorType::UnexpectedClosingComment
                }),
                _ => output.push((line, string))
            }
        }
    }
    Ok(output)
}

pub fn parse_roman_numeral(mut numeral: &str) -> Option<u32> {
    let mut numerals: Vec<(&str, u32)> = vec![
        ("i",   1),
        ("iv",  4),
        ("v",   5),
        ("ix",  9),
        ("x",  10),
        ("xl", 40),
        ("l",  50),
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
        if numeral_len == 0 { return Some(output) };
        if numeral_len < pattern_len || &numeral[starting_index .. starting_index + pattern_len] != pattern {
            tuple = numerals.pop()?;
            consecutive_times = 0;
        } else {
            output += value;
            starting_index += pattern_len;
            if consecutive_times != 0 && ( pattern_len > 1 || consecutive_times > 3 ) {
                return None
            };
            let skips = match value.to_string().chars().next().unwrap() {
                '1'        => 0,
                '4' | '5'  => 1,
                '9'        => 3,
                _          => unreachable!()
            };
            for _ in 0..skips {
                numerals.pop();
            }
            consecutive_times += 1;
        }
    }
}

pub fn build_syntax_tree(mut tokens: TokenStream) -> Result<SyntaxTree> {
    //dbg!(&tokens.clone().count());
    let Marked::<Token> {
        mark: root_mark,
        value: token,
    } = match next_non_newline(&mut tokens) {
        None => {dbg!(&tokens); unreachable!()},
        Some(x) => x
    };
    match token {
        Token::Keyword(keyword) => match keyword {
            Keyword::Lambda => {
                let Marked::<Token> { mark, value: token } = match next_non_newline(&mut tokens) {
                    None => return Err(Error {
                        mark: root_mark,
                        error_message: ErrorType::Empty
                    }),
                    Some(x) => x
                };
                if let Token::Variable(ValueToken::Value(arg)) = token {
                    Ok(SyntaxTree::Lambda(
                        Marked::<String> {
                            mark,
                            value: arg
                        },
                        root_mark,
                        Box::new(build_syntax_tree(tokens)?),
                    ))
                } else {
                    panic!("ashuesa")
                }
            }
            Keyword::Match => {
                let mut indent: u32;
                let mut pattern = Vec::new();
                let mut unmatched_matches = 0;
                loop {
                    let token = tokens.next().expect("atueha");
                    match &token.value {
                        Token::Keyword(Keyword::In) => {
                            if unmatched_matches == 0 {
                                let Marked::<Token> { mark, value: token } = tokens.next().expect("uu");
                                if let Token::NewLine(num) = token {
                                    indent = num;
                                } else {
                                    panic!("no newline")
                                }
                                break
                            } else {
                                unmatched_matches -= 1;
                                pattern.push(token);
                                // check if it's negative
                            }
                        }
                        Token::Keyword(Keyword::Match) => {
                            unmatched_matches += 1;
                            pattern.push(token);
                        }
                        _ => pattern.push(token)
                    }
                                    
                    // let Marked::<Token> { mark, value: token } = tokens.next().expect("atueha");
                    // match token {
                    //     // fail if no branches
                    //     Token::NewLine(num) => {
                    //         indent = num;
                    //         break;
                    //     }
                    //     Token::Variable(ValueToken::Value(word)) => {
                    //         pattern.push(Marked::<String> {
                    //             value: word,
                    //             mark,
                    //         });
                    //     }
                    //     _ => panic!("f"),
                    // }
                }
                let branches: Vec<(Marked<String>, Vec<String>, SyntaxTree)> =
                    get_with_indentation(tokens, indent)
                        .into_iter()
                        .map(parse_branch)
                        .collect::<Result<Vec<_>>>()?;
                //dbg!(format!("{} uwu", pattern.iter().count()));
                Ok(SyntaxTree::Match(Box::new(build_syntax_tree(pattern.into_iter().peekable())?), branches))
            }
            _ => todo!(),
        },
        Token::Variable(ValueToken::Value(word)) => {
            let root = word;
            let mut args = Vec::new();
            loop {
                match tokens.peek() {
                    None => break,
                    Some(x) => {
                        if matches!(x.value, Token::Keyword(_)) {
                            args.push(Argument::Grouped(build_syntax_tree(tokens)?));
                            break;
                        }
                    }
                }
                let Marked::<Token> { mark, value: token } = tokens.next().unwrap(); // safe
                //if let None = tokens.peek() == None {
                //    break
                //}
                match token {
                    Token::Variable(ValueToken::Value(w)) => args.push(
                        Argument::Ungrouped(Marked::<String> {
                            mark: mark,
                            value: w
                        })
                    ),
                    Token::NewLine(indentation) => {
                        if let None = tokens.peek() {
                            break
                        }
                        let arg_groups = get_with_indentation(tokens, indentation)
                            .into_iter()
                            .map(build_syntax_tree);
                        for i in arg_groups {
                            args.push(Argument::Grouped(i?));
                        }
                        break;
                    }
                    _ => unreachable!(),
                }
            }
            Ok(SyntaxTree::Tree(Marked::<String> {value: root, mark: root_mark} , args))
        }
        Token::NewLine(_) => unreachable!(),
    }
}

fn parse_branch(mut tokens: TokenStream) -> Result<(Marked<String>, Vec<String>, SyntaxTree)> {
    let Marked::<Token> { mark, value: token } = tokens.next().expect("uwu");
    let constructor = match token {
        Token::Variable(ValueToken::Value(name)) => Marked::<String> {
            value: name,
            mark,
        },
        _ => panic!("d"),
    };
    let mut args = Vec::new();
    loop {
        let Marked::<Token> { mark, value: token } = tokens.next().expect("blauh");
        match token {
            Token::Variable(ValueToken::Value(name)) => args.push(name),
            Token::Keyword(Keyword::To) => break,
            _ => panic!("bad match syntax"),
        }
    }
    Ok((constructor, args, build_syntax_tree(tokens).expect("c")))
}

pub fn build_tree(
    expected_type: Type,
    input: SyntaxTree,
    mut variables: HashMap<String, (u32, Type)>,
    mut local_vars_count: u32,
    global_vars: &HashMap<String, (usize, Type, bool)>,
) -> Result<Expression> {
    match input {
        SyntaxTree::Lambda(name, keyword_mark, tree) => {
            let mut id = None;
            if let Type::Function(a, b) = expected_type {
                if &name.value != "_" {
                    local_vars_count += 1;
                    variables.insert(name.value, (local_vars_count, *a));
                    id = Some(local_vars_count)
                }
                let body = build_tree(*b, *tree, variables, local_vars_count, global_vars)?;
                Ok(Expression::Lambda {
                    id, 
                    body: Box::new(body),
                })
            } else {
                Err(Error {
                    error_message: ErrorType::Empty,
                    mark: keyword_mark
                })
            }
        }
        SyntaxTree::Tree(root, args) => {
            let mut vec = vec![Argument::Ungrouped(root)];
            for i in args.into_iter() {
                vec.push(i);
            }
            let mut vec = vec.into_iter();
            evaluate_arguments(
                expected_type,
                global_vars,
                &variables,
                local_vars_count,
                &mut vec,
            )
        }
        SyntaxTree::Match(pattern, syntax_branches) => {
            //dbg!(&global_vars);
            //dbg!(&pattern);
            //let (_, tp, _) = global_vars.get(pattern.get(0).expect("sathu")).expect("sah");
            //let root = pattern.get(0).unwrap(); // unsafe
            //let root = syntax_branches[0].0;
            let tp = match variables.get(&syntax_branches[0].0.value) {
                Some((_, tp)) => tp,
                None => match global_vars.get(&syntax_branches[0].0.value) {
                    None => panic!("uwu"),
                    Some((_, tp, _)) => tp,
                },
            };
            let pattern_type = tp.final_type();

            //let mut pattern_as_tree = pattern.into_iter().map(Argument::Ungrouped);
            //let pattern_expression = evaluate_arguments(
            //    Type::Type(pattern_type),
            //    global_vars,
            //    &variables,
            //    local_vars_count,
            //    &mut pattern_as_tree,
            //)
            //.expect("shhh");

            let pattern_expression = build_tree(
                Type::Type(pattern_type),
                *pattern,
                variables.clone(),
                local_vars_count,
                global_vars
            )?;
            let mut branches = HashMap::new();
            for (name, args, expression) in syntax_branches {
                //dbg!(&name);
                let (id, tp, is_constructor) = global_vars.get(&name.value).expect("sh");
                if !(tp.clone().final_type() == pattern_type) {
                    panic!("bbb")
                }
                let arg_types = tp.clone().arg_types();
                if !(arg_types.len() == args.len()) {
                    dbg!(&arg_types);
                    panic!("ccac");
                }
                let mut local_vars = variables.clone();
                let mut local_var_count = local_vars_count;
                let mut arg_nums = Vec::new();
                for (name, tp) in args.into_iter().zip(arg_types.into_iter()) {
                    if &name != "_" {
                        local_var_count += 1;
                        local_vars.insert(name, (local_var_count, tp));
                        arg_nums.push(Some(local_var_count));
                    } else {
                        arg_nums.push(None)
                    }
                }
                branches.insert(
                    *id as u32,
                    (
                        arg_nums,
                        build_tree(
                            // fix data constructor id
                            expected_type.clone(),
                            expression,
                            local_vars,
                            local_var_count,
                            global_vars,
                        )?
                    ),
                );
            }
            Ok(Expression::Match {
                pattern: Box::new(pattern_expression),
                branches: branches,
            })
        }
    }
}

pub fn parse_art(
    width: usize, 
    height: usize, 
    text: Vec<Vec<Marked<char>>>
) -> Vec<HashMap<(u32, u32), (Marked<char>, Marked<char>)>> {
    let mut output: Vec<HashMap<(u32, u32), (Marked<char>, Marked<char>)>> = Vec::new();
    let mut current_index = 0;
    let mut current_starting_line = 0;
    let mut current_starting_char = 0;
    loop {
        let mut current_map = HashMap::new();
        for x in 0..width as usize {
            for y in 0..height as usize {
                let art_char = text
                    [y + current_starting_line]
                    [x + current_starting_char]
                        .clone();
                let color_char = text
                    [y + current_starting_line]
                    [x + current_starting_char + width]
                        .clone();
                current_map.insert(
                    //((x + (current_starting_char / (width * 2)) as usize) as u32, height as u32 - y as u32 - 1), 
                    //((width - x - 1) as u32, y as u32), 
                    (x as u32, (height - y - 1) as u32), 
                    ( art_char, color_char)
                );
            }
        }
        //dbg!("added");
        output.push(current_map);
        current_starting_char += width * 2;
        if current_starting_char + 1 >= text[current_starting_line].len() {
            current_starting_char = 0;
            current_starting_line += height;
            if current_starting_line + 1 > text.len() {
                return output
            }
        }
    }
}

//fn parse_art(
//    width: u32, 
//    height: u32, 
//    strings: Vec<(usize, String)>, 
//    local_vars: HashMap<String, (u32, Type)>,
//    global_vars: &HashMap<String, (usize, Type, bool)>
//) -> Result<Expression> {
//    for (line_number, i) in strings.into_iter() {
//        let mut line: Vec<Marked<char>> = Vec::new();
//        for character in i.chars() {
//            line.push(Marked::<char> {
//                mark: todo!(),
//                value: character
//            })
//        }
//    }
//    todo!()
//}


fn evaluate_arguments(
    mut expected_type: Type,
    global_vars: &HashMap<String, (usize, Type, bool)>,
    local_vars: &HashMap<String, (u32, Type)>,
    local_var_count: u32,
    argument_stream: &mut impl Iterator<Item = Argument>,
) -> Result<Expression> {
    match argument_stream.next().expect("sathe") {
        Argument::Grouped(syntaxtree) => build_tree(
            expected_type,
            syntaxtree,
            local_vars.clone(),
            local_var_count,
            global_vars,
        ),
        Argument::Ungrouped(name) => {
            let (root_id, root_type) = if let Some((a, b)) = local_vars.get(&name.value) {
                (Id::LambdaArg(*a), b)
            } else if let Some((a, b, c)) = global_vars.get(&name.value) {
                (
                    if *c {
                        Id::DataConstructor(*a as u32)
                    } else {
                        Id::Variable(*a)
                    },
                    b,
                )
            } else {
                return Err(Error {
                    error_message: ErrorType::Empty,
                    mark: name.mark
                })
            };
            if !root_type.is_possible(&expected_type) {
                return Err(Error {
                    error_message: ErrorType::Empty,
                    mark: name.mark
                })
            }
            let mut output_args = Vec::new();
            let mut current_type = root_type.to_owned();
            while current_type != expected_type {
                let (next_type, leftover) = match current_type {
                    Type::Function(a, b) => (*a, *b),
                    Type::Type(_) => unreachable!(),
                };
                current_type = leftover;
                let next_arg = evaluate_arguments(
                    next_type,
                    global_vars,
                    local_vars,
                    local_var_count,
                    argument_stream,
                )?;
                output_args.push(next_arg);
            }
            Ok(Expression::Tree {
                root: root_id,
                arguments: output_args,
            })
        }
    }
}

#[derive(Clone, Debug)]
pub enum SyntaxTree {
    Lambda(
        Marked<String>,           // argument name
        Mark,                     // lambda keyword
        Box<SyntaxTree>,          // body
    ),                            //
    Tree(                         //
        Marked<String>,           // root
        Vec<Argument>,            // arguments
    ),                            //
    Match(                        //
        Box<SyntaxTree>,          // pattern
        Vec<(                     //
            Marked<String>,       // constructor
            Vec<String>,          // constructor argument names
            SyntaxTree,           // body
        )>,
    ),
}

#[derive(Clone, Debug)]
pub enum Argument {
    Ungrouped(Marked<String>),
    Grouped(SyntaxTree),
}

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Include,
    Lambda,
    Match,
    In,
    Define,
    Of_type,
    As,
    To,
    Data,
    Contains,
}

#[derive(Debug, Clone)]
pub struct Marked<T> {
    value: T,
    mark: Mark,
}

#[derive(Debug, Clone)]
pub struct Error {
    error_message: ErrorType,
    mark: Mark,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            show_mark(self.mark.clone()),
            self.error_message
        )
    }
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    Empty,
    Custom(String),
    UnexpectedClosingComment,
    UnexpectedOpeningComment(usize),
    UnclosedComment,
    InvalidName,
}

impl std::fmt::Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Custom(s) => write!(f, "{s}"),
            Self::Empty => write!(f, "empty error message"),
            Self::UnexpectedClosingComment => 
                write!(f, "unexpected closing delimiter"),
            Self::UnexpectedOpeningComment(line) => 
                write!(f, 
"unexpected opening delimiter.
expected a closing delimiter for '---' on line {}
", line + 1),
            Self::UnclosedComment => 
                write!(f, "unclosed multi-line comment"),
            Self::InvalidName => 
                write!(f, "invalid keyword or variable name"),
        }
    }
}

//struct Line(usize);
//
//impl Line {
//    fn new(line: usize) -> Line {
//        Self(line)
//    }
//}
//
//impl std::fmt::Display for Line {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        write!(f, "{}", self.0 + 1)
//    }
//}

#[derive(Debug, Clone)]
pub enum Index {
    Expression(usize),
    Art(usize),
    EndOfWord(usize),
    EndOfLine,
}

#[derive(Debug, Clone)]
pub struct Mark {
    pub file_name: Rc<String>,
    pub file: Rc<Vec<String>>,
    pub line: usize,
    pub block: Option<Rc<String>>,
    pub word_index: Index,
}

pub fn show_mark(mark: Mark) -> String {
    //dbg!(&mark);
    let mut number = (mark.line + 1).to_string();
    number.push(' ');
    let indentation = number.chars().count();
    let line: &str = &mark.file[mark.line];
    let mut length_of_word: usize = 0;
    let mut length_to_word: usize = 0;
    let mut output_string = String::new();
    match mark.word_index {
        Index::Expression(size) => {
            let words = (*line).split_whitespace();
            let mut reached = false;
            for (i, word) in words.enumerate() {
                let word_len = word.chars().count();
                if i == size {
                    length_of_word = word_len;
                    reached = true;
                } else {
                    if !reached {
                        length_to_word += word_len + 1;
                    }
                }
                if word == "--" {
                    output_string.push_str("\x1b[90m");
                }
                output_string.push_str(word);
                output_string.push(' ');
            }
        }
        Index::Art(index) => {
            length_to_word = index;
            length_of_word = 1;
            output_string = line.into();
        }
        _ => todo!()
    }
    let mut underline = String::new();
    underline.push_str(&" ".repeat(length_to_word));
    underline.push_str(&"^".repeat(length_of_word));
    let empty_space = " ".repeat(indentation);
    format!(
        "\x1b[91merror \x1b[0min {} at line {}{}\n\x1b[91m{}|\n{} | \x1b[0m{}\n\x1b[91m{}| {}\x1b[0m",
        mark.file_name,
        mark.line + 1,
        match &mark.block {
            None => String::from(""),
            Some(name) => format!(", in the definition of {}", (*name).clone()),
        },
        &empty_space,
        mark.line + 1,
        output_string,
        &empty_space,
        underline,
    )
}

#[derive(Debug, Clone)]
pub enum ValueToken {
    Value(String),
}

#[derive(Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
    NewLine(u32),
    Variable(ValueToken),
}

pub type TokenStream = std::iter::Peekable<std::vec::IntoIter<Marked<Token>>>;

fn next_non_newline(input: &mut TokenStream) -> Option<Marked<Token>> {
    //while matches!(*input.peek()?, Token::NewLine(_)) {
    //    input.next();
    //}
    //input.next()
    loop {
        let next = input.next()?;
        if !matches!(next.value, Token::NewLine(_)) {
            return Some(next);
        }
    }
}

fn get_with_indentation(mut input: TokenStream, indentation: u32) -> Vec<TokenStream> {
    let mut output: Vec<TokenStream> = Vec::new();
    let mut current: Vec<Marked<Token>> = Vec::new();
    while let Some(token) = input.next() {
        if matches!(&token.value, Token::NewLine(i) if *i == indentation) {
            if !current.is_empty() {
                output.push(current.into_iter().peekable());
                current = Vec::new();
            }
        } else {
            current.push(token)
        }
    }
    output.push(current.into_iter().peekable());
    output
}

pub fn tokenize(
    input: Vec<(usize, String)>,
    file_name: Rc<String>,
    file: Rc<Vec<String>>,
) -> Result<TokenStream> {
    let keywords: HashMap<&str, Keyword> = HashMap::from([
        ("match", Keyword::Match),
        ("define", Keyword::Define),
        ("of_type", Keyword::Of_type),
        ("as", Keyword::As),
        ("in", Keyword::In),
        ("to", Keyword::To),
        ("lambda", Keyword::Lambda),
        ("data", Keyword::Data),
        ("contains", Keyword::Contains),
        ("include", Keyword::Include),
    ]);
    let mut output = Vec::new();
    let mut current_indentation: Option<u32> = None;
    let mut block = input.into_iter();
    'lines: while let Some((line_number, line)) = block.next() {
        let indentation = indentation_length(&line);
        if current_indentation == None {
            output.push(Marked::<Token> {
                mark: Mark {
                    file_name: Rc::clone(&file_name),
                    file: Rc::clone(&file),
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
                file_name: Rc::clone(&file_name),
                file: Rc::clone(&file),
                line: line_number,
                block: None,
                word_index: Index::Expression(word_index),
            };
            match word {
                "--" => break 'words,
                "art" => {
                    let x = match words.next() {
                        None => return Err(Error {
                            mark: Mark { word_index: Index::EndOfWord(word_index), ..mark },
                            error_message: ErrorType::Empty,
                        }),
                        Some(x) => match parse_roman_numeral(x) {
                            None => return Err(Error {
                                mark: Mark { word_index: Index::Expression(word_index + 1), ..mark },
                                error_message: ErrorType::Empty,
                            }),
                            Some(x) => x
                        }
                    };
                    let y = match words.next() {
                        None => return Err(Error {
                            mark: Mark { word_index: Index::EndOfWord(word_index), ..mark },
                            error_message: ErrorType::Empty,
                        }),
                        Some(x) => match parse_roman_numeral(x) {
                            None => return Err(Error {
                                mark: Mark { word_index: Index::Expression(word_index + 2), ..mark },
                                error_message: ErrorType::Empty,
                            }),
                            Some(x) => x
                        }
                    };
                    let art: Vec<(usize, Vec<(usize, char)>)> = 
                        block.map(|(index, line)| (index, line.chars().enumerate().collect())).collect();
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
                                }
                            };
                            temp.push(marked_char);
                        }
                        new_output.push(temp);
                    }
                    let aaa = parse_art(x as usize, y as usize, new_output);
                    for i in build_tokens_from_art(mark, aaa)? {
                        output.push(i);
                    }
                    //output.push(Marked::<Token> {
                    //    mark: mark,
                    //    value: Token::Variable(ValueToken::Art( x, y, block.collect(),)),
                    //});
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
                                    error_message: ErrorType::InvalidName,
                                });
                            }
                            Token::Variable(ValueToken::Value(other.to_string()))
                        }
                    },
                }),
            }
            word_index += 1;
            current_indentation = None;
        }
    }
    Ok(output.into_iter().peekable())
}

fn build_tokens_from_art(
    mark: Mark, 
    input: Vec<HashMap<(u32, u32), (Marked<char>, Marked<char>)>>
) -> Result<TokenStream> {
    let mut output = Vec::new();
    for i in input.into_iter() {
        output.push(Marked::<Token> {
            mark: mark.clone(),
            value: Token::Variable(ValueToken::Value("cons_frame".to_string()))
        });
        for ((x, y), (c1, c2)) in i.into_iter() {
            let c1_char = c1.value;
            let c2_char = c2.value;
            if c1_char == ' ' && c2_char == '.' { continue }
            output.push(Marked::<Token> {
                mark: mark.clone(),
                value: Token::Variable(ValueToken::Value("unsafe_cons_cell".to_string()))
            });
            output.push(Marked::<Token> {
                mark: mark.clone(),
                value: Token::Variable(ValueToken::Value("cell".to_string()))
            });
            output.push(Marked::<Token> {
                mark: mark.clone(),
                value: Token::Variable(ValueToken::Value("coordinate".to_string()))
            });
            output.push(Marked::<Token> {
                mark: mark.clone(),
                value: Token::Variable(ValueToken::Value("positive".to_string()))
            });
            for _ in 0..x {
                output.push(Marked::<Token> {
                    mark: mark.clone(),
                    value: Token::Variable(ValueToken::Value("succ".to_string()))
                });
            }
            output.push(Marked::<Token> {
                mark: mark.clone(),
                value: Token::Variable(ValueToken::Value("one".to_string()))
            });
            output.push(Marked::<Token> {
                mark: mark.clone(),
                value: Token::Variable(ValueToken::Value("positive".to_string()))
            });
            for _ in 0..y {
                output.push(Marked::<Token> {
                    mark: mark.clone(),
                    value: Token::Variable(ValueToken::Value("succ".to_string()))
                });
            }
            output.push(Marked::<Token> {
                mark: mark.clone(),
                value: Token::Variable(ValueToken::Value("one".to_string()))
            });
            match (c1_char, c2_char) {
                (' ', '/') => {
                    output.push(Marked::<Token> {
                        mark: mark.clone(),
                        value: Token::Variable(ValueToken::Value("space".to_string()))
                    });
                }
                (c1_char, '*') => {
                    output.push(Marked::<Token> {
                        mark: mark.clone(),
                        value: Token::Variable(ValueToken::Value(c1_char.to_string()))
                    });
                }
                (c1_char, c2_char) => {
                    output.push(Marked::<Token> {
                        mark: mark.clone(),
                        value: Token::Variable(ValueToken::Value("char".to_string()))
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
                        _ => panic!("bad char")
                    }.to_string();
                    output.push(Marked::<Token> {
                        mark: mark.clone(),
                        value: Token::Variable(ValueToken::Value(character))
                    });
                    let color = match c2_char {
                        '0' => "black".to_string(),
                        '1' => "red".to_string(),
                        '2' => "green".to_string(),
                        '3' => "yellow".to_string(),
                        '4' => "blue".to_string(),
                        '5' => "magenta".to_string(),
                        '6' => "cyan".to_string(),
                        '7' => "white".to_string(),
                        x   => String::from(x)
                    };
                    output.push(Marked::<Token> {
                        mark: mark.clone(),
                        value: Token::Variable(ValueToken::Value(color))
                    });
                }
            }
        }
        output.push(Marked::<Token> {
            mark: mark.clone(),
            value: Token::Variable(ValueToken::Value("empty_frame".to_string()))
        });
    }
    output.push(Marked::<Token> {
        mark: mark.clone(),
        value: Token::Variable(ValueToken::Value("empty_video".to_string()))
    });
    Ok(output.into_iter().peekable())
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
    Value(String, Vec<String>),
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
    // unwrap safe assuming it's not empty which it shouldn't be
    let Marked::<Token> {
        mark: mark1,
        value: token,
    } = next_non_newline(input).unwrap();
    match token {
        Token::Keyword(Keyword::Define) => {
            //let Marked::<Token> {mark: mark2, value: token} = next_non_newline(input)?;
            let Marked::<Token> {
                mark: mark2,
                value: token,
            } = next_non_newline(input).ok_or(Error {
                mark: convert_to_after(mark1).unwrap(),
                error_message: ErrorType::Empty,
            })?;
            match token {
                Token::Variable(ValueToken::Value(name)) => {
                    let Marked::<Token> {
                        mark: mark3,
                        value: token,
                    } = next_non_newline(input).ok_or(Error {
                        mark: convert_to_after(mark2).unwrap(),
                        error_message: ErrorType::Empty,
                    })?;
                    if !matches!(token, Token::Keyword(Keyword::Of_type)) {
                        return Err(Error {
                            mark: mark3,
                            error_message: ErrorType::Empty,
                        });
                    }
                    let mut type_strings: Vec<String> = Vec::new();
                    loop {
                        let Marked::<Token> {
                            mark: mark4,
                            value: token,
                        } = match next_non_newline(input) {
                            Some(x) => x,
                            None => {
                                return Err(Error {
                                    mark: convert_to_after(mark3).unwrap(),
                                    error_message: ErrorType::Empty,
                                });
                            }
                        };
                        match token {
                            Token::Variable(ValueToken::Value(word)) => type_strings.push(word),
                            Token::Keyword(Keyword::As) => break,
                            _ => return Err(Error {
                                mark: mark4,
                                error_message: ErrorType::Empty,
                            })
                        }
                    }
                    Ok(Signiture::Value(name, type_strings))
                }
                _ => panic!("extract"),
            }
        }
        Token::Keyword(Keyword::Data) => {
            let Marked::<Token> {
                mark: root_mark,
                value: token,
            } = next_non_newline(input).ok_or(Error {
                mark: convert_to_after(mark1).unwrap(),
                error_message: ErrorType::Empty,
            })?;
            match token {
                Token::Variable(ValueToken::Value(name)) => {
                    if matches!(
                        next_non_newline(input).map(|x| x.value),
                        Some(Token::Keyword(Keyword::Contains))
                    ) {
                        Ok(Signiture::Type(name))
                    } else {
                        panic!("expected 'contains' keyword")
                    }
                }
                _ => panic!("expected a name"),
            }
        }
        x => Err(Error {
            mark: mark1,
            error_message: ErrorType::Custom(format!(
                "expected 'data' or 'define' but found {:?}",
                x
            )),
        }),
    }
}

pub fn parse_type(
    strings: &mut impl Iterator<Item = String>,
    table: &HashMap<String, u32>,
) -> Result<Type> {
    match strings.next().expect("jjjj").as_str() {
        "fn" => {
            let arg1 = parse_type(strings, table)?;
            let arg2 = parse_type(strings, table)?;
            Ok((Type::Function(Box::new(arg1), Box::new(arg2))))
        }
        word => {
            let index = table.get(word).expect("kkkk").clone();
            Ok(Type::Type(index))
        }
    }
}

pub fn parse_data(
    mut tokens: TokenStream,
    types: &HashMap<String, u32>,
    parent_type: u32,
) -> Result<Vec<(String, Type)>> {
    let mut output = Vec::new();
    for mut i in get_with_indentation(tokens, 3).into_iter() {
        let Marked::<Token> {
            mark: root_mark,
            value: token,
        } = next_non_newline(&mut i).expect("asuhas");
        let name = if let Token::Variable(ValueToken::Value(word)) = token {
            word
        } else { 
            return Err(Error {
                error_message: ErrorType::Empty,
                mark: root_mark,
            })
        };
        let mut type_strings = Vec::new();
        for Marked::<Token> {
            mark: root_mark,
            value: token,
        } in i
        {
            match token {
                Token::Variable(ValueToken::Value(word)) => type_strings.push(word),
                Token::NewLine(_) => continue,
                _ => return Err(Error {
                    error_message: ErrorType::Empty,
                    mark: root_mark,
                })
            }
        }
        let mut strings = type_strings.into_iter().peekable();
        let mut arg_types: Vec<Type> = Vec::new();
        while strings.peek().is_some() {
            arg_types.push(parse_type(&mut strings, types)?)
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
