use crate::r#type::Type;
use crate::runtime::{
    Expression,
    Id,
};
use std::collections::HashMap;

pub fn tokenize_file(input: String) -> Option<Vec<TokenStream>> {
    input
        .lines()
        .map(str::trim_end)
        .collect::<Vec<_>>()
        .as_slice()
        .split(|x| x.len() == 0)
        .map(|x| x.into_iter().map(|x| x.to_string()).collect())
        .map(tokenize)
        .collect::<Option<Vec<TokenStream>>>()
}

//parse_file(input: String) -> 

pub fn build_syntax_tree(mut tokens: TokenStream) -> Option<SyntaxTree> {
    match tokens.next_non_newline()? {
        Token::Keyword(keyword) => match keyword {
            Keyword::Lambda => {
                if let Token::Variable(ValueToken::Value(arg)) = tokens.next_non_newline()? {
                    Some(SyntaxTree::Lambda(
                        arg,
                        Box::new(build_syntax_tree(tokens)?),
                    ))
                } else {
                    None
                }
            }
            Keyword::Match => {
                let mut indent: u32;
                let mut pattern = Vec::new();
                loop {
                    match tokens.next()? {
                        // fail if no branches
                        Token::NewLine(num) => {
                            indent = num;
                            break;
                        }
                        Token::Variable(ValueToken::Value(word)) => {
                            pattern.push(word);
                        }
                        _ => panic!("f"),
                    }
                }
                let branches: Vec<(String, Vec<String>, SyntaxTree)> = tokens
                    .get_with_indentation(indent)
                    .into_iter()
                    .map(parse_branch)
                    .collect::<Option<Vec<_>>>()?;
                Some(SyntaxTree::Match(pattern, branches))
            }
            _ => None
        },
        Token::Variable(ValueToken::Art(x, y, art)) => Some(SyntaxTree::Art(x, y, art)),
        Token::Variable(ValueToken::Value(word)) => {
            let root = word;
            let mut args = Vec::new();
            loop {
                match tokens.peek() {
                    None => break,
                    Some(x) => if matches!(x, Token::Keyword(_)) {
                        args.push(Argument::Grouped(build_syntax_tree(tokens)?));
                        break
                    }
                }
                match tokens.next().unwrap() { // unwrap is safe
                    Token::Variable(ValueToken::Value(w)) => args.push(Argument::Ungrouped(w)),
                    Token::Variable(ValueToken::Art(a, b, c)) =>
                        args.push(Argument::Grouped(SyntaxTree::Art(a, b, c))),
                    Token::NewLine(indentation) => {
                        let arg_groups = tokens
                            .get_with_indentation(indentation)
                            .into_iter()
                            .map(build_syntax_tree);
                        for i in arg_groups {
                            args.push(Argument::Grouped(i?));
                        }
                        break
                    }
                    _ => unreachable!()
                }
            }
            Some(SyntaxTree::Tree(root, args))
        },
        Token::NewLine(_) => unreachable!()
    }
}

fn parse_branch(mut tokens: TokenStream) -> Option<(String, Vec<String>, SyntaxTree)> {
    let constructor = match tokens.next().expect("a") {
        Token::Variable(ValueToken::Value(name)) => name,
        _ => panic!("d"),
    };
    let mut args = Vec::new();
    loop {
        match tokens.next().expect("b") {
            Token::Variable(ValueToken::Value(name)) => args.push(name),
            Token::Keyword(Keyword::To) => break,
            _ => panic!("bad match syntax"),
        }
    }
    Some((
        constructor,
        args,
        build_syntax_tree(tokens).expect("c"),
    ))
}

pub fn build_tree(
    expected_type: Type, 
    input: SyntaxTree, 
    mut variables: HashMap<String, (u32, Type)>,
    mut local_vars_count: u32,
    global_vars: &HashMap<String, (usize, Type, bool)>,
) -> Option<Expression> {
    match input {
        SyntaxTree::Lambda(name, tree) => {
            if let Type::Function(a, b) = expected_type {
                local_vars_count += 1;
                variables.insert(name, (local_vars_count, *a));
                let body = build_tree(
                    *b,
                    *tree, 
                    variables,
                    local_vars_count,
                    global_vars,
                )?;
                Some(Expression::Lambda {
                    id: local_vars_count,
                    body: Box::new(body)
                })
            } else {
                panic!("aaa")
            }
        },
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
                &mut vec
            )
        }
        SyntaxTree::Match(pattern, syntax_branches) => {
            //dbg!(&global_vars);
            //dbg!(&pattern);
            //let (_, tp, _) = global_vars.get(pattern.get(0).expect("sathu")).expect("sah");
            let root = pattern.get(0).unwrap();
            let tp = match variables.get(root) {
                Some((_, tp)) => tp,
                None => match global_vars.get(root) {
                    None => return None,
                    Some((_, tp, _)) => tp
                }
            };
            let mut pattern_as_tree = pattern.into_iter().map(Argument::Ungrouped);
            let pattern_type = tp.final_type();
            let pattern_expression = evaluate_arguments(
                Type::Type(pattern_type),
                global_vars,
                &variables,
                local_vars_count,
                &mut pattern_as_tree
            ).expect("shhh");
            let mut branches = HashMap::new();
            for (name, args, expression) in syntax_branches {
                let (id, tp, is_constructor) = global_vars.get(&name).expect("sh");
                if !(tp.clone().final_type() == pattern_type) {
                    panic!("bbb")
                }
                let arg_types = tp.clone().arg_types();
                if !(arg_types.len() == args.len()) {
                    panic!("ccc");
                }
                let mut local_vars = variables.clone();
                let mut local_var_count = local_vars_count;
                let mut arg_nums = Vec::new();
                for (name, tp) in args.into_iter().zip(arg_types.into_iter()) {
                    local_var_count += 1;
                    local_vars.insert(name, (local_var_count, tp));
                    arg_nums.push(local_var_count);
                }
                branches.insert(*id as u32, (arg_nums, build_tree( // fix data constructor id
                    expected_type.clone(),
                    expression,
                    local_vars,
                    local_var_count,
                    global_vars,
                ).expect("sahe")));
            }
            Some(Expression::Match {
                pattern: Box::new(pattern_expression),
                branches: branches,
            })
        }
        SyntaxTree::Art(a, b, c) => todo!()
    }
}
    //Match {
    //    pattern: Box<Expression>,
    //    branches: HashMap<u32, (Vec<u32>, Expression)>,
    //},
    //Match(
    //    Vec<String>, // pattern
    //    Vec<(
    //        String,          // constructor
    //        Vec<String>,     // constructor argument names
    //        Box<SyntaxTree>, // body
    //    )>,
    //),

fn evaluate_arguments(
    mut expected_type: Type, 
    global_vars: &HashMap<String, (usize, Type, bool)>,
    local_vars:  &HashMap<String, (u32, Type)>,
    local_var_count: u32,
    argument_stream: &mut impl Iterator<Item = Argument>,
) -> Option<Expression> {
    match argument_stream.next()? {
        Argument::Grouped(syntaxtree) => build_tree(
            expected_type,
            syntaxtree,
            local_vars.clone(),
            local_var_count,
            global_vars,
        ),
        Argument::Ungrouped(name) => {
            let (root_id, root_type) = if let Some((a, b)) = local_vars.get(&name) {
                (Id::LambdaArg(*a), b)
            } else if let Some((a, b, c)) = global_vars.get(&name) {
                (if *c {
                    Id::DataConstructor(*a as u32)
                } else {
                    Id::Variable(*a)
                }, b)
            } else { panic!("ddd") };
            if !root_type.is_possible(&expected_type) {
                panic!("ccc");
            }
            let mut output_args = Vec::new();
            let mut current_type = root_type.to_owned();
            while current_type != expected_type {
                let (next_type, leftover) = match current_type {
                    Type::Function(a, b) => (*a, *b),
                    Type::Type(_) => unreachable!()
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
            Some(Expression::Tree {
                root: root_id,
                arguments: output_args
            })
        }
    }
}


#[derive(Clone, Debug)]
pub enum SyntaxTree {
    Lambda(
        String,          // argument name
        Box<SyntaxTree>, // body
    ),
    Tree(
        String,             // root
        Vec<Argument>,      // arguments
    ),
    Match(
        Vec<String>, // pattern
        Vec<(
            String,          // constructor
            Vec<String>,     // constructor argument names
            SyntaxTree, // body
        )>,
    ),
    Art(String, String, Vec<String>),
}

#[derive(Clone, Debug)]
enum Argument {
    Ungrouped(String),
    Grouped(SyntaxTree),
}

#[derive(Debug, Clone, Copy)]
enum Keyword {
    Lambda,
    Match,
    Define,
    Of_type,
    As,
    Art,
    To,
    Data,
    Contains,
}

#[derive(Debug, Clone)]
enum ValueToken {
    Value(String),
    Art(String, String, Vec<String>),
}

#[derive(Debug, Clone)]
enum Token {
    Keyword(Keyword),
    NewLine(u32),
    Variable(ValueToken),
}

#[derive(Debug, Clone)]
pub struct TokenStream(Vec<Token>);

impl TokenStream {
    fn new(tokens: Vec<Token>) -> Self {
        TokenStream(tokens)
    }

    fn next(&mut self) -> Option<Token> {
        if self.0.is_empty() { None } else { Some(self.0.remove(0)) }
    }

    fn peek(&self) -> Option<&Token> {
        self.0.get(0)
    }

    fn next_non_newline(&mut self) -> Option<Token> {
        while matches!(*self.peek()?, Token::NewLine(_)) {
            self.next();
        }
        self.next()
    }

    fn get_with_indentation(mut self, indentation: u32) -> Vec<TokenStream> {
        let mut output: Vec<TokenStream> = Vec::new();
        let mut current: Vec<Token> = Vec::new();
        while let Some(token) = self.next() {
            if matches!(&token, Token::NewLine(i) if *i == indentation) {
                if !current.is_empty() {
                    output.push(TokenStream::new(current));
                    current = Vec::new();
                }
            } else {
                current.push(token)
            }
        }
        output.push(TokenStream::new(current));
        output
    }
}

pub fn tokenize(input: Vec<String>) -> Option<TokenStream> {
    let mut output = Vec::new();
    let mut current_indentation: Option<u32> = None;
    let mut block = input.into_iter();
    'lines: while let Some(line) = block.next() {
        let indentation = indentation_length(&line);
        if current_indentation == None {
            output.push(Token::NewLine(indentation));
            current_indentation = Some(indentation)
        }
        let mut words = line.split_whitespace();
        'words: while let Some(word) = words.next() {
            match word {
                "--" => break 'words,
                "art" => {
                    output.push(Token::Variable(ValueToken::Art(
                        words.next().expect("e").to_string(),
                        words.next().expect("f").to_string(),
                        block.collect(),
                    )));
                    break 'lines;
                }
                other => output.push(match other {
                    "match" => Token::Keyword(Keyword::Match),
                    "define" => Token::Keyword(Keyword::Define),
                    "of_type" => Token::Keyword(Keyword::Of_type),
                    "as" => Token::Keyword(Keyword::As),
                    "to" => Token::Keyword(Keyword::To),
                    "lambda" => Token::Keyword(Keyword::Lambda),
                    "data" => Token::Keyword(Keyword::Data),
                    "contains" => Token::Keyword(Keyword::Contains),
                    word => Token::Variable(ValueToken::Value(word.to_string())),
                }),
            }
            current_indentation = None;
        }
    }
    Some(TokenStream::new(output))
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

pub fn extract_signiture(input: &mut TokenStream) -> Option<Signiture> {
    match input.next_non_newline()? {
        Token::Keyword(Keyword::Define) => match input.next_non_newline()? {
            Token::Variable(ValueToken::Value(name)) => {
                if !matches!(input.next()?, Token::Keyword(Keyword::Of_type)) {
                    return None;
                }
                let mut type_strings: Vec<String> = Vec::new();
                loop {
                    match input.next_non_newline()? {
                        Token::Variable(ValueToken::Value(word)) => type_strings.push(word),
                        Token::Keyword(Keyword::As) => break,
                        _ => panic!("bad type"),
                    }
                }
                Some(Signiture::Value(name, type_strings))
            }
            _ => panic!("extract"),
        },
        Token::Keyword(Keyword::Data) => match input.next_non_newline()? {
            Token::Variable(ValueToken::Value(name)) => {
                if matches!(
                    input.next_non_newline(),
                    Some(Token::Keyword(Keyword::Contains))
                ) {
                    Some(Signiture::Type(name))
                } else {
                    panic!("expected 'contains' keyword")
                }
            }
            _ => panic!("expected a name"),
        },
        _ => None
    }
}

pub fn parse_type(
    strings: &mut impl Iterator<Item = String>,
    table: &HashMap<String, u32>,
) -> Option<Type> {
    match strings.next()?.as_str() {
        "fn" => {
            let arg1 = parse_type(strings, table)?;
            let arg2 = parse_type(strings, table)?;
            Some((Type::Function(Box::new(arg1), Box::new(arg2))))
        }
        word => {
            let index = table.get(word)?.clone();
            Some(Type::Type(index))
        }
    }
}

pub fn parse_data(
    mut tokens: TokenStream,
    types: &HashMap<String, u32>,
    parent_type: u32,
) -> Option<Vec<(String, Type)>> {
    let mut output = Vec::new();
    for mut i in tokens.get_with_indentation(2).into_iter() {
        let name = if let Token::Variable(ValueToken::Value(word)) = i.next_non_newline()? {
            word
        } else {
            return None;
        };
        let mut type_strings = Vec::new();
        for i in i.0.into_iter() {
            match i {
                Token::Variable(ValueToken::Value(word)) => type_strings.push(word),
                Token::NewLine(_) => continue,
                _ => return None,
            }
        }
        let mut strings = type_strings.into_iter().peekable();
        let mut arg_types: Vec<Type> = Vec::new();
        while strings.peek().is_some() {
            arg_types.push(parse_type(&mut strings, types)?)
        }
        let mut args = arg_types.into_iter();
        output.push((
            name, 
            build_type(&mut args, Type::Type(parent_type))
        ));
    }
    Some(output)
}

pub fn build_type(input: &mut impl Iterator<Item = Type>, result: Type) -> Type {
    match input.next() {
        None => result,
        Some(x) => {
            Type::Function(Box::new(x), Box::new(build_type(input, result)))
        }
    }
}

