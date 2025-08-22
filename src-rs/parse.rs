use crate::r#type::Type;
use std::collections::HashMap;

pub fn build_syntax_tree(mut tokens: TokenStream) -> Option<SyntaxTree> {
    match tokens.next_non_newline()? {
        Token::NewLine(_) => build_syntax_tree(tokens),
        Token::Keyword(keyword) => match keyword {
            Keyword::Lambda => {
                if let Token::Variable(ValueToken::Value(argument)) = tokens.next_non_newline()? {
                    Some(SyntaxTree::Lambda(
                        argument,
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
                let branches: Vec<(String, Vec<String>, Box<SyntaxTree>)> = tokens
                    .get_with_indentation(indent)
                    .into_iter()
                    .map(parse_branch)
                    .collect::<Option<Vec<_>>>()?;
                Some(SyntaxTree::Match(pattern, branches))
            }
            _ => todo!(),
        },
        Token::Variable(ValueToken::Art(x, y, art)) => Some(SyntaxTree::Art(x, y, art)),
        Token::Variable(ValueToken::Value(word)) => {
            let root = word;
            let mut args = Vec::new();
            loop {
                match tokens.peek() {
                    None => break,
                    Some(x) if matches!(x, Token::Keyword(_)) => {
                        args.push(Group(Box::new(build_syntax_tree(tokens)?)));
                    }
                }
                match tokens.next().unwrap() { // unwrap is safe
                    Token::Variable

            // let mut regular: Vec<String> = Vec::from([word]);
            // let mut last: Option<Box<SyntaxTree>>;
            // loop {
            //     if matches!(tokens.peek(), Some(Token::NewLine(_))) {
            //         tokens.next();
            //     }
            //     match tokens.peek() {
            //         None => {
            //             last = None;
            //             break;
            //         }
            //         Some(next) => {
            //             if matches!(next, Token::Keyword(_)) {
            //                 last = Some(Box::new(build_syntax_tree(tokens)?));
            //                 break;
            //             }
            //         }
            //     }
            //     match tokens.next_non_newline().unwrap() {
            //         Token::Variable(ValueToken::Art(x, y, art)) => {
            //             last = Some(Box::new(SyntaxTree::Art(x, y, art)));
            //             break;
            //         }
            //         Token::Variable(ValueToken::Value(w)) => regular.push(w),
            //         _ => unreachable!(),
            //     }
            // }
            Some(SyntaxTree::Tree(root, args))
        }
    }
}

fn parse_branch(mut tokens: TokenStream) -> Option<(String, Vec<String>, Box<SyntaxTree>)> {
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
        Box::new(build_syntax_tree(tokens).expect("c")),
    ))
}

build_tree(
    expected_type: Type, 
    input: SyntaxTree, 
    mut variables: HashMap<String, (u32, Type)>
    mut local_vars_count: u32,
    global_vars: &HashMap<String, u32>,
    var_types: &Vec<Type>
) -> Option<Expression> {
    match input {
        SyntaxTree::Lambda(name, tree) => {
            if let Function(a, b) = expected_type {
                local_vars_count += 1;
                variables.insert(name, (local_vars_count, a);
                build_tree(
                    b,
                    tree, 
                    variables,
                    local_vars_count,
                    global_vars,
                    var_types,
                )
            } else {
                None
            }
        },
    SyntaxTree::Tree(vec, last) {

    }
        _ => todo!()
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
            Box<SyntaxTree>, // body
        )>,
    ),
    Art(String, String, Vec<String>),
}

enum Argument {
    Ungrouped(String),
    Grouped(Box<SyntaxTree>),
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
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.remove(0))
        }
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
        _ => todo!(),
    }
}

fn parse_type(
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
) -> Option<Vec<(String, Vec<Type>)>> {
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
        output.push((name, arg_types))
    }
    Some(output)
}
