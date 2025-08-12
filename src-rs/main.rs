use std::{collections::HashMap, env, fmt, fs, rc::Rc};
// save unchanging in map, only leave variables in expression!

mod data;

use crate::data::{Expression, Id};

fn main() {
    let string: Vec<String> = 
"match f a b
  cons a b to lambda a b match b
    a b to d
  nil to just six"
        .lines()
        .map(str::to_string)
        .collect();
    dbg!(build_syntax_tree(tokenize(string).unwrap()));
}

fn build_syntax_tree(mut tokens: TokenStream) -> Option<SyntaxTree> {
    match tokens.next_non_newline()? {
        Token::NewLine(_) => build_syntax_tree(tokens),
        Token::Keyword(keyword) => match keyword {
            Keyword::Lambda => {
                if let Token::Variable(ValueToken::Value(argument)) = tokens.next_to_keyword()? {
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
                    match tokens.next()? { // fail if no branches
                        Token::NewLine(num) => {
                            indent = num;
                            break
                        }
                        Token::Variable(ValueToken::Value(word)) => {
                            pattern.push(word);
                        }
                        _ => panic!("f")
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
            let mut regular: Vec<String> = Vec::from([word]);
            let mut last: Option<Box<SyntaxTree>>;
            loop {
                if matches!(tokens.peek(), Some(Token::NewLine(_))) {
                    tokens.consume_one();
                }
                match tokens.peek() {
                    None => {
                        last = None;
                        break;
                    }
                    Some(next) => {
                        if matches!(next, Token::Keyword(_)) {
                            last = Some(Box::new(build_syntax_tree(tokens)?));
                            break;
                        }
                    }
                }
                match tokens.next_non_newline().unwrap() {
                    Token::Variable(ValueToken::Art(x, y, art)) => {
                        last = Some(Box::new(SyntaxTree::Art(x, y, art)));
                        break;
                    }
                    Token::Variable(ValueToken::Value(w)) => regular.push(w),
                    _ => unreachable!(),
                }
            }
            Some(SyntaxTree::Tree(regular, last))
        }
    }
}

fn parse_branch(mut tokens: TokenStream) -> Option<(String, Vec<String>, Box<SyntaxTree>)> {
    let constructor = match tokens.next().expect("a") {
        Token::Variable(ValueToken::Value(name)) => name,
        _ => panic!("d")
    };
    let mut args = Vec::new();
    while let Token::Variable(ValueToken::Value(name)) = tokens.next().expect("b") {
        args.push(name);
    }
    Some((
        constructor, 
        args, 
        Box::new(build_syntax_tree(tokens).expect("c"))
    ))
}


#[derive(Clone, Debug)]
enum SyntaxTree {
    Lambda(
        String,          // argument name
        Box<SyntaxTree>, // body
    ),
    Tree(
        Vec<String>,             // regular values
        Option<Box<SyntaxTree>>, // optoional last one
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

#[derive(Debug, Clone, Copy)]
enum Keyword {
    Lambda,
    Match,
    Define,
    Of_type,
    As,
    Art,
    To,
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
struct TokenStream(Vec<Token>);

impl TokenStream {
    fn next(&mut self) -> Option<Token> {
        self.0.pop()
    }

    fn new(tokens: Vec<Token>) -> Self {
        TokenStream(tokens.into_iter().rev().collect())
    }

    fn next_non_newline(&mut self) -> Option<Token> {
        while matches!(*self.peek()?, Token::NewLine(_)) {
            self.consume_one();
        }
        self.0.pop()
    }

    fn consume_one(&mut self) {
        self.0.pop();
    }

    fn next_to_keyword(&mut self) -> Option<Token> {
        let next = self.0.pop()?;
        match &next {
            Token::NewLine(_) => self.next_to_keyword(),
            Token::Variable(_) => Some(next),
            Token::Keyword(_) => {
                self.0.push(next);
                None
            }
        }
    }

    fn peek(&self) -> Option<&Token> {
        Some(self.0.last()?)
    }

    fn get_with_indentation(mut self, indentation: u32) -> Vec<TokenStream> {
        let mut output: Vec<TokenStream> = Vec::new();
        let mut current: Vec<Token> = Vec::new();
        while let Some(token) = self.0.pop() {
            if matches!(&token, Token::NewLine(i) if *i == indentation) {
                output.push(TokenStream::new(current));
                current = Vec::new();
            } else {
                current.push(token)
            }
        }
        output.push(TokenStream::new(current));
        output
    }
}

fn tokenize(input: Vec<String>) -> Option<TokenStream> {
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

// check if line contains 'art' and cut the rest
// check if line contains 'of' and group the rest

fn test() {
    let mut initial = Expression::Tree {
        root: Id::LambdaArg(5),
        arguments: Vec::from([Expression::Tree {
            root: Id::DataConstructor(7),
            arguments: Vec::new(),
        }]),
    };

    initial.substitute(
        Id::LambdaArg(5),
        Rc::new(Expression::Tree {
            root: Id::DataConstructor(7),
            arguments: Vec::new(),
        }),
    );

    let h = HashMap::new();
    initial.simplify(&h);

    dbg!(initial);
}

/*

const example_code: &str = "lambda x x";


type Constructors = HashMap<String, Vec<Name>>;
struct Types(HashMap<Name, Constructors>);

impl Types {
    fn insert(&mut self, name: Name, constructors: Constructors) -> Result<(), Error> {
        match self.insert(name.clone(), constructors) {
            Err(_) => Ok(()),
            Ok(_) => todo!(),
        }
    }
}

#[derive(Debug)]
struct Mods {
    frame_time: f32,
    directory: Path,
    comment: bool,
    quiet: bool,
    check: bool,
    target: Name,
}

impl Default for Mods {
    fn default() -> Self {
        Mods {
            frame_time: 0.2,
            directory: Path(String::from(".")),
            comment: false,
            quiet: false,
            check: false,
            target: Name::new(String::from("main")).unwrap(),
        }
    }
}

fn real_main() -> Result<ResultType, Error> {
    let (mods, Path(file)) = arguments()?;
    let input = fs::read_to_string(&file).expect("couldn't read file");
    let binding = input
        .lines()
        .map(|s| s.trim_end())
        .enumerate()
        .filter(|(_, s)| !s.is_empty())
        .map(|(a, b)| Marked {
            mark: Mark::File {
                file_path: Path(file.clone()),
                line: a as u32,
                block: None,
            },
            val: b,
        })
        .collect::<Vec<Marked<&str>>>()
        .split(|Marked { mark: _, val: s }| s.to_string() == "end")
        .for_each(|x| {
            parse_block(x.to_owned().into_iter());
        });
    Ok(ResultType::Int(6))
}

fn parse_block<'a>(mut input: impl Iterator<Item = Marked<&'a str>>) {
    let dummy = input.next().unwrap().apply_fn(|x| parse_header(x)).unwrap();
    let Marked { val: v, .. } = input.next().unwrap();
    let mut nums = v.split_whitespace();
    let x: u8 = nums.next().unwrap().parse().unwrap();
    let y: u8 = nums.next().unwrap().parse().unwrap();
    println!("{dummy}, {x}, {y}");
    parse_block_art(input, 5, 5);
}

fn parse_block_art<'a>(
    s: impl Iterator<Item = Marked<&'a str>>,
    width: u8,
    height: u8,
) -> HashMap<Coordinate, (Color, char)> {
    for (i, j) in s.map(|x| {
        let chars = x.val;
        (
            chars.len(),
            Marked {
                val: chars,
                mark: x.mark,
            },
        )
    }) {
        println!("{i}");
    }

    HashMap::new()
}

fn arguments() -> Result<(Mods, Path), Error> {
    let mut args: std::env::Args = env::args();
    args.next();
    parse_args(args, Mods::default())
}

fn parse_args(mut args: std::env::Args, mut mods: Mods) -> Result<(Mods, Path), Error> {
    match args
        .next()
        .ok_or(Error {
            mark: Mark::Arguments,
            error: ErrorType::BadArgument,
        })?
        .as_str()
    {
        "-q" => {
            mods.quiet = true;
            parse_args(args, mods)
        }
        "-c" => {
            mods.check = true;
            parse_args(args, mods)
        }
        "-m" => {
            mods.comment = true;
            parse_args(args, mods)
        }
        f => Ok((mods, Path(f.to_string()))),
    }
}

fn testing() {
    let mut move_x: DummyData = DummyData {
        name: Name::new("move_x".to_string()).unwrap(),
        type_signiture: Type::Fn(Box::new(Type::Int), Box::new(Type::Gif)),
    };
    let five: DummyData = DummyData {
        name: Name::new("five".to_string()).unwrap(),
        type_signiture: Type::Int,
    };

    // println!(
    //     "{}",
    //     Error {
    //         mark: Mark::File {
    //             file_path: Path("~/your/mom".to_string()),
    //             line: 17,
    //             block: Some(Name("robot".to_string()))
    //         },
    //         error: ErrorType::TypeError(add, five)
    //     }

    // match move_x.dummy_apply(five) {
    //     Ok(x) => println!("{x}"),
    //     Err(x) => println!("{x}"),
    // };

    let hd: Marked<&str> = Marked {
        mark: Mark::Arguments,
        val: "tree : fn fn int gif gif",
    };

    match hd.apply_fn(parse_header) {
        Ok(mut x) => {
            match x.dummy_apply(move_x) {
                Ok(x) => println!("{x}"),
                Err(x) => println!("{x}"),
            };
        }
        Err(x) => println!("{x}"),
    }
}

fn parse_header(s: &str) -> Result<DummyData, ErrorType> {
    let mut words = s.split_whitespace(); //.collect::<Vec<&str>>();
    let name: Name = words
        .next()
        .ok_or(ErrorType::BadHeader)
        .and_then(|x| Name::new(x.to_string()))?;

    match words.next() {
        None => return Err(ErrorType::BadHeader),
        Some(":") => (),
        _ => return Err(ErrorType::BadHeader),
    };

    Ok(DummyData {
        name,
        type_signiture: Type::parse(words)?,
    })
}
*/
