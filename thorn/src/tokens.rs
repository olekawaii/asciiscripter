use crate::error::*;
use std::collections::{ LinkedList, HashMap };
use std::sync::Arc;

const INDENTATION: u8 = 4;

#[derive(Debug)]
pub enum ParseError {
    ConflictingAllignment,
    BadIndentation,
    TrailingCharacters,
    InvalidColor,
    InvalidName,
    ExpectedRoman,
    UnexpectedKeyword,
    ExpectedAKeyword,
    ExpectedKeyword(Keyword),
    BadArtLength { width: usize, got: usize },
    BadArtHeight { height: usize, got: usize },
    UnexpectedEnd,
    ArtMissingArgs,
    TranspOnChar,
    ColorOnSpace,
}

impl ErrorType for ParseError {
    fn gist(&self) -> &'static str {
        match self {
            Self::ConflictingAllignment => "conflicting allignment",
            Self::TrailingCharacters => "trailing characters",
            Self::BadIndentation => "indentation not divisible by four",
            Self::InvalidColor => "invalid color",
            Self::ColorOnSpace => "can only be used with non-spaces",
            Self::TranspOnChar => "unexpected character",
            Self::ArtMissingArgs => "art expected more arguments",
            Self::UnexpectedEnd => "unexpected end",
            Self::ExpectedKeyword(_) => "expected a keyword",
            Self::InvalidName => "invalid name",
            Self::ExpectedRoman => "expected a roman numeral",
            Self::UnexpectedKeyword => "unexpected keyword",
            Self::BadArtLength { .. } => "line length not divisible by 2*width",
            Self::BadArtHeight { .. } => "number of lines not divisible by height",
            Self::ExpectedAKeyword => "expected a keyword"
        }
    }

    fn phase(&self) -> &'static str {
        "PARSE"
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedKeyword => write!(f, "encountered an unexpected keyword"),
            Self::TrailingCharacters => write!(f, "expected an end to the expression"),
            Self::ColorOnSpace => write!(f, "colors can not be used on spaces. instead use . or |"),
            Self::TranspOnChar => write!(f, "colors . and | can only be used with spaces to mark transparency"),
            Self::UnexpectedEnd => write!(f, "unexpected end to expression"),
            Self::ExpectedKeyword(k) => write!(f, "expected the keyword '{}'", k),
            Self::InvalidName => write!(f, "invalid keyword or variable name"),
            Self::BadArtLength { width, got } => write!(
                f,
                "expected line length to be divisible hy {}, but it has {got} chars",
                width * 2,
            ),
            Self::BadArtHeight { height, got } => write!(
                f,
                "expected number of lines to be divisible hy {height}, but it has {got} lines",
            ),
            _ => write!(f, "todo")
        }
    }
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

pub fn parse_art(
    width: usize,
    height: usize,
    text: Vec<Vec<Marked<char>>>,
    mark: Mark, // mark of the art keyword
) -> Result<Vec<Vec<Cells>>> {
    let number_of_lines = text.len();
    if !number_of_lines.is_multiple_of(height) {
        return Err(Error {
            error_type: Box::new(ParseError::BadArtHeight {
                height,
                got: number_of_lines,
            }),
            mark,
            note: None,
        });
    }
    for line in text.iter() {
        let length = line.len();
        if length % (width * 2) != 0 {
            return Err(Error {
                error_type: Box::new(ParseError::BadArtLength { width, got: length }),
                mark: line[length - 1].mark.clone(),
                note: None,
            });
        }
    }
    let mut output: Vec<Vec<Cells>> = Vec::new();
    let mut current_starting_line = 0;
    let mut current_starting_char = 0;
    loop {
        let mut current_map = Vec::new();
        for y in 0..height {
            let mut temp = Vec::new();
            for x in 0..width {
                let line = y + current_starting_line;
                let art_char = text[line][x + current_starting_char].clone();
                let color_char = text[line][x + current_starting_char + width].clone();
                temp.push(((x as u32, (height - y - 1) as u32), (art_char, color_char)));
            }
            current_map.push(temp);
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

pub fn words(s: &str) -> Vec<(usize, &str, usize)> {
    let mut output = Vec::new();
    let mut character_index = 0;
    let mut length = 0;
    for (index, character) in s.chars().enumerate() {
        match character {
            ' ' => {
                if length != 0 {
                    output.push((character_index, &s[character_index..character_index+length], length));
                    length = 0;
                }
            }
            _ => {
                if length == 0 {
                    character_index = index;
                }
                length += 1;
            }
        }
    }
    if length != 0 {
        output.push((character_index, &s[character_index..character_index+length], length));
    }
    output
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    ForAll,
    Include,
    Lambda,
    Match,
    With,
    Bind,
    Either,
    Define,
    The,
    As,
    Case,
    Type,
    Contains,
    Undefined,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
    NewLine(u8),
    Word(String),
}

#[derive(Clone, Debug, Default)]
pub struct Tokens {
    pub tokens: LinkedList<Marked<Token>>,
    end: Mark,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::ForAll     => write!(f, "forall"),
            Keyword::Include    => write!(f, "include"),
            Keyword::Match      => write!(f, "match"),
            Keyword::With       => write!(f, "with"),
            Keyword::Lambda     => write!(f, "lambda"),
            Keyword::Bind       => write!(f, "bind"),
            Keyword::Either     => write!(f, "either"),
            Keyword::Define     => write!(f, "define"),
            Keyword::The        => write!(f, "the"),
            Keyword::As         => write!(f, "as"),
            Keyword::Case       => write!(f, "case"),
            Keyword::Type       => write!(f, "type"),
            Keyword::Contains   => write!(f, "contains"),
            Keyword::Undefined  => write!(f, "undefined"),
        }
    }
}

impl Tokens {
    pub fn peek(&self) -> Result<&Marked<Token>> {
        match self.tokens.front() {
            None => {
                let mut mark = self.end.clone();
                mark.length = 1;
                Err(Error {
                    error_type: Box::new(ParseError::UnexpectedEnd), 
                    mark,
                    note: None,
                })
            }
            Some(token) => Ok(token)
        }
    }

    pub fn next(&mut self) -> Result<Marked<Token>> {
        self.peek()?;
        Ok(self.tokens.pop_front().unwrap())
    }

    pub fn remove_leading_newlines(&mut self) {
        while matches!(self.tokens.front().map(|x| &x.value), Some(Token::NewLine(_))) {
            self.tokens.pop_front();
        }
    }

    pub fn next_non_newline(&mut self) -> Result<Marked<Token>> {
        self.remove_leading_newlines();
        self.next()
    }

    pub fn next_word(&mut self) -> Result<Marked<String>> {
        self.remove_leading_newlines();
        match self.peek()?.value {
            Token::Word(_) => {
                let Ok(Marked::<Token> { value: Token::Word(word), mark }) = self.next() else { 
                    unreachable!() 
                };
                Ok(Marked::<String> {
                    value: word,
                    mark,
                })
            }
            _ => {
                Err(make_error(ParseError::UnexpectedKeyword, self.peek()?.mark.clone()))
            }
        }
    }

    pub fn next_keyword(&mut self) -> Result<Marked<Keyword>> {
        self.remove_leading_newlines();
        match self.peek()?.value {
            Token::Keyword(_) => {
                let Ok(Marked::<Token> { value: Token::Keyword(value), mark }) = self.next() else { 
                    unreachable!() 
                };
                Ok(Marked::<Keyword> {
                    value,
                    mark,
                })
            }
            _ => {
                Err(make_error(ParseError::ExpectedAKeyword, self.peek().unwrap().mark.clone()))
            }
        }
    }

    pub fn new(tokens: LinkedList<Marked<Token>>) -> Self {
        assert!(!tokens.is_empty());
        let mut last_mark: Mark = tokens.back().unwrap().mark.clone();
        last_mark.character += last_mark.length;
        Tokens {
            tokens,
            end: last_mark,
        }
    }

    pub fn get_with_indentation(self, indentation: u8) -> Vec<Self> {
        let mut output: Vec<Self> = Vec::new();
        let mut current = self.tokens;

        loop {
            let mut breakoff = None;
            for (index, token) in current.iter().enumerate() {
                if matches!(token.value, Token::NewLine(x) if x == indentation) {
                    breakoff = Some(index);
                    break
                }
            }
            match breakoff {
                None => {
                    if !current.is_empty() {
                        output.push(Self::new(current))
                    }
                    break
                }
                Some(breakoff) => {
                    let mut leftover = current.split_off(breakoff);
                    leftover.pop_front();
                    if !current.is_empty() {
                        output.push(Self::new(current))
                    }
                    current = leftover;
                }
            }
        }
        output
    }

    pub fn expect_keyword(&mut self, keyword: Keyword) -> Result<Mark> {
        self.remove_leading_newlines();
        if let Marked::<Token> { value: Token::Keyword(k), mark } = self.peek()?.clone() && k == keyword {
            let _ = self.next();
            Ok(mark)
        } else {
            Err(make_error(ParseError::ExpectedKeyword(keyword), self.peek().unwrap().mark.clone()))
        } 
    }

    pub fn add_context(&mut self, block_name: &Arc<String>) {
        self.tokens.iter_mut().for_each(|x| x.mark.block = Some(Arc::clone(block_name)));
    }

    pub fn expect_end(&mut self) -> Result<()> {
        match self.peek().map(|x| x.clone().destructure()) {
            Ok((_, mark)) => Err(make_error(ParseError::TrailingCharacters, mark)),
            Err(_) => Ok(())
        }
    }
}

pub fn tokenize(
    input: Vec<(usize, &str)>,
    file: u32,
    ignore_special_chars: bool
) -> Result<Tokens> {
    let keywords: HashMap<&str, Keyword> = HashMap::from([
        ( "include",        Keyword::Include   ),
        ( "forall",         Keyword::ForAll    ),
        ( "type",           Keyword::Type      ),
        ( "contains",       Keyword::Contains  ),
        ( "define",         Keyword::Define    ),
        ( "the",            Keyword::The       ),
        ( "as",             Keyword::As        ),
        ( "lambda",         Keyword::Lambda    ),
        ( "match",          Keyword::Match     ),
        ( "bind",           Keyword::Bind      ),
        ( "either",         Keyword::Either    ),
        ( "with",           Keyword::With      ),
        ( "case",           Keyword::Case      ),
        ( "undefined",      Keyword::Undefined ),
    ]);
    let mut output = LinkedList::new();
    let mut block = input.into_iter().peekable();
    let mut last_index: usize = 0;
    while let Some((line_number, line)) = block.next() {
        let indentation = indentation_length(line);
        if !indentation.is_multiple_of(INDENTATION) {
            return Err(make_error(ParseError::BadIndentation, Mark {
                file,
                line:       line_number,
                block:      None,
                character:  0,
                length:     indentation as usize
            }))
        }
        output.push_back(Marked::<Token> {
            mark: Mark {
                file,
                line:       line_number,
                block:      None,
                character:  last_index,
                length:     1,
            },
            value: Token::NewLine(indentation / 4),
        });
        last_index = 0;
        let mut words = words(line).into_iter();
        'words: while let Some((character, word, length)) = words.next() {
            last_index = character + length;
            let mark: Mark = Mark {
                file,
                line: line_number,
                block: None,
                character,
                length,
            };
            match word {
                "--" => break 'words,
                "art" => {
                    let Some((character, x, length)) = words.next() else { return Err(make_error(
                        ParseError::ArtMissingArgs,
                        //Mark { character: character, ..mark }
                        mark.one_after_the_highlight(),
                    ))};
                    let Some(x) = parse_roman_numeral(x) else { return Err(make_error(
                        ParseError::ExpectedRoman, 
                        Mark { character, length, ..mark }
                    ))};
                    let Some((character, y, length)) = words.next() else { return Err(make_error(
                        ParseError::ArtMissingArgs,
                        mark.one_after_the_highlight(),
                        //Mark { character: character, ..mark }
                    ))};
                    let Some(y) = parse_roman_numeral(y) else { return Err(make_error(
                        ParseError::ExpectedRoman,
                        Mark { character, length, ..mark }
                    ))};
                    let art_indentation = if indentation == 0 {
                        0
                    } else {
                        indentation + INDENTATION
                    };
                    let mut art_lns: Vec<(usize, Vec<(usize, char)>)> = Vec::new();
                    while let Some((_, x)) = block.peek() && indentation_length(x) >= art_indentation {
                        let (line_num, x) = block.next().unwrap();
                        let mut art_chars = x.chars().enumerate();
                        for _ in 0 .. art_indentation {
                            art_chars.next();
                        }
                        art_lns.push((line_num, art_chars.collect()))
                    }
                    let mut new_output = Vec::new();
                    for (line_index, line) in art_lns.into_iter() {
                        let mut temp = Vec::new();
                        for (char_index, character) in line.into_iter() {
                            let marked_char = Marked::<char> {
                                value: character,
                                mark: Mark {
                                    line: line_index,
                                    character: char_index,
                                    length: 1,
                                    ..mark.clone()
                                },
                            };
                            temp.push(marked_char);
                        }
                        new_output.push(temp);
                    }
                    let aaa = parse_art(x as usize, y as usize, new_output, mark.clone())?;
                    output.extend(build_tokens_from_art(mark, aaa)?);
                }
                other => output.push_back(Marked::<Token> {
                    mark: mark.clone(),
                    value: match keywords.get(&other) {
                        Some(keyword) => Token::Keyword(*keyword),
                        None => {
                            if !ignore_special_chars && !other.chars().all(|x| x.is_lowercase() || x == '_' || x == '/' || x == '.') {
                                return Err(Error {
                                    mark,
                                    error_type: Box::new(ParseError::InvalidName),
                                    note: None,
                                });
                            }
                            Token::Word(other.to_string())
                        }
                    },
                }),
            }
        }
    }
    let mut tokens = LinkedList::new();
    for i in output.into_iter() {
        tokens.push_back(i);
    }
    Ok(Tokens::new(tokens))
}

pub fn build_token(name: &str, mark: &Mark) -> Marked<Token> {
    Marked::<Token> {
        value: Token::Word(name.to_string()),
        mark: mark.clone(),
    }
}

type Cells = Vec<((u32, u32), (Marked<char>, Marked<char>))>;

pub fn build_nat(n: u32, buffer: &mut LinkedList<Marked<Token>>, mark: &Mark) {
    (0 .. n - 1).for_each(|_| buffer.push_back(build_token("succ", mark)));
    buffer.push_back(build_token("one", mark));
}

pub fn build_int(n: i32, buffer: &mut LinkedList<Marked<Token>>, mark: &Mark) {
    match n.cmp(&0) {
        std::cmp::Ordering::Equal => {
            buffer.push_back(build_token("zero", mark));
            return
        }
        std::cmp::Ordering::Less => buffer.push_back(build_token("neg", mark)),
        std::cmp::Ordering::Greater => buffer.push_back(build_token("pos", mark)),
    }
    build_nat(n.unsigned_abs(), buffer, mark);
}

pub fn build_shift_by(x: i32, y: i32, buffer: &mut LinkedList<Marked<Token>>, mark: &Mark) {
    if x == 0 && y == 0 {
        return
    }
    buffer.push_back(build_token("shift_by", mark));
    build_int(x, buffer, mark);
    build_int(y, buffer, mark);
}


pub fn build_tokens_from_art(
    mark: Mark,
    input: Vec<Vec<Cells>>,
) -> Result<LinkedList<Marked<Token>>> {
    let mut x_shift = false;
    let mut y_shift = false;
    let mut video_commands = LinkedList::new();
    video_commands.push_back(Marked::<Token> {
        value: Token::Keyword(Keyword::The),
        mark: mark.clone(),
    });
    video_commands.push_back(build_token("list", &mark));
    video_commands.push_back(build_token("frame", &mark));
    let mut output = LinkedList::new();
    for (index, i) in input.into_iter().enumerate() {
        let mut frame_buffer = LinkedList::new();
        let mut frame_commands = LinkedList::new();
        output.push_back(build_token("cons", &mark));
        frame_buffer.push_back(build_token("frame", &mark));
        frame_buffer.push_back(build_token("nil", &mark));
        for line in i.into_iter().rev() {
            frame_buffer.push_back(build_token("cons", &mark));
            frame_buffer.push_back(build_token("horizontal", &mark));
            frame_buffer.push_back(build_token("nil", &mark));
            for ((x, y), (c1, c2)) in line.into_iter() {
                frame_buffer.push_back(build_token("cons", &mark));
                let c1_char = c1.value;
                let c2_char = c2.value.to_ascii_lowercase();
                if matches!((c1_char, c2_char), (_, '.') | (_, '|')) {
                    match c1_char {
                        ' ' => (),
                        'Z' | 'Y' | 'X' => {
                            video_commands.push_back(build_token("entirely", &mark));
                            build_shift_by(
                                if matches!(c1_char, 'Y' | 'Z') {
                                    if x_shift {
                                        return Err(make_error(ParseError::ConflictingAllignment, c1.mark))
                                    }
                                    x_shift = true;
                                    -(x as i32)
                                } else { 
                                    0 
                                }, 
                                if matches!(c1_char, 'X' | 'Z') {
                                    if y_shift {
                                        return Err(make_error(ParseError::ConflictingAllignment, c1.mark))
                                    }
                                    y_shift = true;
                                    -(y as i32)
                                } else {
                                    0
                                },
                                &mut video_commands, &mark
                            );
                        }
                        _ => return Err(make_error(ParseError::TranspOnChar, c2.mark)),
                    }
                }
                if c2_char == '&' {
                    let s = String::from(c1_char.to_ascii_lowercase());
                    frame_buffer.push_back(build_token("empty_grid_cell", &mark));
                    video_commands.push_back(build_token("layer", &mark));
                    if index != 0 {
                        video_commands.push_back(build_token("for", &mark));
                        build_nat(index as u32, &mut video_commands, &mark);
                        video_commands.push_back(build_token("rotate_right", &mark));
                    }
                    video_commands.push_back(build_token("entirely", &mark));
                    build_shift_by(x as i32, y as i32, &mut video_commands, &mark);
                    video_commands.push_back(build_token(&s, &c1.mark));
                    continue;
                }
                if c2_char == '.' {
                    frame_buffer.push_back(build_token("empty_grid_cell", &mark));
                    continue;
                }
                if c2_char == '#' {
                    let s = String::from(c1_char.to_ascii_lowercase());
                    frame_buffer.push_back(build_token("empty_grid_cell", &mark));
                    frame_commands.push_back(build_token("layer_frames", &mark));
                    build_shift_by(x as i32, y as i32, &mut frame_commands, &mark);
                    frame_commands.push_back(build_token(&s, &c1.mark));
                    continue;
                }

                if c2_char == '^' {
                    frame_buffer.push_back(build_token("filter_grid_cell", &mark));
                    let s = String::from(c1_char.to_ascii_lowercase());
                    frame_buffer.push_back(build_token(&s, &c1.mark));
                    continue;
                }

                frame_buffer.push_back(build_token("full_grid_cell", &mark));
                match (c1_char, c2_char) {
                    (_, ' ') => return Err(make_error(ParseError::InvalidColor, c2.mark)),
                    (_, '|') => {
                        frame_buffer.push_back(build_token("cell_space", &c1.mark));
                    }
                    (c1_char, '$') => {
                        let s = String::from(c1_char.to_ascii_lowercase());
                        frame_buffer.push_back(build_token(&s, &c1.mark));
                    }
                    (c1_char, c2_char) => {
                        frame_buffer.push_back(Marked::<Token> {
                            mark: mark.clone(),
                            value: Token::Word("cell".to_string()),
                        });
                        let character = match c1_char {
                            '!' => "exclamation_mark",   'P' => "capital_p",
                            '"' => "quotation_mark",     'Q' => "capital_q",
                            '#' => "number_sign",        'R' => "capital_r",
                            '$' => "dollar_sign",        'S' => "capital_s",
                            '%' => "percent_sign",       'T' => "capital_t",
                            '&' => "ampersand",          'U' => "capital_u",
                            '\'' => "apostrophe",        'V' => "capital_v",
                            '(' => "left_paranthesis",   'W' => "capital_w",
                            ')' => "right_paranthesis",  'X' => "capital_x",
                            '*' => "asterisk",           'Y' => "capital_y",
                            '+' => "plus_sign",          'Z' => "capital_z",
                            ',' => "comma",              '[' => "left_square_bracket",
                            '-' => "hyphen_minus",       '\\' => "reverse_solidus",
                            '.' => "full_stop",          ']' => "right_square_bracket",
                            '/' => "solidus",            '^' => "circumflex_accent",
                            '0' => "digit_zero",         '_' => "low_line",
                            '1' => "digit_one",          '`' => "grave_accent",
                            '2' => "digit_two",          'a' => "small_a",
                            '3' => "digit_three",        'b' => "small_b",
                            '4' => "digit_four",         'c' => "small_c",
                            '5' => "digit_five",         'd' => "small_d",
                            '6' => "digit_six",          'e' => "small_e",
                            '7' => "digit_seven",        'f' => "small_f",
                            '8' => "digit_eight",        'g' => "small_g",
                            '9' => "digit_nine",         'h' => "small_h",
                            ':' => "colon",              'i' => "small_i",
                            ';' => "semicolon",          'j' => "small_j",
                            '<' => "less_than_sign",     'k' => "small_k",
                            '=' => "equals_sign",        'l' => "small_l",
                            '>' => "greater_than_sign",  'm' => "small_m",
                            '?' => "question_mark",      'n' => "small_n",
                            '@' => "commercial_at",      'o' => "small_o",
                            'A' => "capital_a",          'p' => "small_p",
                            'B' => "capital_b",          'q' => "small_q",
                            'C' => "capital_c",          'r' => "small_r",
                            'D' => "capital_d",          's' => "small_s",
                            'E' => "capital_e",          't' => "small_t",
                            'F' => "capital_f",          'u' => "small_u",
                            'G' => "capital_g",          'v' => "small_v",
                            'H' => "capital_h",          'w' => "small_w",
                            'I' => "capital_i",          'x' => "small_x",
                            'J' => "capital_j",          'y' => "small_y",
                            'K' => "capital_k",          'z' => "small_z",
                            'L' => "capital_l",          '{' => "left_curly_brace",
                            'M' => "capital_m",          '|' => "vertical_line",
                            'N' => "capital_n",          '}' => "right_curly_brace",
                            'O' => "capital_o",          '~' => "tilde",        
                            ' ' => {
                                return Err(Error {
                                    error_type: Box::new(ParseError::ColorOnSpace),
                                    mark: c2.mark,
                                    note: None,
                                });
                            }
                            _ => panic!("bad char"),
                        };
                        frame_buffer.push_back(build_token(character, &c1.mark));
                        let color = match c2_char {
                            '0' => Ok("black"),
                            '1' => Ok("red"),
                            '2' => Ok("green"),
                            '3' => Ok("yellow"),
                            '4' => Ok("blue"),
                            '5' => Ok("magenta"),
                            '6' => Ok("cyan"),
                            '7' => Ok("white"),
                            // '8' => Ok("orange"),
                            // '9' => Ok("purple"),
                            x => Err(x),
                        };
                        match color {
                            Ok(x) => frame_buffer.push_back(build_token(x, &mark)),
                            Err(x) => {
                                let s = String::from(x);
                                frame_buffer.push_back(build_token(&s, &c2.mark));
                            }
                        };
                    }
                }
            }
            frame_buffer.push_back(build_token("nil", &mark)); // finish row
        }
        frame_buffer.push_back(build_token("nil", &mark)); // finish column
        output.append(&mut frame_commands);
        output.append(&mut frame_buffer);
    }
    output.push_back(build_token("nil", &mark));
    if !video_commands.is_empty() {
        video_commands.append(&mut output);
        output = video_commands;
    }
    Ok(output)
}

pub fn indentation_length(input: &str) -> u8 {
    let mut counter = 0;
    let mut chars = input.chars();
    while let Some(' ') = chars.next() {
        counter += 1
    }
    counter
}

