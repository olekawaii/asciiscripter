use crate::error::*;
use std::collections::LinkedList;

const INDENTATION: u8 = 4;

#[derive(Debug)]
pub enum ParseError {
    // ConflictingAllignment,
    BadIndentation,
    TrailingCharacters,
    InvalidColor,
    // InvalidName,
    ExpectedRoman,
    UnexpectedKeyword,
    // ExpectedAKeyword,
    ExpectedKeyword(Keyword),
    BadArtLength { width: usize, got: usize },
    BadArtHeight { height: usize, got: usize },
    UnexpectedEnd,
    UnexpectedEndLine,
    ArtMissingArgs,
    TranspOnChar,
    ColorOnSpace,
    CantHaveIndents,
}

impl ErrorType for ParseError {
    fn gist(&self) -> &'static str {
        match self {
            // Self::ConflictingAllignment => "conflicting allignment",
            Self::TrailingCharacters => "trailing characters",
            Self::BadIndentation => "indentation not divisible by four",
            Self::InvalidColor => "invalid color",
            Self::ColorOnSpace => "can only be used with non-spaces",
            Self::TranspOnChar => "unexpected character",
            Self::ArtMissingArgs => "art expected more arguments",
            Self::UnexpectedEnd => "unexpected end",
            Self::UnexpectedEndLine => "unexpected end of line",
            Self::ExpectedKeyword(_) => "expected a keyword",
            // Self::InvalidName => "invalid name",
            Self::ExpectedRoman => "expected a roman numeral",
            Self::UnexpectedKeyword => "unexpected keyword",
            Self::BadArtLength { .. } => "line length not divisible by 2*width",
            Self::BadArtHeight { .. } => "number of lines not divisible by height",
            // Self::ExpectedAKeyword => "expected a keyword",
            Self::CantHaveIndents => "beginning here",
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
            Self::UnexpectedEnd => write!(f, "unexpected end to block"),
            Self::UnexpectedEndLine => write!(f, "unexpected end to line. try puttings args on their own lines"),
            Self::ExpectedKeyword(k) => write!(f, "expected the keyword '{}'", k),
            // Self::InvalidName => write!(f, "invalid keyword or variable name"),
            Self::BadArtLength { width, got } => write!(
                f,
                "expected line length to be divisible hy {}, but it has {got} chars",
                width * 2,
            ),
            Self::BadArtHeight { height, got } => write!(
                f,
                "expected number of lines to be divisible hy {height}, but it has {got} lines",
            ),
            Self::CantHaveIndents => write!(f,
"this block has indented branches. They are not
allowed in this context. If you want them, move
this block into its own branch"),
            _ => write!(f, "todo"),
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
    Let,
    The,
    Be,
    Case,
    Type,
    Contains,
    Undefined,
}

// Invariants:
//     * Lines contain at least one word
//     * Nested blocks are only indented by a single level

#[derive(Clone, Debug)]
pub struct Block {
    pub line_tokens:               Vec<(OwnedToken, Mark)>,
    pub indented_blocks_beneath:   Vec<Block>,
    pub line_end_mark:             Mark,
}

#[allow(unused)]
pub fn debug_print_block(bt: BlockTraversal, indentation: u32) {
    eprint!("{}", " ".repeat(4 * indentation as usize));
    for (i, _) in bt.block.line_tokens.iter().skip(bt.word) {
        match i {
            OwnedToken::Keyword(k) => eprint!("{k} "),
            OwnedToken::Word(w)    => eprint!("{w} "),
        }
    }
    eprintln!();
    bt.block.indented_blocks_beneath.iter().for_each(
        |x| debug_print_block(BlockTraversal::new(x), indentation + 1)
    );
}

#[derive(Clone, Copy, Debug)]
pub struct BlockTraversal<'a> {
    pub block:                     &'a Block,
    pub word:                      usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OwnedToken {
    Keyword(Keyword),
    Word(String)
}

impl<'a> BlockTraversal<'a> {
    pub fn new(block: &'a Block) -> Self {
        BlockTraversal {
            block,
            word: 0,
        }
    }

    pub fn get_indented_blocks(self) -> Vec<Self> {
        self
            .block
            .indented_blocks_beneath
            .iter()
            .map(BlockTraversal::new)
            .collect()
    }

    pub fn reached_end_of_line(self) -> bool {
        self.word == self.block.line_tokens.len()
    }

    pub fn reached_end_of_block(self) -> bool {
        self.reached_end_of_line() && self.block.indented_blocks_beneath.is_empty()
    }

    pub fn expect_end_option(bt: Option<Self>) -> Result<()> {
        match bt {
            None => Ok(()),
            Some(x) => x.expect_end(),
        }
    }

    pub fn expect_no_indents(bt: Option<Self>, mark: &'a Mark) -> Result<Self> {
        match bt {
            Some(x) => Ok(x),
            None    => Err(make_error(ParseError::CantHaveIndents, mark.clone()))
        }
    }

    pub fn expect_end(self) -> Result<()> {
        let token_mark: &Mark = match self.next() {
            Ok(NextOutput::Token(_, mark, _))       => mark,
            Ok(NextOutput::IndentedBlocks(v))       => v[0].next_token_in_line().unwrap().1,
            Err(_)                                  => return Ok(())
        };
        Err(make_error(
            ParseError::TrailingCharacters,
            token_mark.clone()
        ))
    }

    pub fn next_token_in_line(mut self) -> Result<(&'a OwnedToken, &'a Mark, Self)> {
        if self.reached_end_of_line() {
            return Err(make_error(
                ParseError::UnexpectedEndLine,
                self.block.line_end_mark.clone()
            ))
        }
        let (token, mark) = &self.block.line_tokens[self.word];
        self.word += 1;
        Ok((token, mark, self))
    }

    pub fn next_token_in_line_fallthrough(self) -> Result<(&'a OwnedToken, &'a Mark, Self)> {
        match self.next()? {
            NextOutput::Token(t, mark, bt) => Ok((t, mark, bt)),
            NextOutput::IndentedBlocks(v) => {
                assert!(v.len() == 1);
                v[0].next_token_in_line()
            }
        }
    }

    pub fn next(self) -> Result<NextOutput<'a>> {
        if self.reached_end_of_block() {
            return Err(make_error(
                ParseError::UnexpectedEnd,
                self.block.line_end_mark.clone()
            ))
        }
        match self.next_token_in_line() {
            Ok((token, mark, bt)) => Ok(NextOutput::Token(token, mark, bt)),
            Err(_)                => Ok(NextOutput::IndentedBlocks(self.get_indented_blocks()))
        }
    }

    pub fn expect_keyword(self, keyword: Keyword) -> Result<(&'a Mark, Self)> {
        let (token, mark, bt) = self.next_token_in_line_fallthrough()?;
        if *token != OwnedToken::Keyword(keyword) {
            return Err(make_error(
                ParseError::ExpectedKeyword(keyword),
                mark.clone()
            ))
        }
        Ok((mark, bt))
    }

    pub fn expect_word(self) -> Result<(&'a str, &'a Mark, Self)> {
        let (token, mark, bt) = self.next_token_in_line_fallthrough()?;
        if let OwnedToken::Word(w) = token { Ok((w, mark, bt)) }
        else { Err(make_error(ParseError::UnexpectedKeyword, mark.clone())) }
    }
}

pub enum NextOutput<'a> {
    Token(&'a OwnedToken, &'a Mark, BlockTraversal<'a>),
    IndentedBlocks(Vec<BlockTraversal<'a>>)
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
            Keyword::Let        => write!(f, "let"),
            Keyword::The        => write!(f, "the"),
            Keyword::Be         => write!(f, "be"),
            Keyword::Case       => write!(f, "case"),
            Keyword::Type       => write!(f, "type"),
            Keyword::Contains   => write!(f, "contains"),
            Keyword::Undefined  => write!(f, "undefined"),
        }
    }
}

pub fn tokenize_block(input: Vec<(usize, &str)>, file: u32, indentation_level: u32) -> Result<Block> {
    let mut bodies: Vec<Block> = Vec::new();
    let mut lines = input.into_iter();
    let (line_number, first_line) = lines.next().unwrap();
    assert!(indentation_length(first_line) as u32 == indentation_level * INDENTATION as u32);
    let first_line_indentation = indentation_length(first_line);
    if first_line_indentation as u32 != indentation_level * INDENTATION as u32 {
        let mark = Mark {
            file,
            line:       line_number,
            block:      None,
            character:  0,
            length:     first_line_indentation as usize
        };
        return Err(make_error(ParseError::BadIndentation, mark))
    }
    let (root_line, end_mark, art_dimensions) = tokenize_line(first_line, line_number as u32, file)?;
    if let Some((x, y, art_mark)) = art_dimensions {
        let art_indentation = match indentation_level {
            0 => 0,
            _ => (indentation_level + 1) * INDENTATION as u32,
        };
        let art_lns: Vec<(usize, Vec<(usize, char)>)> = lines
            .map(|(n, s)| (n, s.chars().skip(art_indentation as usize).enumerate().collect()))
            .collect();
        let mut new_output = Vec::new();
        for (line_index, line) in art_lns {
            let mut temp = Vec::new();
            for (char_index, character) in line.into_iter() {
                let marked_char = Marked::<char> {
                    value: character,
                    mark: Mark {
                        line: line_index,
                        character: char_index,
                        length: 1,
                        ..art_mark.clone()
                    },
                };
                temp.push(marked_char);
            }
            new_output.push(temp);
        }
        let aaa = parse_art(x as usize, y as usize, new_output, art_mark.clone())?;
        let tokens: Vec<(OwnedToken, Mark)> = build_tokens_from_art(art_mark, aaa)?
            .into_iter()
            .map(|Marked::<OwnedToken> { mark, value} | (value, mark))
            .collect();
        return Ok(Block {
            line_end_mark: end_mark.clone(),
            line_tokens: root_line,
            indented_blocks_beneath: vec![Block {
                line_tokens: tokens,
                line_end_mark: end_mark,
                indented_blocks_beneath: Vec::new(),
            }],
        })
    }
    let mut line_buffer = Vec::new();
    for (line_number, line) in lines {
        let indentation = indentation_length(line) / INDENTATION;
        if indentation as u32 == indentation_level + 1 && !line_buffer.is_empty() {
            let block = tokenize_block(line_buffer, file, indentation_level + 1)?;
            bodies.push(block);
            line_buffer = Vec::new();
        }
        line_buffer.push((line_number, line));
    }
    if !line_buffer.is_empty() {
        let block = tokenize_block(line_buffer, file, indentation_level + 1)?;
        bodies.push(block);
    }
    Ok(Block {
        line_tokens:               root_line,
        indented_blocks_beneath:   bodies,
        line_end_mark:             end_mark
    })
}

// the Option<(u32, u32, Mark)> are the width and
// height of `art` if it exists in the line

fn tokenize_line(line: &str, line_number: u32, file: u32)
    -> Result<(Vec<(OwnedToken, Mark)>, Mark, Option<(u32, u32, Mark)>)>
{
    let mut ret: Vec<(OwnedToken, Mark)> = Vec::new();
    let mut art_ret = None;
    let mut words = words(line).into_iter();
    while let Some((character, word, length)) = words.next() {
        let token_mark = Mark {
            file,
            line: line_number as usize,
            block: None,
            character,
            length,
        };
        let token = match word {
            "--"         =>  break, // comment
            "include"    =>  OwnedToken::Keyword(Keyword::Include   ),
            "forall"     =>  OwnedToken::Keyword(Keyword::ForAll    ),
            "type"       =>  OwnedToken::Keyword(Keyword::Type      ),
            "contains"   =>  OwnedToken::Keyword(Keyword::Contains  ),
            "let"        =>  OwnedToken::Keyword(Keyword::Let       ),
            "the"        =>  OwnedToken::Keyword(Keyword::The       ),
            "be"         =>  OwnedToken::Keyword(Keyword::Be        ),
            "lambda"     =>  OwnedToken::Keyword(Keyword::Lambda    ),
            "match"      =>  OwnedToken::Keyword(Keyword::Match     ),
            "bind"       =>  OwnedToken::Keyword(Keyword::Bind      ),
            "either"     =>  OwnedToken::Keyword(Keyword::Either    ),
            "with"       =>  OwnedToken::Keyword(Keyword::With      ),
            "case"       =>  OwnedToken::Keyword(Keyword::Case      ),
            "undefined"  =>  OwnedToken::Keyword(Keyword::Undefined ),
            "art"        => {
                let Some((character, x, length)) = words.next() else { return Err(make_error(
                    ParseError::ArtMissingArgs,
                    token_mark,
                ))};
                let Some(x) = parse_roman_numeral(x) else { return Err(make_error(
                    ParseError::ExpectedRoman,
                    Mark { character, length, ..token_mark }
                ))};
                let Some((character, y, length)) = words.next() else { return Err(make_error(
                    ParseError::ArtMissingArgs,
                    token_mark,
                    //Mark { character: character, ..mark }
                ))};
                let Some(y) = parse_roman_numeral(y) else { return Err(make_error(
                    ParseError::ExpectedRoman,
                    Mark { character, length, ..token_mark }
                ))};
                art_ret = Some((x, y, token_mark));
                assert!(words.next().is_none());
                break
            }
            s =>  OwnedToken::Word(String::from(s)),
        };
        ret.push((token, token_mark));
    }
    let end_line_mark = ret[ret.len() - 1].1.clone().one_after_the_highlight();
    Ok((ret, end_line_mark, art_ret))
}

pub fn build_token(name: &str, mark: &Mark) -> Marked<OwnedToken> {
    Marked::<OwnedToken> {
        value: OwnedToken::Word(name.to_string()),
        mark: mark.clone(),
    }
}

type Cells = Vec<((u32, u32), (Marked<char>, Marked<char>))>;

pub fn build_nat(n: u32, buffer: &mut LinkedList<Marked<OwnedToken>>, mark: &Mark) {
    (0 .. n - 1).for_each(|_| buffer.push_back(build_token("succ", mark)));
    buffer.push_back(build_token("one", mark));
}

pub fn build_int(n: i32, buffer: &mut LinkedList<Marked<OwnedToken>>, mark: &Mark) {
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

pub fn build_shift_by(x: i32, y: i32, buffer: &mut LinkedList<Marked<OwnedToken>>, mark: &Mark) {
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
) -> Result<LinkedList<Marked<OwnedToken>>> {
    let mut video_commands = LinkedList::new();
    video_commands.push_back(Marked::<OwnedToken> {
        value: OwnedToken::Keyword(Keyword::The),
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
                if c2_char == '?' {
                    frame_buffer.push_back(build_token("filter_grid_cell", &mark));
                    let s = String::from(c1_char.to_ascii_lowercase());
                    frame_buffer.push_back(build_token(&s, &c1.mark));
                    continue;
                }
                if c2_char == '<' {
                    frame_buffer.push_back(build_token("filter_grid_cell", &mark));
                    frame_buffer.push_back(build_token("recolor_character", &mark));
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
                        frame_buffer.push_back(Marked::<OwnedToken> {
                            mark: mark.clone(),
                            value: OwnedToken::Word("cell".to_string()),
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

