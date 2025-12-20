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

use std::sync::Arc;

pub trait ErrorType: std::fmt::Display + std::fmt::Debug {
    fn gist(&self) -> &'static str;
    fn phase(&self) -> &'static str;
}

#[derive(Debug)]
pub struct Error {
    pub error_type: Box<dyn ErrorType>,
    pub mark: Mark,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\n\x1b[91m{} error\x1b[0m {}\n\x1b[90m\n{}\x1b[0m\n",
            self.error_type.phase(),
            show_mark(self.mark.clone(), self.error_type.gist()),
            self.error_type
        )
    }
}

#[derive(Debug, Clone)]
pub enum Index {
    Expression(usize),
    Art(usize),
    EndOfWord(usize),
    EndOfLine,
}

#[derive(Debug, Clone)]
pub struct Mark {
    pub file_name: Arc<String>,
    pub file: Arc<Vec<String>>,
    pub line: usize,
    pub block: Option<Arc<String>>,
    pub word_index: Index,
}

pub fn show_mark(mark: Mark, message: &'static str) -> String {
    //dbg!(&mark);
    let mut number = (mark.line + 1).to_string();
    number.push(' ');
    let indentation = number.chars().count();
    let line: &str = &mark.file[mark.line];
    let mut length_of_word: usize = 0;
    let mut length_to_word: usize = 0;
    let mut output_string = String::new();
    let mut line_before: String = String::new();
    match &mark.word_index {
        Index::Expression(size) | Index::EndOfWord(size) => {
            let words = (*line).split_whitespace();
            let mut reached = false;
            for (i, word) in words.enumerate() {
                let word_len = word.chars().count();
                if i == *size {
                    length_of_word = word_len;
                    reached = true;
                } else if !reached {
                    length_to_word += word_len + 1;
                }
                if word == "--" {
                    output_string.push_str("\x1b[90m");
                }
                output_string.push_str(word);
                output_string.push(' ');
            }
            if matches!(mark.word_index, Index::EndOfWord(_)) {
                length_to_word += length_of_word;
                length_of_word = 1;
            }
        }
        Index::Art(index) => {
            line_before = format!("\x1b[90m{}", mark.file[mark.line - 1]);
            length_to_word = *index;
            length_of_word = 1;
            output_string = line.into();
        }
        Index::EndOfLine => {
            length_to_word = 4;
            length_of_word = 1;
            output_string = line.into();
        }
    }
    let mut underline = String::new();
    underline.push_str(&" ".repeat(length_to_word));
    underline.push_str(&"~".repeat(length_of_word));
    underline.push_str("  ");
    underline.push_str(message);
    let empty_space = " ".repeat(indentation);
    format!(
        "in {}{}\n\x1b[91m{}| {}\n\x1b[91m{} | \x1b[0m{}\n\x1b[91m{}| {}\x1b[0m",
        mark.file_name,
        //mark.line + 1,
        match &mark.block {
            None => String::from(""),
            Some(name) => format!(", in the definition of {}", (*name).clone()),
        },
        &empty_space,
        line_before,
        mark.line + 1,
        output_string,
        &empty_space,
        underline,
    )
}
