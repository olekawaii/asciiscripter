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

const UNDERLINE_CHAR: &str = "^";

use std::{
    sync::{Arc, Mutex},
    fs::read_to_string
};

// debug info used by compile-time/run-time errors

pub struct DebugInfo {
    pub files:       Vec<String>,   // paths to all .th files
}

pub static DEBUG_INFO: Mutex<DebugInfo> = Mutex::new(DebugInfo { 
    files:       Vec::new(),
});

pub fn get_file_name(index: u32) -> String {
    DEBUG_INFO.lock().unwrap().files[index as usize].clone()
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, Hash)]
pub struct Mark {
    pub file: u32,
    pub block: Option<Arc<String>>,
    pub line: usize,
    pub character: usize,
    pub length: usize,
}

impl Mark {
    pub fn one_after_the_highlight(&self) -> Self {
        Self {
            length: 1,
            character: self.character + self.length,
            ..self.clone()
        }
    }
}

impl Default for Mark {
    fn default() -> Self {
        Self {
            file: 67,
            block: None,
            line: 0,
            character: 0,
            length: 0,
        }
    }
}

pub fn show_mark(mark: Mark, message: &'static str) -> String {
    let mut number = (mark.line + 1).to_string();
    number.push(' ');
    let indentation = number.chars().count();
    let mut file_name = get_file_name(mark.file);
    let file_contents = read_to_string(&file_name).unwrap();
    let mut file_name_chars = file_name.chars();
    if file_name_chars.next().unwrap() == '.' {
        file_name_chars.next(); 
        file_name = file_name_chars.collect();
    }
    let mut lines = file_contents.lines().enumerate();
    let line_before = if mark.line == 0 { 
        ""
    } else {
        lines.find(|(n, _)| *n == mark.line - 1).unwrap().1
    };
    let current_line = lines.next().unwrap().1;

    let mut underline = String::new();
    underline.push_str(&" ".repeat(mark.character));
    underline.push_str(&UNDERLINE_CHAR.repeat(mark.length));
    underline.push_str("  ");
    underline.push_str(message);
    let empty_space = " ".repeat(indentation);
    format!(
        "\x1b[90min \x1b[0m{}\x1b[90m:\x1b[0m{}\x1b[90m:\x1b[0m{}\x1b[90m{}\n\
\x1b[91m{}| \x1b[90m{}\n\x1b[91m{} | \x1b[0m{}\n\x1b[91m{}  {}\x1b[0m",
        file_name,
        mark.line + 1,
        mark.character + 1,
        //mark.line + 1,
        match &mark.block {
            None => String::from(""),
            Some(name) => format!(", in the definition of \x1b[0m{}\x1b[90m", (*name).clone()),
        },
        &empty_space,
        line_before,
        mark.line + 1,
        current_line,
        &empty_space,
        underline,
    )
}

#[derive(Debug)]
pub struct Error {
    pub error_type:  Box<dyn ErrorType>,
    pub mark:        Mark,
    pub note:        Option<String>
}

pub fn make_error(error: impl ErrorType + 'static, mark: Mark) -> Error {
    Error {
        mark,
        error_type: Box::new(error),
        note: None,
    }
}

// pub fn add_note<T>(val: &mut Result<T>, note: &str) {
//     if let Err(err) = val {
//         err.note = Some(String::from(note));
//     }
// }

pub trait ErrorType: std::fmt::Display + std::fmt::Debug {
    fn gist(&self) -> &'static str;
    fn phase(&self) -> &'static str;
}

impl PartialEq for Error {
    fn eq(&self, other: &Error) -> bool {
        (&self.mark.file, self.mark.line, self.mark.character).eq(
        &(&other.mark.file, other.mark.line, other.mark.character))
    }
}

impl Eq for Error {
}

impl PartialOrd for Error {
    fn partial_cmp(&self, other: &Error) -> Option<std::cmp::Ordering> { 
        Some(self.cmp(other)) 
    }
}

impl Ord for Error {
    fn cmp(&self, other: &Error) -> std::cmp::Ordering {
        (&self.mark.file, self.mark.line, self.mark.character).cmp(
        &(&other.mark.file, other.mark.line, other.mark.character))
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\x1b[91m{} ERROR \x1b[0m\x1b[0m {}\n\x1b[90m{}{}\x1b[0m\n",
            self.error_type.phase(),
            show_mark(self.mark.clone(), self.error_type.gist()),
            self.error_type,
            if let Some(x) = &self.note {
                format!("\x1b[90m\n\n\x1b[97mnote:\x1b[90m {x}")
            } else {
                "".into()
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct Marked<T> {
    pub value: T,
    pub mark: Mark,
}
