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

mod error;
mod parse;
mod runtime;
mod tokens;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::env;
use std::process::Command;
use std::rc::Rc;

use crate::error::{DEBUG_INFO, Error, Mark, get_file_name, make_error};
use crate::parse::{
    BlockKind, CompilationError, GlobalTypeData, GlobalVarData, Id, LocalVars, NameAndGenerics,
    parse_expression, parse_the,
};
use crate::runtime::Expression;
use crate::tokens::{BlockTraversal, tokenize_block};

const MAIN_EXPR: &str = "/tmp/thorn-input-expr";

fn main() -> std::io::Result<()> {
    let _ = std::fs::write(MAIN_EXPR, "main"); // by default evaluate main
    {
        let mut ptr = parse::TYPES.lock().unwrap();
        *ptr = Some(HashMap::new());
    }
    let Arguments {
        starting_dir,
        expression_provided,
        as_repl,
    } = parse_cli_arguments()?;
    std::env::set_current_dir(&starting_dir)?;
    if as_repl {
        repl()?;
        return Ok(());
    }
    if !reach_project_root()? {
        eprintln!(
            "\x1b[91merror:\x1b[0m reached root without finding \
            main.th\n       make sure you're in a project"
        );
        std::process::exit(1);
    }
    match parse::get_everything() {
        Err(x) => {
            eprintln!("{x}");
            std::process::exit(1)
        }
        Ok(mut loaded) => {
            if !expression_provided {
                let GlobalVarData { var_type, id, .. } = loaded.info_table.get("main").unwrap();
                let id = match id {
                    Id::Variable(n) => n,
                    Id::Constructor(n) => n,
                };
                let main_expr = loaded.expressions[*id].clone();
                main_expr.print(&loaded.var_names, var_type, &loaded.marks, &loaded.patterns);
            } else {
                let x;
                {
                    let mut ptr = DEBUG_INFO.lock().unwrap();
                    ptr.files.push(String::from(MAIN_EXPR));
                    x = ptr.files.len() - 1;
                }
                if let Err(e) = new_evaluate_block(x as u16, &mut loaded) {
                    eprintln!("{e}");
                    std::process::exit(1);
                }
            }
        }
    }
    Ok(())
}

fn read_line(prompt: &str, indent: u8, extra_input: &str) -> String {
    let mut indent_string = String::with_capacity(indent as usize * 4);
    (0..indent).for_each(|_| indent_string.push_str("    "));
    indent_string.push_str(extra_input);
    let mut rl = rustyline::DefaultEditor::new().unwrap();
    let full_prompt: String = format!("\x1b[96m{}\x1b[0m", prompt);
    let readline = rl.readline_with_initial(&full_prompt, (&indent_string, ""));
    match readline {
        Err(_) => std::process::exit(0),
        Ok(line) => {
            let s = line.trim_end().replace('\t', "    ").to_string();
            // if s == "" && prompt == "... " {
            //     println!("\x1b[1A\r\x1b[96m>>>\r\x1b[0m");
            // }
            s
        }
    }
}

fn prompt_to_edit_function(mark: &Mark) -> bool {
    let input = read_line("open it in your editor? [Y/n] ", 0, "");
    println!();
    match input.as_str() {
        "y" | "Y" | "yes" | "" => (),
        _ => return false,
    }
    let location = format!("+call cursor({},{})", mark.line + 1, mark.column + 1);
    let file_name = get_file_name(mark.file);
    let _more = Command::new("vim")
        .arg(&location)
        .arg(&file_name)
        .spawn()
        .unwrap()
        .wait();
    true
}

fn repl() -> std::io::Result<()> {
    let mut loaded = parse::LoadedExpressions {
        info_table: HashMap::new(),
        var_names: Vec::new(),
        expressions: Vec::new(),
        patterns: vec![runtime::Pattern::Dropped],
        marks: Vec::new(),
    };
    println!("thorn, :? for help");
    if let Ok(true) = reach_project_root() {
        println!("including main");
        loop {
            match parse::get_everything() {
                Err(ref err @ Error { ref mark, .. }) => {
                    eprintln!("{err}");
                    if !prompt_to_edit_function(mark) {
                        println!("giving up");
                        break;
                    }
                }
                Ok(l) => {
                    loaded = l;
                    break;
                }
            }
        }
    } else {
        println!("not in a project");
    }
    let mut repl_temp_index = 0;
    loop {
        repl_temp_index += 1;
        let temp_file_path = format!("/tmp/thorn-repl-{}.th", repl_temp_index);
        let temp_file_index = {
            let mut ptr = DEBUG_INFO.lock().unwrap();
            ptr.files.push(temp_file_path.clone());
            ptr.files.len() - 1
        };

        let mut text: String = String::new();
        let mut indent: u8 = 0;
        let mut line: String;
        let mut words: Vec<&str>;
        line = read_line(">>> ", indent, "");
        if line.is_empty() {
            continue;
        }
        words = line.split_whitespace().collect();
        let first_word = words[0].to_string();
        let locked_to_one = words.contains(&"contains");
        if words[0] == "let" || words[0] == "type" || words[0] == "the" || words[0] == "forall" {
            text.push_str(&line);
            loop {
                words = line.split_whitespace().collect();
                let include_case: &str =
                    if words.contains(&"match") && words[words.len() - 1] != "match" {
                        "case "
                    } else {
                        ""
                    };
                indent = tokens::indentation_length(&line) / 4;
                indent += 1;
                if words.contains(&"forall") && !words.contains(&"type") && !words.contains(&"let")
                {
                    indent = 0
                }
                if words.contains(&"case")
                    && !words.contains(&"match")
                    && words[words.len() - 1] != "the"
                    && words.len() != 1
                {
                    indent -= 1;
                }
                if locked_to_one {
                    indent = 1;
                }
                line = read_line(">>> ", indent, include_case);
                text.push('\n');
                text.push_str(&line);
                if line.is_empty() {
                    break;
                }
            }
            let _ = std::fs::write(&temp_file_path, &text);
            loop {
                match first_word.as_str() {
                    "let" | "forall" | "type" => {
                        match compile_block(temp_file_index as u16, &mut loaded) {
                            Err(ref err @ Error { ref mark, .. }) => {
                                eprintln!("\x1b[1A{err}");
                                if !prompt_to_edit_function(mark) {
                                    println!("giving up");
                                    break;
                                }
                            }
                            Ok(()) => break,
                        }
                    }
                    "the" => match new_evaluate_block(temp_file_index as u16, &mut loaded) {
                        Err(ref err @ Error { ref mark, .. }) => {
                            eprintln!("\x1b[1A\x1b[0J{err}");
                            if !prompt_to_edit_function(mark) {
                                println!("giving up");
                                break;
                            }
                        }
                        Ok(_) => {
                            break;
                        }
                    },
                    _ => todo!(),
                }
            }
        } else {
            // println!("{first_line}");
        }
    }
}

fn new_evaluate_block(
    temp_file_name: u16,
    loaded: &mut parse::LoadedExpressions,
) -> error::Result<()> {
    let new_vec = Vec::new();
    let mut temp_local_vars = LocalVars { vars: Vec::new() };
    let file_name = get_file_name(temp_file_name);
    let text = std::fs::read_to_string(file_name).unwrap();
    let lines: Vec<(usize, &str)> = text.lines().enumerate().collect();
    let block = tokenize_block(lines, temp_file_name, 0)?;
    let bt = BlockTraversal::new(&block);
    let (tp, bt) = parse_the(bt, &new_vec)?;
    let possible: [u16; 100] = (0..100).collect::<Vec<u16>>().try_into().unwrap();
    let available_files = HashSet::from(possible);
    let (expr, leftover) = parse_expression(
        loaded,
        &available_files,
        &tp,
        bt,
        &mut temp_local_vars,
        &new_vec,
    )?;
    BlockTraversal::expect_end_option(leftover)?;
    //print!("\x1b[1A\x1b[0J\x1b[90m\n");
    //std::io::stdout().flush();
    expr.print(&loaded.var_names, &tp, &loaded.marks, &loaded.patterns);
    //print!("\x1b[0m\n");
    Ok(())
}

fn compile_block(temp_file_name: u16, loaded: &mut parse::LoadedExpressions) -> error::Result<()> {
    let parse::LoadedExpressions {
        info_table: dummy,
        var_names,
        expressions,
        marks,
        patterns,
    } = loaded;
    let mut temp_local_vars = LocalVars { vars: Vec::new() };
    let file_name = get_file_name(temp_file_name);
    let text = std::fs::read_to_string(file_name).unwrap();
    let block = tokens::tokenize_file(&text, temp_file_name)?
        .into_iter()
        .next()
        .unwrap();
    let bt = BlockTraversal::new(&block);
    let (
        NameAndGenerics {
            name,
            mark,
            generics,
            kind,
        },
        bt,
    ) = parse::extract_name_and_generics(bt)?;
    // block.add_context(&name.clone().into());
    match kind {
        BlockKind::Variable => {
            if let Some(x) = loaded.info_table.get(&name) {
                return Err(make_error(
                    CompilationError::MultipleDeclarations(marks[x.mark as usize].file),
                    mark,
                ));
            }
            let index = loaded.var_names.len();
            loaded.var_names.push(String::new());
            marks.push(mark);
            let mark_id = marks.len() as u16 - 1;
            loaded.expressions.push(Expression::Thunk {
                value: Rc::new(RefCell::new(Expression::default())),
                mark: Some(mark_id),
            });
            let (var_type, bt) = parse_the(bt, &generics)?;
            loaded.info_table.insert(
                name.clone(),
                GlobalVarData {
                    var_type,
                    mark: mark_id,
                    id: Id::Variable(index),
                    generics,
                },
            );
            let GlobalVarData {
                var_type, generics, ..
            } = loaded.info_table.get(&name).unwrap().clone();
            let possible: [u16; 100] = (0..100).collect::<Vec<u16>>().try_into().unwrap();
            let available_files = HashSet::from(possible);
            let (expression, _bt) = match parse_expression(
                loaded,
                &available_files,
                &var_type,
                bt,
                &mut temp_local_vars,
                &generics,
            )
            .and_then(|(e, bt)| BlockTraversal::expect_end_option(bt).and(Ok((e, bt))))
            {
                Ok(x) => x,
                Err(e) => {
                    loaded.expressions.pop();
                    loaded.var_names.pop();
                    loaded.info_table.remove(&name);
                    return Err(e);
                }
            };
            {
                let Expression::Thunk { value: ref x, .. } = loaded.expressions[index] else {
                    unreachable!()
                };
                let Ok(mut inner) = (*x).try_borrow_mut() else {
                    unreachable!()
                };
                *inner = expression;
            }
        }
        BlockKind::Type => {
            let data: GlobalTypeData = {
                let mut ptr = parse::TYPES.lock().unwrap();
                let ptr = ptr.as_mut().unwrap();
                if ptr.contains_key(&name) {
                    return Err(make_error(
                        CompilationError::MultipleDeclarations(mark.file),
                        mark,
                    ));
                }
                let data = GlobalTypeData {
                    mark: mark.clone(),
                    id: ptr.len(),
                    kind: parse::kind_from_generics(generics.len() as u32),
                    generics,
                };
                ptr.insert(name.clone(), data.clone());
                data
            };
            let GlobalTypeData {
                mark: _,
                id: index,
                generics,
                kind: _,
            } = &data;
            marks.push(mark.clone());
            match parse::parse_data(bt, *index as u32, generics) {
                Ok(branches) => {
                    for (name, tp, mark) in branches.into_iter() {
                        loaded.info_table.insert(
                            name.clone(),
                            GlobalVarData {
                                mark: marks.len() as u16 - 1,
                                id: Id::Constructor(loaded.expressions.len()),
                                generics: generics.clone(),
                                var_type: tp,
                            },
                        );
                        let expr = Expression::DataConstructor(loaded.expressions.len() as u32);
                        loaded.expressions.push(expr);
                        loaded.var_names.push(name);
                    }
                }
                Err(e) => {
                    let mut ptr = parse::TYPES.lock().unwrap();
                    let ptr = ptr.as_mut().unwrap();
                    ptr.remove(&name);
                    return Err(e);
                }
            }
        }
    }
    Ok(())
}

// The root of the project is the directory with
// the main.th file. If main.th is not in the
// working directory, we ascend up the parent
// directories until we find it or reach root.

pub fn reach_project_root() -> std::io::Result<bool> {
    loop {
        if std::env::current_dir().unwrap() == std::path::Path::new("/") {
            return Ok(false);
        }
        if std::path::Path::new("main.th").exists() {
            break;
        }
        std::env::set_current_dir("..")?
    }
    Ok(true)
}

struct Arguments {
    starting_dir: String,
    expression_provided: bool,
    as_repl: bool,
}

impl Default for Arguments {
    fn default() -> Arguments {
        Arguments {
            starting_dir: String::from("."),
            expression_provided: false,
            as_repl: false,
        }
    }
}

fn parse_cli_arguments() -> std::io::Result<Arguments> {
    let mut output = Arguments::default();
    let mut args = env::args();
    let _program_name = args.next();
    while let Some(x) = args.next() {
        match x.as_str() {
            "--repl" => output.as_repl = true,
            "--eval" => {
                output.expression_provided = true;
                let arg = args.next().expect("--eval expected a function name");
                if arg.trim() == "-" {
                    let mut buffer = String::new();
                    let stdin = std::io::stdin();
                    for line in stdin.lines() {
                        buffer.push_str(&line.unwrap());
                        buffer.push('\n');
                    }
                    let _ = std::fs::write(MAIN_EXPR, &buffer);
                } else {
                    let _ = std::fs::write(MAIN_EXPR, &arg);
                }
            }
            "--help" | "-h" => {
                eprintln!(
                    "Interpreter of the thorn language

usage:
    thorn [options] [directory]

options:
    --eval EXPR    evaluate EXPR instead of main
    --help         show this help message
    --repl         start the interactive repl

thorn tries to find the project's root directory containing
a main.th file. The seRch starts at the given directory
(working directory if none is provided) and walks up the
filetree until it finds it.

Include statements (include video) recursively look for a
video.th file in any of the project's sudirectories.

The output is the fully evaluated main function. For ASCII
art animations, it is typically piped into thorn-to-sh or
thorn-to-gif. To play a shell script animation:

    $ thorn . | thorn-to-sh | sh
"
                );
                std::process::exit(1);
            }
            _ => output.starting_dir = x,
        }
    }
    Ok(output)
}
