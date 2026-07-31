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


use std::env;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{Stdin, Write};
use std::process::Command;
use std::sync::Arc;
use std::cell::RefCell;

use rustyline;

use crate::parse::{
    Id, 
    GlobalVarData, 
    tokenize_file,
    BlockKind,
    parse_expression,
    CompilationError,
    parse_type,
    NameAndGenerics,
    extract_name_and_generics,
    Type,
    GlobalTypeData
};
use crate::runtime::Expression;
use crate::error::{
    Error, 
    DEBUG_INFO, 
    Mark, 
    get_file_name,
    make_error,
};
use crate::tokens::Keyword;

const MAIN_EXPR: &'static str = "/tmp/thorn-input-expr";

fn main() -> std::io::Result<()> {
    std::fs::write(MAIN_EXPR, "main");     // by default evaluate main
    {
        let mut ptr = parse::TYPES.lock().unwrap();
        *ptr = Some(HashMap::new());
    }
    let Arguments { starting_dir, expression_provided, as_repl } = parse_cli_arguments()?;
    std::env::set_current_dir(&starting_dir)?;
    if as_repl {
        repl()?
    }
    if !reach_project_root()? {
        eprintln!("\x1b[91merror:\x1b[0m reached root without finding \
            main.th\n       make sure you're in a project");
        std::process::exit(1);
    }
    match parse::get_everything() {
        Err(x) => {
            eprintln!("{x}");
            std::process::exit(1)
        }
        Ok((mut expressions, mut var_names, mut dummy)) => {
            if !expression_provided {
                let GlobalVarData {var_type, id, ..} = dummy.get("main").unwrap();
                let id = match id {
                    Id::Variable(n) => n,
                    Id::Constructor(n) => n,
                };
                let main_expr = expressions[*id].clone();
                main_expr.print(&mut var_names, var_type);
            }
            else {
                let x;
                {
                    let mut ptr = DEBUG_INFO.lock().unwrap();
                    ptr.files.push(String::from(MAIN_EXPR));
                    x = ptr.files.len() - 1;
                }
                if let Err(e) = evaluate_block(
                    x as u32,
                    &mut dummy,
                    &mut var_names,
                    &mut expressions,
                ) {
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
        Err(_)   => std::process::exit(0),
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
        _ => return false
            
    }
    let location = format!("+call cursor({},{})", mark.line +1, mark.character +1);
    let file_name = get_file_name(mark.file);
    let mut more = Command::new("vim")
        .arg(&location)
        .arg(&file_name)
        .spawn()
        .unwrap()
        .wait();
    true
}

fn repl() -> std::io::Result<()> {
    let temp_file_path = "/tmp/thorn-repl.th";
    let mut temp_file = File::create(temp_file_path)?;
    let mut dummy: HashMap<String, GlobalVarData> = HashMap::new();
    let mut names: Vec<String> = Vec::new();
    let mut expressions: Vec<Expression> = Vec::new();
    println!("thorn, :? for help");
    if let Ok(true) = reach_project_root() {
        println!("including main");
        loop {
            match parse::get_everything() {
                Err(ref err@Error { ref mark, .. }) => {
                    eprintln!("{err}");
                    if !prompt_to_edit_function(&mark) {
                        println!("giving up");
                        break
                    }
                }
                Ok((e, v, d)) => {
                    expressions = e;
                    dummy = d;
                    names = v;
                    break
                }
            }
        }
    } else {
        println!("not in a project");
    }
    let temp_file_index = {
        let mut ptr = DEBUG_INFO.lock().unwrap();
        ptr.files.push(String::from(temp_file_path));
        ptr.files.len() - 1
    };
    loop {
        let mut text: String = String::new();
        let mut indent: u8 = 0;
        let mut locked_to_one = false;
        let mut line: String;
        let mut words: Vec<&str>;
        let mut first_word: String;
        line = read_line(">>> ", indent, "");
        if line == "" {
            continue
        }
        words = line.split_whitespace().collect();
        first_word = words[0].to_string();
        locked_to_one = words.contains(&"contains");
        if (
            words[0] == "define" || 
            words[0] == "type"   || 
            words[0] == "the"    || 
            words[0] == "forall"
        ) {
            text.push_str(&line);
            loop {
                words = line.split_whitespace().collect();
                let include_case: &str = if 
                    words.contains(&"match") && 
                    words[words.len() - 1] != "match"
                {
                    "case "
                } else {
                    ""
                };
                indent = (tokens::indentation_length(&line) / 4) as u8;
                indent += 1;
                if 
                    words.contains(&"forall") && 
                    !words.contains(&"type") && 
                    !words.contains(&"define") 
                {
                    indent = 0
                }
                if 
                     words.contains(&"case") && 
                    !words.contains(&"match") &&  
                     words[words.len() - 1] != "the" &&
                     words.len() != 1
                {
                    indent -= 1;
                }
                if locked_to_one {
                    indent = 1;
                }
                line = read_line(">>> ", indent, include_case);
                text.push('\n');
                text.push_str(&line);
                if line == "" { 
                    break 
                }
            }
            std::fs::write(temp_file_path, &text);
            loop {
                match first_word.as_str() {
                    "define" | "forall" | "type" => {
                        match compile_block(
                            temp_file_index as u32,
                            &mut dummy,
                            &mut names,
                            &mut expressions,
                        ) {
                            Err(ref err@Error { ref mark, .. }) => {
                                eprintln!("\x1b[1A{err}");
                                if !prompt_to_edit_function(&mark) {
                                    println!("giving up");
                                    break
                                }
                            }
                            Ok(()) => break
                        }
                    }
                    "the" => {
                        match evaluate_block(
                            temp_file_index as u32,
                            &mut dummy,
                            &mut names,
                            &mut expressions,
                        ) {
                            Err(ref err@Error { ref mark, .. }) => {
                                eprintln!("\x1b[1A\x1b[0J{err}");
                                if !prompt_to_edit_function(&mark) {
                                    println!("giving up");
                                    break
                                }
                            }
                            Ok(_) => {
                                break;
                            }
                        }
                    }
                    _ => todo!(),
                }
            }
        } else {
            // println!("{first_line}");
        }
    }
}

fn evaluate_block(
    temp_file_name: u32, 
    dummy: &mut HashMap<String, GlobalVarData>,
    var_names: &mut Vec<String>,
    expressions: &mut Vec<Expression>,
) -> error::Result<()> {
    let temp_local_vars = HashMap::new();
    let file_name = get_file_name(temp_file_name);
    let text = std::fs::read_to_string(file_name).unwrap();
    let mut block = tokenize_file(text, temp_file_name)?.into_iter().next().unwrap();
    block.remove_leading_newlines();
    block.expect_keyword(Keyword::The)?;
    let new_vec = Vec::new();
    let tp: Type = parse_type(&mut block, &new_vec)?;
    let possible: [u32;100] = (0..100).collect::<Vec<u32>>().try_into().unwrap();
    let available_files = HashSet::from(possible);
    let new_vec = Vec::new();
    let expr = parse_expression(
        expressions,
        &available_files,
        tp.clone(), 
        &mut block, 
        &temp_local_vars, 
        0, 
        &dummy, 
        &new_vec,
    )?;
    //print!("\x1b[1A\x1b[0J\x1b[90m\n");
    //std::io::stdout().flush();
    expr.print(var_names, &tp);
    //print!("\x1b[0m\n");
    Ok(())
}


fn compile_block(
    temp_file_name: u32, 
    dummy: &mut HashMap<String, GlobalVarData>,
    var_names: &mut Vec<String>,
    expressions: &mut Vec<Expression>,
) -> error::Result<()> {
    let temp_local_vars = HashMap::new();
    let file_name = get_file_name(temp_file_name);
    let text = std::fs::read_to_string(file_name).unwrap();
    let mut block = tokenize_file(text, temp_file_name)?.into_iter().next().unwrap();
    match extract_name_and_generics(&mut block)? {
        NameAndGenerics { name, mark, generics, kind } => {
            block.add_context(&name.clone().into());
            match kind {
                BlockKind::Variable => {
                    if let Some(x) = dummy.get(&name) {
                        return Err(make_error(
                            CompilationError::MultipleDeclarations(x.mark.file), 
                            mark
                        ))
                    }
                    let index = var_names.len();
                    var_names.push(String::new());
                    expressions.push(Expression::Thunk {
                        value: Arc::new(RefCell::new(Expression::default())),
                        mark: Some(Arc::new(mark.clone()))
                    });
                    match block.expect_keyword(Keyword::The) {
                        Ok(_) => {
                            let var_type = parse_type(&mut block, &generics)?;
                            dummy.insert(name.clone(), GlobalVarData {
                                var_type,
                                mark,
                                id: Id::Variable(index),
                                generics,
                            });
                        }
                        Err(Error { mark, .. }) => {
                            return Err(make_error(
                                CompilationError::TypeAnnotationNeeded, 
                                mark
                            ))
                        }
                    }
                    let Some(GlobalVarData {mark, var_type, id, generics}) = dummy.get(&name) 
                    else { unreachable!() };
                    let possible: [u32;100] = (0..100).collect::<Vec<u32>>().try_into().unwrap();
                    let available_files = HashSet::from(possible);
                    let expression: Expression = match 
                        parse_expression(
                            expressions,
                            &available_files,
                            var_type.clone(), 
                            &mut block,
                            &temp_local_vars,
                            0,
                            &dummy,
                            &generics,
                        ).and_then(|x| block
                            .expect_end()
                            .and_then(|_| Ok(x)) 
                        )
                    {
                        Ok(expr) => expr,
                        Err(e) => {
                            expressions.pop();
                            var_names.pop();
                            dummy.remove(&name);
                            return Err(e);
                        }
                    };
                    {
                        let Expression::Thunk { value: ref x, .. } = expressions[index] else { 
                            unreachable!() 
                        };
                        let Ok(mut inner) = (*x).try_borrow_mut() else { unreachable!() };
                        *inner = expression;
                    }
                }
                BlockKind::Type => {
                    let data: GlobalTypeData = {
                        let mut ptr = parse::TYPES.lock().unwrap();
                        let mut ptr = ptr.as_mut().unwrap();
                        if ptr.contains_key(&name) {
                            return Err(make_error(
                                CompilationError::MultipleDeclarations(mark.file), 
                                mark
                            ))
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
                    if let GlobalTypeData {mark: _, id: index, generics, kind: _ } = &data {
                        match parse::parse_data(
                            block,
                            *index as u32, 
                            &generics
                        ) {
                            Ok(branches) => {
                                for (name, tp, mark) in branches.into_iter() {
                                    dummy.insert(name.clone(), GlobalVarData {
                                        mark: mark.clone(), 
                                        id: Id::Constructor(expressions.len()), 
                                        generics: generics.clone(),
                                        var_type: tp,
                                    }); 
                                    expressions.push(Expression::DataConstructor(expressions.len() as u32));
                                    var_names.push(name);
                                }
                            }
                            Err(e) => {
                                let mut ptr = parse::TYPES.lock().unwrap();
                                let mut ptr = ptr.as_mut().unwrap();
                                ptr.remove(&name);
                                return Err(e);
                            }
                        }
                    } 
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
            return Ok(false)
        }
        if std::path::Path::new("main.th").exists() {
            break
        }
        std::env::set_current_dir("..")?
    }
    Ok(true)
}

struct Arguments {
    starting_dir:          String,
    expression_provided:   bool,
    as_repl:               bool,
}

impl Default for Arguments {
    fn default() -> Arguments {
        Arguments {
            starting_dir:         String::from("."),
            expression_provided:  false,
            as_repl:              false
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
                    std::fs::write(MAIN_EXPR, &buffer);
                }
                else {
                    std::fs::write(MAIN_EXPR, &arg);
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
a main.th file. The search starts at the given directory
(working directory if none is provided) and walks up the 
filetree until it finds it.

Include statements (include video) recursively look for a 
video.th file in any of the project's sudirectories.

The output is the fully evaluated main function. For ASCII
art animations, it is typically piped into thorn-to-sh or
thorn-to-gif. To play a shell script animation:

    $ thorn . | thorn-to-sh | sh
");
                std::process::exit(1);

            }
            _ => output.starting_dir = x,
        }
    }
    Ok(output)
}
