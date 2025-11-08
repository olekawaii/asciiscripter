/* thorn - a pure lazy functional programming language
 * Copyright (C) 2025  Oleksiy Buell <olekawaii@proton.me>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 */

use std::{collections::HashMap, env, fmt, fs, fs::read_to_string};
use std::sync::{Mutex, Arc};
// save unchanging in map, only leave variables in expression!
use std::thread::spawn;

mod parse;
mod runtime;
mod r#type;
mod error;

use crate::{
    parse::{
        Marked, Signiture, SyntaxTree, Token, TokenStream, build_syntax_tree,
        build_tree, build_type, extract_signiture, parse_data, parse_type, tokenize,
        tokenize_file, parse_roman_numeral, parse_art, get_tokens
    },
    runtime::{Expression, COUNTER, ExpressionCache, Id},
    r#type::Type,
    error::{Error, Mark},
};

fn main() -> std::io::Result<()> {
    // main_test();
    let mut args = env::args();
    let _executable = args.next().unwrap();
    let file_name = args.next().expect("missing file name");
    // let file: String = read_to_string(&file_name)?;
    match parse_file(file_name) {
        Err(x) => eprintln!("{x}"),
        Ok((vars, vars_dummy)) => {
            let (main_index, _, _) = vars_dummy.get("main").expect("no main");
            let mut main = vars[*main_index].clone();
            let global_vars = Arc::new(ExpressionCache { expressions: vars });
            main = main.evaluate_strictly(Arc::clone(&global_vars));
            //println!("{:?}",&main);
            //dbg!(&main);
            let mut map = HashMap::new();
            for (name, (index, _, _)) in vars_dummy {
                map.insert(index as u32, name);
            }
            println!("{}", convert_to_file(&main, &map));
        }
    }
    unsafe {dbg!(COUNTER);};
    Ok(())
}

fn parse_file(
    file_name: String,
) -> Result<(Vec<Expression>, HashMap<String, (usize, Type, bool)>), Error> {
    let mut temp_vec = Vec::new();
    let blocks = get_tokens(file_name, &mut temp_vec)?;
    let (type_blocks, values): (Vec<(Signiture, TokenStream)>, _) =
        blocks
            .into_iter()
            .map(|mut x| {
                let signiture = extract_signiture(&mut x)?;
                //dbg!(&signiture);
                Ok((signiture, x))
            })
            .collect::<Result<(Vec<(Signiture, TokenStream)>), Error>>()?
            .into_iter()
            .partition(|(x, _)| matches!(x, Signiture::Type(_)));

    let mut number_of_types = 0;
    let mut types: HashMap<String, u32> = HashMap::new();
    let mut data_blocks: Vec<(u32, TokenStream)> = Vec::new();
    for (signiture, tokens) in type_blocks.into_iter() {
        let name = match signiture {
            Signiture::Type(name) => name,
            _ => unreachable!(),
        };
        types.insert(name, number_of_types);
        data_blocks.push((number_of_types, tokens));
        number_of_types += 1;
    }
    let mut number_of_values = 0;
    let mut global_vars_dummy: HashMap<String, (usize, Type, bool)> = HashMap::new();
    let mut global_vars: Vec<Expression> = Vec::new();
    for (tp, tokens) in data_blocks.into_iter() {
        for (num, (name, tp)) in parse_data(tokens, &types, tp)?
            .into_iter()
            .enumerate()
        {
            //if num +1 == 22 {dbg!(&name);}
            match global_vars_dummy.insert(name, (number_of_values, tp, true)) {
                None => {}
                Some(x) => {dbg!(x); panic!("uwuaaaa")}
            }
            number_of_values += 1;
            global_vars.push(Expression::Tree {
                root: Id::DataConstructor(num as u32),
                arguments: Vec::new(),
            });
        }
    }
    let mut vals: Vec<(Type, TokenStream)> = Vec::new();
    for (signiture, tokens) in values.into_iter() {
        let (name, tp) = match signiture {
            Signiture::Value(name, tp) => (name, {
                let mut actual_type = tp;
                parse_type(&mut actual_type, &types).unwrap()
            }),
            _ => unreachable!(),
        };
        global_vars_dummy.insert(name, (number_of_values, tp.clone(), false));
        vals.push((tp, tokens));
        number_of_values += 1;
    }
    for (tp, tokens) in vals.into_iter() {
        global_vars.push(
            build_tree(
                tp,
                build_syntax_tree(tokens, &types)?,
                HashMap::new(),
                0,
                &global_vars_dummy,
            )?
        )
    }
    Ok((global_vars, global_vars_dummy))
}

fn convert_to_file(expression: &Expression, names: &HashMap<u32, String>) -> String {
    match expression {
        Expression::Tree {root, arguments} => {
            let mut output = match root {
                Id::DataConstructor(x) => names.get(x).unwrap().clone(), // unwrap should be safe
                _ => unreachable!()
            };
            output.push(' ');
            for i in arguments.iter() {
                let a = convert_to_file(i, names);
                output.push_str(&a)
            }
            output
        },
        _ => panic!("uwu")
    }
}
