use std::{collections::HashMap, env, fmt, fs, fs::read_to_string, rc::Rc};
use std::sync::Mutex;
// save unchanging in map, only leave variables in expression!
use std::thread::spawn;

mod parse;
mod runtime;
mod r#type;

use crate::{
    parse::{
        Error, Index, Mark, Marked, Signiture, SyntaxTree, Token, TokenStream, build_syntax_tree,
        build_tree, build_type, extract_signiture, parse_data, parse_type, show_mark, tokenize,
        tokenize_file, parse_roman_numeral, parse_art, get_tokens
    },
    runtime::{Expression, COUNTER, ExpressionCache, Id},
    r#type::Type,
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
            let (main_index, _, _) = vars_dummy.get("main").unwrap();
            let mut main = vars[*main_index].clone();
            let global_vars = ExpressionCache { expressions: vars };
            main.evaluate_strictly(&global_vars);
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

fn main_test() -> std::io::Result<()> {
    let file_name = String::from("main.ascr");
    let file: Vec<String> = read_to_string(&file_name)?
        .lines()
        .map(str::to_string)
        .collect();
    let mark: Mark = Mark {
        file_name: Rc::new(file_name),
        file: Rc::new(file),
        line: 0,
        block: None,
        word_index: Index::Art(0),
    };
    println!("{}", show_mark(mark));
    Ok(())
}



fn parse_file(
    file_name: String,
) -> Result<(Vec<Expression>, HashMap<String, (usize, Type, bool)>), Error> {
    //let mut files: Vec<(String, String)> = Vec::new();
    //for i in file_names.into_iter() {
    //    let file = read_to_string(&i).unwrap();
    //    files.push((i, file))
    //}
    //let mut blocks = Vec::new();
    //for (file_name, file) in files.into_iter() {
    //    let mut vec_blocks = tokenize_file(file, &Rc::new(file_name))?;
    //    blocks.append(&mut vec_blocks);
    //}
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
                Some(x) => {dbg!(x);}
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
                let mut actual_type = tp.into_iter();
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
                build_syntax_tree(tokens)?,
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

//dbg!(&main);

//let mut tree = build_tree(
//   Type::Function(Box::new(Type::Type(0)), Box::new(Type::Type(0))),
//   syntaxtree,
//   HashMap::new(),
//   0,
//   &values,
//).unwrap();

//
//    let signiture = extract_signiture(&mut block).unwrap();
//    match signiture {
//        Signiture::Type(name) => {
//            types.insert(name, number_of_types);
//
//            number_of_types += 1;
//
//        }
//        Signiture::Value(name, tp) => {
//    }
//}
//let mut values: HashMap<String, (usize, Type, bool)> = HashMap::new();
//Ok(())
//}

//#[derive(Debug)]
//pub enum Signiture {
//    Value(String, Vec<String>),
//    Type(String),
//}

//fn main2() {
//    let file_name =
//    let expression: Vec<String> =
//"
//match leaf
//  leaf      to  lambda x node leaf x
//  node a b  to  node b
//"
//        .lines()
//        .map(str::to_string)
//        .collect();
//    let syntaxtree = build_syntax_tree(tokenize(expression).unwrap()).expect("syntax error");
//    let data: Vec<String> = "data tree contains\n  leaf\n  node tree tree"
//        .lines()
//        .map(str::to_string)
//        .collect();
//    let mut tokens = tokenize(data, ).unwrap();
//
//    let mut types: HashMap<String, u32> = HashMap::new();
//    let mut number_of_types = 0;
//
//    let data_signiture = extract_signiture(&mut tokens).unwrap();
//    match data_signiture {
//        Signiture::Type(word) => {
//            types.insert(word, number_of_types);
//            number_of_types += 1
//        },
//        _ => todo!()
//    };
//
//    let mut global_vars = Vec::new();
//    let mut values: HashMap<String, (usize, Type, bool)> = HashMap::new();
//    let mut number_of_values = 0;
//    for (num, (name, tp)) in parse_data(tokens, &types, 0).unwrap().into_iter().enumerate() {
//        values.insert(name, (number_of_values, tp, true));
//        number_of_values += 1;
//        global_vars.push(Expression::Tree {
//            root: Id::DataConstructor(num as u32),
//            arguments: Vec::new()
//        })
//    }
//    let mut tree = build_tree(
//       Type::Function(Box::new(Type::Type(0)), Box::new(Type::Type(0))),
//       syntaxtree,
//       HashMap::new(),
//       0,
//       &values,
//    ).unwrap();
//    let exp = ExpressionCache{
//        expressions: Vec::new()
//    };
//    //tree.simplify(&exp);
//    //dbg!(tree);
//}
//

//fn test() {
//    let mut initial = Expression::Tree {
//        root: Id::LambdaArg(5),
//        arguments: Vec::from([Expression::Tree {
//            root: Id::DataConstructor(7),
//            arguments: Vec::new(),
//        }]),
//    };
//    initial.substitute(
//        5,
//        Rc::new(Mutex::new(Expression::Tree {
//            root: Id::DataConstructor(7),
//            arguments: Vec::new(),
//        })),
//    );
//    let h = ExpressionCache {
//        expressions: Vec::new(),
//    };
//    initial = initial.simplify_owned(&h);
//    dbg!(initial);
//}
