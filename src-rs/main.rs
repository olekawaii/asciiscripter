use std::{collections::HashMap, env, fmt, fs, rc::Rc};
// save unchanging in map, only leave variables in expression!

mod parse;
mod runtime;
mod r#type;

use crate::{
    parse::{
        parse_type,
        TokenStream,
        Signiture,
        build_tree, 
        SyntaxTree, 
        build_syntax_tree, 
        extract_signiture, 
        parse_data, 
        tokenize,
        tokenize_file,
        build_type,
    },
    runtime::{Expression, ExpressionCache, Id},
    r#type::Type
};

fn main() {
    let file = String::from(
"data nat contains 
  one
  succ nat

data bool contains
  true
  false

define eq_nat of_type fn nat fn nat bool as
  lambda x lambda y match x
    one to match y
      one to true
      succ _ to false
    succ a to match y
      one to false
      succ b to eq_nat a b

define infinity of_type nat as succ infinity

define main of_type bool as eq_nat one infinity
");
    let (type_blocks, values): (Vec<(Signiture, TokenStream)>,_) = tokenize_file(file)
        .unwrap()
        .into_iter()
        .map(|mut x| {
            let signiture = extract_signiture(&mut x)?; 
            //dbg!(&signiture);
            Some((signiture, x))
        }) 
        .collect::<Option<(Vec<(Signiture, TokenStream)>)>>()
        .unwrap()
        .into_iter()
        .partition(|(x, _)| matches!(x, Signiture::Type(_)));

    let mut number_of_types = 0;
    let mut types: HashMap<String, u32> = HashMap::new();
    let mut data_blocks: Vec<(u32, TokenStream)> = Vec::new();
    
    for (signiture, tokens) in type_blocks.into_iter() {
        let name = match signiture {
            Signiture::Type(name) => name,
            _ => unreachable!()
        };
        types.insert(name, number_of_types);
        data_blocks.push((number_of_types, tokens));
        number_of_types += 1;
    }

    let mut number_of_values = 0;
    let mut global_vars_dummy: HashMap<String, (usize, Type, bool)> = HashMap::new();
    let mut global_vars: Vec<Expression> = Vec::new();

    for (tp, tokens) in data_blocks.into_iter() {
        for (num, (name, tp)) 
        in parse_data(tokens, &types, tp).unwrap().into_iter().enumerate() {
            global_vars_dummy.insert(name, (number_of_values, tp, true));
            number_of_values += 1;
            global_vars.push(Expression::Tree {
                root: Id::DataConstructor(num as u32),
                arguments: Vec::new()
            })
        }
    }

    let mut vals: Vec<(Type, TokenStream)> = Vec::new();
    for (signiture, tokens) in values.into_iter() {
        let (name, tp) = match signiture {
            Signiture::Value(name, tp) => (name, {
                let mut actual_type = tp
                    .into_iter();
                parse_type(&mut actual_type, &types).unwrap()
            }),
            _ => unreachable!()
        };
        global_vars_dummy.insert(name, (number_of_values, tp.clone(), false));
        vals.push((tp, tokens));
        number_of_values += 1;
    }

    for (tp, tokens) in vals.into_iter() {
        global_vars.push(build_tree(
            tp,
            build_syntax_tree(tokens).unwrap(),
            HashMap::new(),
            0,
            &global_vars_dummy
        ).unwrap())
    }

    let (i, _, _) = global_vars_dummy.get("main").unwrap();

    let mut main = global_vars[*i].clone();

    //dbg!(&main);

    let global_vars = ExpressionCache {
        expressions: global_vars,
    };

    main.simplify(&global_vars);
    main.evaluate_strictly(&global_vars);
    dbg!(&main);

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
}

//#[derive(Debug)]
//pub enum Signiture {
//    Value(String, Vec<String>),
//    Type(String),
//}

fn main2() {
    let expression: Vec<String> = 
"
match leaf
  leaf      to  lambda x node leaf x
  node a b  to  node b
"
        .lines()
        .map(str::to_string)
        .collect();
    let syntaxtree = build_syntax_tree(tokenize(expression).unwrap()).expect("syntax error");
    let data: Vec<String> = "data tree contains\n  leaf\n  node tree tree"
        .lines()
        .map(str::to_string)
        .collect();
    let mut tokens = tokenize(data).unwrap();

    let mut types: HashMap<String, u32> = HashMap::new();
    let mut number_of_types = 0;

    let data_signiture = extract_signiture(&mut tokens).unwrap();
    match data_signiture {
        Signiture::Type(word) => {
            types.insert(word, number_of_types);
            number_of_types += 1
        },
        _ => todo!()
    };

    let mut global_vars = Vec::new();
    let mut values: HashMap<String, (usize, Type, bool)> = HashMap::new();
    let mut number_of_values = 0;
    for (num, (name, tp)) in parse_data(tokens, &types, 0).unwrap().into_iter().enumerate() {
        values.insert(name, (number_of_values, tp, true));
        number_of_values += 1;
        global_vars.push(Expression::Tree {
            root: Id::DataConstructor(num as u32),
            arguments: Vec::new()
        })
    }
    let mut tree = build_tree(
       Type::Function(Box::new(Type::Type(0)), Box::new(Type::Type(0))),
       syntaxtree,
       HashMap::new(),
       0,
       &values,
    ).unwrap();
    let exp = ExpressionCache{
        expressions: Vec::new()
    };
    //tree.simplify(&exp);
    //dbg!(tree);
}

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
    let h = ExpressionCache {
        expressions: Vec::new(),
    };
    initial.simplify(&h);
    dbg!(initial);
}
