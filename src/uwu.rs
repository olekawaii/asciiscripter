use std::{collections::HashMap, fmt};
use std::sync::{Mutex, Arc};
use std::thread;

// instead of Arc<Expression> use Arc<Mutex<Expression>> and first one to use it modifies it

// build_mutex takes a lambdaarg and makes every branch in a tree a mutex, or pattern in match, 
fn build_mutex(input: Expression) -> Arc<Mutex<Expression>> {
    match input {
        Expression::Tree {mut root, mut arguments} => {
            //arguments = arguments.into_iter().map(build_mutex).map(|x| Expression::Tree {root: Id::Thunk(x), arguments: Vec::new()}).collect();
            //if !matches!(root, Id::Thunk(_)) {
            //    root = Id::Thunk(build_mutex(Expression::Tree {root, arguments: Vec::new()}));
            //}
            Arc::new(Mutex::new(Expression::Tree {root, arguments}))
        }
        Expression::Match {mut pattern, branches} => {
            //pattern = Box::new(Expression::Tree {root: Id::Thunk(build_mutex(*pattern)), arguments: Vec::new()});
            Arc::new(Mutex::new(Expression::Match {pattern, branches}))
        }
        l@Expression::Lambda {..} => Arc::new(Mutex::new(l))
    }
}


#[derive(Debug, Clone)]
pub enum Id {
    Thunk(Arc<Mutex<Expression>>),
    LambdaArg(u32),
    Variable(usize),
    DataConstructor(u32),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Tree {
        root: Id,
        arguments: Vec<Expression>,
    },
    Match {
        pattern: Box<Expression>,
        branches: HashMap<u32, (Vec<Option<u32>>, Expression)>,
    },
    Lambda {
        id: Option<u32>,
        body: Box<Expression>,
    },
}

pub static mut COUNTER: usize = 0;
pub static THREADS_USED: Mutex<usize> = Mutex::new(0);

impl Expression {
    fn is_simplified(&self) -> bool {
        matches!(self, Expression::Lambda {..}) ||
        matches!(self, Expression::Tree {root: Id::DataConstructor(_), arguments} if arguments.len() == 0)
    }
    // TODO make simplify non-recursive so it doesn't blow the stack
    // this means adding an attempt_to_simplify finction and calling it in a loop
    // for match expressions only simplify what's in the pattern, with increasing depth
    // for trees if there are arguments, just add them to the root arguments, otherwise
    // substitute them in the root

    // rewrite expression until it starts with either a lambda or a data constructor
    pub fn simplify_owned(self, definitions: Arc<ExpressionCache>) -> Self {
        unsafe {
            COUNTER += 1;
        }
        if matches!(&self, Expression::Tree {root: Id::DataConstructor(_), ..}) {
            return self
        }
        match self {
            Expression::Tree { root, arguments } => {
                let mut output: Expression = match root {
                    Id::Thunk(exp) => {
                        match Arc::try_unwrap(exp) {
                            Ok(x) => x.into_inner().unwrap().simplify_owned(Arc::clone(&definitions)),
                            Err(x) => {
                                let mut inside = (*x).lock().unwrap();
                                if !matches!(
                                    &*inside, 
                                    Expression::Tree {root: Id::DataConstructor(_), ..} | Expression::Lambda {..}
                                ) {
                                    (*inside) = inside.clone().simplify_owned(Arc::clone(&definitions));
                                }
                                inside.clone()
                            }
                        }
                    }
                    Id::Variable(index) => {
                        let var = definitions.get(index);
                        if !matches!(
                            &var, 
                            Expression::Tree {root: Id::DataConstructor(_), ..} | Expression::Lambda {..}
                        ) {
                            var.simplify_owned(Arc::clone(&definitions))
                        } else {var}
                    }
                    _ => unreachable!(),
                };
                for i in arguments {
                    if let Expression::Tree {arguments, ..} = &mut output {
                        arguments.push(i);
                    } else if let Expression::Lambda { id, mut body } = output {
                        let e = if let Expression::Tree {root: Id::Thunk(ref x), arguments: ref a} = i {
                            if a.len() == 0 {
                                //dbg!("skipped a clone!");
                                Arc::clone(&x)
                            } else {
                                Arc::new(Mutex::new(i))
                            }
                        } else {
                            Arc::new(Mutex::new(i))
                        };
                        if let Some(id) = id {
                            body.substitute(id, e);
                        }
                        output = *body;
                    }

                    //match &mut output {
                    //    Expression::Lambda { id, ref mut body } => {
                    //        body.substitute(id.clone(), Arc::new(Mutex::new(i.clone())));
                    //        output = **body;
                    //    }
                    //    Expression::Tree {
                    //        ref mut arguments,
                    //        root,
                    //    } => {
                    //        arguments.push(i.clone());
                    //        //output = Expression::Tree { root, arguments }
                    //    }
                    //    _ => unreachable!(),
                    //}
                    if !matches!(
                        &output, 
                        Expression::Tree {root: Id::DataConstructor(_), ..}
                    ) {
                        output = output.simplify_owned(Arc::clone(&definitions))
                    }
                }
                //output.simplify(definitions);
                output
            }
            Expression::Match { pattern, mut branches } => {
                //pattern.simplify(definitions);
                //if !matches!(
                //    &*pattern, 
                //    Expression::Tree {root: Id::DataConstructor(_), ..}
                //) {
                //    pattern = Box::new(pattern.simplify_owned(definitions))
                //}                 
                match pattern.simplify_owned(Arc::clone(&definitions)) {
                    Expression::Tree { root, arguments } => {
                        let (vec, mut output) = match root {
                            Id::DataConstructor(root) => match branches.remove(&root) {
                                None => {
                                    dbg!(&root); 
                                    //dbg!(&pattern);
                                    dbg!(&branches);
                                    panic!("unmatched pattern");
                                },
                                Some(x) => x,
                            }
                            _ => todo!(),
                        };
                        for (id, expression) in vec.into_iter().zip(arguments.into_iter()) {
                            if let Some(id) = id {
                                output.substitute(id.clone(), {
                                    if let Expression::Tree {root: Id::Thunk(ref x), arguments: ref a} = expression {
                                        if a.len() == 0 {
                                            //dbg!("skipped a clone!");
                                            Arc::clone(&x)
                                        } else {
                                            Arc::new(Mutex::new(expression))
                                        }
                                    } else {
                                        Arc::new(Mutex::new(expression))
                                    }
                                });
                            }
                        }
                        output.simplify_owned(definitions)
                    }
                    _ => todo!(),
                }
            }
            _ => self,
        }
    }

    // pub fn simplify(&mut self, definitions: &ExpressionCache) {
    //     if !matches!(
    //         self, 
    //         Expression::Tree {root: Id::DataConstructor(_), ..}
    //     ) {
    //         *self = self.clone().simplify_owned(definitions);
    //     }
    // }

    //pub fn simplify(&mut self, definitions: &ExpressionCache) {
    //    unsafe {
    //        counter += 1;
    //    }
    //    match self {
    //        Expression::Tree { root, arguments } => {
    //            let mut output = match root {
    //                Id::DataConstructor(_) => return (),
    //                Id::Thunk(exp) => {
    //                    let mut inside = (*exp).lock().unwrap();
    //                    (*inside).simplify(definitions);
    //                    inside.clone()
    //                }
    //                Id::Variable(index) => {
    //                    let mut var = definitions.get(*index);
    //                    var.simplify(definitions);
    //                    var
    //                }
    //                Id::LambdaArg(_) => unreachable!(),
    //            };
    //            for i in arguments {
    //                if let Expression::Tree {arguments, ..} = &mut output {
    //                    arguments.push(i.clone());
    //                } else if let Expression::Lambda { id, mut body } = output {
    //                     let e = if let Expression::Tree {root: Id::Thunk(ref x), arguments: ref a} = *i {
    //                         if a.len() == 0 {
    //                             //dbg!("skipped a clone!");
    //                             Arc::clone(&x)
    //                         } else {
    //                             Arc::new(Mutex::new(i.clone()))
    //                         }
    //                     } else {
    //                         Arc::new(Mutex::new(i.clone()))
    //                     };
    //                     body.substitute(id.clone(), e);
    //                     output = *body;
    //                }
    //
    //                //match &mut output {
    //                //    Expression::Lambda { id, ref mut body } => {
    //                //        body.substitute(id.clone(), Arc::new(Mutex::new(i.clone())));
    //                //        output = **body;
    //                //    }
    //                //    Expression::Tree {
    //                //        ref mut arguments,
    //                //        root,
    //                //    } => {
    //                //        arguments.push(i.clone());
    //                //        //output = Expression::Tree { root, arguments }
    //                //    }
    //                //    _ => unreachable!(),
    //                //}
    //                output.simplify(definitions);
    //            }
    //            //output.simplify(definitions);
    //            *self = output;
    //        }
    //        Expression::Match { pattern, branches } => {
    //            pattern.simplify(definitions);
    //            match *(*pattern).clone() {
    //                Expression::Tree { root, arguments } => {
    //                    let (vec, mut output) = match root {
    //                        Id::DataConstructor(root) => match branches.remove(&root) {
    //                            None => {
    //                                dbg!(&root); 
    //                                dbg!(&pattern);
    //                                dbg!(&branches);
    //                                panic!("unmatched pattern");
    //                            },
    //                            Some(x) => x,
    //                        }
    //                        _ => todo!(),
    //                    };
    //                    *self = output;
    //                    for (id, expression) in vec.into_iter().zip(arguments.into_iter()) {
    //                        self.substitute(id, {
    //                            if let Expression::Tree {root: Id::Thunk(ref x), arguments: ref a} = expression {
    //                                if a.len() == 0 {
    //                                    //dbg!("skipped a clone!");
    //                                    Arc::clone(&x)
    //                                } else {
    //                                    Arc::new(Mutex::new(expression))
    //                                }
    //                            } else {
    //                                Arc::new(Mutex::new(expression))
    //                            }
    //                        })
    //                    }
    //                    self.simplify(definitions);
    //                }
    //                _ => todo!(),
    //            }
    //        }
    //        _ => (),
    //    }
    //}

    // substitute every instance of an id with an expression
    pub fn substitute(&mut self, key: u32, new_expression: Arc<Mutex<Expression>>) {
        match self {
            Expression::Lambda { body, .. } => body.substitute(key, new_expression),
            Expression::Tree { root, arguments } => {
                for i in arguments.iter_mut() {
                    i.substitute(key, Arc::clone(&new_expression));
                }
                if matches!(root, Id::LambdaArg(x) if *x == key) {
                    *root = Id::Thunk(new_expression);
                };

            }
            Expression::Match { pattern, branches } => {
                for (_, (_, i)) in branches.iter_mut() {
                    i.substitute(key, Arc::clone(&new_expression));
                }
                pattern.substitute(key, Arc::clone(&new_expression));
            }
        }
    }

    //    idea
//    pub fn substitute(&mut self, key: u32, new_expression: Arc<Mutex<Expression>>) -> u32 {
//        let mut num = 0;
//        match self {
//            Expression::Lambda { body, .. } => {
//                num = body.substitute(key, new_expression);
//                if num == 0 {
//                    *body = Box::new(Expression::Tree {
//                        root: Id::Thunk(Arc::new(Mutex::new(*body.clone()))),
//                        arguments: Vec::new()
//                    })
//                }
//            }
//            Expression::Tree { root, arguments } => {
//                for mut i in arguments.iter_mut() {
//                    let aaa = i.substitute(key, Arc::clone(&new_expression));
//                    num += aaa;
//                    if aaa == 0 {
//                        *i = Expression::Tree {
//                            root: Id::Thunk(Arc::new(Mutex::new(i.clone()))),
//                            arguments: Vec::new()
//                        }
//                    }
//                }
//                //if *root == key {
//                //    *root = Id::Thunk(new_expression);
//                //};
//                if let Id::LambdaArg(x) = root {
//                    if *x == key {
//                        *root = Id::Thunk(new_expression);
//                    } else {
//                        num += 1
//                    }
//                };
//
//            }
//            Expression::Match { pattern, branches } => {
//                for (_, (_, i)) in branches.iter_mut() {
//                    let aaa = i.substitute(key, Arc::clone(&new_expression));
//                    num += aaa;
//                    if aaa == 0 {
//                        *i = Expression::Tree {
//                            root: Id::Thunk(Arc::new(Mutex::new(i.clone()))),
//                            arguments: Vec::new()
//                        }
//                    }
//                }
//                num += pattern.substitute(key, Arc::clone(&new_expression));
//            }
//        }
//        return num
//    }

    // evaluate an expression strictly, leavivg only a tree of data constructors.
    // can't be called on functions
    pub fn evaluate_strictly(mut self, definitions: Arc<ExpressionCache>) -> Self {
        if !matches!(&self, Expression::Tree {root: Id::DataConstructor(_), ..}) {
            self = self.simplify_owned(Arc::clone(&definitions));
        }
        match self {
            Expression::Tree {
                root,
                mut arguments,
            } => {
                //let mut output = Vec::with_capacity(arguments.len());
                //for i in std::mem::move(&mut arguments).into_iter() {
                //    output.push(i.evaluate_strictly(definitions));
                //}

                enum HandleOrValue<T> {
                    Handle(T),
                    Value(Expression)
                }

                let mut handles = Vec::new();
                for i in arguments {
                    if matches!(
                        &i, 
                        Expression::Tree {root: Id::DataConstructor(_), arguments} if arguments.len() == 0
                    ) {
                        handles.push(HandleOrValue::Value(i));
                    } else {
                        let r = Arc::clone(&definitions);
                        //dbg!("spawned thread");
                        let builder = thread::Builder::new().stack_size(2 * 1024 * 1024);
                        handles.push(
                            HandleOrValue::Handle(builder.spawn( move || i.evaluate_strictly(r)).unwrap())
                        )
                    }
                }
                let mut arguments = Vec::new();
                for i in handles.into_iter() {
                    arguments.push(match i {
                        HandleOrValue::Handle(i) => {
                            i.join().unwrap()
                        },
                        HandleOrValue::Value(i) => {

                            i
                        }
                    });
                     //       unsafe {
                     //           let mut inside = THREADS_USED.lock().unwrap();
                     //           *inside += 1;
                     //           //eprint!("\r{inside:>10}");
                     //       }
                }

                // map simplify_owned instead of evaluate_strictly

                //arguments = arguments.into_iter().map(|x| x.evaluate_strictly(Arc::clone(&definitions))).collect();
                //

                //let mut handles = Vec::new();
                //for (index, i) in arguments.clone().into_iter().enumerate() {
                //    let builder = thread::Builder::new().stack_size(32 * 1024 * 1024);
                //    let r = Arc::clone(&definitions);
                //    //if !matches!(
                //    //    i, 
                //    //    Expression::Tree {root: Id::DataConstructor(_), ..}
                //    //) {
                //    {
                //        dbg!("spawned thread");
                //        handles.push((
                //            index,
                //            builder.spawn( move || i.simplify_owned(r)).unwrap()
                //        ))
                //    }
                //}
                //for (index, handle) in handles.into_iter() {
                //    arguments[index] = handle
                //        .join()
                //        .unwrap()
                //        .evaluate_strictly(Arc::clone(&definitions));
                //}
                //
                Expression::Tree { root, arguments }
            }
            Expression::Lambda { .. } => panic!("attempted to evaluate a function"),
            _ => unreachable!(),
        }
    }

}

pub struct ExpressionCache {
    pub expressions: Vec<Expression>,
}

impl ExpressionCache {
    fn get(&self, index: usize) -> Expression {
        self.expressions[index].clone()
    }
}
