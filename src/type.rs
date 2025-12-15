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

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Type(u32),
    Function(Box<Type>, Box<Type>),
}

impl Type {
    pub fn is_a_function(&self) -> bool {
        match self {
            Self::Type(_) => false,
            Self::Function(_, _) => true,
        }
    }

    pub fn final_type(&self) -> u32 {
        match self {
            Self::Type(x) => *x,
            Self::Function(_, b) => b.final_type(),
        }
    }

    pub fn is_possible(&self, test: &Self) -> bool {
        *self == *test
            || match self {
                Self::Type(_) => false,
                Self::Function(_, output) => output.is_possible(test),
            }
    }

    //pub fn apply_type(self, arg: Self) -> Option<Self> {
    //    match self {
    //        Self::Type(u32) => None,
    //        Self::Function(x, y) => {
    //            if arg == *x {
    //                Some(*y)
    //            } else {
    //                None
    //            }
    //        }
    //    }
    //}

    pub fn arg_types(self) -> Vec<Type> {
        let mut args = Vec::new();
        let mut current_type = self;
        loop {
            match current_type {
                Type::Function(a, b) => {
                    args.push(*a);
                    current_type = *b;
                }
                Type::Type(_) => return args,
            }
        }
    }
    pub fn show(&self, types: &HashMap<String, u32>) -> String {
        fn helper(tp: &Type, types: &HashMap<&u32, &String>, output: &mut String) {
            match tp {
                Type::Type(a) => {
                    let name = types.get(a).unwrap(); // safe
                    output.push_str(name);
                    output.push(' ')
                }
                Type::Function(a, b) => {
                    output.push_str("fn ");
                    helper(a, types, output);
                    helper(b, types, output);
                }
            }
        }
        let mut output = String::new();
        let mut new_map = HashMap::new();
        types.iter().for_each(|(k, w)| { new_map.insert(w, k); });
        helper(self, &new_map, &mut output);
        output
    }
}
