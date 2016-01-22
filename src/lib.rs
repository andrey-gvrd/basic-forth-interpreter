#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::collections::VecDeque;

pub type Value = i32;
pub type ForthResult = Result<(), Error>;

#[derive(Debug, PartialEq, Clone)]
enum ArithWord { Add, Sub, Mul, Div }

#[derive(Debug, PartialEq, Clone)]
enum StackWord { Dup, Drop, Swap, Over }

#[derive(Debug, PartialEq, Clone)]
enum Symbol { Colon, SemiColon }

#[derive(Debug, PartialEq, Clone)]
enum Item {
    Exec_(Exec),
    Symbol_(Symbol),
}

#[derive(Debug, PartialEq, Clone)]
enum Exec {
    Arith_(ArithWord),
    Stack_(StackWord),
    Value_(Value),
}

lazy_static! {
    static ref WORD_MAP: HashMap<String, VecDeque<Item>> = {
        let mut m = HashMap::new();
        m.insert("DUP".to_owned(),  vec![Item::Exec_(Exec::Stack_(StackWord::Dup))].into_iter().collect());
        m.insert("DROP".to_owned(), vec![Item::Exec_(Exec::Stack_(StackWord::Drop))].into_iter().collect());
        m.insert("SWAP".to_owned(), vec![Item::Exec_(Exec::Stack_(StackWord::Swap))].into_iter().collect());
        m.insert("OVER".to_owned(), vec![Item::Exec_(Exec::Stack_(StackWord::Over))].into_iter().collect());
        m.insert("+".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Add))].into_iter().collect());
        m.insert("-".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Sub))].into_iter().collect());
        m.insert("*".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Mul))].into_iter().collect());
        m.insert("/".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Div))].into_iter().collect());
        m.insert(":".to_owned(),    vec![Item::Symbol_(Symbol::Colon)].into_iter().collect());
        m.insert(";".to_owned(),    vec![Item::Symbol_(Symbol::SemiColon)].into_iter().collect());
        m
    };
}

pub struct Forth {
    word_map: HashMap<String, VecDeque<Item>>,
    stack: VecDeque<Value>,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

enum ParseState {
    Normal,         // Parse into existing words
    CustomInit,     // This item is the name of re-defined word
    Custom,         // This item is the body of re-defined word
}

impl Forth {
    pub fn new() -> Forth {
        Forth {
            word_map: WORD_MAP.clone(),
            stack: VecDeque::new(),
        }
    }

    pub fn format_stack(&self) -> String {
        let mut stack_str = String::new();
        for v in self.stack.iter() {
            stack_str.push_str(&v.to_string());
            stack_str.push_str(" ");
        }
        stack_str = stack_str.trim().to_owned();
        stack_str
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        match self.input_parse(input) {
            Ok(v) => {
                for i in v.into_iter() {
                    match i {
                        Item::Exec_(s) => match s {
                            Exec::Arith_(o) => {
                                let (a, b) = match (self.stack.pop_back(), self.stack.pop_back()) {
                                    (Some(a), Some(b)) => (a, b),
                                    (_, _) => return Err(Error::StackUnderflow),
                                };
                                match eval_oper(a, b, o) {
                                    Ok(v) => self.stack.push_back(v),
                                    Err(e) => return Err(e),
                                }
                            },
                            Exec::Stack_(c) => {
                                try!(eval_command(&mut self.stack, c));
                            },
                            Exec::Value_(v) => {
                                self.stack.push_back(v);
                            },
                        },
                        _ => (),
                    }
                }
            },
            Err(e) => return Err(e),
        }
        Ok(())
    }

    fn input_parse(&mut self, input: &str) -> Result<VecDeque<Item>, Error> {
        let mut items: VecDeque<Item> = VecDeque::new();
        let mut state = ParseState::Normal;
        let mut curr_custom_word = String::new();

        let input_uppercased = &input.to_uppercase() as &str;
        let input_separated = to_space_separated(input_uppercased.clone());
        let input_split = input_separated.split_whitespace().collect::<Vec<&str>>();

        for item_str in input_split.iter() {
            match state {
                ParseState::Normal => {
                    match self.str_to_item(item_str.clone().to_owned()) {
                        Ok(v) => {
                            let first_item = try!(v.back().clone().ok_or(Error::InvalidWord));

                            if first_item == &Item::Symbol_(Symbol::Colon) {
                                state = ParseState::CustomInit;
                            } else {
                                for i in v.iter() {
                                    items.push_back((*i).clone());
                                }
                            }
                        },
                        Err(e) => return Err(e),
                    }
                },
                ParseState::CustomInit => {
                    // Cannot re-define numbers
                    match self.str_to_item(item_str.clone().to_owned()) {
                        Ok(v) => {
                            let first_item = try!(v.back().clone().ok_or(Error::InvalidWord));

                            match first_item {
                                &Item::Exec_(Exec::Value_(_)) => return Err(Error::InvalidWord),
                                _ => (),
                            }
                        },
                        _ => (),
                    }

                    curr_custom_word = item_str.clone().to_owned();
                    self.word_map.insert(curr_custom_word.clone(), VecDeque::new());

                    state = ParseState::Custom;
                },
                ParseState::Custom => {
                    match self.str_to_item(item_str.clone().to_owned()) {
                        Ok(v) => {
                            let first_item = try!(v.back().clone().ok_or(Error::InvalidWord));

                            if first_item == &Item::Symbol_(Symbol::SemiColon) {
                                state = ParseState::Normal;
                            } else {
                                match self.word_map.get_mut(&curr_custom_word.clone()) {
                                    Some(w) => {
                                        for i in v.iter() {
                                            w.push_back((*i).clone());
                                        }
                                    },
                                    None => (),
                                }
                            }
                        },
                        Err(e) => return Err(e),
                    }
                },
            }
        }

        match state {
            ParseState::Normal => Ok(items),
            _ => Err(Error::InvalidWord),
        }
    }

    fn str_to_item(&self, s: String) -> Result<VecDeque<Item>, Error> {
        match s.parse::<Value>() {
            Ok(v) => Ok(vec![Item::Exec_(Exec::Value_(v))].into_iter().collect()),
            Err(_) => {
                match self.word_map.get(&s.to_uppercase()) {
                    Some(w) => Ok((*w).clone()),
                    None    => Err(Error::UnknownWord),
                }
            }
        }
    }
}

fn eval_oper(a: Value, b: Value, o: ArithWord) -> Result<Value, Error> {
    match o {
        ArithWord::Add => Ok(b + a),
        ArithWord::Sub => Ok(b - a),
        ArithWord::Mul => Ok(b * a),
        ArithWord::Div => {
            match a {
                0 => Err(Error::DivisionByZero),
                a => Ok(b / a),
            }
        },
    }
}

fn eval_command(stack: &mut VecDeque<Value>, c: StackWord) -> ForthResult {
    match c {
        StackWord::Dup => {
            let a = match stack.back() {
                Some(&a) => a,
                _ => return Err(Error::StackUnderflow),
            };
            stack.push_back(a);
        },
        StackWord::Drop => {
            match stack.pop_back() {
                Some(_) => (),
                _ => return Err(Error::StackUnderflow),
            }
        },
        StackWord::Swap => {
            let (a, b) = match (stack.pop_back(), stack.pop_back()) {
                (Some(a), Some(b)) => (a, b),
                (_, _) => return Err(Error::StackUnderflow),
            };
            stack.push_back(a);
            stack.push_back(b);
        },
        StackWord::Over => {
            let len = stack.len();
            if len < 2 { return Err(Error::StackUnderflow) };
            let a = match stack.get(len - 2) {
                Some(&a) => a,
                _ => return Err(Error::StackUnderflow),
            };
            stack.push_back(a);
        },
    }
    Ok(())
}

fn to_space_separated(s: &str) -> String {
    let mut space_separated = String::new();
    for c in s.chars() {
        match c.is_control() {
            true  => space_separated.push_str(&" ".to_string()),
            false => space_separated.push_str(&c.to_string()),
        }
    }
    space_separated
}
