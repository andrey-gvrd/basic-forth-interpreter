#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::collections::VecDeque;

pub type Value = i32;
pub type ForthResult = Result<(), Error>;

#[derive(Debug, PartialEq, Copy, Clone)]
enum ArithWord { Add, Sub, Mul, Div }

#[derive(Debug, PartialEq, Copy, Clone)]
enum StackWord { Dup, Drop, Swap, Over }

#[derive(Debug, PartialEq, Copy, Clone)]
enum Symbol { Colon, SemiColon }

#[derive(Debug, PartialEq, Copy, Clone)]
enum Item {
    Exec_(Exec),
    Symbol_(Symbol),
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Exec {
    Arith_(ArithWord),
    Stack_(StackWord),
    Value_(Value),
}

lazy_static! {
    static ref WORD_MAP: HashMap<String, VecDeque<Item>> = {
        let mut m = HashMap::new();
        m.insert("DUP".to_owned(),  [Item::Exec_(Exec::Stack_(StackWord::Dup))].iter().cloned().collect());
        m.insert("DROP".to_owned(), [Item::Exec_(Exec::Stack_(StackWord::Drop))].iter().cloned().collect());
        m.insert("SWAP".to_owned(), [Item::Exec_(Exec::Stack_(StackWord::Swap))].iter().cloned().collect());
        m.insert("OVER".to_owned(), [Item::Exec_(Exec::Stack_(StackWord::Over))].iter().cloned().collect());
        m.insert("+".to_owned(),    [Item::Exec_(Exec::Arith_(ArithWord::Add))].iter().cloned().collect());
        m.insert("-".to_owned(),    [Item::Exec_(Exec::Arith_(ArithWord::Sub))].iter().cloned().collect());
        m.insert("*".to_owned(),    [Item::Exec_(Exec::Arith_(ArithWord::Mul))].iter().cloned().collect());
        m.insert("/".to_owned(),    [Item::Exec_(Exec::Arith_(ArithWord::Div))].iter().cloned().collect());
        m.insert(":".to_owned(),    [Item::Symbol_(Symbol::Colon)].iter().cloned().collect());
        m.insert(";".to_owned(),    [Item::Symbol_(Symbol::SemiColon)].iter().cloned().collect());
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
        for v in &self.stack {
            stack_str.push_str(&v.to_string());
            stack_str.push_str(" ");
        }
        stack_str = stack_str.trim().to_owned();
        stack_str
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        let v = try!(self.input_parse(input));
        for i in v {
            if let Item::Exec_(s) = i {
                match s {
                    Exec::Arith_(o) => {
                        let (a, b) = match (self.stack.pop_back(), self.stack.pop_back()) {
                            (Some(a), Some(b)) => (a, b),
                            (_, _) => return Err(Error::StackUnderflow),
                        };
                        let v = try!(eval_oper(a, b, o));
                        self.stack.push_back(v);
                    },
                    Exec::Stack_(c) => {
                        try!(eval_command(&mut self.stack, c));
                    },
                    Exec::Value_(v) => {
                        self.stack.push_back(v);
                    },
                }
            }
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

        for item_str in &input_split {
            match state {
                ParseState::Normal => {
                    let v = try!(self.str_to_item(item_str.clone().to_owned()));
                    let first_item = try!(v.back().clone().ok_or(Error::InvalidWord));

                    if first_item == &Item::Symbol_(Symbol::Colon) {
                        state = ParseState::CustomInit;
                    } else {
                        items.extend(v.iter().cloned());
                    }
                },
                ParseState::CustomInit => {
                    // Cannot re-define numbers
                    if let Ok(v) = self.str_to_item(item_str.clone().to_owned()) {
                        let first_item = try!(v.back().clone().ok_or(Error::InvalidWord));

                        if let &Item::Exec_(Exec::Value_(_)) = first_item {
                            return Err(Error::InvalidWord);
                        }
                    }

                    curr_custom_word = item_str.clone().to_owned();
                    self.word_map.insert(curr_custom_word.clone(), VecDeque::new());

                    state = ParseState::Custom;
                },
                ParseState::Custom => {
                    let v = try!(self.str_to_item(item_str.clone().to_owned()));
                    let first_item = try!(v.back().clone().ok_or(Error::InvalidWord));

                    if first_item == &Item::Symbol_(Symbol::SemiColon) {
                        state = ParseState::Normal;
                    } else if let Some(w) = self.word_map.get_mut(&curr_custom_word.clone()) {
                        w.extend(v.iter().cloned())
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
            Ok(v) => Ok([Item::Exec_(Exec::Value_(v))].iter().cloned().collect()),
            Err(_) => self.word_map.get(&s.to_uppercase()).cloned().ok_or(Error::UnknownWord),
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
            let a = try!(stack.back().cloned().ok_or(Error::StackUnderflow));
            stack.push_back(a);
        },
        StackWord::Drop => {
            if stack.pop_back().is_none() {
                return Err(Error::StackUnderflow);
            }
        },
        StackWord::Swap => {
            match (stack.pop_back(), stack.pop_back()) {
                (Some(a), Some(b)) => {
                    stack.push_back(a);
                    stack.push_back(b);
                },
                (_, _) => return Err(Error::StackUnderflow),
            }
        },
        StackWord::Over => {
            let len = stack.len();
            if len < 2 { return Err(Error::StackUnderflow) };
            let a = try!(stack.get(len - 2).cloned().ok_or(Error::StackUnderflow));
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
