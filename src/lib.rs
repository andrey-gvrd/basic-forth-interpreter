use std::collections::HashMap;
use std::fmt;

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

fn default_word_map() -> HashMap<String, Vec<Item>> {
    let mut m = HashMap::new();
    m.insert("DUP".to_owned(),  vec![Item::Exec_(Exec::Stack_(StackWord::Dup))]);
    m.insert("DROP".to_owned(), vec![Item::Exec_(Exec::Stack_(StackWord::Drop))]);
    m.insert("SWAP".to_owned(), vec![Item::Exec_(Exec::Stack_(StackWord::Swap))]);
    m.insert("OVER".to_owned(), vec![Item::Exec_(Exec::Stack_(StackWord::Over))]);
    m.insert("+".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Add))]);
    m.insert("-".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Sub))]);
    m.insert("*".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Mul))]);
    m.insert("/".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Div))]);
    m.insert(":".to_owned(),    vec![Item::Symbol_(Symbol::Colon)]);
    m.insert(";".to_owned(),    vec![Item::Symbol_(Symbol::SemiColon)]);
    m
}

pub struct Forth {
    word_map: HashMap<String, Vec<Item>>,
    stack: Vec<Value>,
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
            word_map: default_word_map(),
            stack: Vec::new(),
        }
    }

    pub fn format_stack(&self) -> String {
        StackFormat(self).to_string()
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        let v = try!(self.input_parse(input));
        for i in v {
            if let Item::Exec_(s) = i {
                match s {
                    Exec::Arith_(o) => {
                        let (a, b) = match (self.stack.pop(), self.stack.pop()) {
                            (Some(a), Some(b)) => (a, b),
                            (_, _) => return Err(Error::StackUnderflow),
                        };
                        let v = try!(eval_oper(a, b, o));
                        self.stack.push(v);
                    },
                    Exec::Stack_(c) => {
                        try!(eval_command(&mut self.stack, c));
                    },
                    Exec::Value_(v) => {
                        self.stack.push(v);
                    },
                }
            }
        }
        Ok(())
    }

    fn input_parse(&mut self, input: &str) -> Result<Vec<Item>, Error> {
        let mut items = Vec::new();
        let mut state = ParseState::Normal;
        let mut curr_custom_word = None;

        let input_uppercased = input.to_uppercase();
        let input_separated = to_space_separated(&input_uppercased);
        let input_split = input_separated.split_whitespace();

        let first_item_is = |items: &[_], expected| {
            match items.last() {
                Some(&actual) => Ok(actual == expected),
                None => Err(Error::InvalidWord),
            }
        };

        for item_str in input_split {
            match state {
                ParseState::Normal => {
                    let v = try!(self.str_to_item(item_str));

                    if try!(first_item_is(&v, Item::Symbol_(Symbol::Colon))) {
                        state = ParseState::CustomInit;
                    } else {
                        items.extend(v);
                    }
                },
                ParseState::CustomInit => {
                    // Cannot re-define numbers
                    if let Ok(v) = self.str_to_item(item_str) {
                        let first_item = try!(v.last().clone().ok_or(Error::InvalidWord));

                        if let &Item::Exec_(Exec::Value_(_)) = first_item {
                            return Err(Error::InvalidWord);
                        }
                    }

                    let custom_word = item_str.to_owned();
                    curr_custom_word = Some(custom_word.clone());
                    self.word_map.insert(custom_word, Vec::new());

                    state = ParseState::Custom;
                },
                ParseState::Custom => {
                    let v = try!(self.str_to_item(item_str));

                    if try!(first_item_is(&v, Item::Symbol_(Symbol::SemiColon))) {
                        state = ParseState::Normal;
                    } else if let Some(w) = self.word_map.get_mut(curr_custom_word.as_ref().unwrap()) {
                        w.extend(v);
                    }
                },
            }
        }

        match state {
            ParseState::Normal => Ok(items),
            _ => Err(Error::InvalidWord),
        }
    }

    fn str_to_item(&self, s: &str) -> Result<Vec<Item>, Error> {
        match s.parse::<Value>() {
            Ok(v) => Ok(vec![Item::Exec_(Exec::Value_(v))]),
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

fn eval_command(stack: &mut Vec<Value>, c: StackWord) -> ForthResult {
    match c {
        StackWord::Dup => {
            let a = try!(stack.last().cloned().ok_or(Error::StackUnderflow));
            stack.push(a);
        },
        StackWord::Drop => {
            if stack.pop().is_none() {
                return Err(Error::StackUnderflow);
            }
        },
        StackWord::Swap => {
            match (stack.pop(), stack.pop()) {
                (Some(a), Some(b)) => {
                    stack.push(a);
                    stack.push(b);
                },
                (_, _) => return Err(Error::StackUnderflow),
            }
        },
        StackWord::Over => {
            let len = stack.len();
            if len < 2 { return Err(Error::StackUnderflow) };
            let a = try!(stack.get(len - 2).cloned().ok_or(Error::StackUnderflow));
            stack.push(a);
        },
    }
    Ok(())
}

fn to_space_separated(s: &str) -> String {
    s.chars().map(|c| if c.is_control() { ' ' } else { c }).collect()
}

struct StackFormat<'a>(&'a Forth);

impl<'a> fmt::Display for StackFormat<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some((head, tail)) = self.0.stack.split_first() {
            try!(write!(f, "{}", head));

            for v in tail {
                try!(write!(f, " {}", v));
            }
        }
        Ok(())
    }
}
