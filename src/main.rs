use std::{fs, fmt, env};
use std::str::Chars;
use std::iter::Peekable;
use std::collections::HashMap;
use anyhow::Result;

struct Parser<'a> {
    input: Peekable<Chars<'a>>,
}

enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    List(Vec<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "NIL"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "{}", s),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::List(lst) => {
                write!(f, "(")?;
                for val in lst {
                    write!(f, "{} ", val)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            input: source.chars().peekable(),
        }
    }

    fn parse_list(&mut self) -> Value {
        if let Some(&ch) = self.input.peek() {
            assert_eq!(ch, '(');
            self.input.next();
        } else {
            panic!("No more characters!");
        }
        let mut values: Vec<Value> = Vec::new();
        let mut quoted = false;
        while let Some(&c) = self.input.peek() {
            match c {
                ')' => {
                    self.input.next();
                    break;
                }
                '\'' => {
                    quoted = true;
                    self.input.next();
                }
                '(' => {
                    let value = self.parse_list();
                    if quoted {
                        quoted = false;
                        let vec = vec![Value::Symbol("quote".to_string()), value];
                        values.push(Value::List(vec));
                    } else {
                        values.push(value);
                    }
                }
                _ if c.is_whitespace() => {
                    self.input.next();
                }
                _ => {
                    let value = self.parse_atom();
                    if quoted {
                        quoted = false;
                        let vec = vec![Value::Symbol("quote".to_string()), value];
                        values.push(Value::List(vec));
                    } else {
                        values.push(value);
                    }
                }
            }
        }
        Value::List(values)
    }

    fn parse_atom(&mut self) -> Value {
        let mut atom = String::new();
        let mut first_char = '\0';
        while let Some(&c) = self.input.peek() {
            match c {
                _ if c.is_whitespace() || c == ')' => {
                    break;
                }
                _ => {
                    if first_char == '\0' {
                        first_char = c;
                    }
                    atom.push(c);
                }
            }
            self.input.next();
        }
        if atom.is_empty() {
            return Value::Nil;
        }
        atom = atom.to_uppercase();
        match first_char {
            '\"' => {
                Value::String(atom)
            }
            _ if atom == "T" => {
                Value::Bool(true)
            }
            _ if atom == "NIL" => {
                Value::Bool(false)
            }
            _ if first_char.is_digit(10) => {
                if atom.contains('.') {
                    Value::Float(atom.parse::<f64>().unwrap())
                } else {
                    Value::Int(atom.parse::<i64>().unwrap())
                }
            }
            _ => {
                Value::Symbol(atom)
            }
        }
    }
}

fn main() -> Result<()> {
    // Retrieve the arguments provided to the program
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("No Lisp source file provided.");
        return Ok(());
    }

    // Read the lisp source file
    let lisp_source: String = fs::read_to_string(&args[1])?;
    let mut parser = Parser::new(&lisp_source);

    while let Some(&c) = parser.input.peek() {
        match c {
            '(' => {
                println!("{}", parser.parse_list());
            }
            _ => {
                parser.input.next();
            }
        }
    }

    Ok(())
}
