use std::{env, fs};

#[derive(Debug, PartialEq)]
pub enum SElem {
    Number(i64),
    Symbol(String),
}

#[derive(Debug, PartialEq)]
pub enum SExpr {
    Atom(SElem),
    List(Vec<SExpr>),
}

impl SExpr {
    pub fn new(input: &str) -> Result<SExpr, &'static str> {
        // function definition goes here
    }
    
    pub fn evaluate(&self) -> Result<SElem, &'static str> {
        // function definition goes here
    }
}

impl SElem {
    pub fn new(input: &str) -> Result<SElem, &'static str> {
        // function definition goes here
    }

    pub fn as_number(&self) -> Option<i64> {
        // function definition goes here
    }
    
    pub fn as_symbol(&self) -> Option<&str> {
        // function definition goes here
    }
}

fn parse_sexpr(input: &str) -> Result<SExpr, &'static str> {
    // function definition goes here
}

fn main() -> std::io::Result<()> {
    // Retrieve the arguments provided to the program
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("No Lisp source file provided.");
        return Ok(());
    }

    // Read the lisp source file
    let lisp_source = fs::read_to_string(&args[1])?;

    // Parse the lisp source into s-expressions
    let parsed_sexpr = parse_sexpr(&lisp_source);

    match parsed_sexpr {
        Ok(sexpr) => println!("Parsed S-Expression: {:?}", sexpr),
        Err(err) => println!("Error parsing S-Expression: {}", err),
    }

    Ok(())
}
