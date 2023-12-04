use std::{fs, env};
use std::str::Chars;
use std::iter::Peekable;
use anyhow::{Result, Error};

struct Parser<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            input: source.chars().peekable(),
        }
    }

    fn parse_list(&mut self) -> Result<(), Error> {
        self.input.next();
        println!("<list>");
        while let Some(&c) = self.input.peek() {
            if c == ')' {
                self.input.next();
                break;
            } else if c == '(' {
                let _ = self.parse_list();
            } else if c.is_whitespace() {
                self.input.next();
            } else {
                let _ = self.parse_atom();
            }
        }
        println!("</list>");
        Ok(())
    }

    fn parse_atom(&mut self) -> Result<(), Error> {
        let mut atom = String::new();
        while let Some(&c) = self.input.peek() {
            if c.is_whitespace() || c == ')' {
                break;
            } else {
                atom.push(c);
            }
            self.input.next();
        }
        println!("{}", atom);
        Ok(())
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

    // Parse the lisp source into s-expressions
    match parser.parse_list() {
        Ok(_) => println!("Parsing completed!"),
        Err(e) => println!("An error occurred: {:?}", e),
    }

    Ok(())
}
