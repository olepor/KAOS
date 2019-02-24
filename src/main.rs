#[macro_use] extern crate lazy_static;

use std::io;
pub mod lexer;

fn main() {
    // Get user input

    let mut input = String::new();

    loop {
        println!("Please input an expression: ");
        io::stdin().read_line(&mut input).expect("No input!");

        {
        let mut lexer = lexer::Lexer::new(& input);

        let mut tok = lexer.next();
        println!("{:?}", tok);
        while tok != lexer::Token::EOF {
            tok = lexer.next();
            println!("{:?}", tok);
        }
        }
        input.clear();
    }
}
