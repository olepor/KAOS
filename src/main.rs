#[macro_use] extern crate lazy_static;

mod lexer;
mod token;
mod parser;
mod ast;


use std::io;
// use token::Token;
// use parser::Parser;


fn main() {
    // Get user input

    let mut input = String::new();

    loop {
        println!("Please input an expression: ");
        io::stdin().read_line(&mut input).expect("No input!");

        // {
        let lexer = lexer::Lexer::new(& input);


        let parser = parser::Parser::new(lexer);

        let program = parser.parse();
        // let mut tok = lexer.next();
        // println!("{:?}", tok);
        // while tok != Token::EOF {
        //     tok = lexer.next();
        //     println!("{:?}", tok);
        // }
        // input.clear();
    }
}
