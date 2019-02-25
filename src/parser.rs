
use lexer::Lexer;
use ast::Program;
use token::Token;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl <'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        Parser{lexer}
    }

    pub fn parse(& self) -> Program {
        return Program{};
    }

    fn next_token(& mut self) -> Token {
        return self.lexer.next();
    }

    fn peek_token(& self) -> Token {
        return self.lexer.peek();
    }
}
