
use lexer::Lexer;
use ast::Program;
use token::Token;
use ast::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl <'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        Parser{lexer}
    }

    pub fn parse(& mut self) -> Option<Variable> {
        return self.parse_variable();
    }

    fn parse_variable(& mut self) -> Option<Variable> {
        let mut l = self.next_token();
        if l != Token::LET {
            println!("unexpected token: expected let: {:?}", l);
            return Option::None;
        }
        let ident = self.next_token();
        match ident {
            Token::IDENT(_) => {
            },
            _ => {
                return Option::None;
            },
        }
        l = self.next_token();
        if l != Token::ASSIGN {
            println!("unexpected token: expected assign: {:?}", l);
            return Option::None;
        }
        let l = self.parse_expression();
        if let Some(expr) = l {
            return Option::Some(Variable{identifier: ident, value: expr});
        } else {
            println!("unexpected token: expected expression: {:?}", l);
            return Option::None;
        }
    }

    fn parse_expression(& mut self) -> Option<Expression> {
        let l = self.next_token();
        match l {
            Token::IDENT(_) => Option::Some(Expression{identifier: l}),
            _ => Option::None,
        }
    }

    fn next_token(& mut self) -> Token {
        return self.lexer.next();
    }

    fn peek_token(& self) -> Token {
        return self.lexer.peek();
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse() {
        let lexer = Lexer::new("let a = b;");
        let mut parser = Parser::new(lexer);
        assert_eq!(parser.parse(),
                   Some(Variable{
            identifier: Token::IDENT("a".to_string()),
            value: Expression{identifier: Token::IDENT("b".to_string())}
        }));
    }
}
