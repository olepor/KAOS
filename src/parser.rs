use ast::*;
use lexer::Lexer;
use std::error::Error;
use std::fmt;
use std::result::Result;
use token::Token;

pub enum Precedence {
    LOWEST,
    EQUALS,  // == LESSGREATER // > or <
    SUM,     // +
    PRODUCT, // *
    PREFIX,  // -X or !X
    CALL,    // myFunction(X)
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token, String),
    UnImplemented,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(a, msg) => {
                write!(f, "Parse error! Unexpected token: {}: {}", a, msg)
            }
            _ => write!(f, "Parse error (not unexpected token)!"),
        }
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(self)
    }
}

type PrefixParseFn = fn(parser: &mut Parser) -> Result<Box<Statement>, ParseError>;
type InfixParseFn = fn(parser: &mut Parser) -> Result<Box<Statement>, ParseError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer) -> Parser {
        Parser {
            lexer: lexer,
        }
    }

    // fn prefix_fn(&mut self) -> Option<PrefixParseFn> {
    //     match self.next_token() {
    //         Token::IDENT(_) => Some(Parser::parse_identifier),
    //         _ => None,
    //     }
    // }

    // fn infix_fn(&mut self) -> Option<InfixParseFn> {
    //     None
    // }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        return self.parse_program();
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut prog = Program {
            statements: Vec::new(),
        };
        loop {
            let statement = self.parse_statement()?;
            println!("Pushing statement: {}", statement);
            prog.statements.push(statement);
            break;
        }
        return Ok(prog);
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek_token() {
            Token::LET => {
                self.next_token(); // Consume Let
                self.parse_let_statement()
            }
            Token::RETURN => {
                self.next_token(); // Consume Return
                self.parse_return_statement()
            }
            // Token::IF => {
            //     let ifs = self.parse_if()?;
            //     Ok(Box::new(ifs))
            // }
            _ => Err(ParseError::UnexpectedToken(self.cur_token(), String::from("parse_statement: "))),
        }
    }

    // fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
    //     let prefix = self.prefixParseFns.get(&self.next_token());
    //     match prefix {
    //         None => Err(ParseError::ParseErr),
    //         Some(f) => {
    //             return Ok(Box::new(f()));
    //         }
    //     }
    // }

    // fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
    //     stmt.statements = self.parse_expression(Precedence::LOWEST);

    //     self.consume_if(Token::SEMICOLON);

    //     let stmt = ExpressionStatement {
    //         identifier: self.next_token(),
    //         expression: stmt,
    //     };

    //     return stmt;
    // }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        // For a let statement the next token must be an identifier!
        println!("{}", self.cur_token());
        println!("{}", self.peek_token());
        println!("{}", self.peek_token());
        let ident = self.next_token();
        println!("{}", ident);
        match ident { // Maybe this can be made into a partial eq test ?
            Token::IDENT(_) => {}, // No-Op.
            _ => Err(ParseError::UnexpectedToken(self.peek_token(), String::from("parse let statement - no identifier found")))?
        };
        // Consume until semicolon!
        loop {
            if self.cur_token() == Token::SEMICOLON {
                break;
            }
            self.next_token(); // For now - consume all tokens until ';' is encountered.
        }
        Ok(Statement::Let(Box::new(Expression::Identifier(Box::new(ident)))))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {

        let expr = self.next_token();
        println!("{:?}, parse_return_statement", expr);
        // Skip until semicolon!
        loop {
            if self.cur_token() == Token::SEMICOLON {
                break;
            }
            self.next_token(); // For now - consume all tokens until ';' is encountered.
            println!("{:?}, parse_return_statement, loop", self.cur_token());
            break;
        }
        Ok(Statement::Return(Box::new(Expression::Identifier(Box::new(expr)))))
    }

    // fn parse_if_statement(& mut self) -> Result<Statement, ParseError> {
        
    // }

    // fn consume_if<T>(&mut self, T) -> Token

    // fn parse_identifier(parser: &mut Parser) -> Result<Expression, ParseError> {
    //     return Ok(Box::new(Expression::Identifier("a".to_string())));
    // }

    fn cur_token(&self) -> Token {
        self.lexer.cur_token.clone()
    }

    fn next_token(&mut self) -> Token {
        self.lexer.next()
    }

    fn peek_token(&self) -> Token {
        return self.lexer.peek()
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse() {
        let lexer = Lexer::new("let a=b;");
        let mut parser = Parser::new(lexer);
        // Parse and loop through the tokens.
        let res = parser.parse();
        println!("{:?}", res);
        assert!(res.is_ok());
        // let lexer = Lexer::new("let a=b; let a = b");

        // Test parse Return statement
        let lexer = Lexer::new("return 5;");
        let mut parser = Parser::new(lexer);
        // // Parse and loop through the tokens.
        // let res = parser.parse();
        println!("{:?}", res);
        assert!(res.is_ok());
        // let mut parser = Parser::new(lexer);
        // assert_eq!(
        //     parser.parse(),
        //     Some(Program {
        //         statements: vec![
        //             Variable {
        //                 identifier: Token::IDENT("a".to_string()),
        //                 value: Expression {
        //                     identifier: Token::IDENT("b".to_string()),
        //                 },
        //             },
        //             Variable {
        //                 identifier: Token::IDENT("a".to_string()),
        //                 value: Expression {
        //                     identifier: Token::IDENT("b".to_string()),
        //                 },
        //             },
        //         ],
        //     })
        // );
    }
}
