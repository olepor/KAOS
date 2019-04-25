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
    cur_token: Token,
    next_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer) -> Parser {
        let a = lexer.next();
        let b = lexer.next();
        Parser {
            lexer: lexer,
            cur_token: a,
            next_token: b,
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
        match self.cur_token {
            Token::LET => {
                self.parse_let_statement()
            }
            // Token::IF => {
            //     let ifs = self.parse_if()?;
            //     Ok(Box::new(ifs))
            // }
            _ => Err(ParseError::UnexpectedToken(self.cur_token.clone(), String::from("parse_statement: "))),
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
        let ident = match self.peek_token() {
            Token::IDENT(_) => self.next_token(),
            _ => Err(ParseError::UnexpectedToken(self.peek_token(), String::from("parse let statement")))?
        };
        // Consume until semicolon!
        loop {
            if self.cur_token == Token::SEMICOLON {
                break;
            }
        }
        Ok(Statement::Let(Box::new(Expression::Identifier(Box::new(ident)))))
    }

    // fn parse_identifier(parser: &mut Parser) -> Result<Expression, ParseError> {
    //     return Ok(Box::new(Expression::Identifier("a".to_string())));
    // }

    fn next_token(&mut self) -> Token {
        self.cur_token = self.lexer.next();
        self.next_token = self.lexer.next();
        self.cur_token.clone()
    }

    fn peek_token(&self) -> Token {
        return self.next_token.clone();
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse() {
        let lexer = Lexer::new("let a=b");
        let mut parser = Parser::new(lexer);
        // Parse and loop through the tokens.
        let res = parser.parse();
        println!("{:?}", res);
        assert!(res.is_ok());
        // let lexer = Lexer::new("let a=b; let a = b");

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
