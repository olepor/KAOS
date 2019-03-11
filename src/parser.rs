use lexer::Lexer;
use token::Token;
use ast::*;
use std::fmt;
use std::error::Error;

#[derive(Debug)]
pub enum ParseError {
    ParseErr,
    UnexpectedToken(Token, &'static str),
    UnImplemented,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(a,b) => write!(f, "Parse error! Unexpected token: {}, {}", a, b),
            _ => write!(f, "Parse error!"),
        }
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(self)
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }

    pub fn parse(&mut self) -> std::result::Result<Program, ParseError> {
        return self.parse_program();
    }

    fn parse_program(&mut self) -> std::result::Result<Program, ParseError> {
        let mut prog = Program {
            statements: Vec::new(),
        };
        loop {
            let statement = self.parse_statement()?;
            println!("Pushing statement: {}", statement);
            prog.statements.push(Box::new(statement));
            break;
        }
        return Ok(prog);
    }

    fn parse_statement(&mut self) -> std::result::Result<impl Statement, ParseError> {
        let n_tok: Token = self.peek_token().expect_token(Token::LET, "expected 'let' - found")?;
        println!("parse_statement: peek token: {:?}", n_tok);
        match n_tok {
            Token::LET => self.parse_variable(),
            // Token::IF => self.parse_if(),
            _ => Err(ParseError::UnexpectedToken(n_tok, "Found unexpected token")),
        }
    }

    fn parse_if(&mut self) -> std::result::Result<impl Statement, ParseError> {
        // Panic if not if statement. Something is wrong in the implementation.
        self.next_token().expect_token(Token::IF, "").unwrap();

        let cond = self.parse_condition()?;

        self.next_token().expect_token(Token::LBRACE, "error - expected left brace")?;

        let mut ifstatement = IFStatement{identifier: Token::IF, value: cond, body_statements: Vec::new()};
        // Parse all statements in the if body.
        loop {
            if self.peek_token() == Token::RBRACE { break; };
            let statement = self.parse_statement()?;
            ifstatement.body_statements.push(Box::new(statement));
        }

        Ok(ifstatement)

    }

    fn parse_condition(&mut self) -> std::result::Result<Condition, ParseError> {
        let cond = self.next_token();
        match cond {
            Token::TRUE | Token::FALSE => Ok(Condition{identifier: cond}),
            _ => Err(ParseError::UnexpectedToken(cond, "error - failed to parse condition")),
        }
    }

    fn parse_variable(&mut self) -> std::result::Result<Variable, ParseError> {

        println!("parse_variable...");

        self.next_token().expect_token(Token::LET, "error - expected let statement")?;

        println!("parse_variable: Got a let statement");

        let ident = self.next_token().expect_token(Token::IDENT("a".to_string()), "error - expected identifier")?;

        println!("parse_variable: Got an identifier: {:?}", ident);
        println!("...");

        self.next_token().expect_token(Token::ASSIGN, "error - expected assign")?;

        let expr = self.parse_expression()?;

        println!("Expression: {}", expr);

        return Ok(Variable{identifier: ident, value: Box::new(expr)});
    }

    fn parse_expression(&mut self) -> std::result::Result<impl Expression, ParseError> {
        let nr = self.next_token().expect_token(Token::INT(5), "error - expected a number")?;
        return Ok(Number{identifier: nr});
    }

    fn next_token(&mut self) -> Token {
        return self.lexer.next();
    }

    fn peek_token(&self) -> Token {
        return self.lexer.peek();
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse() {
        let lexer = Lexer::new("let a=b");
        let mut parser = Parser::new(lexer);
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
        //         ],
        //     })
        // );

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
