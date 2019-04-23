use ast::*;
use lexer::Lexer;
use std::error::Error;
use std::fmt;
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
    ParseErr,
    UnexpectedToken(Token, &'static str),
    UnImplemented,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(a, b) => {
                write!(f, "Parse error! Unexpected token: {}, {}", a, b)
            }
            _ => write!(f, "Parse error!"),
        }
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(self)
    }
}


type PrefixParseFn = fn(parser: &mut Parser) -> std::result::Result<Box<Statement>, ParseError>;
type InfixParseFn = fn(parser: &mut Parser) -> std::result::Result<Box<Statement>, ParseError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer: lexer,
        }
    }

    fn prefix_fn(&mut self) -> Option<PrefixParseFn> {
        match self.next_token() {
            Token::IDENT(_) => Some(Parser::parse_identifier),
            _ => None,
        }
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
            prog.statements.push(statement);
            break;
        }
        return Ok(prog);
    }

    fn parse_identifier(parser: &mut Parser) -> std::result::Result<Box<Statement>, ParseError> {
        return Ok(Box::new(Identifier{identifier: Token::IDENT("a".to_string())}));
    }

    fn parse_statement(&mut self) -> std::result::Result<Box<Statement>, ParseError> {
        let n_tok = self.peek_token();
        println!("parse_statement: peek token: {:?}", n_tok);
        match n_tok {
            Token::LET => {
                let var = self.parse_variable()?;
                Ok(Box::new(var))
            }
            Token::IF => {
                let ifs = self.parse_if()?;
                Ok(Box::new(ifs))
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> std::result::Result<Box<Statement>, ParseError> {

        stmt.statements = self.parse_expression(Precedence::LOWEST);

        self.consume_if(Token::SEMICOLON);

        let stmt = ExpressionStatement {
            identifier: self.next_token(),
            expression: stmt,
        };

        return stmt;
    }

    fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> std::result::Result<Box<Expression>, ParseError> {
        let prefix = self.prefixParseFns.get(&self.next_token());
        match prefix {
            None => Err(ParseError::ParseErr),
            Some(f) => {
                return Ok(Box::new(f()));
            }
        }

    }

    fn consume_if(&mut self, exp_token: Token) {
        if self.peek_token() == exp_token {
            self.next_token();
        }
    }

    fn parse_if(&mut self) -> std::result::Result<IFStatement, ParseError> {
        // Panic if not if statement. Something is wrong in the implementation.
        self.next_token().expect_token(Token::IF, "").unwrap();

        let cond = self.parse_condition()?;

        self.next_token()
            .expect_token(Token::LBRACE, "error - expected left brace")?;

        let mut ifstatement = IFStatement {
            identifier: Token::IF,
            value: cond,
            body_statements: Vec::new(),
        };
        // Parse all statements in the if body.
        loop {
            if self.peek_token() == Token::RBRACE {
                break;
            };
            let statement = self.parse_statement()?;
            ifstatement.body_statements.push(statement);
        }

        Ok(ifstatement)
    }

    fn parse_condition(&mut self) -> std::result::Result<Condition, ParseError> {
        let cond = self.next_token();
        match cond {
            Token::TRUE | Token::FALSE => Ok(Condition { identifier: cond }),
            _ => Err(ParseError::UnexpectedToken(
                cond,
                "error - failed to parse condition",
            )),
        }
    }

    fn parse_variable(&mut self) -> std::result::Result<Variable, ParseError> {
        println!("parse_variable...");

        self.next_token()
            .expect_token(Token::LET, "error - expected let statement")?;

        println!("parse_variable: Got a let statement");

        let ident = self
            .next_token()
            .expect_token(Token::IDENT("a".to_string()), "error - expected identifier")?;

        println!("parse_variable: Got an identifier: {:?}", ident);
        println!("...");

        self.next_token()
            .expect_token(Token::ASSIGN, "error - expected assign")?;

        let expr = self.parse_expression()?;

        println!("Expression: {}", expr);

        return Ok(Variable {
            identifier: ident,
            value: Box::new(expr),
        });
    }

    fn parse_number(&mut self) -> std::result::Result<i32, ParseError> {
        let a = self.parse_a();
        return Ok(1);
    }

    fn parse_a(&mut self) -> std::result::Result<i32, ParseError> {
        let b = self.parse_b();
        return Ok(2);
    }

    fn parse_b(&mut self) -> std::result::Result<i32, ParseError> {
        // b = "(" number ")"
        //     | number
        //     | digit
        return Ok(2);
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
        // let lexer = Lexer::new("let a=b");
        // let mut parser = Parser::new(lexer);
        // assert_eq!(
        //     parser.parse(),
        //     Some(Program {
        //         statements: vec![Variable {
        //             identifier: Token::IDENT("a".to_string()),
        //             value: Box::new(Expression {
        //                 identifier: Token::IDENT("b".to_string())
        //             },),
        //         },],
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
