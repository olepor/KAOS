use ast;
use lexer::Lexer;
use log::*;
use std::error::Error;
use std::fmt;
use std::result::Result;
use token::Token;
// Current progress: Pratt-parsing of identifiers.
// Not done: Parsing of expressions for return statements.
// Next topic: Prefix-Expressions

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

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer) -> Parser {
        Parser { lexer: lexer }
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

    pub fn parse(&mut self) -> Result<ast::Program, ParseError> {
        return self.parse_program();
    }

    fn parse_program(&mut self) -> Result<ast::Program, ParseError> {
        debug!("[parser::parse_program] Starting the parser...");
        let mut prog = ast::Program {
            statements: Vec::new(),
        };
        loop {
            debug!("[parser::parse_program] *Nom, nom, nom* Parsing the next statement...");
            let statement = self.parse_statement()?;
            println!("Pushing statement: {}", statement);
            prog.statements.push(statement);
            break;
        }
        return Ok(prog);
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
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
            _ => self.parse_expression_statement(),
            // _ => Err(ParseError::UnImplemented),
        }
    }

    fn parse_prefix(&mut self, token: Token) -> Result<ast::Expression, ParseError> {
        debug!("[parser::parse_prefix] token: {:?}", token);
        match token {
            Token::IDENT(i) => Ok(ast::Expression::Identifier(i)),
            Token::INT(i) => Ok(ast::Expression::IntegerLiteral(i)),
            Token::MINUS => {
                // parse the following expression
                let expr = self.parse_expression(Precedence::LOWEST)?;
                Ok(ast::Expression::Prefix(Token::MINUS, Box::new(expr)))
            }
            _ => Err(ParseError::UnImplemented),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Statement, ParseError> {
        debug!("[parser::parse_expression]");
        let cur_tok = self.next_token();
        debug!("[parser::parse_expression] current_token: {:?}", cur_tok);
        let prefix = self.parse_prefix(cur_tok)?;

        return Ok(ast::Statement::ExpressionStatement(Box::new(prefix)));
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement, ParseError> {
        debug!("[parser::parse_expression_statement]");
        let expr = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token() == Token::SEMICOLON {
            self.next_token(); // Consume
        }

        return Ok(expr);
    }

    // fn parse_integer_literal(&mut self) -> Result<Statement, ParseError> {

    //     let val = match self.cur_token() {
    //         Token::INT(v) => v,
    //         _ => Err(ParseError::UnexpectedToken(self.cur_token(), String::from("Nooo")))?,
    //     };

    //     Ok(Statement::ExpressionStatement(Box::new(Expression::IntegerLiteral(val))))
    // }

    fn parse_let_statement(&mut self) -> Result<ast::Statement, ParseError> {
        // For a let statement the next token must be an identifier!
        let ident = self.next_token();
        let ident_ast_node = match ident {
            // Maybe this can be made into a partial eq test, or a macro ?
            Token::IDENT(i) => {
                debug!("[parser::parse_let_statement] parsing identifier {:?}", i);
                ast::Expression::Identifier(i)
            }
            _ => Err(ParseError::UnexpectedToken(
                self.peek_token(),
                String::from("parse let statement - no identifier found"),
            ))?,
        };
        // Consume until semicolon!
        // TODO - ie, no expressions are parsed yet!
        loop {
            if self.cur_token() == Token::SEMICOLON {
                break;
            }
            self.next_token(); // For now - consume all tokens until ';' is encountered.
        }
        Ok(ast::Statement::Let(Box::new(ident_ast_node)))
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement, ParseError> {
        let expr = self.next_token();
        // Skip until semicolon, since we cannot yet parse expressions!
        loop {
            if self.cur_token() == Token::SEMICOLON {
                break;
            }
            self.next_token(); // For now - consume all tokens until ';' is encountered.
            println!("{:?}, parse_return_statement, loop", self.cur_token());
            break;
        }
        // TODO - eventually the ast-tree will parse expressions, and this will become more complicated.
        Ok(ast::Statement::Return(Box::new(ast::Expression::EMPTY)))
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
        debug!("[parser::next_token] Next token! coming up");
        self.lexer.next()
    }

    fn peek_token(&self) -> Token {
        return self.lexer.peek();
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse_let_statement() {
        let lexer = Lexer::new("let a=b;");
        let mut parser = Parser::new(lexer);
        // Parse and loop through the tokens.
        let res = parser.parse();
        assert!(res.is_ok());
        let ast = res.unwrap();
        println!("{:?}", ast);
        // The ast::Program is the root-node of the AST,
        // and thus the result should be something like:
        assert!(ast.statements.len() == 1);
        // Verify the AST structure
        assert_eq!(
            ast.statements[0],
            ast::Statement::Let(Box::new(ast::Expression::Identifier(String::from("a"))))
        );

        // let lexer = Lexer::new("let a=b; let a = b");
    }

    #[test]
    fn test_parse_return_statement() {
        // Test parse Return statement
        let lexer = Lexer::new("return 5;");
        let mut parser = Parser::new(lexer);
        // // Parse and loop through the tokens.
        let res = parser.parse();
        println!("{:?}", res);
        assert!(res.is_ok());
        // TODO - cannot test the value, as the expression
        // parsing is not finished yet.
    }

    #[test]
    fn test_parse_expression_statement() {
        let _ = simple_logger::init();
        // TestExpressionStatement
        let lexer = Lexer::new("foobar;");
        let mut parser = Parser::new(lexer);
        let res = parser.parse();
        println!("{:?}, result", res);
        let prog = res.unwrap();

        assert_eq!(prog.statements.len(), 1);

        // Statement must be an expression statement.
        assert_eq!(
            prog.statements[0],
            ast::Statement::ExpressionStatement(Box::new(ast::Expression::Identifier(
                String::from("foobar")
            )))
        );
    }

    #[test]
    fn test_integer_literal_expression() {
        let _ = simple_logger::init();
        let lexer = Lexer::new("5;");
        let mut parser = Parser::new(lexer);
        let res = parser.parse();
        println!("{:?}, result", res);
        let prog = res.unwrap();

        assert_eq!(prog.statements.len(), 1);

        // Statement must be an expression statement.
        assert_eq!(
            prog.statements[0],
            ast::Statement::ExpressionStatement(Box::new(ast::Expression::IntegerLiteral(5)))
        );
    }

    #[test]
    fn test_parse_prefix_expression() {
        let _ = simple_logger::init();
        let lexer = Lexer::new("-5;");
        let mut parser = Parser::new(lexer);
        let res = parser.parse();
        println!("{:?}, result", res);
        let prog = res.unwrap();

        assert_eq!(prog.statements.len(), 1);

        // Statement must be an expression statement.
        assert_eq!(
            prog.statements[0],
            ast::Statement::ExpressionStatement(Box::new(ast::Expression::Prefix(
                Token::MINUS,
                Box::new(ast::Statement::ExpressionStatement(Box::new(ast::Expression::IntegerLiteral(5))))
            )))
        );
    }
}
