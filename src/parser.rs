use ast;
use lexer::Lexer;
use log::*;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::result::Result;
use token::Token;
// Current progress: Pratt-parsing of if-statements.
// Not done: Parsing of expressions for return statements.
// Next topic: if-statements

#[derive(Clone, PartialEq, PartialOrd, Debug)]
enum Precedence {
    LOWEST,
    EQUALS,      // == LESSGREATER // > or <
    LESSGREATER, // < or > TODO - is the above wrong?
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
}

// PrecedenceMap table relates tokens to their precedence
// through a hashmap (lazily initialized)
lazy_static! {
    static ref PRECEDENCEMAP: HashMap<Token, Precedence> = {
        let mut m: HashMap<Token, Precedence> = HashMap::new();
        m.insert(Token::EQ, Precedence::EQUALS);
        m.insert(Token::NOTEQ, Precedence::EQUALS);
        m.insert(Token::LT, Precedence::LESSGREATER);
        m.insert(Token::GT, Precedence::LESSGREATER);
        m.insert(Token::PLUS, Precedence::SUM);
        m.insert(Token::MINUS, Precedence::SUM);
        m.insert(Token::SLASH, Precedence::PRODUCT);
        m.insert(Token::ASTERISK, Precedence::PRODUCT);
        m
    };
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

    pub fn parse(&mut self) -> Result<ast::Program, ParseError> {
        return self.parse_program();
    }

    fn parse_program(&mut self) -> Result<ast::Program, ParseError> {
        debug!("[parser::parse_program] Starting the parser...");
        let mut prog = ast::Program {
            statements: Vec::new(),
        };
        self.next_token(); // Otherwise the first token is uninitialized
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
        debug!(
            "[parser::parse_statement] current token: {:?}",
            self.cur_token()
        );
        match self.cur_token() {
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
            Token::TRUE => Ok(ast::Expression::Boolean(true)),
            Token::FALSE => Ok(ast::Expression::Boolean(false)),
            Token::LPAREN => self.parse_grouped_expression(),
            Token::BANG => {
                // Parse the following expression
                self.next_token();
                let right = self.parse_expression(Precedence::PREFIX)?;
                Ok(ast::Expression::Prefix(Token::BANG, Box::new(right)))
            }
            Token::MINUS => {
                // Parse the following expression
                self.next_token();
                let right = self.parse_expression(Precedence::PREFIX)?;
                Ok(ast::Expression::Prefix(Token::MINUS, Box::new(right)))
            }
            Token::IF => {
                self.next_token();
                let if_exp = self.parse_if_statement()?;
                Ok(if_exp)
            }
            _ => {
                debug!("[parser::parse_prefix] Unknown prefix: {:?}", token);
                Err(ParseError::UnImplemented)
            }
        }
    }

    fn parse_infix(
        &mut self,
        left: ast::Expression,
        token: Token,
    ) -> Result<ast::Expression, ParseError> {
        debug!("[parser:parse_infix] left: {:?}, token: {:?}", left, token);
        // Register for which tokens the parse_infix should be ran
        match token {
            Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASTERISK
            | Token::EQ
            | Token::NOTEQ
            | Token::LT
            | Token::GT => {
                let operator = self.cur_token();
                let precedence = self.cur_precedence();
                self.next_token();
                let right = self.parse_expression(precedence)?;
                debug!(
                    "[parser::parse_infix] returning: left: {:?}, right: {:?}",
                    left, right
                );
                Ok(ast::Expression::Infix(
                    Box::new(left),
                    operator,
                    Box::new(right),
                ))
            }
            _ => {
                debug!("[parser::parse_infix] unrecognized token: {:?}", token);
                Err(ParseError::UnImplemented)
            }
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression, ParseError> {
        debug!("[parser::parse_expression] precedence: {:?}", precedence);
        let cur_tok = self.cur_token();
        debug!("[parser::parse_expression] current_token: {:?}", cur_tok);
        let mut left_exp = self.parse_prefix(cur_tok.clone())?;
        loop {
            let peek_token = self.peek_token();
            debug!("[parser::parse_expression] precence: {:?}, peek_token: {:?}, peek_precedence: {:?}", precedence, peek_token, self.peek_precedence());
            if peek_token != Token::SEMICOLON && precedence < self.peek_precedence() {
                let peek_tok = self.peek_token();
                self.next_token(); // Consume
                let left = self.parse_infix(left_exp.clone(), peek_tok);
                match left {
                    Err(_) => {
                        // Failed, return infix
                        debug!("[parser::parse_expression] infix parse failed: {:?}", left);
                        // left_exp = left;
                        return Ok(left_exp);
                    }
                    Ok(l) => {
                        debug!("[parser::parse_expression] left - parse_infix: {:?}", l);
                        left_exp = l;
                    }
                }
            // left = self.parse_infix(left, cur_tok.clone())?; // Recurse
            } else {
                debug!("[parser::parse_expression] breaking out of parsing loop");
                break;
            }
        }
        return Ok(left_exp);
    }

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression, ParseError> {
        debug!("[parser::parse_grouped_expression]");
        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST)?;
        if self.peek_token() != Token::RPAREN {
            // TODO - how to handle this case?
            debug!("[parser::parse_grouped_expression]: peek token is not RPAREN. How to handle?");
            return Err(ParseError::UnImplemented);
        }
        debug!("[parser::parse_grouped_expression] expression: {:?}", exp);
        self.next_token(); // Consume RPAREN. Is this correct though?
        Ok(exp)
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement, ParseError> {
        debug!("[parser::parse_expression_statement]");
        let expr = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token() == Token::SEMICOLON {
            self.next_token(); // Consume
        }

        return Ok(ast::Statement::ExpressionStatement(Box::new(expr)));
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement, ParseError> {
        debug!("[parser::parse_let_statement]");
        // For a let statement the next token must be an identifier!
        let ident = self.cur_token();
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

    fn parse_if_statement(&mut self) -> Result<ast::Expression, ParseError> {
        if self.peek_token() != Token::LPAREN {
            // TODO - how to handle this?
            Err(ParseError::UnexpectedToken(
                self.peek_token(),
                String::from("Expected '('"),
            ))?
        }
        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST)?;
        if self.peek_token() != Token::RPAREN {
            Err(ParseError::UnexpectedToken(
                self.peek_token(),
                String::from("Expected ')'"),
            ))?
        }
        self.next_token();
        if self.peek_token() != Token::LBRACE {
            Err(ParseError::UnexpectedToken(
                self.peek_token(),
                String::from("Expected '{'"),
            ))?
        }
        let consequence = self.parse_block_statement()?;
        let mut alternative: Option<Box<ast::Statement>> = None;
        if self.peek_token() == Token::ELSE {
            self.next_token();
            if self.peek_token() != Token::LBRACE {
                Err(ParseError::UnexpectedToken(
                    self.peek_token(),
                    String::from("Expected '('"),
                ))?
            }
            let tmp = self.parse_block_statement()?;
            alternative = Some(Box::new(tmp));
        }
        Ok(ast::Expression::If(Box::new(condition), Box::new(consequence), alternative))
    }

    fn parse_block_statement(&mut self) -> Result<ast::Statement, ParseError> {
        let mut stmts: Vec<ast::Statement> = Vec::new();
        self.next_token();
        while self.cur_token() != Token::RBRACE && self.cur_token() != Token::EOF {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            self.next_token();
        }
        Ok(ast::Statement::Block(stmts))
    }

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

    // cur_precedence returns the precedence of the current token
    fn cur_precedence(&self) -> Precedence {
        let prec = PRECEDENCEMAP.get(&self.cur_token());
        debug!(
            "[parser::cur_precedence] Token: {:?} - Precedence {:?}",
            self.cur_token(),
            prec
        );
        match prec {
            Some(p) => (*p).clone(),
            None => Precedence::LOWEST,
        }
    }

    // peek_precedence returns the precedence of the peek token
    fn peek_precedence(&self) -> Precedence {
        debug!("[parser::peek_precedence]");
        let prec = PRECEDENCEMAP.get(&self.peek_token());
        match prec {
            Some(p) => (*p).clone(),
            None => Precedence::LOWEST,
        }
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
                Box::new(ast::Expression::IntegerLiteral(5))
            )))
        );
        let lexer = Lexer::new("5 + 5 + 5;");
        let mut parser = Parser::new(lexer);
        let res = parser.parse();
        println!("{:?}, result", res);
        let prog = res.unwrap();

        assert_eq!(prog.statements.len(), 1);

        // Statement must be an expression statement.
        assert_eq!(
            prog.statements[0],
            ast::Statement::ExpressionStatement(Box::new(ast::Expression::Infix(
                Box::new(ast::Expression::Infix(
                    Box::new(ast::Expression::IntegerLiteral(5)),
                    Token::PLUS,
                    Box::new(ast::Expression::IntegerLiteral(5))
                )),
                Token::PLUS,
                Box::new(ast::Expression::IntegerLiteral(5))
            )))
        );
    }

    #[test]
    fn test_parse_infix_expressions() {
        let _ = simple_logger::init();
        let lexer = Lexer::new("5 + 5;");
        let mut parser = Parser::new(lexer);
        let res = parser.parse();
        println!("{:?}, result", res);
        let prog = res.unwrap();

        assert_eq!(prog.statements.len(), 1);

        // Statement must be an expression statement.
        assert_eq!(
            prog.statements[0],
            ast::Statement::ExpressionStatement(Box::new(ast::Expression::Infix(
                Box::new(ast::Expression::IntegerLiteral(5)),
                Token::PLUS,
                Box::new(ast::Expression::IntegerLiteral(5))
            )))
        );
    }

    #[test]
    fn test_parse_boolean_expression() {
        let _ = simple_logger::init();
        let lexer = Lexer::new("true;");
        let mut parser = Parser::new(lexer);
        let res = parser.parse();
        println!("{:?}, result", res);
        let prog = res.unwrap();

        assert_eq!(prog.statements.len(), 1);

        // Statement must be an expression statement.
        assert_eq!(
            prog.statements[0],
            ast::Statement::ExpressionStatement(Box::new(ast::Expression::Boolean(true)))
        );
    }

    #[test]
    fn test_parse_grouped_expression() {
        let _ = simple_logger::init();
        let lexer = Lexer::new("(1+2)*3;");
        let mut parser = Parser::new(lexer);
        let res = parser.parse();
        println!("{:?}, result", res);
        let prog = res.unwrap();

        assert_eq!(prog.statements.len(), 1);

        // Statement must be an expression statement.
        assert_eq!(
            prog.statements[0],
            ast::Statement::ExpressionStatement(Box::new(ast::Expression::Infix(
                Box::new(ast::Expression::Infix(
                    Box::new(ast::Expression::IntegerLiteral(1)),
                    Token::PLUS,
                    Box::new(ast::Expression::IntegerLiteral(2))
                )),
                Token::ASTERISK,
                Box::new(ast::Expression::IntegerLiteral(3))
            )))
        );
    }

    #[test]
    fn test_parse_if_statements() {
        let _ = simple_logger::init();
        let lexer = Lexer::new("if (2 > 3) { x } else { y };");
        let mut parser = Parser::new(lexer);
        let res = parser.parse();
        println!("{:?}, result", res);
        let prog = res.unwrap();

        assert_eq!(prog.statements.len(), 1);

        // Statement must be an expression statement.
        assert_eq!(
            prog.statements[0],
            ast::Statement::ExpressionStatement(Box::new(ast::Expression::If(
                Box::new(ast::Expression::Boolean(false)),
                Box::new(ast::Statement::Block(vec!(
                    ast::Statement::ExpressionStatement(Box::new(ast::Expression::Identifier(
                        String::from("x")
                    )))
                ))),
                Some(Box::new(ast::Statement::Block(vec!(
                    ast::Statement::ExpressionStatement(Box::new(ast::Expression::Identifier(
                        String::from("y")
                    )))
                ))))
            )))
        );
    }
}
