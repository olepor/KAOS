use lexer::Lexer;
use token::Token;
use ast::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }

    pub fn parse(&mut self) -> Option<Program> {
        return self.parse_program();
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut prog = Program {
            statements: Vec::new(),
        };
        loop {
            let statement = self.parse_statement();
            prog.statements.push(Box::new(statement));
        }
        return Option::Some(prog);
    }

    fn parse_statement(&mut self) -> impl Statement {
        let n_tok = self.peek_token();
        match n_tok {
            Token::LET => {
                let var = self.parse_variable();
                return var;
            },
            _ => panic!("Wrong token: {:?}", n_tok),
        }
    }

    fn parse_variable(&mut self) -> Variable {

        self.next_token().expect_token(Token::LET, "error - expected let statement");

        let ident = self.next_token().expect_token(Token::IDENT("a".to_string()), "error - expected identifier");

        self.next_token().expect_token(Token::ASSIGN, "error - expected assign");

        let expr = self.parse_expression();

        return Variable{identifier: ident, value: Box::new(expr)};
    }

    fn parse_expression(&mut self) -> impl Expression {
        return Number{identifier: Token::INT(5)};
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
