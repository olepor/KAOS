use std::str::Chars;
use std::collections::HashMap;
use std;
use token::Token;

lazy_static! {
    static ref HASHMAP: HashMap<&'static str, Token> = {
        let mut m : HashMap<&str, Token> = HashMap::new();
        m.insert("function", Token::FUNCTION);
        m.insert("if", Token::IF);
        m.insert("else", Token::ELSE);
        m.insert("return", Token::RETURN);
        m.insert("true", Token::TRUE);
        m.insert("false", Token::FALSE);
        m.insert("let", Token::LET);
        m
    };
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: std::iter::Peekable<Chars<'a>>,
    cur_token: Token,
    peek_token: Token,
}

impl <'a> Lexer <'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut l = Lexer{input: input.chars().peekable(), cur_token: Token::ILLEGAL, peek_token: Token::ILLEGAL};
        l.next();
        l.next();
        l
    }

    pub fn next(& mut self) -> Token {
        let r_tok = self.cur_token.clone();
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.get_next_token();
        return r_tok;
    }

    pub fn peek(& self) -> Token {
        return self.peek_token.clone();
    }


    fn get_next_token(& mut self) -> Token {
        let c = match self.input.next() {
            Some(c) => c,
            None => return Token::EOF,
        };
        match c {
            '=' => {
                match self.input.peek() {
                    Some(&a) => {
                        if a == '=' {
                        self.input.next();
                        return Token::EQ;
                        }
                    },
                    None => {},
                }
                return Token::ASSIGN;
            },
            '+' => Token::PLUS,
            '-' => Token::MINUS,
            '/' => Token::SLASH,
            '*' => Token::ASTERISK,
            '!' => {
                match self.input.peek() {
                    Some(&a) => {
                        if a == '=' {
                            self.input.next();
                            return Token::NOTEQ;
                        }
                    },
                    None => {},
                }
                return Token::BANG;
            },
            '<' => {
                match self.input.peek() {
                    Some(&a) => {
                        if a == '=' {
                            self.input.next();
                            return Token::LTEQ;
                        }
                    },
                    None => {},
                }
                return Token::LT;
            },
            '>' => Token::GT,
            ',' => Token::COMMA,
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            _ => {
                // Branch digit or keyword
                if c.is_numeric() {
                    return self.parse_number(String::from(c.to_string()));
                } else if c.is_whitespace() {
                    // Consume whitespace
                    return self.get_next_token();
                } else {
                    // Keyword
                    assert!(c.is_alphabetic());
                    // TODO - rename this to something sensible!
                    return self.parse_keyword(String::from(c.to_string()));
                }
            },
        }
    }

    fn parse_number(& mut self, mut s: String) -> Token{
        let mut c : char;
        loop {
            c = self.input.next().unwrap_or(' ');
            if c.is_whitespace() || !c.is_numeric() {
                break;
            }
            s.push_str(&c.to_string());
        }
        let digit = s.trim().parse::<i32>().expect("Fatal: parse_number did not parse a numeric");
        return Token::INT(digit);
    }

    fn parse_keyword(& mut self, mut s: String) -> Token {
        let mut c : char;
        loop {
            c = *self.input.peek().unwrap_or(&' ');
            if c.is_whitespace() || !c.is_alphabetic() {
                break;
            }
            s.push_str(&c.to_string());
            self.input.next();
        }
        let tok = HASHMAP.get(s.as_str());
        match tok {
            Some(t) => (*t).clone(),
            None => Token::IDENT(s),
        }
    }

}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next() {
        let mut lexer = Lexer::new("=+-*/!(){},;<>");
        assert_eq!(lexer.next(), Token::ASSIGN);
        assert_eq!(lexer.next(), Token::PLUS);
        assert_eq!(lexer.next(), Token::MINUS);
        assert_eq!(lexer.next(), Token::ASTERISK);
        assert_eq!(lexer.next(), Token::SLASH);
        assert_eq!(lexer.next(), Token::BANG);
        assert_eq!(lexer.next(), Token::LPAREN);
        assert_eq!(lexer.next(), Token::RPAREN);
        assert_eq!(lexer.next(), Token::LBRACE);
        assert_eq!(lexer.next(), Token::RBRACE);
        assert_eq!(lexer.next(), Token::COMMA);
        assert_eq!(lexer.next(), Token::SEMICOLON);
        assert_eq!(lexer.next(), Token::LT);
        assert_eq!(lexer.next(), Token::GT);
        assert_eq!(lexer.next(), Token::EOF);

        // Test double char tokens
        let mut lexer = Lexer::new("== != <=");
        assert_eq!(lexer.next(), Token::EQ);
        assert_eq!(lexer.next(), Token::NOTEQ);
        assert_eq!(lexer.next(), Token::LTEQ);
        assert_eq!(lexer.next(), Token::EOF);

        // Test identifiers
        let mut lexer = Lexer::new("a abc cde");
        assert_eq!(lexer.next(), Token::IDENT("a".to_string()));
        assert_eq!(lexer.next(), Token::IDENT("abc".to_string()));
        assert_eq!(lexer.next(), Token::IDENT("cde".to_string()));
        assert_eq!(lexer.next(), Token::EOF);


        let mut lexer = Lexer::new("let a=b");
        assert_eq!(lexer.next(), Token::LET);
        assert_eq!(lexer.next(), Token::IDENT("a".to_string()));
        assert_eq!(lexer.next(), Token::ASSIGN);
        assert_eq!(lexer.next(), Token::IDENT("b".to_string()));
        assert_eq!(lexer.next(), Token::EOF);


        let mut lexer = Lexer::new("let a=b; let a = b");
        assert_eq!(lexer.next(), Token::LET);
        assert_eq!(lexer.next(), Token::IDENT("a".to_string()));
        assert_eq!(lexer.next(), Token::ASSIGN);
        assert_eq!(lexer.next(), Token::IDENT("b".to_string()));
        assert_eq!(lexer.next(), Token::SEMICOLON);
        assert_eq!(lexer.next(), Token::LET);
        assert_eq!(lexer.next(), Token::IDENT("a".to_string()));
        assert_eq!(lexer.next(), Token::ASSIGN);
        assert_eq!(lexer.next(), Token::IDENT("b".to_string()));
        assert_eq!(lexer.next(), Token::EOF);
    }

    #[test]
    fn test_peek() {
        let mut lexer = Lexer::new("=+-*/!");
        let expects = vec![Token::PLUS, Token::MINUS,
                           Token::ASTERISK, Token::SLASH, Token::BANG];
        for expect in expects.iter() {
            assert_eq!(lexer.peek(), *expect);
            lexer.next();
        }
    }
}
