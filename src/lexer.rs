use std::str::Chars;
use std::collections::HashMap;
use std;

#[derive(Debug, PartialEq, Clone)]
pub enum Token{
    // Specials
    ILLEGAL,
    EOF,

    // Identifiers.
    IDENT(String), // ADD, FOOBAR, X, Y ...
    INT(i32), // 12345 ...

    /////////////
    // Operators
    /////////////

    // Binary
    ASSIGN,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    BANG,
    LT,
    LTEQ,
    GT,
    EQ,
    NOTEQ,

    // Delimeters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    IF,
    ELSE,
    RETURN,
    TRUE,
    FALSE,

}

lazy_static! {
    static ref HASHMAP: HashMap<&'static str, Token> = {
        let mut m : HashMap<&str, Token> = HashMap::new();
        m.insert("function", Token::FUNCTION);
        m.insert("if", Token::IF);
        m.insert("else", Token::ELSE);
        m.insert("return", Token::RETURN);
        m.insert("true", Token::TRUE);
        m.insert("false", Token::FALSE);
        m
    };
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: std::iter::Peekable<Chars<'a>>,
}

impl <'a> Lexer <'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer{input: input.chars().peekable()}
    }

    pub fn next(& mut self) -> Token {
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
                    return self.next();
                } else {
                    // Keyword
                    assert!(c.is_alphabetic());
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
            c = self.input.next().unwrap_or(' ');
            if c.is_whitespace() || !c.is_alphabetic() {
                break;
            }
            s.push_str(&c.to_string());
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
    }
}
