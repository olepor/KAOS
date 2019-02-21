use std::str::Chars;

#[derive(Debug, PartialEq)]
enum Token{
    // Specials
    ILLEGAL,
    EOF,

    // Identifiers.
    IDENT(String), // ADD, FOOBAR, X, Y ...
    INT(i32), // 12345 ...

    // Operators

    // Binary
    ASSIGN,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    BANG,
    LT,
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

struct Lexer<'a> {
    input: Chars<'a>,
}

impl <'a> Lexer <'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        let l = Lexer{input: input.chars()};
        l
    }

    fn next(& mut self) -> Token {
        let c = match self.input.next() {
            Some(c) => c,
            None => return Token::EOF,
        };
        match c {
            '=' => Token::EQ,
            '+' => Token::PLUS,
            '-' => Token::MINUS,
            '/' => Token::SLASH,
            '*' => Token::ASTERISK,
            '!' => Token::BANG,
            '<' => Token::LT,
            '>' => Token::GT,
            ',' => Token::COMMA,
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            _ => Token::ILLEGAL,
        }
    }

}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next() {
        let mut lexer = Lexer::new("=");
        assert_eq!(lexer.next(), Token::ASSIGN);
    }
}
