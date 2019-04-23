#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Token{
    // Specials
    ILLEGAL,
    EOF,
    UNINITIALIZED,

    // Identifiers.
    IDENT(String), // ADD, FOOBAR, X, Y ...
    INT(i32), // 12345 ...
    LET,

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

use parser::ParseError;
use std::mem;

impl Token {
    pub fn expect_token(self, expected_token: Token, msg:&'static str) -> std::result::Result<Token, ParseError> {
        if mem::discriminant(&self) == mem::discriminant(&expected_token) {
            Ok(self)
        } else {
            Err(ParseError::UnexpectedToken(self, msg))
        }
    }

}

use std::fmt;

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?})", self)
    }
}

#[cfg(test)]
mod tests {
    
    use super::*;

    #[test]
    fn test_expect_token() {
        let tokA = Token::INT(4);
        let tokB = Token::INT(5);
        tokA.expect_token(tokB, &"some msg");
    }
}
