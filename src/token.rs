use std::mem::Discriminant;

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
    pub fn expect_token_type(&self, expected_token: Discriminant<Token>) -> std::result::Result<(), ParseError> {
        if mem::discriminant(self) == expected_token {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken(self.clone()))
        }
    }

    pub fn is_identifier(&self) -> bool {
        mem::discriminant(self) == Token::IDENT(String::from("a"))
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
        tokA.expect_token_type(tokB);
    }
}
