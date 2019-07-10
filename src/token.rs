
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

use std::fmt;

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?})", self)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_expect_token() {
        // let tokA = Token::INT(4);
        // let tokB = Token::INT(5);
        // tokA.expect_token_type(Discriminant(tokB));
    }
}
