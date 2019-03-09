#[derive(Debug, PartialEq, Clone)]
pub enum Token{
    // Specials
    ILLEGAL,
    EOF,

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

impl Token {
    pub fn expect_token(self, expected_token: Token, msg: &str) -> Token {
        match self {
            expected_token => expected_token,
            _ => Token::ILLEGAL,
        }
    }

}
