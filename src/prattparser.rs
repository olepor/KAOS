// A Pratt Parser is slightly different from a regular recursive descent parser,
// as it handles semantics along with the tokens.
use ast::*;

enum Precedence {
    LOWEST,
    EQUALS,  // == LESSGREATER // > or <
    LESSGREATER // < or > TODO - is the above wrong?
    SUM,     // +
    PRODUCT, // *
    PREFIX,  // -X or !X
    CALL,    // myFunction(X)
}

// PrecedenceMap table relates tokens to their precedence
// through a hashmap (lazily initialized)
lazy_static! {
    static ref PrecedenceMap: HashMap<Token, Precedence> = {
        let mut m : HashMap<&str, Token> = HashMap::new();
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

