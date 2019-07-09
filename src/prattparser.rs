// A Pratt Parser is slightly different from a regular recursive descent parser,
// as it handles semantics along with the tokens.
use ast::*;

enum Precedence {
        LOWEST,
        EQUALS, // == LESSGREATER // > or <
        SUM, // +
        PRODUCT, // *
        PREFIX, // -X or !X
        CALL, // myFunction(X)
    }
