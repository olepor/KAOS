

pub enum precedence {
    LOWEST,
    EQUALS, // == LESSGREATER // > or <
    SUM, // +
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL, // myFunction(X)
}
