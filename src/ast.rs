use token::Token;

pub struct Program {
    pub statements: Vec<Box<Statement>>,
}

pub trait Statement {
    fn statement_node(& self);
}

pub trait Expression {
    fn expression_node(& self);
}

pub struct  Variable {
    pub identifier: Token,
    pub value: Box<Expression>,
}

impl Statement for Variable {
    fn statement_node(&self) {}
}

#[derive(Debug, PartialEq)]
pub struct  Number {
    pub identifier: Token,
}

impl Expression for Number {
    fn expression_node(& self) {}
}
