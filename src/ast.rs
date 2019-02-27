use token::Token;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Variable>,
}

#[derive(Debug, PartialEq)]
pub struct  Variable {
    pub identifier: Token,
    pub value: Expression,
}

pub struct  Statement {
    
}

#[derive(Debug, PartialEq)]
pub struct  Expression {
    pub identifier: Token,
}
