use token::Token;

pub struct Program {
    pub statements: Vec<Statement>,
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
