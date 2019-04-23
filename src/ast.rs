use token::Token;
use std::fmt;

pub struct Program {
    pub statements: Vec<Box<Statement>>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        println!("Printing statements in program:");
        for statement in &self.statements {
            println!("statement: {}", statement);
            // s.push_str(&statement.to_string());
        }
        write!(f, "{}", s)
    }
}

pub trait Statement : fmt::Display {
    fn statement_node(& self);
}

pub trait Expression : fmt::Display {
    fn expression_node(& self);
}

pub struct  Variable {
    pub identifier: Token,
    pub value: Box<Expression>,
}

impl Statement for Variable {
    fn statement_node(&self) {}
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.identifier, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct  Number {
    pub identifier: Token,
}

impl Expression for Number {
    fn expression_node(& self) {}
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct IFStatement {
    pub identifier: Token,
    pub value: Condition, // The condition value.
    pub body_statements: Vec<Box<Statement>>,
}

impl fmt::Display for IFStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {:?}", self.identifier, self.value)
    }
}

impl Statement for IFStatement {
    fn statement_node(& self) {}
}

#[derive(Debug)]
pub struct Condition {
    pub identifier: Token,
}

pub struct ExpressionStatement {
    pub identifier: Token,
    pub expression: Vec<Box<Expression>>,
}

impl Statement for ExpressionStatement {
    fn statement_node(& self) {}
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {:?}", self.identifier, self.expression)
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub identifier: Token,
}

impl Statement for Identifier {
    fn statement_node(& self) {}
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {:?}", self.identifier, {
            match self.identifier {
                Token::IDENT(val) => val,
                _ => panic!("Unexpected token"),
            }
        })
    }
}

