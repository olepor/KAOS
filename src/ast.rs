use std::fmt;

pub struct Node {
    pub ntype: NodeType,
    pub children: Vec<Node>,
}

#[derive(Debug)]
pub enum NodeType {
    Program(Box<Program>),
    Statement(Box<Statement>),
    Expression(Box<Expression>),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        println!("Printing statements in program:");
        for statement in &self.statements {
            println!("statement: {}", statement);
            s.push_str(&statement.to_string());
        }
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Statement {
    Let(Box<Expression>),
    Return(Box<Expression>),
    ExpressionStatement(Box<Expression>),
    Block(Vec<Statement>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Statement::Let(stmt) => format!("{}", stmt),
            Statement::Return(ret) => format!("{}", ret),
            Statement::ExpressionStatement(exp) => format!("{}", exp),
            Statement::Block(stmts) => format!("{:?}", stmts),
        };
        write!(f, "{}", s)
    }
}

use token::Token;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i32),
    // Operator - Right ExpressionStatement
    Prefix(Token, Box<Expression>),
    // Left ExpressionStatement - Operator - Right ExpressionStatement
    Infix(Box<Expression>, Token, Box<Expression>),
    Boolean(bool),
    // condition <Consequence> <Alternative> (Block statements)
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    EMPTY,
    // Infix(Box<InfixExpression>),
    // String(String),
    // If(Box<IfExpression>),
    // Function(Box<FunctionLiteral>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(s) => write!(f, "{}", s),
            Expression::IntegerLiteral(i) => write!(f, "{}", i),
            Expression::Prefix(i, j) => write!(f, "{}:{}", i, j),
            Expression::Infix(i, j, k) => write!(f, "{}:{}:{}", i, j, k),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::If(cond, cons, alt) => write!(f, "{}:{}:{:?}", cond, cons, alt),
            Expression::EMPTY => write!(f, "Empty expression"),
            // Expression::Prefix(pref) => pref.to_string(),
            // Expression::Infix(infix) => infix.to_string(),
            // Expression::String(s) => s.clone(),
            // Expression::If(exp) => exp.to_string(),
            // Expression::Function(f) => f.to_string(),
            // Expression::Call(c) => c.to_string(),
            // Expression::Array(a) => a.to_string(),
            // Expression::Index(i) => i.to_string(),
            // Expression::Hash(h) => h.to_string(),
        }
    }
}
