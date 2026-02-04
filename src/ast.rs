use std::fmt::{Display, Formatter, Result};
use std::vec;

use crate::lexer::Token;

pub enum Node {
    //    Program(Program),
    //    Statement(Statement),
    Expression(Expression),
}

//impl Display for Node {
//    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
//        match self {
//            Self::Program(program) => write!(f, "{}", program),
//            Self::Statement(statement) => write!(f, "{}", statement),
//            Self::Expression(expression) => write!(f, "{}", expression),
//        }
//    }
//}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
    statements: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Expression(ExpressionStatement),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExpressionStatement {
    pub expr: Expression,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PrefixExpression {
    pub op: Token,
    pub operand: Box<Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InfixExpression {
    pub op: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Identifier(String),
    Number(u64),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Identifier(ident) => write!(f, "{}", ident),
            Self::Number(num) => write!(f, "{}", num),
            Self::Prefix(prefix) => write!(f, "({}, {})", prefix.op, prefix.operand),
            Self::Infix(infix) => write!(f, "({}, {}, {})", infix.op, infix.left, infix.right),
        }
    }
}
