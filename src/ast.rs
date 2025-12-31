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

pub struct Program {
    statements: Vec<Statement>,
}

pub enum Statement {
    Expression(ExpressionStatement),
}

pub struct ExpressionStatement {
    pub expr: Expression,
}

pub struct PrefixExpression {
    op: Token,
    operand: Box<Expression>,
}

pub struct InfixExpression {
    op: Token,
    left: Box<Expression>,
    right: Box<Expression>,
}

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
