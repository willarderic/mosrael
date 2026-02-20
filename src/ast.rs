use std::fmt::{Display, Formatter, Result};
use std::vec;

use crate::lexer::Token;
/*
program     → decl_list EOF
decl        → fn_decl
decl_list   → epsilon
            | decl decl_list ;
fn_decl     → "fn" ident "(" ")" block ;
block       → stmt_list ;
stmt        → return_stmt ;
stmt_list   → epsilon
            | stmt stmt_list ";" ;
return_stmt → "return" integer ;




expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | IDENT | "true" | "false" | "null" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
*/
pub enum Node {
    Program(Vec<Declaration>),
    Declaration(Declaration),
    Statement(Statement),
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Program(decls) => {
                decls
                    .iter()
                    .for_each(|decl| write!(f, "{}\n", decl).unwrap());

                Ok(())
            }
            Self::Declaration(decl) => write!(f, "{}", decl),
            Self::Statement(stmt) => write!(f, "{}", stmt),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Declaration {
    FunctionDeclaration(Function),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub name: String,
    pub stmts: Vec<Statement>,
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::FunctionDeclaration(func) => {
                write!(f, "\t{}\n", func.name).unwrap();
                func.stmts
                    .iter()
                    .for_each(|stmt| write!(f, "\t\t{}\n", stmt).unwrap());
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    ReturnStatement(u64),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::ReturnStatement(num) => write!(f, "RETURN({})", num),
        }
    }
}
//#[derive(Clone, Debug, Eq, PartialEq)]
//pub struct ExpressionStatement {
//    pub expr: Expression,
//}
//
//#[derive(Clone, Debug, Eq, PartialEq)]
//pub struct PrefixExpression {
//    pub op: Token,
//    pub operand: Box<Expression>,
//}
//
//#[derive(Clone, Debug, Eq, PartialEq)]
//pub struct InfixExpression {
//    pub op: Token,
//    pub left: Box<Expression>,
//    pub right: Box<Expression>,
//}
//
//#[derive(Clone, Debug, Eq, PartialEq)]
//pub enum Expression {
//    Identifier(String),
//    Number(u64),
//    Prefix(PrefixExpression),
//    Infix(InfixExpression),
//}
//
//impl Display for Expression {
//    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
//        match self {
//            Self::Identifier(ident) => write!(f, "{}", ident),
//            Self::Number(num) => write!(f, "{}", num),
//            Self::Prefix(prefix) => write!(f, "({}, {})", prefix.op, prefix.operand),
//            Self::Infix(infix) => write!(f, "({}, {}, {})", infix.op, infix.left, infix.right),
//        }
//    }
//}
