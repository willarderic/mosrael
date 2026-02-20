use std::fmt::{Display, Formatter, Result};
use std::vec;

use crate::lexer::Token;

/*
program     → decl_list EOF

decl        → fn_decl
            | var_decl ;
decl_list   → epsilon
            | decl decl_list ;
fn_decl     → "fn" ident "(" ")" block ;
var_decl    → "var" ident "=" expr ;
block       → stmt_list ;

stmt        → return_stmt ;
stmt_list   → epsilon
            | stmt stmt_list ";" ;
return_stmt → "return" expr;

expr        → literal
            | unary
            | binary
            | grouping ;

literal     → NUMBER | STRING | IDENT | "true" | "false" | "null" ;
grouping    → "(" expr")" ;
unary       → ( "-" | "!" ) expr ;
binary      → expression operator expression ;
operator    → "==" | "!=" | "<" | "<=" | ">" | ">="
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
                write!(f, "PROGRAM\n").unwrap();
                decls.iter().for_each(|decl| write!(f, "{}", decl).unwrap());

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
    VariableDeclaration(Variable),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub name: String,
    pub stmts: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Variable {
    pub name: String,
    pub value: Expression,
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::FunctionDeclaration(func) => {
                write!(f, "\tFUNCTION({})\n", func.name).unwrap();
                func.stmts
                    .iter()
                    .for_each(|stmt| write!(f, "\t\t{}", stmt).unwrap());
                Ok(())
            }
            Self::VariableDeclaration(var) => write!(f, "\tVAR({}, {})\n", var.name, var.value),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::ReturnStatement(expr) => write!(f, "RETURN({})", expr),
            Self::ExpressionStatement(expr) => write!(f, "EXPR({})", expr),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnaryExpression {
    pub op: Token,
    pub operand: Box<Expression>,
}
//
//#[derive(Clone, Debug, Eq, PartialEq)]
//pub struct InfixExpression {
//    pub op: Token,
//    pub left: Box<Expression>,
//    pub right: Box<Expression>,
//}
//
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Identifier(String),
    Number(u64),
    Unary(UnaryExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Identifier(ident) => write!(f, "{}", ident),
            Self::Number(num) => write!(f, "{}", num),
            Self::Unary(unary) => write!(f, "({}, {})", unary.op, unary.operand),
        }
    }
}
