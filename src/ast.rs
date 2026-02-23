use std::fmt::{Display, Formatter, Result};
use std::vec;

use crate::lexer::Token;

/*
program     → gdecl_list EOF
gdecl       → fn_decl
            | type_decl
            | var_decl ;
gdecl_list  → epsilon
            | gdecl ";" decl_list ;
fn_decl     → "fn" IDENT "(" ")" fn_return block ;
fn_return   → "->" expr_list ;
block       → decl_list;

decl        → var_decl
            | stmt ;
decl_list   → decl ";" decl_list ;
var_decl    → "var" IDENT var_type "=" expr
            | "var" IDENT var_type
            | "var" IDENT "=" expr ;
short_var_decl → IDENT "=" expr ;
var_type    → epsilon
            | IDENT ;
type        → IDENT
            | IDENT "struct" "{" field_decl_list "}"
field_decl  → IDENT type
field_decl_list      → epsilon
            | field_decl ";" field_decl_list;
stmt        → decl
            | for_stmt
            | if_stmt
            | expr_stmt
            | return_stmt
            | short_var_decl ;
for_stmt    → "for" expr block
            | "for" expr ";" expr ";" expr block ;
if_stmt     → "if" expr block then_arm ;
then_arm    → "else" block ;
return_stmt → "return" expr_list;
expr_stmt   → expr ";" ;
expr        → literal
            | unary
            | binary
            | grouping
            | call ;
expr_list   → epsilon
            | expr "," expr_list
literal     → NUMBER | STRING | IDENT | "true" | "false" | "null" ;
grouping    → "(" expr ")" ;
unary       → ( "-" | "~" | "!" | "*" ) expr ;
binary      → expr op expr ;
op          → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
call        → IDENT "(" expr_list ")" ;
*/
pub enum Node {
    Program(Vec<GlobalDeclaration>),
    GlobalDeclaration(GlobalDeclaration),
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
            Self::GlobalDeclaration(gdecl) => write!(f, "{}", gdecl),
            Self::Declaration(decl) => write!(f, "{}", decl),
            Self::Statement(stmt) => write!(f, "{}", stmt),
        }
    }
}

// Create separate "top" level declarations and inside block declarations

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GlobalDeclaration {
    FunctionDeclaration(Function),
    VariableDeclaration(Variable),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Declaration {
    VariableDeclaration(Variable),
    Statement(Statement),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub name: String,
    pub decls: Vec<Declaration>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Variable {
    pub name: String,
    pub typ: Option<String>,
    pub value: Option<Expression>,
}

impl Display for GlobalDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::FunctionDeclaration(func) => {
                write!(f, "\tFUNCTION({})\n", func.name).unwrap();
                func.decls
                    .iter()
                    .for_each(|stmt| write!(f, "\t{}\n", stmt).unwrap());
                Ok(())
            }
            Self::VariableDeclaration(var) => {
                let mut s = format!("\tVAR({}", var.name);
                let s = match &var.typ {
                    Some(t) => format!("{}, {}", s, t),
                    None => format!("{}, None", s),
                };

                let s = match &var.value {
                    Some(v) => format!("{}, {})", s, v),
                    None => format!("{}, None)", s),
                };
                write!(f, "{}", s)
            }
        }
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::VariableDeclaration(var) => {
                let mut s = format!("\tVAR({}", var.name);
                let s = match &var.typ {
                    Some(t) => format!("{}, {}", s, t),
                    None => format!("{}, None", s),
                };

                let s = match &var.value {
                    Some(v) => format!("{}, {})", s, v),
                    None => format!("{}, None)", s),
                };
                write!(f, "{}", s)
            }
            Self::Statement(stmt) => write!(f, "\tSTATEMENT({})", stmt),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    ForStatement(For),
    IfStatement(),
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
    ShortVarDeclStatement(Variable),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct For {
    pub pre: Option<Box<Statement>>,
    pub cond: Expression,
    pub post: Option<Box<Statement>>,
    pub block: Vec<Declaration>,
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::ReturnStatement(expr) => write!(f, "RETURN({})", expr),
            Self::ExpressionStatement(expr) => write!(f, "EXPR({})", expr),
            Self::ForStatement(for_stmt) => {
                let mut s = String::from("FOR(");
                if let Some(pre) = &for_stmt.pre {
                    s = format!("{}PRE: {},", s, pre);
                };
                s = format!("{}, COND: {}", s, &for_stmt.cond);
                if let Some(post) = &for_stmt.post {
                    s = format!("{}, POST: {}", s, post);
                }
                write!(f, "{})\n", s);
                for_stmt
                    .block
                    .iter()
                    .for_each(|stmt| write!(f, "\t\t{}\n", stmt).unwrap());

                Ok(())
            }
            Self::ShortVarDeclStatement(var) => {
                let mut s = format!("SHORTVAR({}", var.name);
                let s = match &var.typ {
                    Some(t) => format!("{}, {}", s, t),
                    None => format!("{}, None", s),
                };

                let s = match &var.value {
                    Some(v) => format!("{}, {})", s, v),
                    None => format!("{}, None)", s),
                };
                write!(f, "{}", s)
            }
            Self::IfStatement() => write!(f, "IF"),
        }
    }
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
pub struct CallExpression {
    pub func: String,
    pub args: Vec<Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Identifier(String),
    Number(u64),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Call(CallExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Identifier(ident) => write!(f, "{}", ident),
            Self::Number(num) => write!(f, "{}", num),
            Self::Prefix(prefix) => write!(f, "({}, {})", prefix.op, prefix.operand),
            Self::Infix(infix) => write!(f, "({}, {}, {})", infix.op, infix.left, infix.right),
            Self::Call(call) => {
                write!(f, "CALL {}(", call.func).unwrap();
                call.args
                    .iter()
                    .for_each(|arg| write!(f, "{},", arg).unwrap());
                write!(f, ")").unwrap();

                Ok(())
            }
        }
    }
}
