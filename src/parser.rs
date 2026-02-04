/*
 expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
*/
use crate::ast::{Expression, InfixExpression, PrefixExpression};
use crate::lexer::Token;

type ParseError = String;

pub struct Parser {
    tokens: Vec<Token>,
    curr_token: Token,
    peek_token: Token,
    index: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let mut parser = Parser {
            tokens,
            curr_token: Token::UNKNOWN,
            peek_token: Token::UNKNOWN,
            index: 0,
            errors: Vec::new(),
        };

        parser.advance();
        parser.advance();

        parser
    }

    fn advance(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = match self.tokens.get(self.index) {
            Some(token) => token.clone(),
            None => Token::EOF,
        };
        self.index += 1;
    }

    fn parse_identifier_expression(&mut self) -> Result<Expression, ParseError> {
        Ok(Expression::Identifier(self.curr_token.get_literal()))
    }

    fn parse_number_expression(&mut self) -> Result<Expression, ParseError> {
        Ok(Expression::Number(
            self.curr_token.get_literal().parse::<u64>().unwrap(),
        ))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let op = self.curr_token.clone();
        self.advance();
        let operand = self.parse_expression();
        Ok(Expression::Prefix(PrefixExpression {
            op,
            operand: Box::new(operand.unwrap()),
        }))
    }

    fn parse_binary_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let op = self.curr_token.clone();
        self.advance();
        let right = self.parse_expression();
        Ok(Expression::Infix(InfixExpression {
            op,
            left: Box::new(left),
            right: Box::new(right.unwrap()),
        }))
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // Prefix
        let mut left = match self.curr_token {
            Token::IDENTIFIER(_) => self.parse_identifier_expression(),
            Token::NUMBER(_) => self.parse_number_expression(),
            Token::DASH => self.parse_prefix_expression(),
            _ => return Err(String::from("No matching prefix parselet")),
        };

        self.advance();
        // Infix
        left = match self.curr_token {
            Token::PLUS | Token::DASH => self.parse_binary_expression(left.unwrap()),
            Token::ASTERISK | Token::SLASH => self.parse_binary_expression(left.unwrap()),
            _ => left, // not an infix expression
        };

        left
    }
}
