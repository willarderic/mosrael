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
use crate::lexer::Token;

trait PrefixParselet {
    fn parse(left: Expression, token: Token) -> Expression;
}

trait InfixParselet {
    fn parse(left: Expression, token: Token) -> Expression;
}

trait PostfixParselet {
    fn parse(left: Expression, token: Token) -> Expression;
}

struct Parser {
    tokens: Vec<Token>,
    curr: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, curr: 0 }
    }

    fn peek(&self) -> &Token {
        &self.tokens[curr + 1]
    }

    fn next_token(&self) -> &Token {
        &self.tokens[curr]
    }

    fn prefixParslet(token: &Token) {
        match token.token_type {}
    }

    fn parse(&mut self) -> Expression {}
}
