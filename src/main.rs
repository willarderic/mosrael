mod ast;
mod lexer;
mod parser;

use crate::parser::Parser;

fn main() {
    let input = String::from("x + 10 - 12");
    let tokens = lexer::lex(input);
    for tok in &tokens {
        println!("{}", tok);
    }
    let mut parser = Parser::new(tokens);
    let expr = parser.parse_expression();
    println!("{}", expr.unwrap());
}
