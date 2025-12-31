mod ast;
mod lexer;
mod parser;

fn main() {
    let input = String::from("(x + 10) - 12 * y");
    let tokens = lexer::lex(input);
    for tok in &tokens {
        println!("TokenType: {:?}, Lexeme: {:?}", tok.token_type, tok.lexeme);
    }
}
