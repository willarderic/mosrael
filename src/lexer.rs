use std::iter;
use std::iter::Peekable;
use std::vec;

#[derive(Debug)]
pub enum TokenType {
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,
    SEMICOLON,
    COLON,
    // operators
    PLUS,
    DASH,
    ASTERISK,
    SLASH,
    ASSIGN,
    BANG,
    BANGEQ,
    LT,
    GT,
    LEQ,
    GEQ,
    QUESTION,
    IDENTIFIER,
    FN,
    IF,
    RETURN,
    NUMBER,
    UNKNOWN,
}

pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String) -> Token {
        Token { token_type, lexeme }
    }
}

fn tokentype_from(lexeme: &str) -> TokenType {
    match lexeme {
        "<=" => TokenType::LEQ,
        ">=" => TokenType::GEQ,
        "!=" => TokenType::BANGEQ,
        "<" => TokenType::LT,
        ">" => TokenType::GT,
        "!" => TokenType::BANG,
        ":" => TokenType::COLON,
        "+" => TokenType::PLUS,
        "-" => TokenType::DASH,
        "/" => TokenType::SLASH,
        "*" => TokenType::ASTERISK,
        "{" => TokenType::LBRACE,
        "}" => TokenType::RBRACE,
        ")" => TokenType::RPAREN,
        "(" => TokenType::LPAREN,
        "[" => TokenType::LBRACKET,
        "]" => TokenType::RBRACKET,
        "?" => TokenType::QUESTION,
        ";" => TokenType::SEMICOLON,
        "=" => TokenType::ASSIGN,
        _ => TokenType::UNKNOWN,
    }
}

fn is_punctuator(c: char) -> bool {
    match c {
        '(' => true,
        ')' => true,
        '[' => true,
        ']' => true,
        '{' => true,
        '}' => true,
        '+' => true,
        '-' => true,
        '/' => true,
        '*' => true,
        '=' => true,
        '<' => true,
        '>' => true,
        '?' => true,
        ':' => true,
        ';' => true,
        _ => false,
    }
}

fn is_digit(c: char) -> bool {
    '0' <= c && c <= '9'
}

fn is_alpha(c: char) -> bool {
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
}

fn is_alphanumeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}

fn is_whitespace(c: char) -> bool {
    match c {
        ' ' => true,
        '\t' => true,
        '\n' => true,
        '\r' => true,
        _ => false,
    }
}

fn punctuator_eq<I: Iterator<Item = char>>(c: char, chars: &mut Peekable<I>) -> Token {
    let peeked: char = match chars.peek() {
        Some(x) => x.clone(),
        None => '\0',
    };

    if peeked == '=' {
        let peeked = chars.next().unwrap();
        let lexeme: String = format!("{c}{peeked}");
        return Token::new(tokentype_from(lexeme.as_str()), lexeme);
    } else {
        let lexeme: String = c.to_string();
        return Token::new(tokentype_from(lexeme.as_str()), lexeme);
    }
}

fn handle_punctuator<I: Iterator<Item = char>>(c: char, chars: &mut Peekable<I>) -> Token {
    match c {
        '>' => punctuator_eq(c, chars),
        '<' => punctuator_eq(c, chars),
        '!' => punctuator_eq(c, chars),
        _ => {
            let lexeme: String = c.to_string();
            Token::new(tokentype_from(lexeme.as_str()), lexeme)
        }
    }
}

fn handle_number<I: Iterator<Item = char>>(c: char, chars: &mut Peekable<I>) -> Token {
    let mut number: String = String::from(c);
    while is_digit(peek(chars)) {
        let n = chars.next().unwrap();
        if !is_digit(n) {
            break;
        }
        number.push(n);
    }
    Token::new(TokenType::NUMBER, number)
}

fn handle_keyword(word: &str) -> Option<Token> {
    match word {
        "if" => Some(Token::new(TokenType::IF, String::from("if"))),
        "return" => Some(Token::new(TokenType::RETURN, String::from("return"))),
        "fn" => Some(Token::new(TokenType::FN, String::from("fn"))),
        _ => None,
    }
}

fn peek<I: Iterator<Item = char>>(chars: &mut Peekable<I>) -> char {
    match chars.peek() {
        Some(x) => *x,
        None => '\0',
    }
}

pub fn lex(input: String) -> Vec<Token> {
    let mut tokens = Vec::new();

    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        if is_punctuator(c) {
            tokens.push(handle_punctuator(c, &mut chars));
            continue;
        }

        if is_digit(c) {
            tokens.push(handle_number(c, &mut chars));
            continue;
        }

        if is_alpha(c) {
            let mut word = String::from(c);
            while is_alphanumeric(peek(&mut chars)) {
                let c = chars.next().unwrap();
                word.push(c);
            }
            tokens.push(match handle_keyword(word.as_str()) {
                Some(tok) => tok,
                None => Token::new(TokenType::IDENTIFIER, word),
            })
        }
    }

    tokens
}
