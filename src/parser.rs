/*


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
use crate::ast::{Declaration, Function, Node, Statement};
use crate::lexer::Token;

type ParseError = String;

pub struct Parser {
    tokens: Vec<Token>,
    curr_token: Token,
    peek_token: Token,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let mut parser = Parser {
            tokens,
            curr_token: Token::UNKNOWN,
            peek_token: Token::UNKNOWN,
            index: 0,
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
        println!("curr: {}, peek: {}", self.curr_token, self.peek_token);
    }

    fn consume(&mut self, tok: Token) {
        if self.curr_token == tok {
            self.advance();
        } else {
            panic!("Expected {}, got {}", tok, self.curr_token);
        }
    }

    pub fn parse(&mut self) -> Node {
        let mut decls = Vec::new();
        while self.curr_token != Token::EOF {
            decls.push(self.parse_decl());
        }

        Node::Program(decls)
    }

    fn parse_decl(&mut self) -> Declaration {
        match self.curr_token {
            Token::FN => self.parse_fn_decl(),
            _ => panic!("No matching declaration"),
        }
    }

    fn parse_fn_decl(&mut self) -> Declaration {
        self.consume(Token::FN);

        let name = match &self.curr_token {
            Token::IDENTIFIER(ident) => ident.clone(),
            _ => panic!("Expected identifier, got {}", self.curr_token),
        };
        self.advance();

        self.consume(Token::LPAREN);
        self.consume(Token::RPAREN);

        let stmts = self.parse_block();

        Declaration::FunctionDeclaration(Function { name, stmts })
    }

    fn parse_block(&mut self) -> Vec<Statement> {
        self.consume(Token::LBRACE);
        let mut stmts: Vec<Statement> = Vec::new();
        while self.curr_token != Token::RBRACE {
            stmts.push(self.parse_statement());
        }
        self.consume(Token::RBRACE);

        stmts
    }

    fn parse_statement(&mut self) -> Statement {
        let stmt = match self.curr_token {
            Token::RETURN => self.parse_return_statement(),
            _ => panic!("Expect statement, got {}", self.curr_token),
        };

        self.consume(Token::SEMICOLON);

        stmt
    }

    fn parse_return_statement(&mut self) -> Statement {
        self.consume(Token::RETURN);
        match self.curr_token {
            Token::NUMBER(num) => {
                self.advance();
                Statement::ReturnStatement(num)
            }
            _ => panic!("Expected number, got {}", self.curr_token),
        }
    }
}
