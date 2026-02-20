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
use crate::ast::{Declaration, Expression, Function, Node, Statement, UnaryExpression, Variable};
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
            Token::VAR => self.parse_var_decl(),
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

    fn parse_var_decl(&mut self) -> Declaration {
        self.consume(Token::VAR);

        let name = match &self.curr_token {
            Token::IDENTIFIER(ident) => ident.clone(),
            _ => panic!("Expected identifier, got {}", self.curr_token),
        };
        self.advance();
        self.consume(Token::ASSIGN);
        let value = self.parse_expr();
        self.consume(Token::SEMICOLON);

        Declaration::VariableDeclaration(Variable { name, value })
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

        Statement::ReturnStatement(self.parse_expr())
    }

    fn parse_expr_statement(&mut self) -> Statement {
        Statement::ExpressionStatement(self.parse_expr())
    }

    fn parse_expr(&mut self) -> Expression {
        let left = match self.curr_token {
            Token::IDENTIFIER(_) => self.parse_ident_expr(),
            Token::NUMBER(_) => self.parse_num_expr(),
            Token::DASH => self.parse_prefix_expr(),
            _ => panic!("Expected expression, got {}", self.curr_token),
        };

        left
    }

    fn parse_ident_expr(&mut self) -> Expression {
        let ident = match &self.curr_token {
            Token::IDENTIFIER(s) => s.clone(),
            _ => panic!("Expected ident, got {}", self.curr_token),
        };

        self.advance();

        Expression::Identifier(ident)
    }

    fn parse_num_expr(&mut self) -> Expression {
        let num = match self.curr_token {
            Token::NUMBER(x) => x,
            _ => panic!("Expected number but got {}", self.curr_token),
        };
        self.advance();

        Expression::Number(num)
    }

    fn parse_prefix_expr(&mut self) -> Expression {
        let op = match self.curr_token {
            Token::DASH => self.curr_token.clone(),
            _ => panic!("Expected prefix operator, got {}", self.curr_token),
        };
        self.advance();
        let operand = Box::new(self.parse_expr());

        Expression::Unary(UnaryExpression { op, operand })
    }
}
