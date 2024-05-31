use std::fmt::Display;

use super::{
    token::{Keyword, Paren, Punc, Token},
    Binary, BinaryOp, Call, Decl, Expr, Func, Index, Unary, UnaryOp,
};
use crate::ast::{token::Op, Assign, Block, Pat};

type Result = std::result::Result<Expr, Error>;

pub fn parse_ast(tokens: &[Token]) -> Result {
    let mut parser = Parser::new(tokens);

    parse_expr(&mut parser)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    MissingDelimiter { delimiter: Paren, found: Token },
    Unexpected(Token),
    ExpectedIdent { found: Token },
    Expected { expected: Token, found: Token },
    CannotAssign,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::MissingDelimiter { delimiter, found } => {
                write!(
                    f,
                    "missing closing delimiter `{delimiter}`, found `{found}`"
                )
            }
            Error::Unexpected(token) => write!(f, "unexpected token `{token}`"),
            Error::ExpectedIdent { found } => write!(f, "expected identifier, found `{found}`"),
            Error::Expected { expected, found } => {
                write!(f, "expected `{expected}`, found `{found}`")
            }
            Error::CannotAssign => write!(f, "cannot assign to a temporary value")
        }
    }
}

fn parse_expr(parser: &mut Parser) -> Result {
    let mut new_parser = Parser::new(parser.tokens);
    let result = new_parser.parse();

    parser.tokens = new_parser.tokens;

    result
}

fn parse_block(parser: &mut Parser) -> Result {
    let curr = parser.next();
    if curr != &Token::Paren(Paren::LCurly) {
        return Err(Error::Expected {
            expected: Token::Paren(Paren::LCurly),
            found: curr.clone(),
        });
    }

    let mut content = Vec::new();
    let mut tail = None;

    #[allow(irrefutable_let_patterns)]
    while let curr = parser.curr()
        && curr != &Token::Eof
        && curr != &Token::Paren(Paren::RCurly)
    {
        let expr = parse_expr(parser)?;

        match parser.curr() {
            Token::Paren(Paren::RCurly) => {
                tail = Some(expr);
                break;
            }
            Token::Punc(Punc::Semicolon) => {
                content.push(expr);
                parser.next();
            }
            _ => break,
        }
    }

    if parser.curr() != &Token::Paren(Paren::RCurly) {
        return Err(Error::MissingDelimiter {
            delimiter: Paren::RCurly,
            found: parser.next().clone(),
        });
    }

    parser.next();

    Ok(Expr::Block(Box::new(Block::new(content, tail))))
}

fn parse_decl(parser: &mut Parser) -> Result {
    if parser.curr() != &Token::Keyword(Keyword::Let) {
        return parser.parse();
    }

    parser.next();

    let curr = parser.curr().clone();

    let Token::Ident(name) = curr else {
        parser.next();

        return Err(Error::ExpectedIdent { found: curr });
    };

    parser.next();

    let assign = parser.next();
    if assign != &Token::Op(Op::Assign) {
        return Err(Error::Expected {
            expected: Token::Op(Op::Assign),
            found: assign.clone(),
        });
    }

    let expr = parse_expr(parser)?;

    Ok(Expr::Decl(Box::new(Decl {
        name: name.clone(),
        expr,
    })))
}

fn parse_assign(parser: &mut Parser) -> Result {
    let left = parser.parse()?;

    if parser.curr() != &Token::Op(Op::Assign) {
        return Ok(left);
    }

    parser.next();

    let right = parse_expr(parser)?;

    let assign = match left {
        Expr::Ident(name) => Assign::new(Pat::Ident(name), right),
        Expr::Index(index) => Assign::new(Pat::Index { expr: index.expr, index: index.index }, right),
        _ => return Err(Error::CannotAssign)
    };

    Ok(Expr::Assign(Box::new(assign)))
}

fn parse_func(parser: &mut Parser) -> Result {
    if parser.curr() != &Token::Keyword(Keyword::Fn) {
        return parser.parse();
    }

    parser.next();

    let name = match parser.curr().clone() {
        Token::Ident(name) => {
            parser.next();
            Some(name)
        }
        _ => None,
    };

    if parser.curr() != &Token::Paren(Paren::LParen) {
        return Err(Error::Expected {
            expected: Token::Paren(Paren::LParen),
            found: parser.curr().clone(),
        });
    }

    parser.next();

    let mut args = Vec::new();

    while parser.curr() != &Token::Paren(Paren::RParen) {
        let curr = parser.next().clone();

        let Token::Ident(name) = curr else {
            return Err(Error::ExpectedIdent {
                found: curr.clone(),
            });
        };

        args.push(name);

        if parser.curr() == &Token::Punc(Punc::Comma) {
            parser.next();
        } else {
            break;
        }
    }

    if parser.curr() != &Token::Paren(Paren::RParen) {
        return Err(Error::MissingDelimiter {
            delimiter: Paren::RParen,
            found: parser.curr().clone(),
        });
    }

    parser.next();

    let body = parse_block(parser)?;

    Ok(Expr::Func(Box::new(Func::new(name, args, body))))
}

fn parse_binary(parser: &mut Parser) -> Result {
    use BinaryOp::*;

    const OTHER: fn(&mut Parser) -> Result = |parser: &mut Parser| parser.parse();
    const PARSE_MULTIPLICATIVE: fn(&mut Parser) -> Result =
        |parser| parse_binary_with_ops(parser, &[Mul, Div, Mod], OTHER);
    const PARSE_ADDITIVE: fn(&mut Parser) -> Result =
        |parser| parse_binary_with_ops(parser, &[Plus, Minus], PARSE_MULTIPLICATIVE);
    const PARSE_CMP: fn(&mut Parser) -> Result =
        |parser| parse_binary_with_ops(parser, &[Lt, Gt, Le, Ge], PARSE_ADDITIVE);
    const PARSE_EQ: fn(&mut Parser) -> Result =
        |parser| parse_binary_with_ops(parser, &[Eq, Nq], PARSE_CMP);
    const PARSE_AND: fn(&mut Parser) -> Result =
        |parser| parse_binary_with_ops(parser, &[And], PARSE_EQ);
    const PARSE_OR: fn(&mut Parser) -> Result =
        |parser| parse_binary_with_ops(parser, &[Or], PARSE_AND);

    PARSE_OR(parser)
}

fn parse_binary_with_ops(
    parser: &mut Parser,
    ops: &[BinaryOp],
    lower: fn(&mut Parser) -> Result,
) -> Result {
    let mut left = lower(parser)?;

    while let Some(op) = ops
        .iter()
        .find(|op| &Token::Op((**op).into()) == parser.curr())
    {
        parser.next();

        let right = lower(parser)?;

        left = Expr::Binary(Box::new(Binary::new(left, *op, right)));
    }

    Ok(left)
}

fn parse_unary(parser: &mut Parser) -> Result {
    if let Token::Op(op) = parser.curr()
        && let Ok(op) = UnaryOp::try_from(*op)
    {
        parser.next();

        let expr = parse_unary(parser)?;

        return Ok(Expr::Unary(Box::new(Unary::new(op, expr))));
    }

    parser.parse()
}

fn parse_postfix(parser: &mut Parser) -> Result {
    let mut expr = parser.parse()?;

    loop {
        match parser.curr() {
            Token::Paren(Paren::LParen) => {
                parser.next();

                let mut args = Vec::new();

                while parser.curr() != &Token::Paren(Paren::RParen) {
                    let arg = parse_expr(parser)?;
                    args.push(arg);

                    if parser.curr() == &Token::Punc(Punc::Comma) {
                        parser.next();
                    } else {
                        break;
                    }
                }

                if parser.curr() != &Token::Paren(Paren::RParen) {
                    return Err(Error::MissingDelimiter {
                        delimiter: Paren::RParen,
                        found: parser.curr().clone(),
                    });
                }

                parser.next();

                expr = Expr::Call(Box::new(Call::new(expr, args)));
            }
            Token::Paren(Paren::LBracket) => {
                parser.next();

                let index = parse_expr(parser)?;

                if parser.curr() != &Token::Paren(Paren::RBracket) {
                    return Err(Error::MissingDelimiter {
                        delimiter: Paren::RBracket,
                        found: parser.curr().clone(),
                    });
                }

                parser.next();

                expr = Expr::Index(Box::new(Index::new(expr, index)));
            }
            _ => break,
        }
    }

    Ok(expr)
}

fn parse_primary(parser: &mut Parser) -> Result {
    let expr = match parser.curr() {
        Token::Ident(name) => Ok(Expr::Ident(name.clone())),
        Token::Lit(literal) => Ok(Expr::Lit(literal.clone())),
        Token::Paren(Paren::LCurly) => parse_block(parser),
        Token::Paren(Paren::LBracket) => {
            parser.next();

            let mut exprs = Vec::new();

            while parser.curr() != &Token::Paren(Paren::RBracket) && parser.curr() != &Token::Eof {
                let expr = parse_expr(parser)?;
                exprs.push(expr);

                match parser.curr() {
                    Token::Punc(Punc::Comma) => {
                        parser.next();
                    }
                    _ => break,
                }
            }

            if parser.curr() != &Token::Paren(Paren::RBracket) {
                return Err(Error::MissingDelimiter {
                    delimiter: Paren::RBracket,
                    found: parser.curr().clone(),
                });
            }

            Ok(Expr::List(exprs))
        }
        Token::Paren(Paren::LParen) => {
            parser.next();

            let expr = parse_expr(parser);

            if parser.curr() != &Token::Paren(Paren::RParen) {
                return Err(Error::MissingDelimiter {
                    delimiter: Paren::RParen,
                    found: parser.next().clone(),
                });
            }

            expr
        }
        _ => parser.parse(),
    };

    parser.next();

    expr
}

struct Parser<'tokens> {
    tokens: &'tokens [Token],
    parse_fns: &'static [fn(&mut Parser) -> Result],
}

impl<'tokens> Parser<'tokens> {
    fn new(tokens: &'tokens [Token]) -> Self {
        Self {
            tokens,
            parse_fns: &[
                parse_decl,
                parse_assign,
                parse_func,
                parse_binary,
                parse_unary,
                parse_postfix,
                parse_primary,
            ],
        }
    }

    fn parse(&mut self) -> Result {
        let Some(parse) = self.parse_fns.get(0) else {
            return Err(Error::Unexpected(self.next().clone()));
        };
        let mut new_parser = Parser {
            tokens: &self.tokens,
            parse_fns: &self.parse_fns[1..],
        };

        let result = parse(&mut new_parser);

        self.tokens = new_parser.tokens;

        result
    }

    fn next(&mut self) -> &Token {
        let curr = &self.tokens[0];

        if self.tokens.len() > 1 {
            self.tokens = &self.tokens[1..];
        }

        curr
    }

    fn curr(&self) -> &Token {
        &self.tokens[0]
    }
}
