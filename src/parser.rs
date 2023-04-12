use chumsky::prelude::*;
use thiserror::Error;

use crate::ast::Expr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Int(u64),
    Ident(String),
    Op(char),
    LParen,
    RParen,
    Comma,
    Semi,
    Let,
    Fn,
    Assign,
    End,
}

impl Token {
    pub fn int(&self) -> Option<u64> {
        match self {
            Token::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn ident(&self) -> Option<&str> {
        match self {
            Token::Ident(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<String> {
        match self {
            Token::Ident(s) => Some(s),
            _ => None,
        }
    }

    pub fn op(&self) -> Option<char> {
        match self {
            Token::Op(c) => Some(*c),
            _ => None,
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let int = text::int(10).map(|s: String| Token::Int(s.parse().unwrap()));
    let ident = text::ident().map(Token::Ident);
    let op = choice((just('+'), just('-'), just('*'), just('/'))).map(Token::Op);
    let lparen = just('(').map(|_| Token::LParen);
    let rparen = just(')').map(|_| Token::RParen);
    let comma = just(',').map(|_| Token::Comma);
    let semi = just(';').map(|_| Token::Semi);
    let let_ = text::keyword("let").map(|_| Token::Let);
    let fn_ = text::keyword("fn").map(|_| Token::Fn);
    let assign = just('=').map(|_| Token::Assign);
    let end = end().ignored().map(|()| Token::End);

    let comment_head = choice((
        just('#').ignored(),
        just('/').ignored().then_ignore(just('/')),
    ));
    let comment = comment_head
        .then_ignore(just('\n').not().repeated())
        .then_ignore(just('\n'));
    let whitespace = filter(|c: &char| c.is_whitespace()).ignored();

    // pad <- (whitespace / comment)*
    let pad = choice((whitespace, comment)).repeated();

    let token = choice((
        op, lparen, rparen, comma, semi, let_, fn_, assign, int, ident,
    ))
    // .padded()
    .padded_by(pad);

    token.repeated().then(end).map(|(tokens, end)| {
        let mut tokens = tokens;
        tokens.push(end);
        tokens
    })
}

pub fn token_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    // num <- int
    let num = filter(|t: &Token| matches!(t, Token::Int(_)))
        .map(|t: Token| Expr::Num(t.int().unwrap() as f64));
    // op <- .
    let op = |c: char| just(Token::Op(c)).map(|t| t.op().unwrap());
    // ident
    let ident =
        filter(|t: &Token| matches!(t, Token::Ident(_))).map(|t: Token| t.into_ident().unwrap());

    let arith = recursive(|arith| {
        // atom <- num / '(' arith ')' / ident
        let atom = choice((
            num,
            arith
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
            ident.map(Expr::Var),
        ));
        // unary <- ('-')* atom
        let unary = op('-')
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::Neg(rhs.into()));
        // product <- unary (('*' / '/') unary)*
        let product = unary
            .clone()
            .then(choice((op('*'), op('/'))).then(unary).repeated())
            .foldl(|lhs, (op, rhs)| match op {
                '*' => Expr::Mul(lhs.into(), rhs.into()),
                '/' => Expr::Div(lhs.into(), rhs.into()),
                _ => unreachable!(),
            });
        // sum <- product (('+' / '-') product)*
        let sum = product
            .clone()
            .then(choice((op('+'), op('-'))).then(product).repeated())
            .foldl(|lhs, (op, rhs)| match op {
                '+' => Expr::Add(lhs.into(), rhs.into()),
                '-' => Expr::Sub(lhs.into(), rhs.into()),
                _ => unreachable!(),
            });

        // arith <- sum
        let arith = sum;

        #[allow(clippy::let_and_return)]
        arith
    });

    let expr = recursive(|expr| {
        // decl <- "let" ident '=' expr ';' expr
        let decl = filter(|t: &Token| matches!(t, Token::Let))
            .ignore_then(ident)
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::Semi))
            .then(expr.clone())
            .map(|((name, rhs), then): ((String, Expr), Expr)| Expr::Let {
                name,
                rhs: rhs.into(),
                then: then.into(),
            });
        // func <- "fn" ident ident* '=' expr ';' expr
        let func = filter(|t: &Token| matches!(t, Token::Fn))
            .ignore_then(ident)
            .then(ident.repeated())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::Semi))
            .then(expr.clone())
            .map(
                |(((name, args), body), then): (((String, Vec<String>), Expr), Expr)| Expr::Fn {
                    name,
                    args,
                    body: body.into(),
                    then: then.into(),
                },
            );
        // call <- ident '(' (expr (',' expr)*)* ','? ')'
        let call = ident
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map(|(func, params): (String, Vec<Expr>)| Expr::Call { func, params });
        // expr <- decl / func / call / arith
        let expr = choice((decl, func, call, arith));

        #[allow(clippy::let_and_return)]
        expr
    });

    // s <- expr $
    let s = expr.then_ignore(just(Token::End));

    #[allow(clippy::let_and_return)]
    s
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("lexer error")]
    Lexer(Vec<Simple<char>>),
    #[error("parser error")]
    Parser(Vec<Simple<Token>>),
}

pub fn parse(src: &str) -> Result<Expr, Error> {
    let lexer = lexer();
    let parser = token_parser();
    let tokens = match lexer.parse(src) {
        Ok(tokens) => tokens,
        Err(e) => return Err(Error::Lexer(e)),
    };
    match parser.parse(tokens) {
        Ok(e) => Ok(e),
        Err(e) => Err(Error::Parser(e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comment() {
        let src = "// comment
        // comment
        # comment
        let x = 42; // comment
        // comment
42 // comment
 // comment
";
        let e_expected = Expr::Let {
            name: "x".to_string(),
            rhs: Expr::Num(42.0).into(),
            then: Expr::Num(42.0).into(),
        };

        let e = parse(src).unwrap();
        assert_eq!(e, e_expected);
    }

    #[test]
    fn digit() {
        let src = "5
";
        let e_expected = Expr::Num(5.0);

        let e = parse(src).unwrap();
        assert_eq!(e, e_expected);
    }

    #[test]
    fn number() {
        let src = "42
    ";
        let e_expected = Expr::Num(42.0);

        let e = parse(src).unwrap();
        assert_eq!(e, e_expected);
    }

    #[test]
    fn unary() {
        let src = "- -42";
        let e_expected = Expr::Neg(Expr::Neg(Expr::Num(42.0).into()).into());

        let e = parse(src).unwrap();
        assert_eq!(e, e_expected);
    }

    #[test]
    fn product() {
        let src = "5 * 3";
        let e_expected = Expr::Mul(Expr::Num(5.0).into(), Expr::Num(3.0).into());

        let e = parse(src).unwrap();
        assert_eq!(e, e_expected);
    }

    #[test]
    fn sum() {
        let src = "5 * 3 + 2 * 4";
        let e_expected = Expr::Add(
            Expr::Mul(Expr::Num(5.0).into(), Expr::Num(3.0).into()).into(),
            Expr::Mul(Expr::Num(2.0).into(), Expr::Num(4.0).into()).into(),
        );

        let e = parse(src).unwrap();
        assert_eq!(e, e_expected);
    }

    #[test]
    fn parentheses() {
        let src = "(5 + 3) * (2 * 4)";
        let e_expected = Expr::Mul(
            Expr::Add(Expr::Num(5.0).into(), Expr::Num(3.0).into()).into(),
            Expr::Mul(Expr::Num(2.0).into(), Expr::Num(4.0).into()).into(),
        );

        let e = parse(src).unwrap();
        assert_eq!(e, e_expected);
    }

    #[test]
    fn var() {
        let src = "x";
        let e_expected = Expr::Var("x".into());

        let e = parse(src).unwrap();
        assert_eq!(e, e_expected);
    }

    #[test]
    fn let_() {
        let src = "
    let x = 5;
    let x = 3 + x;
    x * 3
    ";
        let ast_expected = Expr::Let {
            name: "x".into(),
            rhs: Expr::Num(5.0).into(),
            then: Expr::Let {
                name: "x".into(),
                rhs: Expr::Add(Expr::Num(3.0).into(), Expr::Var("x".into()).into()).into(),
                then: Expr::Mul(Expr::Var("x".into()).into(), Expr::Num(3.0).into()).into(),
            }
            .into(),
        };
        let eval_expected = 24.0;

        let ast = parse(src).unwrap();
        assert_eq!(ast, ast_expected);

        let eval = ast.eval().unwrap();
        assert_eq!(eval, eval_expected);
    }

    #[test]
    fn func() {
        let src = "
    let five = 5;
    let eight = 3 + five;
    fn add x y = x + y;
    add ( five , eight )
    ";
        let ast_expected = Expr::Let {
            name: "five".into(),
            rhs: Expr::Num(5.0).into(),
            then: Expr::Let {
                name: "eight".into(),
                rhs: Expr::Add(Expr::Num(3.0).into(), Expr::Var("five".into()).into()).into(),
                then: Expr::Fn {
                    name: "add".into(),
                    args: vec!["x".into(), "y".into()],
                    body: Expr::Add(Expr::Var("x".into()).into(), Expr::Var("y".into()).into())
                        .into(),
                    then: Expr::Call {
                        func: "add".into(),
                        params: vec![Expr::Var("five".into()), Expr::Var("eight".into())],
                    }
                    .into(),
                }
                .into(),
            }
            .into(),
        };

        let ast = parse(src).unwrap();
        assert_eq!(ast, ast_expected);

        let eval = ast.eval().unwrap();
        assert_eq!(eval, 13.0);
    }

    #[test]
    fn scope() {
        let src = "
    let g = 1;
    let x = 5;
    fn f x = x + g;
    let f = f(3);
    f + x
    ";
        let eval_expected = 9.0;

        let eval = parse(src).unwrap().eval().unwrap();
        assert_eq!(eval, eval_expected);
    }
}
