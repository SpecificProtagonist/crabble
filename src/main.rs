use chumsky::prelude::*;

#[derive(Debug)]
enum Expr {
    Num(f64),
    Var(String),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(Box<Expr>, Vec<Expr>),
    Fn {
        args: Vec<String>,
        body: Box<Expr>,
        then: Box<Expr>,
    },

    Let {
        name: String,
        rhs: Box<Expr>,
        then: Box<Expr>,
    },

    Error,
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    println!("{:?}", parser().parse(&src));

    match parser().parse(&src).into_result() {
        Ok(ast) => match eval(&ast) {
            Ok(output) => println!("{}", output),
            Err(eval_err) => println!("Evaluation error: {}", eval_err),
        },
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e)),
    };
}

type Extra<'a> = extra::Full<Rich<'a, char>, (), ()>;

fn parser<'a>() -> impl Parser<'a, &'a str, Expr, Extra<'a>> {
    let number = text::int(10)
        .map(|s: &str| Expr::Num(s.parse().unwrap()))
        .padded()
        .labelled("number");

    let op = |c| just(c).padded();

    let expr = recursive(|expr| {
        let bracketed = just('(')
            .then(expr)
            .then(just(')'))
            .padded()
            .map(|((_, expr), _)| expr)
            .or(number)
            // Todo: try out nested_delimiters instead
            .recover_with(via_parser(
                just('(')
                    .then(none_of(')').repeated().then(just(')')))
                    .map(|_| Expr::Error),
            ));

        let unary = op('-')
            .then(bracketed.clone())
            .map(|(_, expr)| Expr::Neg(expr.into()))
            .or(bracketed);

        let product = unary.clone().foldl(
            choice((
                op('*').to(Expr::Mul as fn(_, _) -> _),
                op('/').to(Expr::Div as fn(_, _) -> _),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        product
            .clone()
            .foldl(
                choice((
                    op('+').to(Expr::Add as fn(_, _) -> _),
                    op('-').to(Expr::Sub as fn(_, _) -> _),
                ))
                .then(product)
                .repeated(),
                |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
            )
            .labelled("expression")
    });

    expr.then_ignore(end())
}

fn eval(expr: &Expr) -> Result<f64, String> {
    match expr {
        Expr::Num(x) => Ok(*x),
        Expr::Neg(a) => Ok(-eval(a)?),
        Expr::Add(a, b) => Ok(eval(a)? + eval(b)?),
        Expr::Sub(a, b) => Ok(eval(a)? - eval(b)?),
        Expr::Mul(a, b) => Ok(eval(a)? * eval(b)?),
        Expr::Div(a, b) => Ok(eval(a)? / eval(b)?),
        _ => todo!(), // We'll handle other cases later
    }
}
