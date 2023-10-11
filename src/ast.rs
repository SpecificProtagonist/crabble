use chumsky::prelude::*;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<String>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    // pub decls: Vec<String>,
    pub exprs: Vec<BlockExpr>,
}

#[derive(Debug, Clone)]
pub enum BlockExpr {
    Blank,
    Expr(Expr),
    Assign {
        decl: bool,
        ident: String,
        value: Expr,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Num(f64),
    Var(String),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(Box<Expr>, Vec<Expr>),

    Block(Block),

    Error,
}

type Extra<'a> = extra::Full<Rich<'a, char>, (), ()>;

pub fn parse(src: &str) -> ParseResult<Function, Rich<'_, char>> {
    parser().parse(src)
}

fn parser<'a>() -> impl Parser<'a, &'a str, Function, Extra<'a>> {
    let ident = text::ident().padded();

    let expr = recursive(|expr| {
        let number = text::int(10)
            .map(|s: &str| Expr::Num(s.parse().unwrap()))
            .padded()
            .labelled("number");

        let var = ident.map(|s: &str| Expr::Var(s.into()));

        let bracketed = expr
            .clone()
            .delimited_by(just('('), just(')'))
            .padded()
            .or(number)
            .or(var)
            // Todo: try out nested_delimiters instead
            .recover_with(via_parser(
                just('(')
                    .then(none_of(')').repeated().then(just(')')))
                    .map(|_| Expr::Error),
            ));

        let expr_list = bracketed
            .clone()
            .separated_by(just(','))
            .allow_trailing()
            .collect::<Vec<Expr>>();

        let call = bracketed
            .clone()
            .then(expr_list.delimited_by(just('('), just(')')))
            .map(|(function, args)| Expr::Call(function.into(), args))
            .or(bracketed);

        let op = |c| just(c).padded();

        let unary = op('-')
            .then(call.clone())
            .map(|(_, expr)| Expr::Neg(expr.into()))
            .or(call);

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

    let assign = just("let")
        .or_not()
        .then(ident)
        .then_ignore(just('='))
        .then(expr.clone())
        .padded()
        .map(
            |((decl, name), value): ((Option<&str>, &str), Expr)| BlockExpr::Assign {
                decl: decl.is_some(),
                ident: name.into(),
                value,
            },
        );

    let block_expr = choice((
        expr.map(BlockExpr::Expr),
        assign,
        just("").to(BlockExpr::Blank),
    ));

    let block = block_expr
        .separated_by(just(';'))
        .collect()
        .map(|exprs: Vec<BlockExpr>| Block {
            // decls: exprs
            //     .iter()
            //     .filter_map(|e| {
            //         if let BlockExpr::Assign {
            //             decl: true, ident, ..
            //         } = e
            //         {
            //             Some(ident.clone())
            //         } else {
            //             None
            //         }
            //     })
            //     .collect(),
            exprs,
        });

    let function = ident
        .then(
            ident
                .separated_by(just(','))
                .collect()
                .delimited_by(just('('), just(')')),
        )
        .padded()
        .then(block.delimited_by(just('{'), just('}')))
        .map(|((name, args), body): ((&str, Vec<&str>), _)| Function {
            name: name.into(),
            args: args.iter().map(|t| t.to_string()).collect(),
            body,
        });

    function.then_ignore(end())
}
