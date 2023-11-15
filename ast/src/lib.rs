// mod string_interner;

use chumsky::prelude::*;

pub type Span = chumsky::span::SimpleSpan<usize>;

#[derive(Debug, Copy, Clone)]
pub enum Op1 {
    Neg,
}

#[derive(Debug, Copy, Clone)]
pub enum Op2 {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Span,
    pub args: Vec<(Decl, Span)>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub enum Item {
    Blank,
    Expr(Expr),
    Assign {
        decl: Option<Decl>,
        ident: Span,
        value: Expr,
    },
    Fun(Function),
}

#[derive(Debug, Clone)]
pub struct Expr(pub ExprV, pub Span);

#[derive(Debug, Clone)]
pub enum ExprV {
    Literal(f64),
    Var,

    Op1(Op1, Box<Expr>),
    Op2(Op2, Box<Expr>, Box<Expr>),

    Call(Box<Expr>, Vec<Expr>),

    Block(Vec<Item>),
    Error,
}

type Extra<'a> = extra::Full<Rich<'a, char>, (), ()>;

pub fn parse(src: &str) -> ParseResult<Expr, Rich<'_, char>> {
    parser().parse(src)
}

fn parser<'a>() -> impl Parser<'a, &'a str, Expr, Extra<'a>> {
    let ident = text::ident().map_with(|_, e| e.span()).padded();

    let block = recursive(|block| {
        let expr = recursive(|expr| {
            let number = text::int(10)
                .map_with(|s: &str, e| Expr(ExprV::Literal(s.parse().unwrap()), e.span()))
                .padded()
                .labelled("number");

            let var = ident.map(|s| Expr(ExprV::Var, s));

            let bracketed = expr
                .clone()
                .delimited_by(just('('), just(')'))
                .padded()
                .or(number)
                .or(var)
                .or(block.clone().delimited_by(just('{'), just('}')))
                // Todo: try out nested_delimiters instead
                .recover_with(via_parser(
                    just('(')
                        .then(none_of(')').repeated().then(just(')')))
                        .map_with(|_, e| Expr(ExprV::Error, e.span())),
                ));

            let expr_list = bracketed
                .clone()
                .separated_by(just(','))
                .allow_trailing()
                .collect::<Vec<Expr>>();

            let call = bracketed
                .clone()
                .then(expr_list.delimited_by(just('('), just(')')))
                .map_with(|(function, args), e| Expr(ExprV::Call(function.into(), args), e.span()))
                .or(bracketed);

            let op = |c| just(c).padded();

            let unary = op('-')
                .then(call.clone())
                .map_with(|(_, expr), e| Expr(ExprV::Op1(Op1::Neg, expr.into()), e.span()))
                .or(call);

            let product = unary.clone().foldl_with(
                choice((op('*').to(Op2::Mul), op('/').to(Op2::Div)))
                    .then(unary)
                    .repeated(),
                |lhs, (op, rhs), e| Expr(ExprV::Op2(op, Box::new(lhs), Box::new(rhs)), e.span()),
            );

            product
                .clone()
                .foldl_with(
                    choice((op('+').to(Op2::Add), op('-').to(Op2::Sub)))
                        .then(product)
                        .repeated(),
                    |lhs, (op, rhs), e| {
                        Expr(ExprV::Op2(op, Box::new(lhs), Box::new(rhs)), e.span())
                    },
                )
                .labelled("expression")
        });

        let assign = (just("let").then(just("mut").padded().or_not()))
            .or_not()
            .then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .padded()
            .map(|((decl, ident), value)| Item::Assign {
                decl: if let Some((_, mutable)) = decl {
                    Some(Decl {
                        mutable: mutable.is_some(),
                    })
                } else {
                    None
                },
                ident,
                value,
            });

        let function = ident
            .then(
                just("mut")
                    .or_not()
                    .padded()
                    .map(|mutable| Decl {
                        mutable: mutable.is_some(),
                    })
                    .then(ident)
                    .separated_by(just(','))
                    .collect()
                    .delimited_by(just('('), just(')')),
            )
            .then(block.clone().delimited_by(just('{'), just('}')).padded())
            .map(|((name, args), body)| Function { name, args, body });

        let item = choice((
            expr.clone().map(Item::Expr).then_ignore(just(';')),
            assign,
            function.map(Item::Fun),
            block.delimited_by(just('{'), just('}')).map(Item::Expr),
        ));

        item.repeated()
            .collect::<Vec<_>>()
            .then(expr.clone().or_not())
            .map_with(|(mut items, last), e| {
                items.push(last.map(Item::Expr).unwrap_or(Item::Blank));
                Expr(ExprV::Block(items), e.span())
            })
            .boxed()
    });

    block.padded().then_ignore(end())
}
