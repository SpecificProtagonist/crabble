use ast::{Op1, Op2};

use crate::*;

#[derive(Default)]
pub struct Hir {
    pub types: Vec<TypV>,
    pub functions: Vec<Function>,
}

struct Typ(u32);
struct Fun(u32);

enum TypV {
    F64,
}

pub struct Function {
    pub name: Option<Span>,
    pub args: Vec<Span>,
    pub value: Expr,
}

pub enum BlockExpr {
    Expr(Expr),
    Decl(u32),
    Assign(u32, Expr),
}

pub struct Expr {
    pub variant: ExprV,
    pub span: Span,
}

pub enum ExprV {
    Literal(f64),
    Var(u32),

    Op1(Op1, Box<Expr>),
    Op2(Op2, Box<Expr>, Box<Expr>),

    Block(Vec<BlockExpr>),
    Call(Box<Expr>, Vec<Expr>),
}

pub fn build(src: &str, ast: &ast::Expr) -> Result<Hir> {
    let mut builder = Builder {
        src,
        hir: default(),
        vars: default(),
        scope_vars: default(),
    };

    let body = builder.expr(ast)?;
    let mut hir = builder.hir;
    hir.functions.push(Function {
        name: default(),
        args: default(),
        value: body,
    });

    Ok(hir)
}

struct Builder<'a> {
    src: &'a str,
    hir: Hir,
    vars: Vec<Span>,
    scope_vars: Vec<Map<&'a str, u32>>,
}

fn s(src: &str, span: Span) -> &str {
    &src[span.start..span.end]
}

impl<'a> Builder<'a> {
    fn lookup(&self, name: &str) -> Option<u32> {
        self.scope_vars
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    // fn fun(&mut, fun: &ast::Function) {
    //
    // }

    fn block(&mut self, items: &[ast::Item]) -> Result<Vec<BlockExpr>> {
        self.scope_vars.push(default());
        let mut hir_items = Vec::new();
        for item in items {
            match item {
                ast::Item::Blank | ast::Item::Fun(_) => {}
                ast::Item::Assign { decl, ident, value } => {
                    let str = s(self.src, *ident);
                    let id = if *decl {
                        let id = self.vars.len() as u32;
                        self.vars.push(*ident);
                        self.scope_vars.last_mut().unwrap().insert(str, id);
                        hir_items.push(BlockExpr::Decl(id));
                        id
                    } else if let Some(id) = self.lookup(str) {
                        id
                    } else {
                        return Err(Err::NotFound { ident: *ident });
                    };
                    hir_items.push(BlockExpr::Assign(id, self.expr(value)?));
                }
                ast::Item::Expr(e) => {
                    hir_items.push(BlockExpr::Expr(self.expr(e)?));
                }
            }
        }
        self.scope_vars.pop();
        Ok(hir_items)
    }

    fn expr(&mut self, expr: &ast::Expr) -> Result<Expr> {
        let ast::Expr(expr, span) = expr;
        Ok(Expr {
            span: *span,
            variant: match expr {
                ast::ExprV::Literal(lit) => ExprV::Literal(*lit),
                ast::ExprV::Var => ExprV::Var(
                    self.lookup(s(self.src, *span))
                        .ok_or(Err::NotFound { ident: *span })?,
                ),
                ast::ExprV::Op1(op, val) => ExprV::Op1(*op, self.expr(val)?.into()),
                ast::ExprV::Op2(op, val_1, val_2) => {
                    ExprV::Op2(*op, self.expr(val_1)?.into(), self.expr(val_2)?.into())
                }
                ast::ExprV::Block(items) => ExprV::Block(self.block(items)?),
                ast::ExprV::Call(_fun, _args) => ExprV::Call(todo!(), todo!()),
                ast::ExprV::Error => todo!(),
            },
        })
    }
}
