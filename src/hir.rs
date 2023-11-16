use crate::{
    err::miette,
    typ::{Typ, TypV},
};
use ast::{Op1, Op2};

use crate::*;

#[derive(Default, Debug)]
pub struct Hir {
    pub types: Set<TypV>,
    /// The entrypoint is the first function
    pub functions: Vec<Function>,
}

type FunId = u32;
type DeclId = u32;

#[derive(Debug, Clone)]
pub enum Const {
    Empty,
    F64(f64),
    Fun(FunId),
}

#[derive(Debug)]
struct Decl {
    span: Span,
    typ: Option<Typ>,
    kind: DeclKind,
}

#[derive(Debug)]
enum DeclKind {
    Mutable,
    Readonly,
    Const(Const),
}

#[derive(Debug)]
pub struct Function {
    pub name: Option<Span>,
    pub id: FunId,
    pub args: Vec<DeclId>,
    pub value: Expr,
}

#[derive(Debug)]
pub enum BlockExpr {
    Expr(Expr),
    Decl(u32),
    Assign(u32, Expr),
}

#[derive(Debug)]
pub struct Expr {
    pub variant: ExprV,
    pub span: Span,
    pub typ: Option<Typ>,
}

impl Expr {
    fn err(span: Span) -> Self {
        Self {
            variant: ExprV::Error,
            span,
            typ: None,
        }
    }
}

impl Default for Expr {
    fn default() -> Self {
        Self {
            variant: ExprV::Error,
            span: Span::splat(0),
            typ: None,
        }
    }
}

#[derive(Debug, Default)]
pub enum ExprV {
    Const(Const),
    Var(u32),

    Op1(Op1, Box<Expr>),
    Op2(Op2, Box<Expr>, Box<Expr>),

    Block(Vec<BlockExpr>),
    Call(Box<Expr>, Vec<Expr>),

    #[default]
    Error,
}

struct Builder<'a> {
    src: &'a str,
    diag: &'a mut dyn FnMut(Diag),
    hir: Hir,
    decls: Vec<Decl>,
    scope_decls: Vec<Map<&'a str, DeclId>>,
}

pub fn build(src: &str, ast: &ast::Expr, diags: &mut dyn FnMut(Diag)) -> Hir {
    let mut hir = Hir::default();
    hir.functions.push(Function {
        name: None,
        id: 0,
        args: default(),
        value: default(),
    });
    let mut builder = Builder {
        src,
        diag: diags,
        hir,
        decls: default(),
        scope_decls: default(),
    };

    let body = builder.expr(ast);
    let mut hir = builder.hir;
    hir.functions[0] = Function {
        name: default(),
        id: 0,
        args: default(),
        value: body,
    };

    hir
}

fn s(src: &str, span: Span) -> &str {
    &src[span.start..span.end]
}

impl<'a> Builder<'a> {
    fn lookup(&self, name: &str) -> Option<u32> {
        self.scope_decls
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    fn check_const_redefinition(&mut self, ident: Span) {
        let str = s(self.src, ident);
        if let Some(existing) = self.lookup(str) {
            let existing = &self.decls[existing as usize];
            if matches!(existing.kind, DeclKind::Const(_)) {
                (self.diag)(Diag::ConstRedefinition {
                    ident: miette(ident),
                    existing: miette(existing.span),
                });
                return;
            }
        }
    }

    fn function(&mut self, fun: &ast::Function) {
        self.check_const_redefinition(fun.name);

        let mut args = Vec::new();
        for (decl, ident) in &fun.args {
            let str = s(self.src, *ident);
            let id = self.decls.len() as u32;
            let decl = Decl {
                span: *ident,
                typ: None,
                kind: if decl.mutable {
                    DeclKind::Mutable
                } else {
                    DeclKind::Readonly
                },
            };
            args.push(id);
            self.decls.push(decl);
            self.scope_decls.last_mut().unwrap().insert(str, id);
        }

        let body = self.expr(&fun.body);
        let fun_id = self.hir.functions.len() as u32;
        self.hir.functions.push(Function {
            name: Some(fun.name),
            id: fun_id,
            args,
            value: body,
        });
        let decl_id = self.decls.len() as u32;
        self.decls.push(Decl {
            span: fun.name,
            typ: None,
            kind: DeclKind::Const(Const::Fun(fun_id)),
        });
        self.scope_decls
            .last_mut()
            .unwrap()
            .insert(s(&self.src, fun.name), decl_id);
    }

    fn block(&mut self, items: &[ast::Item]) -> Vec<BlockExpr> {
        self.scope_decls.push(default());
        let mut hir_items = Vec::new();
        // Order-independant
        for item in items {
            match item {
                ast::Item::Fun(fun) => self.function(fun),
                _ => {}
            }
        }
        // Order-dependant
        for item in items {
            match item {
                ast::Item::Blank | ast::Item::Fun(_) => {}
                ast::Item::Assign { decl, ident, value } => {
                    let str = s(self.src, *ident);
                    let id = if let Some(decl) = decl {
                        self.check_const_redefinition(*ident);
                        let id = self.decls.len() as u32;
                        self.decls.push(Decl {
                            span: *ident,
                            typ: None,
                            kind: if decl.mutable {
                                DeclKind::Mutable
                            } else {
                                DeclKind::Readonly
                            },
                        });
                        self.scope_decls.last_mut().unwrap().insert(str, id);
                        hir_items.push(BlockExpr::Decl(id));
                        id
                    } else if let Some(id) = self.lookup(str) {
                        let decl = &self.decls[id as usize];
                        if !matches!(decl.kind, DeclKind::Mutable) {
                            (self.diag)(Diag::Immutable {
                                ident: miette(*ident),
                                decl: miette(decl.span),
                            });
                        }
                        id
                    } else {
                        (self.diag)(Diag::NotFound {
                            ident: miette(*ident),
                        });
                        hir_items.push(BlockExpr::Expr(default()));
                        continue;
                    };
                    hir_items.push(BlockExpr::Assign(id, self.expr(value)));
                }
                ast::Item::Expr(e) => {
                    hir_items.push(BlockExpr::Expr(self.expr(e)));
                }
            }
        }
        self.scope_decls.pop();
        hir_items
    }

    fn expr(&mut self, expr: &ast::Expr) -> Expr {
        let ast::Expr(expr, span) = expr;
        Expr {
            span: *span,
            typ: None,
            variant: match expr {
                ast::ExprV::Literal(lit) => ExprV::Const(Const::F64(*lit)),
                ast::ExprV::Var => {
                    if let Some(id) = self.lookup(s(self.src, *span)) {
                        let decl = &self.decls[id as usize];
                        match &decl.kind {
                            DeclKind::Mutable | DeclKind::Readonly => ExprV::Var(id),
                            DeclKind::Const(value) => ExprV::Const((*value).clone()),
                        }
                    } else {
                        (self.diag)(Diag::NotFound {
                            ident: miette(*span),
                        });
                        ExprV::Error
                    }
                }
                ast::ExprV::Op1(op, val) => ExprV::Op1(*op, self.expr(val).into()),
                ast::ExprV::Op2(op, val_1, val_2) => {
                    ExprV::Op2(*op, self.expr(val_1).into(), self.expr(val_2).into())
                }
                ast::ExprV::Block(items) => ExprV::Block(self.block(items)),
                ast::ExprV::Call(fun, args) => {
                    let mut vec = Vec::with_capacity(args.len());
                    for arg in args {
                        vec.push(self.expr(arg));
                    }
                    ExprV::Call(self.expr(fun).into(), vec)
                }
                ast::ExprV::Error => ExprV::Error,
            },
        }
    }
}
