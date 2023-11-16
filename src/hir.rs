use crate::err::miette;
use ast::{Op1, Op2};

use crate::*;

#[derive(Default, Debug)]
pub struct Hir {
    pub types: Set<TypV>,
    pub functions: Vec<Function>,
}

// TODO: niche
#[derive(Debug)]
pub struct Typ(pub u32);

type FunId = u32;
type DeclId = u32;

#[derive(Debug)]
pub enum TypV {
    Empty,
    F64,
    Fun { args: Vec<Typ>, ret: Typ },
}

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

#[derive(Debug)]
pub enum ExprV {
    Const(Const),
    Var(u32),

    Op1(Op1, Box<Expr>),
    Op2(Op2, Box<Expr>, Box<Expr>),

    Block(Vec<BlockExpr>),
    Call(Box<Expr>, Vec<Expr>),
}

struct Builder<'a> {
    src: &'a str,
    hir: Hir,
    decls: Vec<Decl>,
    scope_decls: Vec<Map<&'a str, DeclId>>,
}

pub fn build(src: &str, ast: &ast::Expr) -> Result<Hir> {
    let mut builder = Builder {
        src,
        hir: default(),
        decls: default(),
        scope_decls: default(),
    };

    let body = builder.expr(ast)?;
    let mut hir = builder.hir;
    let entrypoint = hir.functions.len() as u32;
    hir.functions.push(Function {
        name: default(),
        id: entrypoint,
        args: default(),
        value: body,
    });

    Ok(hir)
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

    fn check_const_redefinition(&self, ident: Span) -> Result<()> {
        let str = s(self.src, ident);
        if let Some(existing) = self.lookup(str) {
            let existing = &self.decls[existing as usize];
            if matches!(existing.kind, DeclKind::Const(_)) {
                return Err(Err::ConstRedefinition {
                    ident: miette(ident),
                    existing: miette(existing.span),
                });
            }
        }
        Ok(())
    }

    fn function(&mut self, fun: &ast::Function) -> Result<()> {
        self.check_const_redefinition(fun.name)?;

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

        let body = self.expr(&fun.body)?;
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
        Ok(())
    }

    fn block(&mut self, items: &[ast::Item]) -> Result<Vec<BlockExpr>> {
        self.scope_decls.push(default());
        let mut hir_items = Vec::new();
        // Order-independant
        for item in items {
            match item {
                ast::Item::Fun(fun) => self.function(fun)?,
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
                        self.check_const_redefinition(*ident)?;
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
                            return Err(Err::Immutable {
                                ident: miette(*ident),
                                decl: miette(decl.span),
                            });
                        }
                        id
                    } else {
                        return Err(Err::NotFound {
                            ident: miette(*ident),
                        });
                    };
                    hir_items.push(BlockExpr::Assign(id, self.expr(value)?));
                }
                ast::Item::Expr(e) => {
                    hir_items.push(BlockExpr::Expr(self.expr(e)?));
                }
            }
        }
        self.scope_decls.pop();
        Ok(hir_items)
    }

    fn expr(&mut self, expr: &ast::Expr) -> Result<Expr> {
        let ast::Expr(expr, span) = expr;
        Ok(Expr {
            span: *span,
            typ: None,
            variant: match expr {
                ast::ExprV::Literal(lit) => ExprV::Const(Const::F64(*lit)),
                ast::ExprV::Var => {
                    let id = self.lookup(s(self.src, *span)).ok_or(Err::NotFound {
                        ident: miette(*span),
                    })?;
                    let decl = &self.decls[id as usize];
                    match &decl.kind {
                        DeclKind::Mutable | DeclKind::Readonly => ExprV::Var(id),
                        DeclKind::Const(value) => ExprV::Const((*value).clone()),
                    }
                }
                ast::ExprV::Op1(op, val) => ExprV::Op1(*op, self.expr(val)?.into()),
                ast::ExprV::Op2(op, val_1, val_2) => {
                    ExprV::Op2(*op, self.expr(val_1)?.into(), self.expr(val_2)?.into())
                }
                ast::ExprV::Block(items) => ExprV::Block(self.block(items)?),
                ast::ExprV::Call(fun, args) => {
                    let mut vec = Vec::with_capacity(args.len());
                    for arg in args {
                        vec.push(self.expr(arg)?);
                    }
                    ExprV::Call(self.expr(fun)?.into(), vec)
                }
                ast::ExprV::Error => todo!(),
            },
        })
    }
}
