use crate::ast;

use cranelift::prelude::{types::F64, *};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};

pub struct Jit {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_description: DataDescription,
    module: JITModule,
}

impl Default for Jit {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
        }
    }
}

impl Jit {
    pub fn compile(&mut self, fun: &ast::Function) -> *const u8 {
        for _ in &fun.args {
            self.ctx.func.signature.params.push(AbiParam::new(F64));
        }
        self.ctx.func.signature.returns.push(AbiParam::new(F64));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let mut vars = Vec::new();
        for (i, arg) in fun.args.iter().enumerate() {
            let val = builder.block_params(entry_block)[i];
            let var = Variable::from_u32(i as u32);
            builder.declare_var(var, F64);
            builder.def_var(var, val);
            vars.push((arg.clone(), var));
        }

        let mut trans = FunctionTranslator {
            builder,
            vars,
            module: &mut self.module,
        };

        let tmp_no_return = trans.builder.ins().f64const(0);
        let mut return_value = tmp_no_return;
        for expr in &fun.body.exprs {
            return_value = match expr {
                ast::BlockExpr::Blank => tmp_no_return,
                ast::BlockExpr::Expr(e) => trans.translate_expr(e),
                ast::BlockExpr::Assign { decl, ident, value } => {
                    trans.translate_assign(*decl, ident, value);
                    tmp_no_return
                }
            }
        }

        trans.builder.ins().return_(&[return_value]);
        trans.builder.finalize();

        let id = self
            .module
            .declare_function(&fun.name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        self.module.define_function(id, &mut self.ctx).unwrap();
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();
        self.module.get_finalized_function(id)
    }
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    vars: Vec<(String, Variable)>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    fn var(&self, ident: &String) -> Variable {
        self.vars
            .iter()
            .rev()
            .find(|(name, _)| name == ident)
            .unwrap_or_else(|| panic!("Variable {ident} not found"))
            .1
    }

    fn translate_assign(&mut self, decl: bool, ident: &String, value: &ast::Expr) {
        let value = self.translate_expr(value);
        if decl {
            let var = Variable::from_u32(self.vars.len() as u32);
            self.builder.declare_var(var, F64);
            self.vars.push((ident.clone(), var))
        } else {
            let var = self.var(ident);
            self.builder.def_var(var, value);
        }
    }

    fn translate_expr(&mut self, expr: &ast::Expr) -> Value {
        match expr {
            &ast::Expr::Num(val) => self.builder.ins().f64const(val),
            ast::Expr::Var(ident) => {
                let var = self.var(ident);
                self.builder.use_var(var)
            }
            ast::Expr::Neg(e) => {
                let value = self.translate_expr(e);
                self.builder.ins().fneg(value)
            }
            ast::Expr::Add(e1, e2) => {
                let v1 = self.translate_expr(e1);
                let v2 = self.translate_expr(e2);
                self.builder.ins().fadd(v1, v2)
            }
            ast::Expr::Sub(e1, e2) => {
                let v1 = self.translate_expr(e1);
                let v2 = self.translate_expr(e2);
                self.builder.ins().fsub(v1, v2)
            }
            ast::Expr::Mul(e1, e2) => {
                let v1 = self.translate_expr(e1);
                let v2 = self.translate_expr(e2);
                self.builder.ins().fmul(v1, v2)
            }
            ast::Expr::Div(e1, e2) => {
                let v1 = self.translate_expr(e1);
                let v2 = self.translate_expr(e2);
                self.builder.ins().fdiv(v1, v2)
            }
            ast::Expr::Call(_, _) => todo!(),
            ast::Expr::Block(_) => todo!(),
            ast::Expr::Error => panic!("Parse error"),
        }
    }
}
