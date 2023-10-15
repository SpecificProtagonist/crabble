use cranelift::prelude::{types::F64, *};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};

use crate::hir;

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
    pub fn compile(&mut self, fun: &hir::Function) -> *const u8 {
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
            module: &mut self.module,
        };

        let tmp_no_return = trans.builder.ins().f64const(0);
        let mut return_value = tmp_no_return;
        for expr in &fun.body {
            return_value = match expr {
                hir::BlockExpr::Expr(e) => trans.translate_expr(e),
                hir::BlockExpr::Decl(var) => {
                    trans.translate_decl(*var);
                    tmp_no_return
                }
                hir::BlockExpr::Assign(var, value) => {
                    trans.translate_assign(*var, value);
                    tmp_no_return
                }
            }
        }

        trans.builder.ins().return_(&[return_value]);
        trans.builder.finalize();

        let id = self
            .module
            .declare_function("main_body", Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        self.module.define_function(id, &mut self.ctx).unwrap();
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();
        self.module.get_finalized_function(id)
    }
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_decl(&mut self, var: u32) {
        self.builder.declare_var(Variable::from_u32(var), F64);
    }

    fn translate_assign(&mut self, var: u32, value: &hir::Expr) {
        let value = self.translate_expr(value);
        self.builder.def_var(Variable::from_u32(var), value);
    }

    fn translate_expr(&mut self, expr: &hir::Expr) -> Value {
        match &expr.variant {
            &hir::ExprV::Literal(val) => self.builder.ins().f64const(val),
            &hir::ExprV::Var(var) => self.builder.use_var(Variable::from_u32(var)),
            hir::ExprV::Op1(op, e) => match op {
                ast::Op1::Neg => {
                    let value = self.translate_expr(e);
                    self.builder.ins().fneg(value)
                }
            },
            hir::ExprV::Op2(op, e1, e2) => {
                let v1 = self.translate_expr(e1);
                let v2 = self.translate_expr(e2);
                match op {
                    ast::Op2::Add => self.builder.ins().fadd(v1, v2),
                    ast::Op2::Sub => self.builder.ins().fsub(v1, v2),
                    ast::Op2::Mul => self.builder.ins().fmul(v1, v2),
                    ast::Op2::Div => self.builder.ins().fdiv(v1, v2),
                }
            }
            hir::ExprV::Call(_, _) => todo!(),
        }
    }
}
