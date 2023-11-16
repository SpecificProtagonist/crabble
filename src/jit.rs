use cranelift::prelude::{
    types::{F64, I64},
    *,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};

use crate::{
    default,
    hir::{self, Expr, Hir},
};

const ADDR: Type = I64;

struct Jit<'a> {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_description: DataDescription,
    module: JITModule,
    hir: &'a Hir,
    functions: Vec<FuncId>,
}

pub fn compile(hir: &Hir) -> *const u8 {
    let mut jit = Jit::new(hir);

    // Declare functions
    for fun in hir.functions.iter() {
        let sig = jit.make_sig(fun);
        let id = jit
            .module
            .declare_function(&format!("@{}", fun.id), Linkage::Local, &sig)
            .unwrap();
        jit.functions.push(id);
    }

    // Define functions
    for fun in hir.functions.iter() {
        jit.translate_fn(fun)
    }
    jit.module.finalize_definitions().unwrap();

    let entrypoint = jit.functions[0];
    jit.module.get_finalized_function(entrypoint)
}

impl<'a> Jit<'a> {
    fn new(hir: &'a Hir) -> Self {
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
            hir,
            functions: default(),
        }
    }

    fn make_sig(&mut self, fun: &hir::Function) -> Signature {
        let mut sig = self.module.make_signature();
        for _ in &fun.args {
            sig.params.push(AbiParam::new(F64));
        }
        sig.returns.push(AbiParam::new(F64));
        sig
    }

    fn translate_fn(&mut self, fun: &hir::Function) {
        self.ctx.func.signature = self.make_sig(fun);

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let mut vars = Vec::new();
        for (i, arg) in fun.args.iter().enumerate() {
            let val = builder.block_params(entry_block)[i];
            let var = Variable::from_u32(*arg);
            builder.declare_var(var, F64);
            builder.def_var(var, val);
            vars.push((arg.clone(), var));
        }

        let mut trans = FunctionTranslator {
            builder,
            module: &mut self.module,
            hir: self.hir,
            functions: &mut self.functions,
        };

        let return_value = trans.translate_expr(&fun.value);

        trans.builder.ins().return_(&[return_value]);
        trans.builder.finalize();

        let id = self.functions[fun.id as usize];

        self.module.define_function(id, &mut self.ctx).unwrap();
        self.module.clear_context(&mut self.ctx);
    }
}

struct FunctionTranslator<'a> {
    hir: &'a Hir,
    builder: FunctionBuilder<'a>,
    module: &'a mut JITModule,
    functions: &'a mut Vec<FuncId>,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_block(&mut self, block: &[hir::BlockExpr]) -> Value {
        let tmp_no_return = self.builder.ins().f64const(0);
        let mut return_value = tmp_no_return;
        for expr in block {
            return_value = match expr {
                hir::BlockExpr::Expr(e) => self.translate_expr(e),
                hir::BlockExpr::Decl(var) => {
                    self.translate_decl(*var);
                    tmp_no_return
                }
                hir::BlockExpr::Assign(var, value) => {
                    self.translate_assign(*var, value);
                    tmp_no_return
                }
            }
        }
        return_value
    }

    fn translate_decl(&mut self, var: u32) {
        self.builder.declare_var(Variable::from_u32(var), F64);
    }

    fn translate_assign(&mut self, var: u32, value: &hir::Expr) {
        let value = self.translate_expr(value);
        self.builder.def_var(Variable::from_u32(var), value);
    }

    fn translate_expr(&mut self, expr: &hir::Expr) -> Value {
        match &expr.variant {
            hir::ExprV::Const(val) => match val {
                hir::Const::Empty => todo!("proper types"),
                hir::Const::F64(num) => self.builder.ins().f64const(*num),
                hir::Const::Fun(fun_id) => {
                    let id = self.functions[*fun_id as usize];
                    let func_ref = self.module.declare_func_in_func(id, self.builder.func);
                    self.builder.ins().func_addr(ADDR, func_ref)
                }
            },
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
            hir::ExprV::Block(items) => self.translate_block(items),
            hir::ExprV::Call(callee, args) => {
                if let hir::ExprV::Const(hir::Const::Fun(fun_id)) = callee.variant {
                    let id = self.functions[fun_id as usize];
                    let local_callee = self.module.declare_func_in_func(id, self.builder.func);
                    let mut arg_values = Vec::new();
                    for arg in args {
                        arg_values.push(self.translate_expr(arg))
                    }
                    let call = self.builder.ins().call(local_callee, &arg_values);
                    self.builder.inst_results(call)[0]
                } else {
                    todo!("call_indirect: use import_signature; needs type information")
                }
            }
            hir::ExprV::Error => {
                // Erroneous code path executed, abort
                // TODO: actually abort
                self.builder.ins().f64const(0)
                // TODO: dead code elimination so there aren't any instructions after the trap
                // (because the block is already full)
                // self.builder.ins().trap(TrapCode::UnreachableCodeReached)
            }
        }
    }
}
