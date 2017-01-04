// There's a lot of unsafe stuff in this file. General word of advice: do not run this in multiple
// threads and everything will be fine

use parser;

use std::collections::HashMap;

use llvm_sys::core;
use llvm_sys::prelude::LLVMValueRef;
use llvm::{self, Module};

pub type IRBuildingResult = Result<LLVMValueRef, String>;

pub trait IRBuilder {
    fn codegen(&self, context: &mut Context, module: &mut Module) -> IRBuildingResult;
}

pub struct Context {
    context: &'static llvm::Context,
    builder: llvm::CSemiBox<'static, llvm::Builder>,
    named_values: HashMap<String, LLVMValueRef>,
    double_ty: &'static llvm::Type,
}

impl Context {
    pub fn new() -> Context {
        let context = unsafe { llvm::Context::get_global() };
        let builder = llvm::Builder::new(context);
        let named_values = HashMap::new();
        let double_ty = unsafe { core::LLVMDoubleType().into() }; // Everything is a double

        Context {
            context: context,
            builder: builder,
            named_values: named_values,
            double_ty: double_ty
        }
    }
}

impl IRBuilder for parser::Expr {
    fn codegen(&self, context: &mut Context, module: &mut Module) -> IRBuildingResult {
        match self {
            &parser::Expr::Ident(ref s) => {
                context.named_values.get(s)
                                    .map(|v| *v)
                                    .ok_or(format!("Unbound identifier '{}'", s))
            }
            &parser::Expr::NumLit(n) => {
                unsafe {
                    let ir_const = core::LLVMConstReal(context.double_ty.into(), n);
                    Ok(ir_const)
                }
            }
            &parser::Expr::BinaryOp { ref a, ref op, ref b } => {
                let ir_a = a.codegen(context, module)?;
                let ir_b = b.codegen(context, module)?;
                let builder = &context.builder;
                let ir_binop = match op.as_str() {
                    "+" => builder.build_add(ir_a.into(), ir_b.into()),
                    "-" => builder.build_sub(ir_a.into(), ir_b.into()),
                    "*" => builder.build_mul(ir_a.into(), ir_b.into()),
                    "/" => builder.build_div(ir_a.into(), ir_b.into()),
                    "**" => {
                        let powi: &llvm::Function = module.get_function("llvm.powi.f64")
                                                          .unwrap()
                                                          .into();
                        builder.build_call(powi, &[ir_a.into(), ir_b.into()])
                    }
                    _ => {
                        return Err(format!("Unrecognized operator '{}'", op));
                    }
                };

                Ok(ir_binop.into())
            }
            &parser::Expr::Call { ref target, ref args } => {
                let mut ir_args: Vec<&llvm::Value> = Vec::new();
                for arg in args {
                    let ir_arg = arg.codegen(context, module)?;
                    ir_args.push(ir_arg.into());
                }
                let func = module.get_function(target)
                                 .ok_or(format!("Undefined function '{}'", target))?;

                let ir_call = context.builder.build_call(func, &*ir_args);
                Ok(ir_call.into())
            }
        }
    }
}

impl IRBuilder for parser::Prototype {
    fn codegen(&self, context: &mut Context, module: &mut Module) -> IRBuildingResult {
        let name = &self.name;
        let num_args = self.args.len();

        // If the function or prototype has already been declared, error out
        if module.get_function(name).is_some() {
            return Err(format!("Duplicate prototype of function {}", name));
        }

        // Otherwise make a prototype and add it to the module
        let args_ty = vec![context.double_ty; num_args];
        let ir_func_ty = llvm::FunctionType::new(context.double_ty, &*args_ty);
        let func: &mut llvm::Function = module.add_function(name, &*ir_func_ty);

        unsafe {
            let mut ir_func_args = Vec::with_capacity(num_args);
            ir_func_args.set_len(num_args);
            core::LLVMGetParams(func.into(), ir_func_args.as_mut_ptr());
            for (arg, arg_name) in ir_func_args.into_iter().zip(self.args.iter()) {
                let val: &llvm::Value = arg.into();
                val.set_name(arg_name);
            }
        }
        Ok(func.into())
    }
}

impl IRBuilder for parser::Function {
    fn codegen(&self, context: &mut Context, module: &mut Module) -> IRBuildingResult {
        let ir_func: &mut llvm::Function = self.proto.codegen(context, module)?.into();

        // Extend named_values to the function arguments
        unsafe {
            let num_args = self.proto.args.len();
            let mut ir_func_args = Vec::with_capacity(num_args);
            ir_func_args.set_len(num_args);
            core::LLVMGetParams(ir_func.into(), ir_func_args.as_mut_ptr());
            for arg in ir_func_args {
                let val: &llvm::Value = arg.into();
                let name = val.get_name().expect("unnamed parameter!");
                context.named_values.insert(name.to_string(), arg);
            }
        }

        // This scope exists because bb borrows ir_func immutably
        {
            // Make a new basic block and point the builder to the end of the block. All things we
            // use the builder for will now go into this basic block
            let bb = ir_func.append("entry");
            context.builder.position_at_end(bb);

            // Put the function body in the BasicBlock of the function
            let ir_body = self.body.codegen(context, module)?;
            context.builder.build_ret(ir_body.into());
        }

        // Remove the function arguments from the scope
        context.named_values.clear();
        Ok(ir_func.into())
    }
}

impl IRBuilder for parser::Stmt {
    fn codegen(&self, context: &mut Context, module: &mut Module) -> IRBuildingResult {
        match self {
            &parser::Stmt::Prototype(ref proto) => proto.codegen(context, module),
            &parser::Stmt::Function(ref func) => func.codegen(context, module),
        }
    }
}

// Returns a reference to a new anonymous function with no arguments
fn new_lambda(context: &mut Context, module: &mut Module) -> LLVMValueRef {
    // Fn () -> f64
    let ir_func_ty = llvm::FunctionType::new(context.double_ty, &[]);
    module.add_function("", &*ir_func_ty).into()
}

impl IRBuilder for parser::StmtOrExpr {
    fn codegen(&self, context: &mut Context, module: &mut Module) -> IRBuildingResult {
        match self {
            &parser::StmtOrExpr::Stmt(ref stmt) => stmt.codegen(context, module),

            // Top-level expressions get treated as anonymous functions
            &parser::StmtOrExpr::Expr(ref expr) => {
                let ir_func: &mut llvm::Function = new_lambda(context, module).into();
                // This scope exists because bb borrows ir_func immutably
                {
                    // Make a new basic block and point the builder to the end of the block. All
                    // things we use the builder for will now go into this basic block
                    let bb = ir_func.append("entry");
                    context.builder.position_at_end(bb);

                    // Put the function body in the BasicBlock of the function
                    let ir_body = expr.codegen(context, module)?;
                    context.builder.build_ret(ir_body.into());
                }

                Ok(ir_func.into())
            }
        }
    }
}

impl IRBuilder for parser::ASTForest {
    fn codegen(&self, context: &mut Context, mut module: &mut Module) -> IRBuildingResult {
        let mut final_val = Err("Empty AST".into());
        for stmt_or_expr in self.iter() {
            final_val = Ok(stmt_or_expr.codegen(context, &mut module)?);
        }
        final_val
    }
}


pub fn new_module_and_ctx(mod_name: &str) -> (llvm::CSemiBox<Module>, Context) {
    let context = Context::new();
    let module = Module::new(mod_name, context.context);

    // Declare the intrinsics we need
    let powi_ty = llvm::FunctionType::new(context.double_ty,
                                          &[context.double_ty, context.double_ty]);
    module.add_function("llvm.powi.f64", &*powi_ty);

    (module, context)
}

#[test]
fn test_codegen() {
    use lexer;

    let input = "extern foo()\
                 extern bar(a, b)\
                 def baz(c, d, e) bar(c + d, e ** c) / (foo() * foo())";
    let expected_dump =
r#"; ModuleID = 'testmod'
source_filename = "testmod"

; Function Attrs: nounwind readnone
declare double @llvm.powi.f64(double, double) #0

declare double @foo()

declare double @bar(double, double)

define double @baz(double %c, double %d, double %e) {
entry:
  %0 = fadd double %c, %d
  %1 = call double @llvm.powi.f64(double %e, double %c)
  %2 = call double @bar(double %0, double %1)
  %3 = call double @foo()
  %4 = call double @foo()
  %5 = fmul double %3, %4
  %6 = fdiv double %2, %5
  ret double %6
}

attributes #0 = { nounwind readnone }
"#;

   let (mut module, mut context) = new_module_and_ctx("testmod");
   let tokens = lexer::tokenize(input);
   let (parsed, _) = parser::parse(&*tokens).unwrap();
   parsed.codegen(&mut context, &mut module).unwrap();

   assert_eq!(&*format!("{:?}", module), expected_dump);
}
