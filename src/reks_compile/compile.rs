use crate::reks_compile::reksgen::LLVMCodegen;
use crate::reks_eval::cir::SSACIRBuilder;
use crate::reks_parse::utnode::UntypedExpr;
use crate::reks_type::infer::TypeInferencer;
use crate::reks_type::resolve::NameResolver;
use inkwell::context::Context;

pub fn compile(program: Vec<UntypedExpr>) {
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let typed_ast = inferencer.infer_program(&program).unwrap();
    let mut builder = SSACIRBuilder::new();
    let cir = builder.lower_program(&typed_ast);

    // Print CIR for debugging
    println!("SSA CIR Blocks:");
    for block in &cir.blocks {
        println!("Block {}:", block.id);
        for (i, instr) in block.instructions.iter().enumerate() {
            println!("  {}: {} = {:?}", i, instr.result.0, instr.op);
        }
    }

    // LLVM codegen without consteval
    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context);
    codegen.codegen_cir(&cir);
    println!("LLVM IR (before writing):\n{}", codegen.print_to_string());
    codegen.print_to_file("output.ll").unwrap();
}
