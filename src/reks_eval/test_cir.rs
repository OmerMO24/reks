use crate::reks_eval::cir::*;
use crate::reks_parse::{operators::*, utnode::*};
use crate::reks_type::{infer::*, resolve::*};

pub fn test_cir_ssa_simple() {
    let test_program = vec![
        // fn add(a: i32, b: i32) -> i32 { a + b }
        UntypedExpr::Fn {
            name: Value::Identifier("add"),
            params: vec![
                Param {
                    name: Value::Identifier("a"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("b"),
                    ty: Value::Identifier("i32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
                }],
            }),
        },
        // fn main() -> i32 { add(5, 3) }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::Call {
                    name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                    args: vec![
                        UntypedExpr::Value(Value::Num(5)),
                        UntypedExpr::Value(Value::Num(3)),
                    ],
                }],
            }),
        },
    ];

    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let typed_ast = match inferencer.infer_program(&test_program) {
        Ok(ast) => ast,
        Err(errors) => {
            println!("Inference errors:");
            for err in errors {
                println!("  {:?}", err);
            }
            return;
        }
    };

    let mut builder = SSACIRBuilder::new();
    let cir = builder.lower_program(&typed_ast);
    println!("SSA CIR Blocks:");
    for block in &cir.blocks {
        println!("Block {}:", block.id);
        for (i, instr) in block.instructions.iter().enumerate() {
            println!("  {}: {} = {:?}", i, instr.result.0, instr.op);
        }
    }
}
