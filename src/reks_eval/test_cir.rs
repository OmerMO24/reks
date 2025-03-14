use crate::reks_eval::{cir::*, ripcore::*};
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
    let mut interp = Interpreter::new(cir);
    let result = interp.run(1); // Block 1 is main
    println!("Result: {:?}", result);
}

pub fn test_cir_ssa_complex() {
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
        // fn triple(x: i32) -> i32 { add(x, add(x, x)) }
        UntypedExpr::Fn {
            name: Value::Identifier("triple"),
            params: vec![Param {
                name: Value::Identifier("x"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::Call {
                    name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                    args: vec![
                        UntypedExpr::Value(Value::Identifier("x")),
                        UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Identifier("x")),
                            ],
                        },
                    ],
                }],
            }),
        },
        // fn main() -> i32 { triple(5) }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::Call {
                    name: Box::new(UntypedExpr::Value(Value::Identifier("triple"))),
                    args: vec![UntypedExpr::Value(Value::Num(5))],
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

pub fn test_cir_ssa_conditionals() {
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
        // fn main() -> i32 { if 5 > 3 { add(5, 3) } else { 10 } }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::If {
                    condition: Box::new(UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Value(Value::Num(5))),
                        op: InfixOpKind::Greater,
                        right: Box::new(UntypedExpr::Value(Value::Num(3))),
                    }),
                    then_branch: Box::new(UntypedExpr::Call {
                        name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                        args: vec![
                            UntypedExpr::Value(Value::Num(5)),
                            UntypedExpr::Value(Value::Num(3)),
                        ],
                    }),
                    else_branch: Box::new(UntypedExpr::Value(Value::Num(10))),
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
    let mut interp = Interpreter::new(cir);
    let result = interp.run(1); // Block 1 is main
    println!("Result: {:?}", result);
}

pub fn test_cir_ssa_structs() {
    let test_program = vec![
        UntypedExpr::Struct {
            id: Value::Identifier("P"),
            fields: vec![
                Param {
                    name: Value::Identifier("upper"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("lower"),
                    ty: Value::Identifier("i32"),
                },
            ],
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(5))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("p"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::StructInit {
                            id: Value::Identifier("P"),
                            fields: vec![
                                (
                                    Value::Identifier("upper"),
                                    UntypedExpr::Value(Value::Identifier("x")),
                                ),
                                (
                                    Value::Identifier("lower"),
                                    UntypedExpr::Value(Value::Num(2)),
                                ),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::FieldAccess {
                        id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                        field: Value::Identifier("upper"),
                    },
                ],
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
    let mut interp = Interpreter::new(cir);
    let result = interp.run(0); // Changed to run(0)
    println!("Result: {:?}", result);
}
