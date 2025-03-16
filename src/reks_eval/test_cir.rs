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

pub fn test_cir_ssa_assignments() {
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
                        constness: Const::No, // Mutable
                    },
                    UntypedExpr::Assign {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                        right: Box::new(UntypedExpr::Value(Value::Num(10))),
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
    let result = interp.run(0); // Block 0 since main is first
    println!("Result: {:?}", result);
}

pub fn test_cir_ssa_lists() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                UntypedExpr::Let {
                    id: Value::Identifier("l"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::List {
                        items: vec![
                            UntypedExpr::Value(Value::Num(1)),
                            UntypedExpr::Value(Value::Num(2)),
                            UntypedExpr::Value(Value::Num(3)),
                        ],
                    }),
                    constness: Const::Yes,
                },
                UntypedExpr::Index {
                    expr: Box::new(UntypedExpr::Value(Value::Identifier("l"))),
                    index: Box::new(UntypedExpr::Value(Value::Num(1))),
                },
            ],
        }),
    }];

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
    let result = interp.run(0);
    println!("Result: {:?}", result);
}

pub fn test_cir_ssa_binops() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![UntypedExpr::BinOp {
                left: Box::new(UntypedExpr::Value(Value::Num(5))),
                op: InfixOpKind::Sub,
                right: Box::new(UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Value(Value::Num(2))),
                        op: InfixOpKind::Mul,
                        right: Box::new(UntypedExpr::Value(Value::Num(3))),
                    }),
                    op: InfixOpKind::Div,
                    right: Box::new(UntypedExpr::Value(Value::Num(2))),
                }),
            }],
        }),
    }];

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
    let result = interp.run(0);
    println!("Result: {:?}", result);
}

pub fn test_cir_ssa_binops_extended() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![UntypedExpr::BinOp {
                left: Box::new(UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Num(2))),
                    op: InfixOpKind::Exp,
                    right: Box::new(UntypedExpr::Value(Value::Num(3))),
                }),
                op: InfixOpKind::Mod,
                right: Box::new(UntypedExpr::Value(Value::Num(5))),
            }],
        }),
    }];

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
    let result = interp.run(0);
    println!("Result: {:?}", result);
}

pub fn test_cir_ssa_add() {
    let test_program = vec![
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
            body: Box::new(UntypedExpr::BinOp {
                left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                op: InfixOpKind::Add,
                right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Call {
                name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                args: vec![
                    UntypedExpr::Value(Value::Num(3)),
                    UntypedExpr::Value(Value::Num(4)),
                ],
            }),
        },
    ];

    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    println!("The resolution map: {:?}", resolution_map);
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
    let result = interp.run(1); // Block 1 for main
    println!("Result: {:?}", result);
}

pub fn test_cir_ssa_factorial_simple() {
    let test_program = vec![
        UntypedExpr::Fn {
            name: Value::Identifier("factorial"),
            params: vec![Param {
                name: Value::Identifier("n"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("result"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(1))),
                        constness: Const::No,
                    },
                    UntypedExpr::If {
                        condition: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("n"))),
                            op: InfixOpKind::Greater,
                            right: Box::new(UntypedExpr::Value(Value::Num(0))),
                        }),
                        then_branch: Box::new(UntypedExpr::Block {
                            statements: vec![UntypedExpr::Assign {
                                left: Box::new(UntypedExpr::Value(Value::Identifier("result"))),
                                right: Box::new(UntypedExpr::BinOp {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("result"))),
                                    op: InfixOpKind::Mul,
                                    right: Box::new(UntypedExpr::Value(Value::Identifier("n"))),
                                }),
                            }],
                        }),
                        else_branch: Box::new(UntypedExpr::Block {
                            statements: vec![UntypedExpr::Value(Value::Num(1))],
                        }),
                    },
                    UntypedExpr::Value(Value::Identifier("result")),
                ],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Call {
                name: Box::new(UntypedExpr::Value(Value::Identifier("factorial"))),
                args: vec![UntypedExpr::Value(Value::Num(100))],
            }),
        },
    ];

    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    println!("The resolution map: {:?}", resolution_map);
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
    let result = interp.run(1); // Block 1 for main
    println!("Result: {:?}", result);
}

use crate::reks_parse::operators::InfixOpKind;
use crate::reks_parse::utnode::{Const, Param, TypePath, Value};
use crate::reks_type::infer::{Type, TypeInfo, TypedExpr, TypedExprKind};

pub fn test_cir_factorial_direct() {
    let typed_ast = vec![
        // fn factorial(n: i32) -> i32
        TypedExpr {
            kind: TypedExprKind::Fn {
                name: Value::Identifier("factorial"),
                params: vec![Param {
                    name: Value::Identifier("n"),
                    ty: Value::Identifier("i32"),
                }],
                retty: Box::new(TypedExpr {
                    kind: TypedExprKind::Value(Value::Identifier("i32")),
                    type_info: TypeInfo::Known(Type::Int),
                    node_id: 0,
                }),
                body: Box::new(TypedExpr {
                    kind: TypedExprKind::Block {
                        statements: vec![
                            // if n > 0 { n * factorial(n - 1) } else { 1 }
                            TypedExpr {
                                kind: TypedExprKind::If {
                                    condition: Box::new(TypedExpr {
                                        kind: TypedExprKind::BinOp {
                                            left: Box::new(TypedExpr {
                                                kind: TypedExprKind::Value(Value::Identifier("n")),
                                                type_info: TypeInfo::Known(Type::Int),
                                                node_id: 1,
                                            }),
                                            op: InfixOpKind::Greater,
                                            right: Box::new(TypedExpr {
                                                kind: TypedExprKind::Value(Value::Num(0)),
                                                type_info: TypeInfo::Known(Type::Int),
                                                node_id: 2,
                                            }),
                                        },
                                        type_info: TypeInfo::Known(Type::Bool),
                                        node_id: 3,
                                    }),
                                    then_branch: Box::new(TypedExpr {
                                        kind: TypedExprKind::BinOp {
                                            left: Box::new(TypedExpr {
                                                kind: TypedExprKind::Value(Value::Identifier("n")),
                                                type_info: TypeInfo::Known(Type::Int),
                                                node_id: 4,
                                            }),
                                            op: InfixOpKind::Mul,
                                            right: Box::new(TypedExpr {
                                                kind: TypedExprKind::Call {
                                                    name: Box::new(TypedExpr {
                                                        kind: TypedExprKind::Value(
                                                            Value::Identifier("factorial"),
                                                        ),
                                                        type_info: TypeInfo::Known(Type::Function(
                                                            vec![Type::Int],
                                                            Box::new(Type::Int),
                                                        )),
                                                        node_id: 5,
                                                    }),
                                                    args: vec![TypedExpr {
                                                        kind: TypedExprKind::BinOp {
                                                            left: Box::new(TypedExpr {
                                                                kind: TypedExprKind::Value(
                                                                    Value::Identifier("n"),
                                                                ),
                                                                type_info: TypeInfo::Known(
                                                                    Type::Int,
                                                                ),
                                                                node_id: 6,
                                                            }),
                                                            op: InfixOpKind::Sub,
                                                            right: Box::new(TypedExpr {
                                                                kind: TypedExprKind::Value(
                                                                    Value::Num(1),
                                                                ),
                                                                type_info: TypeInfo::Known(
                                                                    Type::Int,
                                                                ),
                                                                node_id: 7,
                                                            }),
                                                        },
                                                        type_info: TypeInfo::Known(Type::Int),
                                                        node_id: 8,
                                                    }],
                                                },
                                                type_info: TypeInfo::Known(Type::Int),
                                                node_id: 9,
                                            }),
                                        },
                                        type_info: TypeInfo::Known(Type::Int),
                                        node_id: 10,
                                    }),
                                    else_branch: Box::new(TypedExpr {
                                        kind: TypedExprKind::Block {
                                            statements: vec![TypedExpr {
                                                kind: TypedExprKind::Value(Value::Num(1)),
                                                type_info: TypeInfo::Known(Type::Int),
                                                node_id: 11,
                                            }],
                                        },
                                        type_info: TypeInfo::Known(Type::Int),
                                        node_id: 12,
                                    }),
                                },
                                type_info: TypeInfo::Known(Type::Int),
                                node_id: 13,
                            },
                        ],
                    },
                    type_info: TypeInfo::Known(Type::Int),
                    node_id: 14,
                }),
            },
            type_info: TypeInfo::Known(Type::Function(vec![Type::Int], Box::new(Type::Int))),
            node_id: 15,
        },
        // fn main() -> i32 { factorial(5) }
        TypedExpr {
            kind: TypedExprKind::Fn {
                name: Value::Identifier("main"),
                params: vec![],
                retty: Box::new(TypedExpr {
                    kind: TypedExprKind::Value(Value::Identifier("i32")),
                    type_info: TypeInfo::Known(Type::Int),
                    node_id: 16,
                }),
                body: Box::new(TypedExpr {
                    kind: TypedExprKind::Call {
                        name: Box::new(TypedExpr {
                            kind: TypedExprKind::Value(Value::Identifier("factorial")),
                            type_info: TypeInfo::Known(Type::Function(
                                vec![Type::Int],
                                Box::new(Type::Int),
                            )),
                            node_id: 17,
                        }),
                        args: vec![TypedExpr {
                            kind: TypedExprKind::Value(Value::Num(10)),
                            type_info: TypeInfo::Known(Type::Int),
                            node_id: 18,
                        }],
                    },
                    type_info: TypeInfo::Known(Type::Int),
                    node_id: 19,
                }),
            },
            type_info: TypeInfo::Known(Type::Function(vec![], Box::new(Type::Int))),
            node_id: 20,
        },
    ];

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
    let result = interp.run(1); // Block 1 for main
    println!("Result: {:?}", result);
}

pub fn test_cir_fib_direct() {
    let typed_ast = vec![
        // fn fib(n: i32) -> i32
        TypedExpr {
            kind: TypedExprKind::Fn {
                name: Value::Identifier("fib"),
                params: vec![Param {
                    name: Value::Identifier("n"),
                    ty: Value::Identifier("i32"),
                }],
                retty: Box::new(TypedExpr {
                    kind: TypedExprKind::Value(Value::Identifier("i32")),
                    type_info: TypeInfo::Known(Type::Int),
                    node_id: 0,
                }),
                body: Box::new(TypedExpr {
                    kind: TypedExprKind::Block {
                        statements: vec![
                            // if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
                            TypedExpr {
                                kind: TypedExprKind::If {
                                    condition: Box::new(TypedExpr {
                                        kind: TypedExprKind::BinOp {
                                            left: Box::new(TypedExpr {
                                                kind: TypedExprKind::Value(Value::Identifier("n")),
                                                type_info: TypeInfo::Known(Type::Int),
                                                node_id: 1,
                                            }),
                                            op: InfixOpKind::LessOrEq, // <=
                                            right: Box::new(TypedExpr {
                                                kind: TypedExprKind::Value(Value::Num(1)),
                                                type_info: TypeInfo::Known(Type::Int),
                                                node_id: 2,
                                            }),
                                        },
                                        type_info: TypeInfo::Known(Type::Bool),
                                        node_id: 3,
                                    }),
                                    then_branch: Box::new(TypedExpr {
                                        kind: TypedExprKind::Value(Value::Identifier("n")),
                                        type_info: TypeInfo::Known(Type::Int),
                                        node_id: 4,
                                    }),
                                    else_branch: Box::new(TypedExpr {
                                        kind: TypedExprKind::BinOp {
                                            left: Box::new(TypedExpr {
                                                kind: TypedExprKind::Call {
                                                    name: Box::new(TypedExpr {
                                                        kind: TypedExprKind::Value(
                                                            Value::Identifier("fib"),
                                                        ),
                                                        type_info: TypeInfo::Known(Type::Function(
                                                            vec![Type::Int],
                                                            Box::new(Type::Int),
                                                        )),
                                                        node_id: 5,
                                                    }),
                                                    args: vec![TypedExpr {
                                                        kind: TypedExprKind::BinOp {
                                                            left: Box::new(TypedExpr {
                                                                kind: TypedExprKind::Value(
                                                                    Value::Identifier("n"),
                                                                ),
                                                                type_info: TypeInfo::Known(
                                                                    Type::Int,
                                                                ),
                                                                node_id: 6,
                                                            }),
                                                            op: InfixOpKind::Sub,
                                                            right: Box::new(TypedExpr {
                                                                kind: TypedExprKind::Value(
                                                                    Value::Num(1),
                                                                ),
                                                                type_info: TypeInfo::Known(
                                                                    Type::Int,
                                                                ),
                                                                node_id: 7,
                                                            }),
                                                        },
                                                        type_info: TypeInfo::Known(Type::Int),
                                                        node_id: 8,
                                                    }],
                                                },
                                                type_info: TypeInfo::Known(Type::Int),
                                                node_id: 9,
                                            }),
                                            op: InfixOpKind::Add,
                                            right: Box::new(TypedExpr {
                                                kind: TypedExprKind::Call {
                                                    name: Box::new(TypedExpr {
                                                        kind: TypedExprKind::Value(
                                                            Value::Identifier("fib"),
                                                        ),
                                                        type_info: TypeInfo::Known(Type::Function(
                                                            vec![Type::Int],
                                                            Box::new(Type::Int),
                                                        )),
                                                        node_id: 10,
                                                    }),
                                                    args: vec![TypedExpr {
                                                        kind: TypedExprKind::BinOp {
                                                            left: Box::new(TypedExpr {
                                                                kind: TypedExprKind::Value(
                                                                    Value::Identifier("n"),
                                                                ),
                                                                type_info: TypeInfo::Known(
                                                                    Type::Int,
                                                                ),
                                                                node_id: 11,
                                                            }),
                                                            op: InfixOpKind::Sub,
                                                            right: Box::new(TypedExpr {
                                                                kind: TypedExprKind::Value(
                                                                    Value::Num(2),
                                                                ),
                                                                type_info: TypeInfo::Known(
                                                                    Type::Int,
                                                                ),
                                                                node_id: 12,
                                                            }),
                                                        },
                                                        type_info: TypeInfo::Known(Type::Int),
                                                        node_id: 13,
                                                    }],
                                                },
                                                type_info: TypeInfo::Known(Type::Int),
                                                node_id: 14,
                                            }),
                                        },
                                        type_info: TypeInfo::Known(Type::Int),
                                        node_id: 15,
                                    }),
                                },
                                type_info: TypeInfo::Known(Type::Int),
                                node_id: 16,
                            },
                        ],
                    },
                    type_info: TypeInfo::Known(Type::Int),
                    node_id: 17,
                }),
            },
            type_info: TypeInfo::Known(Type::Function(vec![Type::Int], Box::new(Type::Int))),
            node_id: 18,
        },
        // fn main() -> i32 { fib(6) }
        TypedExpr {
            kind: TypedExprKind::Fn {
                name: Value::Identifier("main"),
                params: vec![],
                retty: Box::new(TypedExpr {
                    kind: TypedExprKind::Value(Value::Identifier("i32")),
                    type_info: TypeInfo::Known(Type::Int),
                    node_id: 19,
                }),
                body: Box::new(TypedExpr {
                    kind: TypedExprKind::Call {
                        name: Box::new(TypedExpr {
                            kind: TypedExprKind::Value(Value::Identifier("fib")),
                            type_info: TypeInfo::Known(Type::Function(
                                vec![Type::Int],
                                Box::new(Type::Int),
                            )),
                            node_id: 20,
                        }),
                        args: vec![TypedExpr {
                            kind: TypedExprKind::Value(Value::Num(14)),
                            type_info: TypeInfo::Known(Type::Int),
                            node_id: 21,
                        }],
                    },
                    type_info: TypeInfo::Known(Type::Int),
                    node_id: 22,
                }),
            },
            type_info: TypeInfo::Known(Type::Function(vec![], Box::new(Type::Int))),
            node_id: 23,
        },
    ];

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
    let result = interp.run(1); // Block 1 for main
    println!("Result: {:?}", result);
}

pub fn test_compute_four() {
    let test_program = vec![
        UntypedExpr::Fn {
            name: Value::Identifier("compute"),
            params: vec![Param {
                name: Value::Identifier("x"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("a"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(2))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::If {
                        condition: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                            op: InfixOpKind::Greater,
                            right: Box::new(UntypedExpr::Value(Value::Num(0))),
                        }),
                        then_branch: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::BinOp {
                                left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                                op: InfixOpKind::Mul,
                                right: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                            }),
                            op: InfixOpKind::Add,
                            right: Box::new(UntypedExpr::Value(Value::Num(3))),
                        }),
                        else_branch: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                            op: InfixOpKind::Sub,
                            right: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                        }),
                    },
                ],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Call {
                name: Box::new(UntypedExpr::Value(Value::Identifier("compute"))),
                args: vec![UntypedExpr::Value(Value::Num(4))],
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

pub fn test_multiple_assignments() {
    let test_program = vec![
        UntypedExpr::Fn {
            name: Value::Identifier("adjust"),
            params: vec![Param {
                name: Value::Identifier("x"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("y"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                        constness: Const::No,
                    },
                    UntypedExpr::Assign {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("y"))),
                        right: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("y"))),
                            op: InfixOpKind::Add,
                            right: Box::new(UntypedExpr::Value(Value::Num(2))),
                        }),
                    },
                    UntypedExpr::Assign {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("y"))),
                        right: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("y"))),
                            op: InfixOpKind::Mul,
                            right: Box::new(UntypedExpr::Value(Value::Num(3))),
                        }),
                    },
                    UntypedExpr::Value(Value::Identifier("y")),
                ],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Call {
                name: Box::new(UntypedExpr::Value(Value::Identifier("adjust"))),
                args: vec![UntypedExpr::Value(Value::Num(5))],
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

pub fn test_struct_stuff() {
    let test_program = vec![
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("i32"),
                },
            ],
        },
        UntypedExpr::Fn {
            name: Value::Identifier("get_x"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("p"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::StructInit {
                            id: Value::Identifier("Point"),
                            fields: vec![
                                (Value::Identifier("x"), UntypedExpr::Value(Value::Num(7))),
                                (Value::Identifier("y"), UntypedExpr::Value(Value::Num(9))),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::FieldAccess {
                        id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                        field: Value::Identifier("x"),
                    },
                ],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Call {
                name: Box::new(UntypedExpr::Value(Value::Identifier("get_x"))),
                args: vec![],
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

pub fn test_untyped_factorial() {
    let test_program = vec![
        UntypedExpr::Fn {
            name: Value::Identifier("factorial"),
            params: vec![Param {
                name: Value::Identifier("n"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::If {
                condition: Box::new(UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("n"))),
                    op: InfixOpKind::Greater,
                    right: Box::new(UntypedExpr::Value(Value::Num(1))),
                }),
                then_branch: Box::new(UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("n"))),
                    op: InfixOpKind::Mul,
                    right: Box::new(UntypedExpr::Call {
                        name: Box::new(UntypedExpr::Value(Value::Identifier("factorial"))),
                        args: vec![UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("n"))),
                            op: InfixOpKind::Sub,
                            right: Box::new(UntypedExpr::Value(Value::Num(1))),
                        }],
                    }),
                }),
                else_branch: Box::new(UntypedExpr::Value(Value::Num(1))),
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Call {
                name: Box::new(UntypedExpr::Value(Value::Identifier("factorial"))),
                args: vec![UntypedExpr::Value(Value::Num(5))],
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

pub fn test_multiple_functions() {
    let test_program = vec![
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
            body: Box::new(UntypedExpr::BinOp {
                left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                op: InfixOpKind::Add,
                right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("mul"),
            params: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("i32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::If {
                condition: Box::new(UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("y"))),
                    op: InfixOpKind::Greater,
                    right: Box::new(UntypedExpr::Value(Value::Num(0))),
                }),
                then_branch: Box::new(UntypedExpr::Call {
                    name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                    args: vec![
                        UntypedExpr::Value(Value::Identifier("x")),
                        UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("mul"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::BinOp {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("y"))),
                                    op: InfixOpKind::Sub,
                                    right: Box::new(UntypedExpr::Value(Value::Num(1))),
                                },
                            ],
                        },
                    ],
                }),
                else_branch: Box::new(UntypedExpr::Value(Value::Num(0))),
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("compute"),
            params: vec![Param {
                name: Value::Identifier("n"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::BinOp {
                left: Box::new(UntypedExpr::Call {
                    name: Box::new(UntypedExpr::Value(Value::Identifier("mul"))),
                    args: vec![
                        UntypedExpr::Value(Value::Identifier("n")),
                        UntypedExpr::Value(Value::Num(3)),
                    ],
                }),
                op: InfixOpKind::Add,
                right: Box::new(UntypedExpr::Call {
                    name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                    args: vec![
                        UntypedExpr::Value(Value::Identifier("n")),
                        UntypedExpr::Value(Value::Num(2)),
                    ],
                }),
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Call {
                name: Box::new(UntypedExpr::Value(Value::Identifier("compute"))),
                args: vec![UntypedExpr::Value(Value::Num(4))],
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
