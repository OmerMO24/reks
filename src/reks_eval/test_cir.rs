use crate::reks_eval::cir::*;
use crate::reks_parse::{operators::*, utnode::*};
use crate::reks_type::{infer::*, resolve::*};

pub fn test_cir_lowering_simple() {
    let test_program = vec![
        // struct Point { x: i32 }
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![Param {
                name: Value::Identifier("x"),
                ty: Value::Identifier("i32"),
            }],
        },
        // fn double(x: i32) -> i32 { x + x }
        UntypedExpr::Fn {
            name: Value::Identifier("double"),
            params: vec![Param {
                name: Value::Identifier("x"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                }],
            }),
        },
        // fn main(p: Point) -> i32 { if p.x > 0 { double(p.x) } else { 42 } }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("p"),
                ty: Value::Identifier("Point"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::If {
                    condition: Box::new(UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                            field: Value::Identifier("x"),
                        }),
                        op: InfixOpKind::Greater,
                        right: Box::new(UntypedExpr::Value(Value::Num(0))),
                    }),
                    then_branch: Box::new(UntypedExpr::Call {
                        name: Box::new(UntypedExpr::Value(Value::Identifier("double"))),
                        args: vec![UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                            field: Value::Identifier("x"),
                        }],
                    }),
                    else_branch: Box::new(UntypedExpr::Value(Value::Num(42))),
                }],
            }),
        },
    ];

    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    println!("Resolution Map: {:?}", resolution_map);
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

    let mut builder = CIRBuilder::new(resolution_map.clone());
    let (main_cir, functions) = builder.lower_program(&typed_ast);
    println!("Main CIR Instructions:");
    for (i, instr) in main_cir.iter().enumerate() {
        println!("{}: {:?}", i, instr.op);
    }
    println!("\nFunction CIR Instructions:");
    for (node_id, cir) in functions {
        println!("Function NodeId: {:?}", node_id);
        for (i, instr) in cir.iter().enumerate() {
            println!("  {}: {:?}", i, instr.op);
        }
    }
}

pub fn test_cir_nested_structs() {
    let test_program = vec![
        // struct Vector { dx: i32, dy: i32 }
        UntypedExpr::Struct {
            id: Value::Identifier("Vector"),
            fields: vec![
                Param {
                    name: Value::Identifier("dx"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("dy"),
                    ty: Value::Identifier("i32"),
                },
            ],
        },
        // struct Point { x: i32, v: Vector }
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("v"),
                    ty: Value::Identifier("Vector"),
                },
            ],
        },
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
        // fn move_point(p: Point, delta: i32) -> i32 { add(add(p.x, p.v.dx), delta) }
        UntypedExpr::Fn {
            name: Value::Identifier("move_point"),
            params: vec![
                Param {
                    name: Value::Identifier("p"),
                    ty: Value::Identifier("Point"),
                },
                Param {
                    name: Value::Identifier("delta"),
                    ty: Value::Identifier("i32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::Call {
                    name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                    args: vec![
                        UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::FieldAccess {
                                    id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                    field: Value::Identifier("x"),
                                },
                                UntypedExpr::FieldAccess {
                                    id: Box::new(UntypedExpr::FieldAccess {
                                        id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                        field: Value::Identifier("v"),
                                    }),
                                    field: Value::Identifier("dx"),
                                },
                            ],
                        },
                        UntypedExpr::Value(Value::Identifier("delta")),
                    ],
                }],
            }),
        },
        // fn main(p: Point) -> i32 { const base = move_point(p, 5); const offset = add(p.v.dy, base); offset }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("p"),
                ty: Value::Identifier("Point"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("base"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("move_point"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("p")),
                                UntypedExpr::Value(Value::Num(5)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("offset"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::FieldAccess {
                                    id: Box::new(UntypedExpr::FieldAccess {
                                        id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                        field: Value::Identifier("v"),
                                    }),
                                    field: Value::Identifier("dy"),
                                },
                                UntypedExpr::Value(Value::Identifier("base")),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Identifier("offset")),
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

    let mut builder = CIRBuilder::new(resolution_map.clone());
    let (main_cir, functions) = builder.lower_program(&typed_ast);
    println!("Main CIR Instructions:");
    for (i, instr) in main_cir.iter().enumerate() {
        println!("{}: {:?}", i, instr.op);
    }
    println!("\nFunction CIR Instructions:");
    for (node_id, cir) in functions {
        println!("Function NodeId: {:?}", node_id);
        for (i, instr) in cir.iter().enumerate() {
            println!("  {}: {:?}", i, instr.op);
        }
    }
}

pub fn test_cir_nested_conditionals() {
    let test_program = vec![
        // struct Point { x: i32, y: i32 }
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
        // fn main(p: Point) -> i32 { mut x: i32 = p.x; if p.x > 0 { if p.y != 0 { x = add(p.x, p.y) } else { x = 10 } } x }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("p"),
                ty: Value::Identifier("Point"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("i32"),
                        },
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                            field: Value::Identifier("x"),
                        }),
                        constness: Const::No,
                    },
                    UntypedExpr::If {
                        condition: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::FieldAccess {
                                id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                field: Value::Identifier("x"),
                            }),
                            op: InfixOpKind::Greater,
                            right: Box::new(UntypedExpr::Value(Value::Num(0))),
                        }),
                        then_branch: Box::new(UntypedExpr::Block {
                            statements: vec![UntypedExpr::If {
                                condition: Box::new(UntypedExpr::BinOp {
                                    left: Box::new(UntypedExpr::FieldAccess {
                                        id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                        field: Value::Identifier("y"),
                                    }),
                                    op: InfixOpKind::NotEq,
                                    right: Box::new(UntypedExpr::Value(Value::Num(0))),
                                }),
                                then_branch: Box::new(UntypedExpr::Block {
                                    statements: vec![UntypedExpr::Assign {
                                        left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                                        right: Box::new(UntypedExpr::Call {
                                            name: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "add",
                                            ))),
                                            args: vec![
                                                UntypedExpr::FieldAccess {
                                                    id: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("p"),
                                                    )),
                                                    field: Value::Identifier("x"),
                                                },
                                                UntypedExpr::FieldAccess {
                                                    id: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("p"),
                                                    )),
                                                    field: Value::Identifier("y"),
                                                },
                                            ],
                                        }),
                                    }],
                                }),
                                else_branch: Box::new(UntypedExpr::Block {
                                    statements: vec![UntypedExpr::Assign {
                                        left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                                        right: Box::new(UntypedExpr::Value(Value::Num(10))),
                                    }],
                                }),
                            }],
                        }),
                        else_branch: Box::new(UntypedExpr::Block { statements: vec![] }),
                    },
                    UntypedExpr::Value(Value::Identifier("x")),
                ],
            }),
        },
    ];

    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    println!("Resolution Map: {:?}", resolution_map);
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

    let mut builder = CIRBuilder::new(resolution_map.clone());
    let (main_cir, functions) = builder.lower_program(&typed_ast);
    println!("Main CIR Instructions:");
    for (i, instr) in main_cir.iter().enumerate() {
        println!("{}: {:?}", i, instr.op);
    }
    println!("\nFunction CIR Instructions:");
    for (node_id, cir) in functions {
        println!("Function NodeId: {:?}", node_id);
        for (i, instr) in cir.iter().enumerate() {
            println!("  {}: {:?}", i, instr.op);
        }
    }
}

pub fn test_cir_lists_and_edge_cases() {
    let test_program = vec![
        // struct Vector { dx: i32, dy: i32 }
        UntypedExpr::Struct {
            id: Value::Identifier("Vector"),
            fields: vec![
                Param {
                    name: Value::Identifier("dx"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("dy"),
                    ty: Value::Identifier("i32"),
                },
            ],
        },
        // struct Point { x: i32, v: Vector }
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("i32"),
                },
                Param {
                    name: Value::Identifier("v"),
                    ty: Value::Identifier("Vector"),
                },
            ],
        },
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
        // fn double(x: i32) -> i32 { add(x, x) }
        UntypedExpr::Fn {
            name: Value::Identifier("double"),
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
                        UntypedExpr::Value(Value::Identifier("x")),
                    ],
                }],
            }),
        },
        // fn main(p: Point) -> i32 { ... }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("p"),
                ty: Value::Identifier("Point"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("distances"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::List {
                            items: vec![
                                UntypedExpr::Call {
                                    name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                                    args: vec![
                                        UntypedExpr::FieldAccess {
                                            id: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "p",
                                            ))),
                                            field: Value::Identifier("x"),
                                        },
                                        UntypedExpr::Value(Value::Num(1)),
                                    ],
                                },
                                UntypedExpr::Call {
                                    name: Box::new(UntypedExpr::Value(Value::Identifier("double"))),
                                    args: vec![UntypedExpr::FieldAccess {
                                        id: Box::new(UntypedExpr::FieldAccess {
                                            id: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "p",
                                            ))),
                                            field: Value::Identifier("v"),
                                        }),
                                        field: Value::Identifier("dx"),
                                    }],
                                },
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("i32"),
                        },
                        expr: Box::new(UntypedExpr::Value(Value::Identifier("distances"))),
                        constness: Const::No,
                    },
                    UntypedExpr::If {
                        condition: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::FieldAccess {
                                id: Box::new(UntypedExpr::FieldAccess {
                                    id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                    field: Value::Identifier("v"),
                                }),
                                field: Value::Identifier("dy"),
                            }),
                            op: InfixOpKind::Greater,
                            right: Box::new(UntypedExpr::Value(Value::Num(0))),
                        }),
                        then_branch: Box::new(UntypedExpr::Block {
                            statements: vec![UntypedExpr::Assign {
                                left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                                right: Box::new(UntypedExpr::Call {
                                    name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                                    args: vec![
                                        UntypedExpr::Call {
                                            name: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "double",
                                            ))),
                                            args: vec![UntypedExpr::FieldAccess {
                                                id: Box::new(UntypedExpr::Value(
                                                    Value::Identifier("p"),
                                                )),
                                                field: Value::Identifier("x"),
                                            }],
                                        },
                                        UntypedExpr::FieldAccess {
                                            id: Box::new(UntypedExpr::FieldAccess {
                                                id: Box::new(UntypedExpr::Value(
                                                    Value::Identifier("p"),
                                                )),
                                                field: Value::Identifier("v"),
                                            }),
                                            field: Value::Identifier("dy"),
                                        },
                                    ],
                                }),
                            }],
                        }),
                        else_branch: Box::new(UntypedExpr::Block { statements: vec![] }),
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("total"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Num(5)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Assign {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("total"))),
                        right: Box::new(UntypedExpr::Value(Value::Num(10))),
                    },
                    UntypedExpr::Value(Value::Identifier("total")),
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

    let mut builder = CIRBuilder::new(resolution_map.clone());
    let (main_cir, functions) = builder.lower_program(&typed_ast);
    println!("Main CIR Instructions:");
    for (i, instr) in main_cir.iter().enumerate() {
        println!("{}: {:?}", i, instr.op);
    }
    println!("\nFunction CIR Instructions:");
    for (node_id, cir) in functions {
        println!("Function NodeId: {:?}", node_id);
        for (i, instr) in cir.iter().enumerate() {
            println!("  {}: {:?}", i, instr.op);
        }
    }
}
