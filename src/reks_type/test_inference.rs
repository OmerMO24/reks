use crate::reks_parse::{operators::*, utnode::*};
use crate::reks_type::{infer::TypeInfo, infer::*, resolve::*};

fn print_typed_expr(expr: &TypedExpr, indent: usize) {
    let indent_str = "  ".repeat(indent);
    match &expr.type_info {
        TypeInfo::Known(t) => println!("{}Expr: Known {:?}", indent_str, t),
        TypeInfo::Inferred(t) => println!("{}Expr: Inferred {:?}", indent_str, t),
        TypeInfo::Unknown => println!("{}Expr: Unknown", indent_str),
    }

    match &expr.kind {
        TypedExprKind::Value(val) => println!("{}  Value: {:?}", indent_str, val),
        TypedExprKind::Let {
            id,
            expr: init_expr,
            ..
        } => {
            println!("{}  Let: {:?}", indent_str, id);
            print_typed_expr(init_expr, indent + 1);
        }
        TypedExprKind::Fn {
            name,
            params,
            retty,
            body,
        } => {
            println!("{}  Fn: {:?}", indent_str, name);
            println!("{}  Params:", indent_str);
            for param in params {
                println!("{}    {:?}", indent_str, param);
            }
            println!("{}  Return type:", indent_str);
            print_typed_expr(retty, indent + 1);
            println!("{}  Body:", indent_str);
            print_typed_expr(body, indent + 1);
        }
        TypedExprKind::Call { name, args } => {
            println!("{}  Call:", indent_str);
            print_typed_expr(name, indent + 1);
            println!("{}  Args:", indent_str);
            for arg in args {
                print_typed_expr(arg, indent + 1);
            }
        }
        TypedExprKind::BinOp { left, op, right } => {
            println!("{}  BinOp: {:?}", indent_str, op);
            print_typed_expr(left, indent + 1);
            print_typed_expr(right, indent + 1);
        }
        TypedExprKind::Block { statements } => {
            println!("{}  Block:", indent_str);
            for stmt in statements {
                print_typed_expr(stmt, indent + 1);
            }
        }
        TypedExprKind::Struct { id, fields } => {
            println!("{}  Struct: {:?}", indent_str, id);
            println!("{}  Fields:", indent_str);
            for field in fields {
                println!("{}    {:?}", indent_str, field);
            }
        }
        TypedExprKind::List { elements } => {
            println!("{}  List:", indent_str);
            for elem in elements {
                print_typed_expr(elem, indent + 1);
            }
        }
        TypedExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            println!("{}  If:", indent_str);
            print_typed_expr(condition, indent + 1);
            println!("{}  Then:", indent_str);
            print_typed_expr(then_branch, indent + 1);
            println!("{}  Else:", indent_str);
            print_typed_expr(else_branch, indent + 1);
        }
        TypedExprKind::FieldAccess { expr, field } => {
            println!("{}  FieldAccess: {:?}", indent_str, field);
            print_typed_expr(expr, indent + 1);
        }
        TypedExprKind::Assign { target, expr } => {
            println!("{}  Assign:", indent_str);
            print_typed_expr(target, indent + 1);
            print_typed_expr(expr, indent + 1);
        }
        _ => todo!(),
    }
}

pub fn test_harness(program: &Vec<UntypedExpr<'_>>) {
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());

    match inferencer.infer_program(&program) {
        Ok(typed_ast) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                println!("Top-level Expr {}:", i);
                print_typed_expr(expr, 1);
            }
        }
        Err(errors) => {
            println!("Type errors:");
            for err in errors {
                println!("  {:?}", err);
            }
        }
    }
}

pub fn test_simple_program() {
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

    test_harness(&test_program);
}

pub fn test_complex_realistic_program() {
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
        // fn main(p: Point, steps: i32) -> i32 { ... }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![
                Param {
                    name: Value::Identifier("p"),
                    ty: Value::Identifier("Point"),
                },
                Param {
                    name: Value::Identifier("steps"),
                    ty: Value::Identifier("i32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // mut x: i32 = p.x
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
                    // const distances = [p.v.dx, p.v.dy]
                    UntypedExpr::Let {
                        id: Value::Identifier("distances"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::List {
                            items: vec![
                                UntypedExpr::FieldAccess {
                                    id: Box::new(UntypedExpr::FieldAccess {
                                        id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                        field: Value::Identifier("v"),
                                    }),
                                    field: Value::Identifier("dx"),
                                },
                                UntypedExpr::FieldAccess {
                                    id: Box::new(UntypedExpr::FieldAccess {
                                        id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                        field: Value::Identifier("v"),
                                    }),
                                    field: Value::Identifier("dy"),
                                },
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    // if steps > 0 { x = add(x, p.v.dx) }
                    UntypedExpr::If {
                        condition: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("steps"))),
                            op: InfixOpKind::Greater,
                            right: Box::new(UntypedExpr::Value(Value::Num(0))),
                        }),
                        then_branch: Box::new(UntypedExpr::Block {
                            statements: vec![UntypedExpr::Assign {
                                left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                                right: Box::new(UntypedExpr::Call {
                                    name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                                    args: vec![
                                        UntypedExpr::Value(Value::Identifier("x")),
                                        UntypedExpr::FieldAccess {
                                            id: Box::new(UntypedExpr::FieldAccess {
                                                id: Box::new(UntypedExpr::Value(
                                                    Value::Identifier("p"),
                                                )),
                                                field: Value::Identifier("v"),
                                            }),
                                            field: Value::Identifier("dx"),
                                        },
                                    ],
                                }),
                            }],
                        }),
                        else_branch: Box::new(UntypedExpr::Block { statements: vec![] }),
                    },
                    // return add(x, p.v.dy)
                    UntypedExpr::Call {
                        name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                        args: vec![
                            UntypedExpr::Value(Value::Identifier("x")),
                            UntypedExpr::FieldAccess {
                                id: Box::new(UntypedExpr::FieldAccess {
                                    id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                    field: Value::Identifier("v"),
                                }),
                                field: Value::Identifier("dy"),
                            },
                        ],
                    },
                ],
            }),
        },
    ];

    test_harness(&test_program);
}

pub fn test_immutable_assignment_error() {
    let test_program = vec![
        // struct Point { x: i32 }
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![Param {
                name: Value::Identifier("x"),
                ty: Value::Identifier("i32"),
            }],
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
                    // const x: i32 = p.x
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("i32"),
                        },
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                            field: Value::Identifier("x"),
                        }),
                        constness: Const::Yes, // Const variable
                    },
                    // x = add(x, 10) -- Should trigger ImmutableAssignment error
                    UntypedExpr::Assign {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                        right: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Num(10)),
                            ],
                        }),
                    },
                    // return x
                    UntypedExpr::Value(Value::Identifier("x")),
                ],
            }),
        },
    ];

    test_harness(&test_program);
}

pub fn test_final_fuzz_program() {
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
        // struct Path { start: Point, end: Point }
        UntypedExpr::Struct {
            id: Value::Identifier("Path"),
            fields: vec![
                Param {
                    name: Value::Identifier("start"),
                    ty: Value::Identifier("Point"),
                },
                Param {
                    name: Value::Identifier("end"),
                    ty: Value::Identifier("Point"),
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
        // fn distance(p: Path) -> i32 { add(p.start.x, p.end.x) }
        UntypedExpr::Fn {
            name: Value::Identifier("distance"),
            params: vec![Param {
                name: Value::Identifier("p"),
                ty: Value::Identifier("Path"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::Call {
                    name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                    args: vec![
                        UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::FieldAccess {
                                id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                field: Value::Identifier("start"),
                            }),
                            field: Value::Identifier("x"),
                        },
                        UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::FieldAccess {
                                id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                                field: Value::Identifier("end"),
                            }),
                            field: Value::Identifier("x"),
                        },
                    ],
                }],
            }),
        },
        // fn main(p: Path) -> i32 { ... }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("p"),
                ty: Value::Identifier("Path"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // mut steps: i32 = 0
                    UntypedExpr::Let {
                        id: Value::Identifier("steps"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("i32"),
                        },
                        expr: Box::new(UntypedExpr::Value(Value::Num(0))),
                        constness: Const::No,
                    },
                    // const distances = [p.start.v.dx, add(p.end.v.dy, steps)]
                    UntypedExpr::Let {
                        id: Value::Identifier("distances"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::List {
                            items: vec![
                                UntypedExpr::FieldAccess {
                                    id: Box::new(UntypedExpr::FieldAccess {
                                        id: Box::new(UntypedExpr::FieldAccess {
                                            id: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "p",
                                            ))),
                                            field: Value::Identifier("start"),
                                        }),
                                        field: Value::Identifier("v"),
                                    }),
                                    field: Value::Identifier("dx"),
                                },
                                UntypedExpr::Call {
                                    name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                                    args: vec![
                                        UntypedExpr::FieldAccess {
                                            id: Box::new(UntypedExpr::FieldAccess {
                                                id: Box::new(UntypedExpr::FieldAccess {
                                                    id: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("p"),
                                                    )),
                                                    field: Value::Identifier("end"),
                                                }),
                                                field: Value::Identifier("v"),
                                            }),
                                            field: Value::Identifier("dy"),
                                        },
                                        UntypedExpr::Value(Value::Identifier("steps")),
                                    ],
                                },
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    // if add(p.start.x, p.end.v.dx) > 0 { steps = distance(p); if p.start.v.dy != 0 { steps = add(steps, 1) } }
                    UntypedExpr::If {
                        condition: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Call {
                                name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                                args: vec![
                                    UntypedExpr::FieldAccess {
                                        id: Box::new(UntypedExpr::FieldAccess {
                                            id: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "p",
                                            ))),
                                            field: Value::Identifier("start"),
                                        }),
                                        field: Value::Identifier("x"),
                                    },
                                    UntypedExpr::FieldAccess {
                                        id: Box::new(UntypedExpr::FieldAccess {
                                            id: Box::new(UntypedExpr::FieldAccess {
                                                id: Box::new(UntypedExpr::Value(
                                                    Value::Identifier("p"),
                                                )),
                                                field: Value::Identifier("end"),
                                            }),
                                            field: Value::Identifier("v"),
                                        }),
                                        field: Value::Identifier("dx"),
                                    },
                                ],
                            }),
                            op: InfixOpKind::Greater,
                            right: Box::new(UntypedExpr::Value(Value::Num(0))),
                        }),
                        then_branch: Box::new(UntypedExpr::Block {
                            statements: vec![
                                UntypedExpr::Assign {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("steps"))),
                                    right: Box::new(UntypedExpr::Call {
                                        name: Box::new(UntypedExpr::Value(Value::Identifier(
                                            "distance",
                                        ))),
                                        args: vec![UntypedExpr::Value(Value::Identifier("p"))],
                                    }),
                                },
                                UntypedExpr::If {
                                    condition: Box::new(UntypedExpr::BinOp {
                                        left: Box::new(UntypedExpr::FieldAccess {
                                            id: Box::new(UntypedExpr::FieldAccess {
                                                id: Box::new(UntypedExpr::FieldAccess {
                                                    id: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("p"),
                                                    )),
                                                    field: Value::Identifier("start"),
                                                }),
                                                field: Value::Identifier("v"),
                                            }),
                                            field: Value::Identifier("dy"),
                                        }),
                                        op: InfixOpKind::NotEq,
                                        right: Box::new(UntypedExpr::Value(Value::Num(0))),
                                    }),
                                    then_branch: Box::new(UntypedExpr::Block {
                                        statements: vec![UntypedExpr::Assign {
                                            left: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "steps",
                                            ))),
                                            right: Box::new(UntypedExpr::Call {
                                                name: Box::new(UntypedExpr::Value(
                                                    Value::Identifier("add"),
                                                )),
                                                args: vec![
                                                    UntypedExpr::Value(Value::Identifier("steps")),
                                                    UntypedExpr::Value(Value::Num(1)),
                                                ],
                                            }),
                                        }],
                                    }),
                                    else_branch: Box::new(UntypedExpr::Block {
                                        statements: vec![],
                                    }),
                                },
                            ],
                        }),
                        else_branch: Box::new(UntypedExpr::Block { statements: vec![] }),
                    },
                    // const total = add(distance(p), steps)
                    UntypedExpr::Let {
                        id: Value::Identifier("total"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Call {
                                    name: Box::new(UntypedExpr::Value(Value::Identifier(
                                        "distance",
                                    ))),
                                    args: vec![UntypedExpr::Value(Value::Identifier("p"))],
                                },
                                UntypedExpr::Value(Value::Identifier("steps")),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    // total = 5 -- Should trigger ImmutableAssignment error
                    UntypedExpr::Assign {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("total"))),
                        right: Box::new(UntypedExpr::Value(Value::Num(5))),
                    },
                    // return total
                    UntypedExpr::Value(Value::Identifier("total")),
                ],
            }),
        },
    ];

    test_harness(&test_program);
}

pub fn test_inference_loops() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("test_loops"),
        params: vec![],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                UntypedExpr::Let {
                    id: Value::Identifier("limit"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Num(3))),
                    constness: Const::Yes,
                },
                UntypedExpr::Let {
                    id: Value::Identifier("counter"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Num(0))),
                    constness: Const::No,
                },
                UntypedExpr::While {
                    guard: Box::new(UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("counter"))),
                        op: InfixOpKind::Less,
                        right: Box::new(UntypedExpr::Value(Value::Identifier("limit"))),
                    }),
                    body: Box::new(UntypedExpr::Assign {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("counter"))),
                        right: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("counter"))),
                            op: InfixOpKind::Add,
                            right: Box::new(UntypedExpr::Value(Value::Num(1))),
                        }),
                    }),
                },
                UntypedExpr::Let {
                    id: Value::Identifier("numbers"),
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
                UntypedExpr::Let {
                    id: Value::Identifier("sum"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Num(0))),
                    constness: Const::No,
                },
                UntypedExpr::For {
                    var: Value::Identifier("n"),
                    iterable: Box::new(UntypedExpr::Value(Value::Identifier("numbers"))),
                    body: Box::new(UntypedExpr::Assign {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("sum"))),
                        right: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("sum"))),
                            op: InfixOpKind::Add,
                            right: Box::new(UntypedExpr::Value(Value::Identifier("n"))),
                        }),
                    }),
                },
                UntypedExpr::Value(Value::Identifier("sum")),
            ],
        }),
    }];

    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    println!("Name Resolution Map: {:?}", resolution_map);

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
    println!("Typed AST:");
    for expr in &typed_ast {
        println!("  {:?}", expr);
    }
}

pub fn test_inference_complex_loops() {
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
                    UntypedExpr::Let {
                        id: Value::Identifier("m"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Identifier("n"))),
                        constness: Const::No, // Mutable local copy
                    },
                    UntypedExpr::While {
                        guard: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("m"))),
                            op: InfixOpKind::Greater,
                            right: Box::new(UntypedExpr::Value(Value::Num(1))),
                        }),
                        body: Box::new(UntypedExpr::Block {
                            statements: vec![
                                UntypedExpr::Assign {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("result"))),
                                    right: Box::new(UntypedExpr::BinOp {
                                        left: Box::new(UntypedExpr::Value(Value::Identifier(
                                            "result",
                                        ))),
                                        op: InfixOpKind::Mul,
                                        right: Box::new(UntypedExpr::Value(Value::Identifier("m"))),
                                    }),
                                },
                                UntypedExpr::Assign {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("m"))),
                                    right: Box::new(UntypedExpr::BinOp {
                                        left: Box::new(UntypedExpr::Value(Value::Identifier("m"))),
                                        op: InfixOpKind::Sub,
                                        right: Box::new(UntypedExpr::Value(Value::Num(1))),
                                    }),
                                },
                            ],
                        }),
                    },
                    UntypedExpr::Value(Value::Identifier("result")),
                ],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("process_lists"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("numbers"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::List {
                            items: vec![
                                UntypedExpr::Value(Value::Num(2)),
                                UntypedExpr::Value(Value::Num(3)),
                                UntypedExpr::Value(Value::Num(4)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("sums"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::List {
                            items: vec![
                                UntypedExpr::Value(Value::Num(0)),
                                UntypedExpr::Value(Value::Num(0)),
                                UntypedExpr::Value(Value::Num(0)),
                            ],
                        }),
                        constness: Const::No,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("i"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(0))),
                        constness: Const::No,
                    },
                    UntypedExpr::While {
                        guard: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("i"))),
                            op: InfixOpKind::Less,
                            right: Box::new(UntypedExpr::Value(Value::Num(3))),
                        }),
                        body: Box::new(UntypedExpr::Block {
                            statements: vec![
                                UntypedExpr::For {
                                    var: Value::Identifier("x"),
                                    iterable: Box::new(UntypedExpr::Value(Value::Identifier(
                                        "numbers",
                                    ))),
                                    body: Box::new(UntypedExpr::If {
                                        condition: Box::new(UntypedExpr::BinOp {
                                            left: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "x",
                                            ))),
                                            op: InfixOpKind::Greater,
                                            right: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "i",
                                            ))),
                                        }),
                                        then_branch: Box::new(UntypedExpr::Assign {
                                            left: Box::new(UntypedExpr::Index {
                                                expr: Box::new(UntypedExpr::Value(
                                                    Value::Identifier("sums"),
                                                )),
                                                index: Box::new(UntypedExpr::Value(
                                                    Value::Identifier("i"),
                                                )),
                                            }),
                                            right: Box::new(UntypedExpr::BinOp {
                                                left: Box::new(UntypedExpr::Index {
                                                    expr: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("sums"),
                                                    )),
                                                    index: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("i"),
                                                    )),
                                                }),
                                                op: InfixOpKind::Add,
                                                right: Box::new(UntypedExpr::Call {
                                                    name: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("factorial"),
                                                    )),
                                                    args: vec![UntypedExpr::Value(
                                                        Value::Identifier("x"),
                                                    )],
                                                }),
                                            }),
                                        }),
                                        else_branch: Box::new(UntypedExpr::Block {
                                            statements: vec![],
                                        }),
                                    }),
                                },
                                UntypedExpr::Assign {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("i"))),
                                    right: Box::new(UntypedExpr::BinOp {
                                        left: Box::new(UntypedExpr::Value(Value::Identifier("i"))),
                                        op: InfixOpKind::Add,
                                        right: Box::new(UntypedExpr::Value(Value::Num(1))),
                                    }),
                                },
                            ],
                        }),
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("total"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(0))),
                        constness: Const::No,
                    },
                    UntypedExpr::For {
                        var: Value::Identifier("s"),
                        iterable: Box::new(UntypedExpr::Value(Value::Identifier("sums"))),
                        body: Box::new(UntypedExpr::Assign {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("total"))),
                            right: Box::new(UntypedExpr::BinOp {
                                left: Box::new(UntypedExpr::Value(Value::Identifier("total"))),
                                op: InfixOpKind::Add,
                                right: Box::new(UntypedExpr::Value(Value::Identifier("s"))),
                            }),
                        }),
                    },
                    UntypedExpr::Value(Value::Identifier("total")),
                ],
            }),
        },
    ];

    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    println!("Name Resolution Map: {:?}", resolution_map);

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
    println!("Typed AST:");
    for expr in &typed_ast {
        println!("  {:?}", expr);
    }
}
