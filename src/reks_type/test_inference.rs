use crate::reks_parse::{operators::*, utnode::*};
use crate::reks_type::{infer::TypeInfo, infer::*, resolve::*};

pub fn test_monolithic_program() {
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
        // struct Container { p: Point }
        UntypedExpr::Struct {
            id: Value::Identifier("Container"),
            fields: vec![Param {
                name: Value::Identifier("p"),
                ty: Value::Identifier("Point"),
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
        // fn main(c: Container, flag: bool) -> i32 { ... }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![
                Param {
                    name: Value::Identifier("c"),
                    ty: Value::Identifier("Container"),
                },
                Param {
                    name: Value::Identifier("flag"),
                    ty: Value::Identifier("bool"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // const x: i32 = 5 (annotated let)
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("i32"),
                        },
                        expr: Box::new(UntypedExpr::Value(Value::Num(5))),
                        constness: Const::Yes,
                    },
                    // const y = x (unannotated let with variable)
                    UntypedExpr::Let {
                        id: Value::Identifier("y"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                        constness: Const::Yes,
                    },
                    // const z = c.p.y (unannotated let with nested field access)
                    UntypedExpr::Let {
                        id: Value::Identifier("z"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::FieldAccess {
                                id: Box::new(UntypedExpr::Value(Value::Identifier("c"))),
                                field: Value::Identifier("p"),
                            }),
                            field: Value::Identifier("y"),
                        }),
                        constness: Const::Yes,
                    },
                    // const sum = add(x, z) (function call)
                    UntypedExpr::Let {
                        id: Value::Identifier("sum"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Identifier("z")),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    // const numbers = [1, 2, sum] (non-empty list)
                    UntypedExpr::Let {
                        id: Value::Identifier("numbers"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::List {
                            items: vec![
                                UntypedExpr::Value(Value::Num(1)),
                                UntypedExpr::Value(Value::Num(2)),
                                UntypedExpr::Value(Value::Identifier("sum")),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    // const empty = [] (empty list, polymorphic)
                    UntypedExpr::Let {
                        id: Value::Identifier("empty"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::List { items: vec![] }),
                        constness: Const::Yes,
                    },
                    // const result = if flag { sum } else { z } (if statement)
                    UntypedExpr::Let {
                        id: Value::Identifier("result"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::If {
                            condition: Box::new(UntypedExpr::Value(Value::Identifier("flag"))),
                            then_branch: Box::new(UntypedExpr::Value(Value::Identifier("sum"))),
                            else_branch: Box::new(UntypedExpr::Value(Value::Identifier("z"))),
                        }),
                        constness: Const::Yes,
                    },
                    // return result
                    UntypedExpr::Value(Value::Identifier("result")),
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

// Test Function
pub fn test_type_inference() {
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
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
                }],
            }),
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
                        id: Value::Identifier("y"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("i32"),
                        },
                        expr: Box::new(UntypedExpr::Value(Value::Num(10))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Call {
                        name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                        args: vec![
                            UntypedExpr::Value(Value::Identifier("x")),
                            UntypedExpr::Value(Value::Identifier("y")),
                        ],
                    },
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver; // Adjust path
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
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

// New Test for Unannotated Let Expressions
pub fn test_unannotated_let_inference() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                // const x = 55;
                UntypedExpr::Let {
                    id: Value::Identifier("x"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Num(55))),
                    constness: Const::Yes,
                },
                // // const y: i8 = 55;
                // UntypedExpr::Let {
                //     id: Value::Identifier("y"),
                //     pat: TypePath::Typed {
                //         ident: Value::Identifier("i8"),
                //     },
                //     expr: Box::new(UntypedExpr::Value(Value::Num(55))),
                //     constness: Const::Yes,
                // },
                // mut z = 42;
                UntypedExpr::Let {
                    id: Value::Identifier("z"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Num(42))),
                    constness: Const::No,
                },
                // Return x + z to use the variables
                UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("z"))),
                },
            ],
        }),
    }];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                // Print types of variables in the block for clarity
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

// Test for All Unknowns
pub fn test_all_unknowns_inference() {
    let test_program = vec![
        UntypedExpr::Fn {
            name: Value::Identifier("add"),
            params: vec![
                Param {
                    name: Value::Identifier("a"),
                    ty: Value::Identifier("i32"),
                }, // No type annotation
                Param {
                    name: Value::Identifier("b"),
                    ty: Value::Identifier("i32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))), // No return type annotation
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
                }],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))), // No return type annotation
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(5))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("y"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Num(42)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Identifier("y")), // Return y
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

// Test for Function Calls with Mandatory Annotations
pub fn test_function_call_inference() {
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
            body: Box::new(UntypedExpr::Block {
                statements: vec![UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
                }],
            }),
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
                        id: Value::Identifier("y"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Num(42)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("z"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("y")),
                                UntypedExpr::Value(Value::Identifier("x")),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Identifier("z")), // Return z
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

// Test for Structs and Function Calls
pub fn test_struct_function_inference() {
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
        // fn move_point(p: Point, dx: i32) -> Point { p }
        UntypedExpr::Fn {
            name: Value::Identifier("move_point"),
            params: vec![
                Param {
                    name: Value::Identifier("p"),
                    ty: Value::Identifier("Point"),
                },
                Param {
                    name: Value::Identifier("dx"),
                    ty: Value::Identifier("i32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("Point"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Value(Value::Identifier("p")), // Just return p
                ],
            }),
        },
        // fn main() -> i32 { ... }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // Assume p0 is a Point defined elsewhere (placeholder)
                    UntypedExpr::Let {
                        id: Value::Identifier("p0"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("Point"),
                        }, // Temporary annotation
                        expr: Box::new(UntypedExpr::Value(Value::Identifier("some_point"))), // Placeholder
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("p1"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("move_point"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("p0")),
                                UntypedExpr::Value(Value::Num(5)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("p2"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("move_point"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("p1")),
                                UntypedExpr::Value(Value::Num(10)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Num(42)), // Return 42 for i32
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_generic_struct_inference() {
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
        UntypedExpr::Fn {
            name: Value::Identifier("move_point"),
            params: vec![
                Param {
                    name: Value::Identifier("p"),
                    ty: Value::Identifier("Point"),
                },
                Param {
                    name: Value::Identifier("v"),
                    ty: Value::Identifier("Vector"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("Point"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Value(Value::Identifier("p")), // Return the parameter p
                ],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![
                Param {
                    name: Value::Identifier("initial_p"),
                    ty: Value::Identifier("Point"),
                },
                Param {
                    name: Value::Identifier("initial_v"),
                    ty: Value::Identifier("Vector"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("p1"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("move_point"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("initial_p")),
                                UntypedExpr::Value(Value::Identifier("initial_v")),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Num(42)), // Return 42
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_list_inference() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                UntypedExpr::Let {
                    id: Value::Identifier("xs"),
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
                    id: Value::Identifier("ys"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::List {
                        items: vec![], // Empty list
                    }),
                    constness: Const::Yes,
                },
                UntypedExpr::Value(Value::Num(42)), // Return 42
            ],
        }),
    }];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_if_statement_inference() {
    let test_program = vec![UntypedExpr::Fn {
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
                    id: Value::Identifier("y"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::If {
                        condition: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                            op: InfixOpKind::Equals,
                            right: Box::new(UntypedExpr::Value(Value::Num(5))),
                        }),
                        then_branch: Box::new(UntypedExpr::Value(Value::Num(10))),
                        else_branch: Box::new(UntypedExpr::Value(Value::Num(20))),
                    }),
                    constness: Const::Yes,
                },
                UntypedExpr::Value(Value::Identifier("y")), // Return y
            ],
        }),
    }];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_field_access_inference() {
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
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("p"),
                ty: Value::Identifier("Point"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("x_coord"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                            field: Value::Identifier("x"),
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Value(Value::Identifier("x_coord")), // Return x_coord
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_nested_struct_field_access() {
    let test_program = vec![
        // struct Inner { a: i32 }
        UntypedExpr::Struct {
            id: Value::Identifier("Inner"),
            fields: vec![Param {
                name: Value::Identifier("a"),
                ty: Value::Identifier("i32"),
            }],
        },
        // struct Outer { inner: Inner }
        UntypedExpr::Struct {
            id: Value::Identifier("Outer"),
            fields: vec![Param {
                name: Value::Identifier("inner"),
                ty: Value::Identifier("Inner"),
            }],
        },
        // fn main(outer: Outer) -> i32 { ... }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("outer"),
                ty: Value::Identifier("Outer"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // const inner = outer.inner
                    UntypedExpr::Let {
                        id: Value::Identifier("inner"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("outer"))),
                            field: Value::Identifier("inner"),
                        }),
                        constness: Const::Yes,
                    },
                    // const a = inner.a
                    UntypedExpr::Let {
                        id: Value::Identifier("a"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("inner"))),
                            field: Value::Identifier("a"),
                        }),
                        constness: Const::Yes,
                    },
                    // return a
                    UntypedExpr::Value(Value::Identifier("a")),
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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

pub fn test_unannotated_let_with_field_access() {
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
                    // const x = p.y
                    UntypedExpr::Let {
                        id: Value::Identifier("x"),
                        pat: TypePath::Empty, // Unannotated
                        expr: Box::new(UntypedExpr::FieldAccess {
                            id: Box::new(UntypedExpr::Value(Value::Identifier("p"))),
                            field: Value::Identifier("y"),
                        }),
                        constness: Const::Yes,
                    },
                    // return x
                    UntypedExpr::Value(Value::Identifier("x")),
                ],
            }),
        },
    ];

    use crate::reks_type::resolve::NameResolver;
    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);
    let mut inferencer = TypeInferencer::new(resolution_map.clone());
    let mut typed_ast = inferencer.convert_program(&test_program);

    match inferencer.infer_program(&mut typed_ast) {
        Ok(()) => {
            println!("Type inference successful!");
            for (i, expr) in typed_ast.iter().enumerate() {
                match &expr.type_info {
                    TypeInfo::Known(t) => println!("Expr {}: Known {:?}", i, t),
                    TypeInfo::Inferred(t) => println!("Expr {}: Inferred {:?}", i, t),
                    TypeInfo::Unknown => println!("Expr {}: Unknown", i),
                }
                if let UntypedExpr::Fn { body, .. } = &expr.kind {
                    if let UntypedExpr::Block { statements } = body.as_ref() {
                        for (j, stmt) in statements.iter().enumerate() {
                            if let UntypedExpr::Let {
                                id: Value::Identifier(name),
                                ..
                            } = stmt
                            {
                                if let Some(decl_id) =
                                    inferencer.resolution_map.get_declaration_id(name)
                                {
                                    if let Some(ty) = inferencer.env.get(&decl_id) {
                                        println!("  Let {} ({}): {:?}", j, name, ty);
                                    }
                                }
                            }
                        }
                    }
                }
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
