use crate::reks_parse::{operators::*, utnode::*};
use crate::reks_type::resolve::*;
use std::collections::HashMap;

// Tests for the name resolution system
pub fn test_name_resolver() {
    // Test 1: Basic Variable Declaration and Use
    println!("=== Test 1: Basic Variable Declaration and Use ===");
    let test_basic = vec![UntypedExpr::Block {
        statements: vec![
            UntypedExpr::Let {
                id: Value::Identifier("x"),
                pat: TypePath::Typed {
                    ident: Value::Identifier("i32"),
                },
                expr: Box::new(UntypedExpr::Value(Value::Num(5))),
                constness: Const::Yes,
            },
            UntypedExpr::Let {
                id: Value::Identifier("y"),
                pat: TypePath::Empty,
                expr: Box::new(UntypedExpr::Value(Value::Identifier("x"))), // Reference to x
                constness: Const::Yes,
            },
            UntypedExpr::BinOp {
                left: Box::new(UntypedExpr::Value(Value::Identifier("x"))), // Reference to x
                op: InfixOpKind::Add,
                right: Box::new(UntypedExpr::Value(Value::Identifier("y"))), // Reference to y
            },
        ],
    }];

    let mut resolver = NameResolver::new();
    resolver.resolve_program(&test_basic);

    // Print the resolution map
    print_resolution_map(resolver.get_resolution_map());

    // Test 2: Nested Scopes and Shadowing
    println!("\n=== Test 2: Nested Scopes and Shadowing ===");
    let test_scopes = vec![UntypedExpr::Block {
        statements: vec![
            UntypedExpr::Let {
                id: Value::Identifier("outer"),
                pat: TypePath::Empty,
                expr: Box::new(UntypedExpr::Value(Value::Num(10))),
                constness: Const::Yes,
            },
            UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("inner"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(20))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("outer"), // Shadows outer 'outer'
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Num(30))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("outer"))), // Should refer to inner 'outer'
                        op: InfixOpKind::Add,
                        right: Box::new(UntypedExpr::Value(Value::Identifier("inner"))),
                    },
                ],
            },
            UntypedExpr::Value(Value::Identifier("outer")), // Should refer to outer 'outer'
        ],
    }];

    let mut resolver = NameResolver::new();
    resolver.resolve_program(&test_scopes);

    // Print the resolution map
    print_resolution_map(resolver.get_resolution_map());

    // Test 3: Function Declarations and Calls
    println!("\n=== Test 3: Function Declarations and Calls ===");
    let test_functions = vec![
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
                    left: Box::new(UntypedExpr::Value(Value::Identifier("a"))), // Reference to parameter
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("b"))), // Reference to parameter
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
                        expr: Box::new(UntypedExpr::Value(Value::Num(10))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Call {
                        name: Box::new(UntypedExpr::Value(Value::Identifier("add"))), // Function call
                        args: vec![
                            UntypedExpr::Value(Value::Identifier("x")), // Reference to x
                            UntypedExpr::Value(Value::Identifier("y")), // Reference to y
                        ],
                    },
                ],
            }),
        },
    ];

    let mut resolver = NameResolver::new();
    resolver.resolve_program(&test_functions);

    // Print the resolution map
    print_resolution_map(resolver.get_resolution_map());

    // Test 4: Struct Declaration and Field Access
    println!("\n=== Test 4: Struct Declaration and Field Access ===");
    let test_structs = vec![
        UntypedExpr::Struct {
            id: Value::Identifier("Point"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("f32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("f32"),
                },
            ],
        },
        UntypedExpr::Fn {
            name: Value::Identifier("create_point"),
            params: vec![
                Param {
                    name: Value::Identifier("x_val"),
                    ty: Value::Identifier("f32"),
                },
                Param {
                    name: Value::Identifier("y_val"),
                    ty: Value::Identifier("f32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("Point"))), // Reference to struct
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // Return statement would create a Point here
                ],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("test_point"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("f32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("p"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("Point"),
                        }, // Reference to struct
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("create_point"))), // Function call
                            args: vec![
                                UntypedExpr::Value(Value::Float(1.0)),
                                UntypedExpr::Value(Value::Float(2.0)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::FieldAccess {
                        id: Box::new(UntypedExpr::Value(Value::Identifier("p"))), // Reference to local variable
                        field: Value::Identifier("x"),                            // Field access
                    },
                ],
            }),
        },
    ];

    let mut resolver = NameResolver::new();
    resolver.resolve_program(&test_structs);

    // Print the resolution map
    print_resolution_map(resolver.get_resolution_map());

    // Test 5: Complex Program with Multiple Features
    println!("\n=== Test 5: Complex Program with Multiple Features ===");
    let test_complex = vec![
        UntypedExpr::Struct {
            id: Value::Identifier("Counter"),
            fields: vec![Param {
                name: Value::Identifier("value"),
                ty: Value::Identifier("i32"),
            }],
        },
        UntypedExpr::Fn {
            name: Value::Identifier("increment"),
            params: vec![
                Param {
                    name: Value::Identifier("counter"),
                    ty: Value::Identifier("Counter"),
                },
                Param {
                    name: Value::Identifier("amount"),
                    ty: Value::Identifier("i32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("Counter"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("new_value"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::FieldAccess {
                                id: Box::new(UntypedExpr::Value(Value::Identifier("counter"))),
                                field: Value::Identifier("value"),
                            }),
                            op: InfixOpKind::Add,
                            right: Box::new(UntypedExpr::Value(Value::Identifier("amount"))),
                        }),
                        constness: Const::Yes,
                    },
                    // Return new Counter would be here
                ],
            }),
        },
        UntypedExpr::Fn {
            name: Value::Identifier("complex_test"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("c"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("Counter"),
                        },
                        expr: Box::new(UntypedExpr::Struct {
                            id: Value::Identifier("Counter"),
                            fields: vec![Param {
                                name: Value::Identifier("value"),
                                ty: Value::Identifier("i32"),
                            }],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("c2"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("increment"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("c")),
                                UntypedExpr::Value(Value::Num(5)),
                            ],
                        }),
                        constness: Const::Yes,
                    },
                    UntypedExpr::If {
                        condition: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::FieldAccess {
                                id: Box::new(UntypedExpr::Value(Value::Identifier("c2"))),
                                field: Value::Identifier("value"),
                            }),
                            op: InfixOpKind::Greater,
                            right: Box::new(UntypedExpr::Value(Value::Num(10))),
                        }),
                        then_branch: Box::new(UntypedExpr::Block {
                            statements: vec![UntypedExpr::Value(Value::Num(1))],
                        }),
                        else_branch: Box::new(UntypedExpr::Block {
                            statements: vec![UntypedExpr::Value(Value::Num(0))],
                        }),
                    },
                ],
            }),
        },
    ];

    let mut resolver = NameResolver::new();
    resolver.resolve_program(&test_complex);

    // Print the resolution map
    print_resolution_map(resolver.get_resolution_map());
}

// Helper function to print the resolution map
pub fn print_resolution_map(map: &NameResolutionMap) {
    println!("=== Declarations ===");
    for (id, info) in &map.declarations {
        println!(
            "ID {}: {} (scope level {}) - {:?}",
            id, info.name, info.scope_level, info.symbol
        );
    }

    println!("\n=== Bindings ===");
    for (ref_id, decl_id) in &map.bindings {
        if let Some(info) = map.declarations.get(decl_id) {
            println!(
                "Reference {} -> Declaration {} ({})",
                ref_id, decl_id, info.name
            );
        } else {
            println!("Reference {} -> Unknown Declaration {}", ref_id, decl_id);
        }
    }

    println!("\n=== Summary ===");
    println!("Total declarations: {}", map.declarations.len());
    println!("Total bindings: {}", map.bindings.len());
}

pub fn test_complex_nesting() {
    // A deeply nested, complex program
    let test_complex = vec![
        // Struct definitions
        UntypedExpr::Struct {
            id: Value::Identifier("Vec2"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("f32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("f32"),
                },
            ],
        },
        UntypedExpr::Struct {
            id: Value::Identifier("Vec3"),
            fields: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("f32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("f32"),
                },
                Param {
                    name: Value::Identifier("z"),
                    ty: Value::Identifier("f32"),
                },
            ],
        },
        // Helper function with captured outer scope variables
        UntypedExpr::Fn {
            name: Value::Identifier("create_vec2"),
            params: vec![
                Param {
                    name: Value::Identifier("x"),
                    ty: Value::Identifier("f32"),
                },
                Param {
                    name: Value::Identifier("y"),
                    ty: Value::Identifier("f32"),
                },
            ],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("Vec2"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // Return a Vec2
                    UntypedExpr::Value(Value::Identifier("Vec2")), // Reference to struct
                ],
            }),
        },
        // Main program with complex logic
        UntypedExpr::Fn {
            name: Value::Identifier("compute"),
            params: vec![],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    // Level 1 variables
                    UntypedExpr::Let {
                        id: Value::Identifier("global_factor"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("f32"),
                        },
                        expr: Box::new(UntypedExpr::Value(Value::Float(2.0))),
                        constness: Const::Yes,
                    },
                    UntypedExpr::Let {
                        id: Value::Identifier("global_offset"),
                        pat: TypePath::Typed {
                            ident: Value::Identifier("f32"),
                        },
                        expr: Box::new(UntypedExpr::Value(Value::Float(1.0))),
                        constness: Const::Yes,
                    },
                    // Level 1 function that captures variables
                    UntypedExpr::Let {
                        id: Value::Identifier("transform_vec2"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Fn {
                            name: Value::Identifier("transform_vec2_inner"),
                            params: vec![Param {
                                name: Value::Identifier("v"),
                                ty: Value::Identifier("Vec2"),
                            }],
                            retty: Box::new(UntypedExpr::Value(Value::Identifier("Vec2"))),
                            body: Box::new(UntypedExpr::Block {
                                statements: vec![
                                    // Calculate new coordinates using outer variables
                                    UntypedExpr::Let {
                                        id: Value::Identifier("new_x"),
                                        pat: TypePath::Empty,
                                        expr: Box::new(UntypedExpr::BinOp {
                                            left: Box::new(UntypedExpr::BinOp {
                                                left: Box::new(UntypedExpr::FieldAccess {
                                                    id: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("v"),
                                                    )),
                                                    field: Value::Identifier("x"),
                                                }),
                                                op: InfixOpKind::Mul,
                                                right: Box::new(UntypedExpr::Value(
                                                    Value::Identifier("global_factor"),
                                                )),
                                            }),
                                            op: InfixOpKind::Add,
                                            right: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "global_offset",
                                            ))),
                                        }),
                                        constness: Const::Yes,
                                    },
                                    UntypedExpr::Let {
                                        id: Value::Identifier("new_y"),
                                        pat: TypePath::Empty,
                                        expr: Box::new(UntypedExpr::BinOp {
                                            left: Box::new(UntypedExpr::BinOp {
                                                left: Box::new(UntypedExpr::FieldAccess {
                                                    id: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("v"),
                                                    )),
                                                    field: Value::Identifier("y"),
                                                }),
                                                op: InfixOpKind::Mul,
                                                right: Box::new(UntypedExpr::Value(
                                                    Value::Identifier("global_factor"),
                                                )),
                                            }),
                                            op: InfixOpKind::Add,
                                            right: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "global_offset",
                                            ))),
                                        }),
                                        constness: Const::Yes,
                                    },
                                    // Return a Vec2
                                    UntypedExpr::Call {
                                        name: Box::new(UntypedExpr::Value(Value::Identifier(
                                            "create_vec2",
                                        ))),
                                        args: vec![
                                            UntypedExpr::Value(Value::Identifier("new_x")),
                                            UntypedExpr::Value(Value::Identifier("new_y")),
                                        ],
                                    },
                                ],
                            }),
                        }),
                        constness: Const::Yes,
                    },
                    // Nested block with local variables and shadowing
                    UntypedExpr::Block {
                        statements: vec![
                            // Level 2 - shadowing global_factor
                            UntypedExpr::Let {
                                id: Value::Identifier("global_factor"),
                                pat: TypePath::Empty,
                                expr: Box::new(UntypedExpr::Value(Value::Float(3.0))),
                                constness: Const::Yes,
                            },
                            UntypedExpr::Let {
                                id: Value::Identifier("vec1"),
                                pat: TypePath::Typed {
                                    ident: Value::Identifier("Vec2"),
                                },
                                expr: Box::new(UntypedExpr::Call {
                                    name: Box::new(UntypedExpr::Value(Value::Identifier(
                                        "create_vec2",
                                    ))),
                                    args: vec![
                                        UntypedExpr::Value(Value::Float(10.0)),
                                        UntypedExpr::Value(Value::Float(20.0)),
                                    ],
                                }),
                                constness: Const::Yes,
                            },
                            // Call transform_vec2 which captures outer global_offset but uses shadowed global_factor
                            UntypedExpr::Let {
                                id: Value::Identifier("transformed"),
                                pat: TypePath::Empty,
                                expr: Box::new(UntypedExpr::Call {
                                    name: Box::new(UntypedExpr::Value(Value::Identifier(
                                        "transform_vec2",
                                    ))),
                                    args: vec![UntypedExpr::Value(Value::Identifier("vec1"))],
                                }),
                                constness: Const::Yes,
                            },
                            // Deeper nested block with more shadowing
                            UntypedExpr::Block {
                                statements: vec![
                                    // Level 3 - shadowing again
                                    UntypedExpr::Let {
                                        id: Value::Identifier("global_offset"),
                                        pat: TypePath::Empty,
                                        expr: Box::new(UntypedExpr::Value(Value::Float(5.0))),
                                        constness: Const::Yes,
                                    },
                                    // Conditional block with its own scope
                                    UntypedExpr::If {
                                        condition: Box::new(UntypedExpr::BinOp {
                                            left: Box::new(UntypedExpr::FieldAccess {
                                                id: Box::new(UntypedExpr::Value(
                                                    Value::Identifier("transformed"),
                                                )),
                                                field: Value::Identifier("x"),
                                            }),
                                            op: InfixOpKind::Greater,
                                            right: Box::new(UntypedExpr::Value(Value::Float(30.0))),
                                        }),
                                        then_branch: Box::new(UntypedExpr::Block {
                                            statements: vec![
                                                // Level 4 - yet another scope
                                                UntypedExpr::Let {
                                                    id: Value::Identifier("result"),
                                                    pat: TypePath::Empty,
                                                    expr: Box::new(UntypedExpr::Value(Value::Num(
                                                        1,
                                                    ))),
                                                    constness: Const::Yes,
                                                },
                                                // Using values from multiple outer scopes
                                                UntypedExpr::BinOp {
                                                    left: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("result"),
                                                    )),
                                                    op: InfixOpKind::Add,
                                                    right: Box::new(UntypedExpr::BinOp {
                                                        left: Box::new(UntypedExpr::FieldAccess {
                                                            id: Box::new(UntypedExpr::Value(
                                                                Value::Identifier("vec1"),
                                                            )),
                                                            field: Value::Identifier("x"),
                                                        }),
                                                        op: InfixOpKind::Mul,
                                                        right: Box::new(UntypedExpr::Value(
                                                            Value::Identifier("global_factor"),
                                                        )),
                                                    }),
                                                },
                                            ],
                                        }),
                                        else_branch: Box::new(UntypedExpr::Block {
                                            statements: vec![
                                                // Level 4 - another branch
                                                // Shadowing variable from then branch
                                                UntypedExpr::Let {
                                                    id: Value::Identifier("result"),
                                                    pat: TypePath::Empty,
                                                    expr: Box::new(UntypedExpr::Value(Value::Num(
                                                        0,
                                                    ))),
                                                    constness: Const::Yes,
                                                },
                                                // Referring to variables from various outer scopes
                                                UntypedExpr::BinOp {
                                                    left: Box::new(UntypedExpr::Value(
                                                        Value::Identifier("result"),
                                                    )),
                                                    op: InfixOpKind::Add,
                                                    right: Box::new(UntypedExpr::BinOp {
                                                        left: Box::new(UntypedExpr::FieldAccess {
                                                            id: Box::new(UntypedExpr::Value(
                                                                Value::Identifier("transformed"),
                                                            )),
                                                            field: Value::Identifier("y"),
                                                        }),
                                                        op: InfixOpKind::Mul,
                                                        right: Box::new(UntypedExpr::Value(
                                                            Value::Identifier("global_offset"),
                                                        )),
                                                    }),
                                                },
                                            ],
                                        }),
                                    },
                                ],
                            },
                        ],
                    },
                    // Return statement
                    UntypedExpr::Value(Value::Num(0)),
                ],
            }),
        },
    ];

    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_complex);

    println!("=== Complex Program with Multiple Levels of Nesting ===");
    print_resolution_map(resolution_map);

    println!("\n=== Scope Nesting Analysis ===");
    println!("Scope levels found in declarations:");
    let mut scope_counts = HashMap::new();
    for (_, info) in &resolution_map.declarations {
        *scope_counts.entry(info.scope_level).or_insert(0) += 1;
    }

    // Sort scope levels manually
    let mut levels: Vec<_> = scope_counts.iter().collect();
    levels.sort_by_key(|&(k, _)| *k);

    for (level, count) in levels {
        println!("  Scope level {}: {} declarations", level, count);
    }

    println!("\n=== Variable Shadowing Analysis ===");
    let mut shadow_map = HashMap::new();
    for (id, info) in &resolution_map.declarations {
        shadow_map
            .entry(info.name.clone())
            .or_insert(Vec::new())
            .push((*id, info.scope_level));
    }

    for (name, instances) in shadow_map.iter() {
        if instances.len() > 1 {
            println!("Variable '{}' appears in multiple scopes:", name);

            // Sort instances manually
            let mut sorted_instances = instances.clone();
            sorted_instances.sort_by_key(|&(_, level)| level);

            for (id, level) in sorted_instances {
                println!("  ID {}: scope level {}", id, level);
            }
        }
    }

    println!("\n=== Cross-Scope References ===");
    for (ref_id, decl_id) in &resolution_map.bindings {
        if let Some(decl_info) = resolution_map.declarations.get(decl_id) {
            // Determine what scope the reference is in (simplified approach)
            // This is a simplification that might not be accurate for all references
            let ref_scope = 0; // Default to global scope

            if ref_scope != decl_info.scope_level {
                println!(
                    "Reference {} -> Declaration {} '{}' (scope {})",
                    ref_id, decl_id, decl_info.name, decl_info.scope_level
                );
            }
        }
    }
}

pub fn test_manual_verification() {
    // A single function that contains most edge cases
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("verify_all"),
        params: vec![
            Param {
                name: Value::Identifier("param1"),
                ty: Value::Identifier("i32"),
            },
            Param {
                name: Value::Identifier("param2"),
                ty: Value::Identifier("f32"),
            },
        ],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                // Basic variable declaration
                UntypedExpr::Let {
                    id: Value::Identifier("x"),
                    pat: TypePath::Typed {
                        ident: Value::Identifier("i32"),
                    },
                    expr: Box::new(UntypedExpr::Value(Value::Num(10))),
                    constness: Const::Yes,
                },
                // Reference to parameter
                UntypedExpr::Let {
                    id: Value::Identifier("y"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Identifier("param1"))),
                    constness: Const::Yes,
                },
                // Binary op using variables from different sources
                UntypedExpr::Let {
                    id: Value::Identifier("z"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                        op: InfixOpKind::Add,
                        right: Box::new(UntypedExpr::Value(Value::Identifier("y"))),
                    }),
                    constness: Const::Yes,
                },
                // Nested block with shadowing
                UntypedExpr::Block {
                    statements: vec![
                        // Shadow outer x
                        UntypedExpr::Let {
                            id: Value::Identifier("x"),
                            pat: TypePath::Empty,
                            expr: Box::new(UntypedExpr::Value(Value::Num(20))),
                            constness: Const::Yes,
                        },
                        // Use shadowed x and outer z
                        UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                            op: InfixOpKind::Mul,
                            right: Box::new(UntypedExpr::Value(Value::Identifier("z"))),
                        },
                        // Deeper nesting to test multi-level resolution
                        UntypedExpr::Block {
                            statements: vec![
                                // Variable only available in deepest scope
                                UntypedExpr::Let {
                                    id: Value::Identifier("deep"),
                                    pat: TypePath::Empty,
                                    expr: Box::new(UntypedExpr::Value(Value::Num(30))),
                                    constness: Const::Yes,
                                },
                                // Expression using variables from multiple scopes
                                UntypedExpr::BinOp {
                                    left: Box::new(UntypedExpr::BinOp {
                                        left: Box::new(UntypedExpr::Value(Value::Identifier(
                                            "deep",
                                        ))),
                                        op: InfixOpKind::Add,
                                        right: Box::new(UntypedExpr::Value(Value::Identifier("x"))), // Should reference shadowed x
                                    }),
                                    op: InfixOpKind::Add,
                                    right: Box::new(UntypedExpr::Value(Value::Identifier(
                                        "param2",
                                    ))), // Reference to parameter
                                },
                            ],
                        },
                    ],
                },
                // Final reference to x, should be the original outer x, not the shadowed one
                UntypedExpr::Value(Value::Identifier("x")),
            ],
        }),
    }];

    let mut resolver = NameResolver::new();
    let resolution_map = resolver.resolve_program(&test_program);

    // Print detailed output for manual verification
    // println!("=== Manual Verification Test ===");

    // println!("\n=== Declarations by Scope ===");
    // let mut by_scope: HashMap<usize, Vec<(NodeId, &DeclarationInfo)>> = HashMap::new();
    // for (id, info) in &resolution_map.declarations {
    //     by_scope
    //         .entry(info.scope_level)
    //         .or_insert(Vec::new())
    //         .push((*id, info));
    // }

    // // Print by scope level
    // let mut scope_levels: Vec<_> = by_scope.keys().collect();
    // scope_levels.sort();

    // for &level in &scope_levels {
    //     println!("Scope Level {}:", level);
    //     for (id, info) in &by_scope[&level] {
    //         if info.name != "i64"
    //             && info.name != "i32"
    //             && info.name != "f32"
    //             && info.name != "bool"
    //             && info.name != "string"
    //             && !info.name.starts_with("u")
    //             && info.name != "i16"
    //             && info.name != "i8"
    //         {
    //             println!("  ID {}: {} - {:?}", id, info.name, info.symbol);
    //         }
    //     }
    // }

    // Print detailed output for manual verification
    println!("=== Manual Verification Test ===");

    println!("\n=== Declarations by Scope ===");
    let mut by_scope: HashMap<usize, Vec<(NodeId, &DeclarationInfo)>> = HashMap::new();
    for (id, info) in &resolution_map.declarations {
        by_scope
            .entry(info.scope_level)
            .or_insert(Vec::new())
            .push((*id, info));
    }

    // Print by scope level
    let mut scope_levels: Vec<_> = by_scope.keys().collect();
    scope_levels.sort();

    for &level in &scope_levels {
        println!("Scope Level {}:", level);
        for (id, info) in &by_scope[&level] {
            if info.name != "i64"
                && info.name != "i32"
                && info.name != "f32"
                && info.name != "bool"
                && info.name != "string"
                && !info.name.starts_with("u")
                && info.name != "i16"
                && info.name != "i8"
            {
                println!("  ID {}: {} - {:?}", id, info.name, info.symbol);
            }
        }
    }

    // Use the new function for printing references with IDs
    print_resolution_map_with_ids(&resolution_map);

    println!("\n=== Important References ===");
    // Get declarations
    let mut declarations_by_name: HashMap<String, Vec<(NodeId, usize)>> = HashMap::new();
    for (id, info) in &resolution_map.declarations {
        declarations_by_name
            .entry(info.name.clone())
            .or_insert(Vec::new())
            .push((*id, info.scope_level));
    }

    // Check each binding
    for (ref_id, decl_id) in &resolution_map.bindings {
        if let Some(decl_info) = resolution_map.declarations.get(decl_id) {
            // Only show variables we care about
            if decl_info.name != "i64"
                && decl_info.name != "i32"
                && decl_info.name != "f32"
                && decl_info.name != "bool"
                && decl_info.name != "string"
                && !decl_info.name.starts_with("u")
                && decl_info.name != "i16"
                && decl_info.name != "i8"
            {
                // Check if this name has multiple declarations
                if let Some(declarations) = declarations_by_name.get(&decl_info.name) {
                    if declarations.len() > 1 {
                        println!("Reference {} -> {} (scope {}) [SHADOWED - multiple declarations for this name]", 
                                ref_id, decl_info.name, decl_info.scope_level);
                    } else {
                        println!(
                            "Reference {} -> {} (scope {})",
                            ref_id, decl_info.name, decl_info.scope_level
                        );
                    }
                } else {
                    println!(
                        "Reference {} -> {} (scope {})",
                        ref_id, decl_info.name, decl_info.scope_level
                    );
                }
            }
        }
    }
}

// Helper function to print the resolution map with detailed reference information
pub fn print_resolution_map_with_ids(map: &NameResolutionMap) {
    println!("\n=== Important References (with IDs) ===");

    // Get declarations
    let mut declarations_by_name: HashMap<String, Vec<(NodeId, usize)>> = HashMap::new();
    for (id, info) in &map.declarations {
        declarations_by_name
            .entry(info.name.clone())
            .or_insert(Vec::new())
            .push((*id, info.scope_level));
    }

    // Check each binding
    for (ref_id, decl_id) in &map.bindings {
        if let Some(decl_info) = map.declarations.get(decl_id) {
            // Only show variables we care about (skip built-in types)
            if decl_info.name != "i64"
                && decl_info.name != "i32"
                && decl_info.name != "f32"
                && decl_info.name != "bool"
                && decl_info.name != "string"
                && !decl_info.name.starts_with("u")
                && decl_info.name != "i16"
                && decl_info.name != "i8"
            {
                // Check if this name has multiple declarations
                if let Some(declarations) = declarations_by_name.get(&decl_info.name) {
                    if declarations.len() > 1 {
                        println!(
                            "Reference {} -> Declaration ID {} ({}) (scope {}) [SHADOWED]",
                            ref_id, decl_id, decl_info.name, decl_info.scope_level
                        );
                    } else {
                        println!(
                            "Reference {} -> Declaration ID {} ({}) (scope {})",
                            ref_id, decl_id, decl_info.name, decl_info.scope_level
                        );
                    }
                } else {
                    println!(
                        "Reference {} -> Declaration ID {} ({}) (scope {})",
                        ref_id, decl_id, decl_info.name, decl_info.scope_level
                    );
                }
            }
        }
    }
}
