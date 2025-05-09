use crate::reks_compile::compile::*;
use crate::reks_eval::cir::SSACIRBuilder;
use crate::reks_parse::{operators::*, utnode::*};
use crate::reks_type::infer::TypeInferencer;
use crate::reks_type::resolve::NameResolver;

pub fn test_llvm_codegen_no_consteval() {
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
                    constness: Const::No,
                },
                UntypedExpr::Assign {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                    right: Box::new(UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                        op: InfixOpKind::Add,
                        right: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Num(3))),
                            op: InfixOpKind::Mul,
                            right: Box::new(UntypedExpr::Value(Value::Num(2))),
                        }),
                    }),
                },
                UntypedExpr::Value(Value::Identifier("x")),
            ],
        }),
    }];

    compile(test_program);
}

pub fn test_llvm_codegen_with_param() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![Param {
            name: Value::Identifier("a"),
            ty: Value::Identifier("i32"),
        }],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                UntypedExpr::Let {
                    id: Value::Identifier("x"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                    constness: Const::No,
                },
                UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Value(Value::Num(3))),
                        op: InfixOpKind::Mul,
                        right: Box::new(UntypedExpr::Value(Value::Num(2))),
                    }),
                },
            ],
        }),
    }];

    compile(test_program);
}

pub fn test_better_llvm_codegen_with_param() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![Param {
            name: Value::Identifier("a"),
            ty: Value::Identifier("i32"),
        }],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                UntypedExpr::Let {
                    id: Value::Identifier("x"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                        op: InfixOpKind::Mul,
                        right: Box::new(UntypedExpr::Value(Value::Num(2))),
                    }),
                    constness: Const::No,
                },
                UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                    op: InfixOpKind::Add,
                    right: Box::new(UntypedExpr::Value(Value::Num(3))),
                },
            ],
        }),
    }];

    compile(test_program);
}

pub fn test_llvm_branching() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                // let y = 7;
                UntypedExpr::Let {
                    id: Value::Identifier("y"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Num(7))),
                    constness: Const::No,
                },
                // const x = if y > 5 { 8 } else { 9 };
                UntypedExpr::Let {
                    id: Value::Identifier("x"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::If {
                        condition: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("y"))),
                            op: InfixOpKind::Greater,
                            right: Box::new(UntypedExpr::Value(Value::Num(5))),
                        }),
                        then_branch: Box::new(UntypedExpr::Value(Value::Num(8))),
                        else_branch: Box::new(UntypedExpr::Value(Value::Num(9))),
                    }),
                    constness: Const::Yes,
                },
                // return x;
                UntypedExpr::Value(Value::Identifier("x")),
            ],
        }),
    }];

    compile(test_program);
}

pub fn test_llvm_branching_stress() {
    let test_program = vec![UntypedExpr::Fn {
        name: Value::Identifier("main"),
        params: vec![Param {
            name: Value::Identifier("a"),
            ty: Value::Identifier("i32"),
        }],
        retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
        body: Box::new(UntypedExpr::Block {
            statements: vec![
                // let mut x = a;
                UntypedExpr::Let {
                    id: Value::Identifier("x"),
                    pat: TypePath::Empty,
                    expr: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                    constness: Const::No,
                },
                // if x > 5 { x = x + 2; if x < 10 { x = 8; } else { x = 12; } } else { x = x * 3; }
                UntypedExpr::If {
                    condition: Box::new(UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                        op: InfixOpKind::Greater,
                        right: Box::new(UntypedExpr::Value(Value::Num(5))),
                    }),
                    then_branch: Box::new(UntypedExpr::Block {
                        statements: vec![
                            UntypedExpr::Assign {
                                left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                                right: Box::new(UntypedExpr::BinOp {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                                    op: InfixOpKind::Add,
                                    right: Box::new(UntypedExpr::Value(Value::Num(2))),
                                }),
                            },
                            UntypedExpr::If {
                                condition: Box::new(UntypedExpr::BinOp {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                                    op: InfixOpKind::Less,
                                    right: Box::new(UntypedExpr::Value(Value::Num(10))),
                                }),
                                then_branch: Box::new(UntypedExpr::Assign {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                                    right: Box::new(UntypedExpr::Value(Value::Num(8))),
                                }),
                                else_branch: Box::new(UntypedExpr::Assign {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                                    right: Box::new(UntypedExpr::Value(Value::Num(12))),
                                }),
                            },
                        ],
                    }),
                    else_branch: Box::new(UntypedExpr::Assign {
                        left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                        right: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                            op: InfixOpKind::Mul,
                            right: Box::new(UntypedExpr::Value(Value::Num(3))),
                        }),
                    }),
                },
                // return x;
                UntypedExpr::Value(Value::Identifier("x")),
            ],
        }),
    }];

    compile(test_program);
}

pub fn test_llvm_function_calls() {
    let test_program = vec![
        // fn add_two(x: i32) -> i32 { x + 2 }
        UntypedExpr::Fn {
            name: Value::Identifier("add_two"),
            params: vec![Param {
                name: Value::Identifier("x"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::BinOp {
                left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
                op: InfixOpKind::Add,
                right: Box::new(UntypedExpr::Value(Value::Num(2))),
            }),
        },
        // fn main(a: i32) -> i32 { let y = add_two(a); y }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("a"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("y"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("add_two"))),
                            args: vec![UntypedExpr::Value(Value::Identifier("a"))],
                            compeval: false,
                        }),
                        constness: Const::No,
                    },
                    UntypedExpr::Value(Value::Identifier("y")),
                ],
            }),
        },
    ];

    compile(test_program);
}

pub fn test_llvm_branching_new() {
    let test_program = vec![
        // fn max(a: i32, b: i32) -> i32 { if a > b { a } else { b } }
        UntypedExpr::Fn {
            name: Value::Identifier("max"),
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
            body: Box::new(UntypedExpr::If {
                condition: Box::new(UntypedExpr::BinOp {
                    left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                    op: InfixOpKind::Greater,
                    right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
                }),
                then_branch: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
                else_branch: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
            }),
        },
        // fn main(x: i32) -> i32 { let y = max(x, 42); y }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
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
                        expr: Box::new(UntypedExpr::Call {
                            name: Box::new(UntypedExpr::Value(Value::Identifier("max"))),
                            args: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Num(42)),
                            ],
                            compeval: false,
                        }),
                        constness: Const::No,
                    },
                    UntypedExpr::Value(Value::Identifier("y")),
                ],
            }),
        },
    ];

    compile(test_program);
}

pub fn test_llvm_arrays() {
    let test_program = vec![
        // fn main(x: i32) -> i32 { let arr = [x, 42]; arr[0] + arr[1] }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("x"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("arr"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::List {
                            items: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Num(42)),
                            ],
                        }),
                        constness: Const::No,
                    },
                    UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Index {
                            expr: Box::new(UntypedExpr::Value(Value::Identifier("arr"))),
                            index: Box::new(UntypedExpr::Value(Value::Num(0))),
                        }),
                        op: InfixOpKind::Add,
                        right: Box::new(UntypedExpr::Index {
                            expr: Box::new(UntypedExpr::Value(Value::Identifier("arr"))),
                            index: Box::new(UntypedExpr::Value(Value::Num(1))),
                        }),
                    },
                ],
            }),
        },
    ];

    compile(test_program);
}

use crate::reks_parse::{operators::InfixOpKind, utnode::*};

pub fn test_llvm_array_writes() {
    let test_program = vec![
        // fn main(x: i32) -> i32 { let arr = [x, 42]; arr[0] = 10; arr[0] + arr[1] }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("x"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("arr"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::List {
                            items: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Num(42)),
                            ],
                        }),
                        constness: Const::No,
                    },
                    UntypedExpr::Assign {
                        left: Box::new(UntypedExpr::Index {
                            expr: Box::new(UntypedExpr::Value(Value::Identifier("arr"))),
                            index: Box::new(UntypedExpr::Value(Value::Num(0))),
                        }),
                        right: Box::new(UntypedExpr::Value(Value::Num(10))),
                    },
                    UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Index {
                            expr: Box::new(UntypedExpr::Value(Value::Identifier("arr"))),
                            index: Box::new(UntypedExpr::Value(Value::Num(0))),
                        }),
                        op: InfixOpKind::Add,
                        right: Box::new(UntypedExpr::Index {
                            expr: Box::new(UntypedExpr::Value(Value::Identifier("arr"))),
                            index: Box::new(UntypedExpr::Value(Value::Num(1))),
                        }),
                    },
                ],
            }),
        },
    ];

    compile(test_program);
}

pub fn test_llvm_while_loops() {
    let test_program = vec![
        // fn main(x: i32) -> i32 { let arr = [x, 42]; let i = 0; while i < 2 { arr[i] = arr[i] + 1; i = i + 1; } arr[0] + arr[1] }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("x"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Block {
                statements: vec![
                    UntypedExpr::Let {
                        id: Value::Identifier("arr"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::List {
                            items: vec![
                                UntypedExpr::Value(Value::Identifier("x")),
                                UntypedExpr::Value(Value::Num(42)),
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
                            right: Box::new(UntypedExpr::Value(Value::Num(2))),
                        }),
                        body: Box::new(UntypedExpr::Block {
                            statements: vec![
                                UntypedExpr::Assign {
                                    left: Box::new(UntypedExpr::Index {
                                        expr: Box::new(UntypedExpr::Value(Value::Identifier(
                                            "arr",
                                        ))),
                                        index: Box::new(UntypedExpr::Value(Value::Identifier("i"))),
                                    }),
                                    right: Box::new(UntypedExpr::BinOp {
                                        left: Box::new(UntypedExpr::Index {
                                            expr: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "arr",
                                            ))),
                                            index: Box::new(UntypedExpr::Value(Value::Identifier(
                                                "i",
                                            ))),
                                        }),
                                        op: InfixOpKind::Add,
                                        right: Box::new(UntypedExpr::Value(Value::Num(1))),
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
                    UntypedExpr::BinOp {
                        left: Box::new(UntypedExpr::Index {
                            expr: Box::new(UntypedExpr::Value(Value::Identifier("arr"))),
                            index: Box::new(UntypedExpr::Value(Value::Num(0))),
                        }),
                        op: InfixOpKind::Add,
                        right: Box::new(UntypedExpr::Index {
                            expr: Box::new(UntypedExpr::Value(Value::Identifier("arr"))),
                            index: Box::new(UntypedExpr::Value(Value::Num(1))),
                        }),
                    },
                ],
            }),
        },
    ];

    compile(test_program);
}

pub fn test_llvm_factorial_with_call() {
    let test_program = vec![
        // fn factorial(n: i32) -> i32 { ... }
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
                        id: Value::Identifier("i"),
                        pat: TypePath::Empty,
                        expr: Box::new(UntypedExpr::Value(Value::Identifier("n"))),
                        constness: Const::No,
                    },
                    UntypedExpr::While {
                        guard: Box::new(UntypedExpr::BinOp {
                            left: Box::new(UntypedExpr::Value(Value::Identifier("i"))),
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
                                        right: Box::new(UntypedExpr::Value(Value::Identifier("i"))),
                                    }),
                                },
                                UntypedExpr::Assign {
                                    left: Box::new(UntypedExpr::Value(Value::Identifier("i"))),
                                    right: Box::new(UntypedExpr::BinOp {
                                        left: Box::new(UntypedExpr::Value(Value::Identifier("i"))),
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
        // fn main(n: i32) -> i32 { factorial(n) }
        UntypedExpr::Fn {
            name: Value::Identifier("main"),
            params: vec![Param {
                name: Value::Identifier("n"),
                ty: Value::Identifier("i32"),
            }],
            retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
            body: Box::new(UntypedExpr::Call {
                name: Box::new(UntypedExpr::Value(Value::Identifier("factorial"))),
                args: vec![UntypedExpr::Value(Value::Identifier("n"))],
                compeval: false,
            }),
        },
    ];

    compile(test_program);
}
