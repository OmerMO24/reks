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
