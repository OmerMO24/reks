mod reks_compile;
mod reks_eval;
mod reks_parse;
mod reks_type;
use chumsky::{input::Stream, prelude::*};
use reks_eval::test_cir::*;
use reks_type::test_inference::*;

// fn main() -> Result<(), Box<dyn Error>> {
//let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

//     let src = std::fs::read_to_string("fn.reks")?;
//     let it = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
//         Ok(tok) => (tok, span.into()),
//         Err(()) => (Token::Error, span.into()),
//     });

//     // Turn the token iterator into a stream that chumsky can use for things like backtracking
//     let token_stream = Stream::from_iter(it)
//         // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
//         // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
//         .map((0..src.len()).into(), |(t, s): (_, _)| (t, s));

//     match parse_expr().parse(token_stream).into_result() {
//         // If parsing was successful, attempt to evaluate the s-expression
//         Ok(sexpr) => println!("{:?}", sexpr),

//         Err(sexpr) => println!("we dun goofed: {:?}", sexpr),
//         // If parsing was unsuccessful, generate a nice user-friendly diagnostic with ariadne. You could also use
//         // codespan, or whatever other diagnostic library you care about. You could even just display-print the errors
//         // with Rust's built-in `Display` trait, but it's a little crude
//         // Err(errs) => {
//         //     for err in errs {
//         //         Report::build(ReportKind::Error, (), err.span().start)
//         //             .with_code(3)
//         //             .with_message(err.to_string())
//         //             .with_label(
//         //                 Label::new(err.span().into_range())
//         //                     .with_message(err.reason().to_string())
//         //                     .with_color(Color::Red),
//         //             )
//         //             .finish()
//         //             .eprint(Source::from(src))
//         //             .unwrap();
//         //     }
//         // }
//     }

//     Ok(())

//     //println!("{}", src);
// }

fn main() {
    // let test_variables = vec![UntypedExpr::Fn {
    //     name: Value::Identifier("test"),
    //     params: vec![],
    //     retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
    //     body: Box::new(UntypedExpr::Block {
    //         statements: vec![
    //             UntypedExpr::Let {
    //                 id: Value::Identifier("x"),
    //                 pat: TypePath::Typed {
    //                     ident: Value::Identifier("i32"),
    //                 },
    //                 expr: Box::new(UntypedExpr::Value(Value::Num(5))),
    //                 constness: Const::Yes,
    //             },
    //             UntypedExpr::Let {
    //                 id: Value::Identifier("y"),
    //                 pat: TypePath::Empty,
    //                 expr: Box::new(UntypedExpr::BinOp {
    //                     left: Box::new(UntypedExpr::Value(Value::Identifier("x"))),
    //                     op: InfixOpKind::Add,
    //                     right: Box::new(UntypedExpr::Value(Value::Num(10))),
    //                 }),
    //                 constness: Const::Yes,
    //             },
    //             UntypedExpr::Value(Value::Identifier("y")),
    //         ],
    //     }),
    // }];

    // let test_scopes = vec![UntypedExpr::Fn {
    //     name: Value::Identifier("scope_test"),
    //     params: vec![],
    //     retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
    //     body: Box::new(UntypedExpr::Block {
    //         statements: vec![
    //             UntypedExpr::Let {
    //                 id: Value::Identifier("outer"),
    //                 pat: TypePath::Empty,
    //                 expr: Box::new(UntypedExpr::Value(Value::Num(10))),
    //                 constness: Const::Yes,
    //             },
    //             UntypedExpr::Block {
    //                 statements: vec![
    //                     UntypedExpr::Let {
    //                         id: Value::Identifier("inner"),
    //                         pat: TypePath::Empty,
    //                         expr: Box::new(UntypedExpr::Value(Value::Num(20))),
    //                         constness: Const::Yes,
    //                     },
    //                     UntypedExpr::Let {
    //                         id: Value::Identifier("outer"),
    //                         pat: TypePath::Empty,
    //                         expr: Box::new(UntypedExpr::Value(Value::Num(30))),
    //                         constness: Const::Yes,
    //                     },
    //                     UntypedExpr::BinOp {
    //                         left: Box::new(UntypedExpr::Value(Value::Identifier("outer"))),
    //                         op: InfixOpKind::Add,
    //                         right: Box::new(UntypedExpr::Value(Value::Identifier("inner"))),
    //                     },
    //                 ],
    //             },
    //             UntypedExpr::Value(Value::Identifier("outer")),
    //         ],
    //     }),
    // }];

    // let test_structs = vec![
    //     UntypedExpr::Struct {
    //         id: Value::Identifier("Point"),
    //         fields: vec![
    //             Param {
    //                 name: Value::Identifier("x"),
    //                 ty: Value::Identifier("f32"),
    //             },
    //             Param {
    //                 name: Value::Identifier("y"),
    //                 ty: Value::Identifier("f32"),
    //             },
    //         ],
    //     },
    //     UntypedExpr::Fn {
    //         name: Value::Identifier("calculate_distance"),
    //         params: vec![
    //             Param {
    //                 name: Value::Identifier("p1"),
    //                 ty: Value::Identifier("Point"),
    //             },
    //             Param {
    //                 name: Value::Identifier("p2"),
    //                 ty: Value::Identifier("Point"),
    //             },
    //         ],
    //         retty: Box::new(UntypedExpr::Value(Value::Identifier("f32"))),
    //         body: Box::new(UntypedExpr::Block {
    //             statements: vec![
    //                 UntypedExpr::Let {
    //                     id: Value::Identifier("dx"),
    //                     pat: TypePath::Empty,
    //                     expr: Box::new(UntypedExpr::BinOp {
    //                         left: Box::new(UntypedExpr::FieldAccess {
    //                             id: Box::new(UntypedExpr::Value(Value::Identifier("p2"))),
    //                             field: Value::Identifier("x"),
    //                         }),
    //                         op: InfixOpKind::Sub,
    //                         right: Box::new(UntypedExpr::FieldAccess {
    //                             id: Box::new(UntypedExpr::Value(Value::Identifier("p1"))),
    //                             field: Value::Identifier("x"),
    //                         }),
    //                     }),
    //                     constness: Const::Yes,
    //                 },
    //                 UntypedExpr::Let {
    //                     id: Value::Identifier("dy"),
    //                     pat: TypePath::Empty,
    //                     expr: Box::new(UntypedExpr::BinOp {
    //                         left: Box::new(UntypedExpr::FieldAccess {
    //                             id: Box::new(UntypedExpr::Value(Value::Identifier("p2"))),
    //                             field: Value::Identifier("y"),
    //                         }),
    //                         op: InfixOpKind::Sub,
    //                         right: Box::new(UntypedExpr::FieldAccess {
    //                             id: Box::new(UntypedExpr::Value(Value::Identifier("p1"))),
    //                             field: Value::Identifier("y"),
    //                         }),
    //                     }),
    //                     constness: Const::Yes,
    //                 },
    //                 UntypedExpr::BinOp {
    //                     left: Box::new(UntypedExpr::Value(Value::Identifier("dx"))),
    //                     op: InfixOpKind::Add,
    //                     right: Box::new(UntypedExpr::Value(Value::Identifier("dy"))),
    //                 },
    //             ],
    //         }),
    //     },
    // ];

    // let test_function_calls = vec![
    //     UntypedExpr::Fn {
    //         name: Value::Identifier("add"),
    //         params: vec![
    //             Param {
    //                 name: Value::Identifier("a"),
    //                 ty: Value::Identifier("i32"),
    //             },
    //             Param {
    //                 name: Value::Identifier("b"),
    //                 ty: Value::Identifier("i32"),
    //             },
    //         ],
    //         retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
    //         body: Box::new(UntypedExpr::Block {
    //             statements: vec![UntypedExpr::BinOp {
    //                 left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
    //                 op: InfixOpKind::Add,
    //                 right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
    //             }],
    //         }),
    //     },
    //     UntypedExpr::Fn {
    //         name: Value::Identifier("compute"),
    //         params: vec![],
    //         retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
    //         body: Box::new(UntypedExpr::Block {
    //             statements: vec![
    //                 UntypedExpr::Let {
    //                     id: Value::Identifier("x"),
    //                     pat: TypePath::Empty,
    //                     expr: Box::new(UntypedExpr::Value(Value::Num(5))),
    //                     constness: Const::Yes,
    //                 },
    //                 UntypedExpr::Let {
    //                     id: Value::Identifier("y"),
    //                     pat: TypePath::Empty,
    //                     expr: Box::new(UntypedExpr::Value(Value::Num(10))),
    //                     constness: Const::Yes,
    //                 },
    //                 UntypedExpr::Call {
    //                     name: Box::new(UntypedExpr::Value(Value::Identifier("add"))),
    //                     args: vec![
    //                         UntypedExpr::Value(Value::Identifier("x")),
    //                         UntypedExpr::Value(Value::Identifier("y")),
    //                     ],
    //                 },
    //             ],
    //         }),
    //     },
    // ];

    // let test_conditionals = vec![UntypedExpr::Fn {
    //     name: Value::Identifier("max"),
    //     params: vec![
    //         Param {
    //             name: Value::Identifier("a"),
    //             ty: Value::Identifier("i32"),
    //         },
    //         Param {
    //             name: Value::Identifier("b"),
    //             ty: Value::Identifier("i32"),
    //         },
    //     ],
    //     retty: Box::new(UntypedExpr::Value(Value::Identifier("i32"))),
    //     body: Box::new(UntypedExpr::Block {
    //         statements: vec![UntypedExpr::If {
    //             condition: Box::new(UntypedExpr::BinOp {
    //                 left: Box::new(UntypedExpr::Value(Value::Identifier("a"))),
    //                 op: InfixOpKind::Greater,
    //                 right: Box::new(UntypedExpr::Value(Value::Identifier("b"))),
    //             }),
    //             then_branch: Box::new(UntypedExpr::Block {
    //                 statements: vec![UntypedExpr::Value(Value::Identifier("a"))],
    //             }),
    //             else_branch: Box::new(UntypedExpr::Block {
    //                 statements: vec![UntypedExpr::Value(Value::Identifier("b"))],
    //             }),
    //         }],
    //     }),
    // }];

    // run_test("Variable Declaration and Use", &test_variables);
    // run_test("Nested Scopes and Shadowing", &test_scopes);
    // run_test("Struct Declaration and Field Access", &test_structs);
    // run_test("Function Calls", &test_function_calls);
    // run_test("Conditional Logic", &test_conditionals);

    //test_name_resolver();
    //test_complex_nesting();
    //test_manual_verification();
    //test_type_inference(); passed
    //test_unannotated_let_inference(); passed
    //test_all_unknowns_inference(); passed
    //test_function_call_inference(); passed
    //test_generic_struct_inference(); passed
    //test_list_inference();
    //test_if_statement_inference();
    //test_field_access_inference();
    //test_nested_struct_field_access();
    //test_unannotated_let_with_field_access();
    // test_monolithic_program();
    // test_simple_program();
    // test_complex_realistic_program();
    // test_immutable_assignment_error();
    // test_final_fuzz_program();
    //

    // test_cir_ssa_simple();
    // test_cir_ssa_complex();
    test_cir_ssa_conditionals();
    test_cir_ssa_structs();
    test_cir_ssa_assignments();
    // test_cir_ssa_lists();
    // test_cir_ssa_binops();
    // test_cir_ssa_binops_extended();
    // test_cir_ssa_add();
    // test_cir_ssa_factorial_simple();
    // test_cir_factorial_direct();
}
