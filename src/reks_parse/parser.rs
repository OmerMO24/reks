use super::utnode::{self, *};
use crate::reks_parse::lexer::*;
use crate::reks_parse::operators::*;
use chumsky::{input::ValueInput, prelude::*};

pub fn parse_expr<'src, I>(
) -> impl Parser<'src, I, UntypedExpr<'src>, extra::Err<Rich<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let base = recursive(|expr| {
        let unit = select! {
            Token::TOKEN_INT(x) => utnode::UntypedExpr::Value(Value::Num(x)),
            Token::TOKEN_FLOAT(x) => utnode::UntypedExpr::Value(Value::Float(x.parse().unwrap())),
            Token::TOKEN_STRING(x) => utnode::UntypedExpr::Value(Value::String(x)),
            Token::TOKEN_IDENT(x) => utnode::UntypedExpr::Value(Value::Identifier(x)),
        };

        let ident =
            select! { Token::TOKEN_IDENT(name) => name }.map(|name| Value::Identifier(name));

        let compeval = just(Token::TOKEN_COMP_EVAL).or_not();

        let path = just(Token::TOKEN_COLON)
            .ignore_then(ident)
            .map(|annotation| TypePath::Typed { ident: annotation })
            .or_not();

        let items = expr
            .clone()
            .separated_by(just(Token::TOKEN_COMMA))
            .allow_trailing()
            .collect::<Vec<_>>();

        let list = items
            .clone()
            .delimited_by(just(Token::TOKEN_L_BRACK), just(Token::TOKEN_R_BRACK))
            .map(|elements| utnode::UntypedExpr::List { items: elements });

        let _let = just(Token::TOKEN_CONST)
            .to(Const::Yes)
            .or(just(Token::TOKEN_MUT).to(Const::No))
            .then(ident)
            .then(path)
            .then_ignore(just(Token::TOKEN_ASSIGN))
            .then(expr.clone())
            .map(
                |(((constness, name), annotation), value)| utnode::UntypedExpr::Let {
                    id: name,
                    expr: Box::new(value),
                    pat: annotation.unwrap_or(TypePath::Empty),
                    constness,
                },
            );

        let block = expr
            .clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::TOKEN_L_BRACE), just(Token::TOKEN_R_BRACE))
            .map(|statements_| utnode::UntypedExpr::Block {
                statements: statements_,
            });

        let if_statement = just(Token::TOKEN_IF)
            .ignore_then(expr.clone())
            .then(block.clone())
            .then(just(Token::TOKEN_ELSE).ignore_then(block.clone()))
            .map(|((cond, then_), else_)| utnode::UntypedExpr::If {
                condition: Box::new(cond),
                then_branch: Box::new(then_),
                else_branch: Box::new(else_),
            });

        let while_loop = just(Token::TOKEN_WHILE)
            .ignore_then(expr.clone())
            .then(block.clone())
            .map(|(guard_, body_)| utnode::UntypedExpr::While {
                guard: Box::new(guard_),
                body: Box::new(body_),
            });

        // let struct_init = ident
        //     .then(
        //         just(Token::TOKEN_L_BRACE)
        //             .ignore_then(
        //                 ident
        //                     .then_ignore(just(Token::TOKEN_COLON))
        //                     .then(expr.clone())
        //                     .map(|(field_name, field_value)| (field_name, field_value))
        //                     .separated_by(just(Token::TOKEN_COMMA))
        //                     .allow_trailing()
        //                     .collect::<Vec<_>>(),
        //             )
        //             .then_ignore(just(Token::TOKEN_R_BRACE)),
        //     )
        //     .map(|(struct_name, fields)| utnode::UntypedExpr::StructInit {
        //         id: struct_name,
        //         fields,
        //     });

        let atom = unit
            .or(list)
            .or(_let)
            .or(if_statement)
            .or(while_loop)
            //.or(struct_init)
            .or(expr
                .clone()
                .delimited_by(just(Token::TOKEN_L_PAREN), just(Token::TOKEN_R_PAREN)));

        let call = compeval
            .then(atom.clone())
            .then(items.delimited_by(just(Token::TOKEN_L_PAREN), just(Token::TOKEN_R_PAREN)))
            .then_ignore(just(Token::TOKEN_SEMICOLON).or_not())
            .map(|((is_compeval, func), args_)| utnode::UntypedExpr::Call {
                name: Box::new(func),
                args: args_,
                compeval: match is_compeval {
                    Some(_) => true,
                    None => false,
                },
            });

        let index = atom
            .clone()
            .then(
                expr.clone()
                    .delimited_by(just(Token::TOKEN_L_BRACK), just(Token::TOKEN_R_BRACK)),
            )
            .map(|(expr_, index_)| utnode::UntypedExpr::Index {
                expr: Box::new(expr_),
                index: Box::new(index_),
            });

        // Simple field access (just a.b, no chaining)
        let field_access = atom
            .clone()
            .then(just(Token::TOKEN_DOT).ignore_then(ident))
            .map(|(base, field)| utnode::UntypedExpr::FieldAccess {
                id: Box::new(base),
                field,
            });

        let unary_op = just(Token::TOKEN_BANG)
            .to(PrefixOpKind::Bang)
            .or(just(Token::TOKEN_ADDRESS_OF).to(PrefixOpKind::AddressOf));

        let unary =
            unary_op
                .then(expr.clone())
                .map(|(op_kind, operand)| utnode::UntypedExpr::UnaryOp {
                    op: op_kind,
                    operand: Box::new(operand),
                });

        // let term = call.or(atom).or(unary);

        let term = field_access.or(call).or(index).or(atom).or(unary);

        let prod_op = just(Token::TOKEN_MUL)
            .to(InfixOpKind::Mul)
            .or(just(Token::TOKEN_DIV).to(InfixOpKind::Div));

        let product =
            term.clone()
                .foldl(prod_op.then(term.clone()).repeated(), |a, (op_kind, b)| {
                    utnode::UntypedExpr::BinOp {
                        left: Box::new(a),
                        op: op_kind,
                        right: Box::new(b),
                    }
                });

        let sum_op = just(Token::TOKEN_ADD)
            .to(InfixOpKind::Add)
            .or(just(Token::TOKEN_SUB).to(InfixOpKind::Sub));

        let sum = product.clone().foldl(
            sum_op.then(product.clone()).repeated(),
            |a, (op_kind, b)| utnode::UntypedExpr::BinOp {
                left: Box::new(a),
                op: op_kind,
                right: Box::new(b),
            },
        );

        let comp = sum.clone().foldl(
            choice((
                just(Token::TOKEN_LESS).to(InfixOpKind::Less),
                just(Token::TOKEN_GREATER).to(InfixOpKind::Greater),
                just(Token::TOKEN_LESS_OR_EQ).to(InfixOpKind::LessOrEq),
                just(Token::TOKEN_GREATER_OR_EQ).to(InfixOpKind::GreaterOrEq),
                just(Token::TOKEN_EQUAL).to(InfixOpKind::Equals),
                just(Token::TOKEN_NOT_EQ).to(InfixOpKind::NotEq),
            ))
            .then(sum.clone())
            .repeated(),
            |a, (op_kind, b)| utnode::UntypedExpr::BinOp {
                left: Box::new(a),
                op: op_kind,
                right: Box::new(b),
            },
        );

        let assignment = comp
            .clone()
            .then(just(Token::TOKEN_ASSIGN).ignore_then(expr.clone()).or_not())
            .then_ignore(just(Token::TOKEN_SEMICOLON).or_not())
            .map(|(lhs, rhs_opt)| {
                if let Some(rhs) = rhs_opt {
                    utnode::UntypedExpr::Assign {
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    }
                } else {
                    lhs
                }
            });

        // comp.or(block)
        //comp.or(term)

        assignment
    });

    base.then_ignore(just(Token::TOKEN_SEMICOLON).or_not())
}

pub fn parse_block<'src, I>(
) -> impl Parser<'src, I, UntypedExpr<'src>, extra::Err<Rich<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let block = parse_expr()
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::TOKEN_L_BRACE), just(Token::TOKEN_R_BRACE))
        .map(|statements_| utnode::UntypedExpr::Block {
            statements: statements_,
        });

    block
}

pub fn parse_fn_decl<'src, I>(
) -> impl Parser<'src, I, UntypedExpr<'src>, extra::Err<Rich<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select! { Token::TOKEN_IDENT(name) => name }.map(|name| Value::Identifier(name));

    let param = ident
        .then_ignore(just(Token::TOKEN_COLON))
        .then(ident)
        .map(|(name_, ty_)| Param {
            name: name_,
            ty: ty_,
        });

    let params = param
        .separated_by(just(Token::TOKEN_COMMA))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::TOKEN_L_PAREN), just(Token::TOKEN_R_PAREN));

    let fn_decl = just(Token::TOKEN_FN)
        .ignore_then(ident)
        .then(params)
        .then_ignore(just(Token::TOKEN_ARROW))
        .then(parse_expr())
        .then(parse_block())
        .map(
            |(((name_, params_), ret_ty), body_)| utnode::UntypedExpr::Fn {
                name: name_,
                params: params_,
                retty: Box::new(ret_ty),
                body: Box::new(body_),
            },
        );

    fn_decl
}

pub fn parse_struct_decl<'src, I>(
) -> impl Parser<'src, I, UntypedExpr<'src>, extra::Err<Rich<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select! { Token::TOKEN_IDENT(name) => name }.map(|name| Value::Identifier(name));

    let param = ident
        .then_ignore(just(Token::TOKEN_COLON))
        .then(ident)
        .map(|(name_, ty_)| Param {
            name: name_,
            ty: ty_,
        });

    let fields = param
        .separated_by(just(Token::TOKEN_COMMA))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::TOKEN_L_BRACE), just(Token::TOKEN_R_BRACE));

    let struct_decl = just(Token::TOKEN_STRUCT)
        .ignore_then(ident)
        .then(fields)
        .map(|(name, fields_)| utnode::UntypedExpr::Struct {
            id: name,
            fields: fields_,
        });

    struct_decl
}

pub fn parse_program<'src, I>(
) -> impl Parser<'src, I, Vec<UntypedExpr<'src>>, extra::Err<Rich<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    parse_fn_decl().or(parse_struct_decl()).repeated().collect()
}
