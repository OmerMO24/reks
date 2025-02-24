use super::utnode::{self, *};
use crate::frontend::{lexer::*, operators, utnode::*};
use chumsky::{input::ValueInput, prelude::*, recursive};

pub fn parse_expr<'src, I>(
) -> impl Parser<'src, I, Vec<UntypedExpr<'src>>, extra::Err<Rich<'src, Token<'src>>>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let atom = select! {
            Token::TOKEN_INT(x) => utnode::UntypedExpr::Value(Value::Num(x.parse().unwrap())),
            Token::TOKEN_FLOAT(x) => utnode::UntypedExpr::Value(Value::Float(x.parse().unwrap())),
        };

        atom
    })
    .repeated()
    .collect::<Vec<UntypedExpr>>()
}
