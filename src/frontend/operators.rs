use crate::frontend::lexer::*;

#[derive(Debug, Clone)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,

    Exp,
    Mod,

    And,
    Or,
    Equals,
    Greater,
    GreaterOrEq,
    Less,
    LessOrEq,
    NotEq,

    Dot,
}

pub enum UnaryOpKind {
    Bang,
    AddressOf,
}
