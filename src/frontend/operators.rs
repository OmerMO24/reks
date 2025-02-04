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

pub trait FromKind {
    pub fn from_kind(token: Token) -> Option<Self>;
}

impl FromKind for BinOpKind {
    fn from_kind(token: Token) -> Option<Self> {
        match token {
            Token::TOKEN_ADD => Some(BinOpKind::Add),
            Token::TOKEN_SUB => Some(BinOpKind::Sub),
            Token::TOKEN_MUL => Some(BinOpKind::Mul),
            Token::TOKEN_DIV => Some(BinOpKind::Div),
            Token::TOKEN_EXP => Some(BinOpKind::Exp),
            Token::TOKEN_MOD => Some(BinOpKind::Mod),

            Token::TOKEN_AND_AND => Some(BinOpKind::And),
            Token::TOKEN_OR_OR => Some(BinOpKind::Or),

            Token::TOKEN_EQUAL => Some(BinOpKind::Equals),
            Token::TOKEN_GREATER => Some(BinOpKind::Greater),
            Token::TOKEN_GREATER_OR_EQ => Some(BinOpKind::GreaterOrEq),
            Token::TOKEN_LESS => Some(BinOpKind::Less),
            Token::TOKEN_LESS_OR_EQ => Some(BinOpKind::LessOrEq),
            Token::TOKEN_NOT_EQ => Some(BinOpKind::NotEq),
            Token::TOKEN_DOT => Some(BinOpKind::Add),

            _ => None,
        }
    }
}

impl FromKind for UnaryOpKind {
    fn from_kind(token: Token) -> Option<Self> {
        match token {
            Token::TOKEN_BANG => Some(UnaryOpKind::Bang),
            Token::TOKEN_ADDRESS_OF => Some(UnaryOpKind::Bang),
            _ => None,
        }
    }
}
