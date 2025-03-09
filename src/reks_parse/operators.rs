use crate::reks_parse::lexer::*;
use crate::reks_parse::FromKind;

#[derive(Debug, Clone)]
pub enum InfixOpKind {
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

    Assign,

    Dot,
}

#[derive(Debug, Clone)]
pub enum PrefixOpKind {
    Bang,
    AddressOf,
}

impl FromKind for InfixOpKind {
    fn from_kind(token: Token) -> Option<Self> {
        match token {
            Token::TOKEN_ADD => Some(InfixOpKind::Add),
            Token::TOKEN_SUB => Some(InfixOpKind::Sub),
            Token::TOKEN_MUL => Some(InfixOpKind::Mul),
            Token::TOKEN_DIV => Some(InfixOpKind::Div),
            Token::TOKEN_EXP => Some(InfixOpKind::Exp),
            Token::TOKEN_MOD => Some(InfixOpKind::Mod),

            Token::TOKEN_AND_AND => Some(InfixOpKind::And),
            Token::TOKEN_OR_OR => Some(InfixOpKind::Or),

            Token::TOKEN_EQUAL => Some(InfixOpKind::Equals),
            Token::TOKEN_GREATER => Some(InfixOpKind::Greater),
            Token::TOKEN_GREATER_OR_EQ => Some(InfixOpKind::GreaterOrEq),
            Token::TOKEN_LESS => Some(InfixOpKind::Less),
            Token::TOKEN_LESS_OR_EQ => Some(InfixOpKind::LessOrEq),
            Token::TOKEN_NOT_EQ => Some(InfixOpKind::NotEq),
            Token::TOKEN_DOT => Some(InfixOpKind::Dot),

            _ => None,
        }
    }
}

impl FromKind for PrefixOpKind {
    fn from_kind(token: Token) -> Option<Self> {
        match token {
            Token::TOKEN_BANG => Some(PrefixOpKind::Bang),
            Token::TOKEN_ADDRESS_OF => Some(PrefixOpKind::Bang),
            _ => None,
        }
    }
}
