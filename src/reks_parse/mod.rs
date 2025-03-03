pub mod lexer;
pub mod operators;
pub mod parser;
pub mod utnode;

use crate::reks_parse::lexer::Token;

// Used to go from a token to some sort of AST representation
pub trait FromKind {
    fn from_kind(token: Token) -> Option<Self>
    where
        Self: Sized;
}
