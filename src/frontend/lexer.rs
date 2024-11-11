use std::fs;

use logos::{Lexer, Logos, Span};

#[derive(Debug)]
enum LexingError {
    ParseError,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"//[^\n]*")] // skip single-line comments
#[logos(skip r"/\*(?:[^*]|\*[^/])*\*/")] // skip multi-line comments
#[logos(skip r"[ \t\n\f]+")] // whitespaces
pub enum Token<'src> {
    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    #[token("[")]
    BracketOpen,

    #[token("]")]
    BraceketClose,

    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    #[token(":")]
    Colon,

    #[token(";")]
    SemiColon,

    #[token(",")]
    Comma,

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice())]
    String(&'src str),

    // Loop constructs
    #[token("for")]
    For,

    #[token("while")]
    While,

    // Logical operators
    #[token("&&")]
    And,

    #[token("||")]
    Or,

    #[token("!")]
    Not,

    // Memory operators
    #[token("&")]
    AddressOf,

    // Relational and Comparison Operators
    #[token("->")]
    ArrowOp,

    #[token("==")]
    Equal,

    #[token(">")]
    GreaterThan,

    #[token("<")]
    LessThan,

    #[token(">=")]
    GreaterThanEq,

    #[token("<=")]
    LessThanEq,

    #[token("!=")]
    NotEqual,

    // Arithmetic Operators
    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("^")]
    Exp,

    #[token("%")]
    Modulo,

    // Numbers

    //Signed Integers
    #[token("i8")]
    I8Type,

    #[token("i16")]
    I16Type,

    #[token("i32")]
    I32Type,

    #[token("i64")]
    I64Type,

    #[token("i128")]
    I128Type,

    // Unsigned Integers
    #[token("u8")]
    U8Type,

    #[token("u16")]
    U16Type,

    #[token("u32")]
    U32Type,

    #[token("u64")]
    U64Type,

    #[token("u128")]
    U128Type,

    // Floating Points
    #[token("f32")]
    F32Type,

    #[token("f64")]
    F64Type,

    // Keywords
    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("fn")]
    FunctionDecl,

    #[token("struct")]
    StructDecl,

    #[token("const")]
    ConstDecl,

    #[token("mut")]
    MutDecl,

    // Literals
    #[regex(r"-?[0-9]+", |lex| lex.slice())]
    IntegerLiteral(&'src str),

    #[regex(r"-?[0-9]*\.[0-9]+", |lex| lex.slice())]
    Float(&'src str),

    //Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Identifier(&'src str),
}

pub fn stream(source: &str) -> Vec<Token> {
    let lexer = Token::lexer(source);
    let mut stream = Vec::new();

    for token in lexer {
        match token {
            Ok(token) => stream.push(token),
            Err(_) => panic!(),
        }
    }
    stream
}
