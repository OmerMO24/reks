#![allow(non_camel_case_types)]

use logos::{Lexer, Logos, Span};
use std::fs;

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
    TOKEN_L_BRACE,

    #[token("}")]
    TOKEN_R_BRACE,

    #[token("[")]
    TOKEN_L_BRACK,

    #[token("]")]
    TOKEN_R_BRACK,

    #[token("(")]
    TOKEN_L_PAREN,

    #[token(")")]
    TOKEN_R_PAREN,

    #[token(":")]
    TOKEN_COLON,

    #[token(";")]
    TOKEN_SEMICOLON,

    #[token(",")]
    TOKEN_COMMA,

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice())]
    TOKEN_STRING(&'src str),

    // Loop constructs
    #[token("for")]
    TOKEN_FOR,

    #[token("while")]
    TOKEN_WHILE,

    // Logical operators
    #[token("&&")]
    TOKEN_AND_AND,

    #[token("||")]
    TOKEN_OR_OR,

    #[token("!")]
    TOKEN_BANG,

    // Memory operators
    #[token("&")]
    TOKEN_ADDRESS_OF,

    // Relational and Comparison Operators
    #[token("->")]
    ArrowOp,

    #[token("==")]
    TOKEN_EQUAL,

    #[token(">")]
    TOKEN_GREATER,

    #[token("<")]
    TOKEN_LESS,

    #[token(">=")]
    TOKEN_GREATER_OR_EQ,

    #[token("<=")]
    TOKEN_LESS_OR_EQ,

    #[token("!=")]
    TOKEN_NOT_EQ,

    // Arithmetic Operators
    #[token("+")]
    TOKEN_ADD,

    #[token("-")]
    TOKEN_SUB,

    #[token("*")]
    TOKEN_MUL,

    #[token("/")]
    TOKEN_DIV,

    #[token("^")]
    TOKEN_EXP,

    #[token("%")]
    TOKEN_MOD,

    #[token(".")]
    TOKEN_DOT,

    // Numbers

    //Signed Integers
    #[token("i8")]
    TOKEN_I8,

    #[token("i16")]
    TOKEN_I16,

    #[token("i32")]
    TOKEN_I32,

    #[token("i64")]
    TOKEN_I64,

    // Unsigned Integers
    #[token("u8")]
    TOKEN_U8,

    #[token("u16")]
    TOKEN_U16,

    #[token("u32")]
    TOKEN_U32,

    #[token("u64")]
    TOKEN_U64,

    // Floating Points
    #[token("f32")]
    TOKEN_F32,

    #[token("f64")]
    TOKEN_F64,

    // Keywords
    #[token("if")]
    TOKEN_IF,

    #[token("else")]
    TOKEN_ELSE,

    #[token("fn")]
    TOKEN_FN,

    #[token("struct")]
    TOKEN_STRUCT,

    #[token("const")]
    TOKEN_CONST,

    #[token("mut")]
    TOKEN_MUT,

    // Literals
    #[regex(r"-?[0-9]+", |lex| lex.slice())]
    TOKEN_INT(&'src str),

    #[regex(r"-?[0-9]*\.[0-9]+", |lex| lex.slice())]
    TOKEN_FLOAT(&'src str),

    //Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    TOKEN_IDENT(&'src str),
}
