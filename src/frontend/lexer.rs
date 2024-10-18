use logos::{Lexer, Logos, Span};


#[derive(Logos, Debug, PartialEq)]
enum Token<'source> {

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
    SemiColon;

    #[token(",")]
    Comma,

    #[token()]



    
}


