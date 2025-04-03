mod reks_compile;
mod reks_eval;
mod reks_parse;
mod reks_type;
use chumsky::{input::Stream, prelude::*};
use logos::Logos;
use reks_compile::compile::*;
use reks_parse::{lexer::Token, parser::parse_program};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let src = std::fs::read_to_string("examples/reverse_list.reks")?;
    let it = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });

    let token_stream = Stream::from_iter(it)
        // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
        // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
        .map((0..src.len()).into(), |(t, s): (_, _)| (t, s));

    let program = match parse_program().parse(token_stream).into_result() {
        Ok(prog) => prog,
        Err(_prog) => vec![],
    };

    for node in &program {
        println!("{:?}", node);
    }

    compile(program);

    Ok(())
}
