mod frontend;
use chumsky::{input::Stream, prelude::*};
use extra::ParserExtra;
use frontend::lexer::Token;
use frontend::parser::*;
use frontend::utnode::Value;
use logos::Logos;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    //let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let src = std::fs::read_to_string("fn.reks")?;
    let it = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });

    // Turn the token iterator into a stream that chumsky can use for things like backtracking
    let token_stream = Stream::from_iter(it)
        // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
        // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
        .map((0..src.len()).into(), |(t, s): (_, _)| (t, s));

    match parse_expr().parse(token_stream).into_result() {
        // If parsing was successful, attempt to evaluate the s-expression
        Ok(sexpr) => println!("{:?}", sexpr),

        Err(sexpr) => println!("we dun goofed: {:?}", sexpr),
        // If parsing was unsuccessful, generate a nice user-friendly diagnostic with ariadne. You could also use
        // codespan, or whatever other diagnostic library you care about. You could even just display-print the errors
        // with Rust's built-in `Display` trait, but it's a little crude
        // Err(errs) => {
        //     for err in errs {
        //         Report::build(ReportKind::Error, (), err.span().start)
        //             .with_code(3)
        //             .with_message(err.to_string())
        //             .with_label(
        //                 Label::new(err.span().into_range())
        //                     .with_message(err.reason().to_string())
        //                     .with_color(Color::Red),
        //             )
        //             .finish()
        //             .eprint(Source::from(src))
        //             .unwrap();
        //     }
        // }
    }

    Ok(())

    //println!("{}", src);
}
