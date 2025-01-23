mod frontend;
use frontend::lexer::Token;
use logos::Logos;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    //let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let src = std::fs::read_to_string("fn.reks")?;
    let it = Token::lexer(&src);

    for tok in it {
        println!("{:?}", tok.unwrap());
    }

    Ok(())
    //println!("{}", src);
}
