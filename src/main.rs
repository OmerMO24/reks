mod frontend;
use frontend::lexer::stream;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    //let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let src = std::fs::read_to_string("fn.reks")?;
    let vec = stream(&src);

    for token in vec {
        println!("This da token: {:?}", token);
    }

    Ok(())
    //println!("{}", src);
}
