use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

mod error;
use error::{Error, Result};

fn main() {
    process::exit(if let Some(filename) = env::args().nth(1) {
        if let Err(e) = compile_file(filename) {
            eprintln!("Compiler Error: {}", e);
            42
        } else {
            0
        }
    } else {
        eprintln!("Error: wrong number of arguments, expected a filename");
        2
    })
}

fn compile_file(filename: String) -> Result<()> {
    println!("{}", read_file(filename)?);
    // TODO: other stages
    Ok(())
}

/// This is stage 0 from the docs.
fn read_file(filename: String) -> Result<String> {
    let mut f = File::open(filename).expect("Could not open file");
    let mut contents = Vec::new();
    f.read_to_end(&mut contents).expect("Could not read file");

    // Check for non-ASCII characters.
    if contents.iter().any(|&c| c > 127) {
        return Err(Error::new("File contains non-ascii character", 0, 0))
    }

    // All non-ASCII characters have been filtered out, so this is a UTF-8 subset.
    Ok(unsafe { String::from_utf8_unchecked(contents) })
}
