use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Read;
use std::iter::FromIterator;
use std::vec::Vec;

mod error;
use error::{Error, Result};

type Terminal = String;
type NonTerminal = String;
type TerminalOrNonTerminal = String;
type Production = Vec<String>;

struct Grammar {
    // Hard coded to S
    start: NonTerminal,
    // Loaded from test/appel_terminals.txt
    terminals: HashSet<Terminal>,
    // Loaded from test/appel_nonterminals.txt
    non_terminals: HashSet<NonTerminal>,
    // Loaded from test/appel_grammar.txt
    productions: HashMap<NonTerminal, Vec<TerminalOrNonTerminal>>,
}


type OracleState = usize;

enum OracleTransition {
    Error,
    Accept,
    Goto(OracleState),
    Shift(OracleState),
    Reduce(OracleState),
}

// Loaded from test/appel_oracle.txt
struct Oracle {
    transitions: Vec<OracleTransition>
}


type Tokens = Vec<Terminal>;


fn main() {
    let grammar = read_grammar("test/appel_terminals.txt",
                               "test/appel_nonterminals.txt",
                               "test/appel_grammar.txt").unwrap();
    //let oracle = read_oracle("test/appel_oracle.txt").unwrap();
    let tokens = read_tokens("test/appel_tokens.txt").unwrap();
}

fn read_file(filename: &str) -> Result<String> {
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

fn read_grammar(terminals_filename: &str,
                nonterminals_filename: &str,
                grammar_filename: &str) -> Result<Grammar> {
    Ok(Grammar {
        start: "S".to_string(),
        terminals: read_file(terminals_filename)?.lines()
            .map(|s| s.to_string()).collect(),
        non_terminals: read_file(nonterminals_filename)?.lines()
            .map(|s| s.to_string()).collect(),
        productions: HashMap::from_iter(read_file(grammar_filename)?.lines().map(|line| {
            let tokens: Vec<_> = line.split_whitespace().collect();
            (tokens[0].to_string(), tokens[1..].iter().map(|s| s.to_string()).collect())
        })),
    })
}

//fn read_oracle(filename: &str) -> Result<Oracle> {
    //fail!("not implemented"); // TODO
//}

fn read_tokens(filename: &str) -> Result<Tokens> {
    Ok(read_file(filename)?.lines().map(|line| {
        line.split_whitespace().nth(0).unwrap().to_string()
    }).collect())
}
