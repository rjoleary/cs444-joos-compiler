use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Read;
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
    productions: Vec<Production>,
}


type ParseState = usize;

// One token of lookahead
#[derive(Hash, Eq, PartialEq, Debug)]
struct ParseTransition(ParseState, TerminalOrNonTerminal);

enum ParseAction {
    // Error states exist implicitly where the HashMap does not contain an action.
    Accept,
    Goto(ParseState),
    Shift(ParseState),
    Reduce(ParseState),
}

// Loaded from test/appel_oracle.txt
// The action and goto tables are combined into the parse table.
type ParseTable = HashMap<ParseTransition, ParseAction>;


type Tokens = Vec<Terminal>;


fn main() {
    let grammar = read_grammar("test/appel_terminals.txt",
                               "test/appel_nonterminals.txt",
                               "test/appel_grammar.txt").unwrap();
    let table = read_parse_table("test/appel_oracle.txt").unwrap();
    let mut tokens = read_tokens("test/appel_tokens.txt").unwrap();
    tokens.push("$".to_string()); // augment the grammar
    parse(&grammar, &table, &tokens)
}

// Parser as defined in Appel. This supports goto transitions and error states.
fn parse(grammar: &Grammar, table: &ParseTable, tokens: &Tokens) {
    // parser state
    let mut stack = Vec::new();
    let mut tokens = &tokens[..];

    // never reduced, but simplifies the parser
    stack.push(ParseTransition(1, "".to_string()));

    loop {
        let &ParseTransition(state, _) = stack.last().unwrap();
        //println!("Stack: {:?}", stack);
        //println!("Input: {:?}", tokens);
        match table.get(&ParseTransition(state, tokens[0].clone())) {
            Some(&ParseAction::Shift(state)) => {
                //println!("Action: shift {}", state);
                stack.push(ParseTransition(state, tokens[0].clone()));
                tokens = &tokens[1..];
            },
            Some(&ParseAction::Reduce(production_idx)) => {
                //println!("Action: reduce {}", state);
                let production = &grammar.productions[production_idx-1];
                println!("{:?}", production);
                for _ in 1..production.len() {
                    stack.pop();
                }
                let nonterminal = &production[0];
                let &ParseTransition(state, _) = stack.last().unwrap();
                if let Some(&ParseAction::Goto(state)) =
                        table.get(&ParseTransition(state, nonterminal.to_string())) {
                    stack.push(ParseTransition(state, nonterminal.to_string()));
                } else {
                    eprintln!("Error: could not find goto action");
                    return;
                }
            },
            Some(&ParseAction::Accept) => {
                //println!("Action: accept {}", state);
                return;
            },
            Some(&ParseAction::Goto(_)) => {
                eprintln!("Error: unexpected goto action");
                return;
            },
            None => {
                eprintln!("Error: parse error");
                return;
            }
        }
        //println!();
    }
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
        productions: read_file(grammar_filename)?.lines()
            .map(|line| line.split_whitespace().map(|s| s.to_string()).collect()).collect(),
    })
}

fn read_parse_table(filename: &str) -> Result<ParseTable> {
    let mut table = ParseTable::new();

    for line in read_file(filename)?.lines() {
        let line = line.trim();
        if line.len() == 0 || line.starts_with("#") {
            continue;
        }

        let tokens: Vec<_> = line.split_whitespace().collect();
        if tokens.len() < 2 {
            return Err(Error::new("Could not load parse table", 0, 0));
        }
        let transition = ParseTransition(
            tokens[0].parse().unwrap(),
            tokens[1].to_string(),
        );

        if tokens.len() == 3 && tokens[2] == "a" {
            table.insert(transition, ParseAction::Accept);
        } else if tokens.len() == 4 && tokens[2] == "g" {
            table.insert(transition, ParseAction::Goto(tokens[3].parse().unwrap()));
        } else if tokens.len() == 4 && tokens[2] == "s" {
            table.insert(transition, ParseAction::Shift(tokens[3].parse().unwrap()));
        } else if tokens.len() == 4 && tokens[2] == "r" {
            table.insert(transition, ParseAction::Reduce(tokens[3].parse().unwrap()));
        } else {
            return Err(Error::new("Could not load parse table", 0, 0))
        }
    }

    Ok(table)
}

fn read_tokens(filename: &str) -> Result<Tokens> {
    Ok(read_file(filename)?.lines().map(|line| {
        line.split_whitespace().nth(0).unwrap().to_string()
    }).collect())
}
