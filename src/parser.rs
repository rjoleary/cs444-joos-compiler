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
    nonterminals: HashSet<NonTerminal>,
    // Loaded from test/appel_grammar.txt
    productions: Vec<Production>,
}


type ParseState = usize;
type NonTerminalIdx = usize;

// One token of lookahead
#[derive(Hash, Eq, PartialEq, Debug)]
struct ParseTransition(ParseState, TerminalOrNonTerminal);

enum ParseAction {
    // An Error action exists implicitly where the HashMap does not contain an action. Shift action
    // on a non-terminal is equivalent to a Goto action in the Appel textbook.
    Accept,
    Shift(ParseState),
    Reduce(NonTerminalIdx),
}

// Loaded from test/appel_oracle.txt
// The action and goto tables are combined into the parse table.
type ParseTable = HashMap<ParseTransition, ParseAction>;


type Tokens = Vec<Terminal>;


fn main() {
    let (grammar, table) = read_grammar("def/appel.lr1").unwrap();
    let mut tokens = read_tokens("test/appel_tokens.txt").unwrap();
    let mut augmented = vec!["BOF".to_string()];
    augmented.append(&mut tokens);
    augmented.push("EOF".to_string());
    parse(&grammar, &table, &augmented)
}

// Parser as defined in Appel. This supports goto transitions and error states.
fn parse(grammar: &Grammar, table: &ParseTable, tokens: &Tokens) {
    // parser state
    let mut stack = Vec::new();
    let mut tokens = &tokens[..];

    // never reduced, but simplifies the parser
    stack.push(ParseTransition(0, "".to_string()));

    loop {
        let &ParseTransition(state, _) = stack.last().unwrap();
        //println!("Stack: {:?}", stack);
        //println!("Input: {:?}", tokens);
        match table.get(&ParseTransition(state, tokens[0].clone())) {
            Some(&ParseAction::Shift(state)) => {
                //println!("{}", state);
                stack.push(ParseTransition(state, tokens[0].clone()));
                tokens = &tokens[1..];
            },
            Some(&ParseAction::Reduce(production_idx)) => {
                //println!("{}", state);
                let production = &grammar.productions[production_idx];
                println!("{}", production.join(" "));
                for _ in 1..production.len() {
                    stack.pop();
                }
                let nonterminal = &production[0];
                let &ParseTransition(state, _) = stack.last().unwrap();
                if let Some(&ParseAction::Shift(state)) =
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

fn read_grammar(filename: &str) -> Result<(Grammar, ParseTable)> {
    let mut grammar = Grammar{
        start: "S".to_string(),
        terminals: HashSet::new(),
        nonterminals: HashSet::new(),
        productions: Vec::new(),
    };
    let mut table = ParseTable::new();
    let contents = read_file(filename)?;
    let mut lines = contents.lines();

    // Parse terminals.
    let num_terminals: usize = lines.next().unwrap().parse().unwrap();
    grammar.terminals = HashSet::new();
    for _ in 0..num_terminals {
        grammar.terminals.insert(lines.next().unwrap().to_string());
    }

    // Parse non-terminals.
    let num_nonterminals: usize = lines.next().unwrap().parse().unwrap();
    grammar.nonterminals = HashSet::new();
    for _ in 0..num_nonterminals {
        grammar.nonterminals.insert(lines.next().unwrap().to_string());
    }

    // Parse start symbol.
    grammar.start = lines.next().unwrap().to_string();

    // Parse productions.
    let num_productions: usize = lines.next().unwrap().parse().unwrap();
    grammar.productions.reserve_exact(num_productions);
    for _ in 0..num_productions {
        grammar.productions.push(lines.next().unwrap().split_whitespace().map(|s| s.to_string()).collect());
    }

    // Parse states. (ignored)
    lines.next();

    // Parse transitions.
    let num_transitions: usize = lines.next().unwrap().parse().unwrap();
    for _ in 0..num_transitions {
        let line = lines.next().unwrap();

        let tokens: Vec<_> = line.split_whitespace().collect();
        if tokens.len() != 4 {
            return Err(Error::new("Could not load parse table", 0, 0));
        }
        let transition = ParseTransition(
            tokens[0].parse().unwrap(),
            tokens[1].to_string(),
        );

        table.insert(transition, match tokens[2] {
            "reduce" => ParseAction::Reduce(tokens[3].parse().unwrap()),
            "shift" => {
                if tokens[1] == "EOF" {
                    ParseAction::Accept
                } else {
                    ParseAction::Shift(tokens[3].parse().unwrap())
                }
            },
            _ => {
                return Err(Error::new("Could not load parse table", 0, 0))
            }
        });
    }

    Ok((grammar, table))
}

fn read_tokens(filename: &str) -> Result<Tokens> {
    Ok(read_file(filename)?.lines().map(|line| {
        line.split_whitespace().nth(0).unwrap().to_string()
    }).collect())
}
