extern crate docopt;
extern crate rust_kaleidoscope;

use rust_kaleidoscope::parser::parse;
use rust_kaleidoscope::lexer::tokenize;

use std::cmp;
use std::io::{self, Write};

use docopt::Docopt;

const USAGE: &'static str = "
Usage: rust_kaleidoscope (-l | -p | -h)

Options:
    -l         Run only the lexer and show the output
    -p         Run the lexer and the parser and show the output
    -h --help  Show this usage message
";

fn slice_at_most<T>(n: usize, slice: &[T]) -> &[T] {
    &slice[..cmp::min(n, slice.len())]
}

fn main() {
    let args = Docopt::new(USAGE).unwrap().help(true).parse().unwrap_or_else(|e| e.exit());
    let wants_parse = args.get_bool("-p");

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut line = String::new();
    loop {
        line.clear();
        print!("> ");
        stdout.flush().unwrap();

        // If there's an error or Ctrl-D, quit
        match stdin.read_line(&mut line) {
            Ok(0) | Err(_) => {
                println!("\nQuitting...");
                break;
            }
            _ => (),
        }

        let toks = tokenize(&*line);
        // If we're parsing, just print the final parse forest (or a parsing error)
        if wants_parse {
            let parsed = match parse(&*toks) {
                Ok((ast, _)) => ast,
                Err((err_str, unparsed_toks)) => {
                    println!("Error: {}\nContext: {:?}",
                             err_str,
                             slice_at_most(10, unparsed_toks));
                    continue;
                }
            };

            println!("{:#?}", parsed);
        }
        // Just print the tokens
        else {
            println!("{:?}", toks);
        }
    }
}
