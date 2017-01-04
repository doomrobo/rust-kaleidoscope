extern crate docopt;
extern crate rust_kaleidoscope;

use rust_kaleidoscope::parser::parse;
use rust_kaleidoscope::lexer::tokenize;
use rust_kaleidoscope::codegen::{self, IRBuilder};

use std::cmp;
use std::io::{self, Write};

use docopt::Docopt;

const USAGE: &'static str = "
Usage: rust_kaleidoscope ([-lpc] | -h)

Options:
    -l         Run only the lexer and show the output
    -p         Run the lexer and the parser and show the output
    -c         Run codegen in addition to everything else
    -h --help  Show this usage message
";

fn slice_at_most<T>(n: usize, slice: &[T]) -> &[T] {
    &slice[..cmp::min(n, slice.len())]
}

fn main() {
    let doc = Docopt::new(USAGE).unwrap();
    let args = doc.help(true).parse().unwrap_or_else(|e| e.exit());

    let wants_lex = args.get_bool("-l");
    let wants_parse = args.get_bool("-p");
    let wants_codegen = args.get_bool("-c");

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut line = String::new();

    let (mut llvm_mod, mut llvm_ctx) = codegen::new_module_and_ctx("testmod");

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
        if wants_lex {
            println!("======= LEXER =======\n");
            println!("{:?}", toks);
        }

        if !wants_parse && !wants_codegen {
            continue;
        }

        // Just print the final parse forest (or a parsing error)
        let parsed = match parse(&*toks) {
            Ok((ast, _)) => ast,
            Err((err_str, unparsed_toks)) => {
                println!("\nParse error: {}\nContext: {:?}",
                         err_str,
                         slice_at_most(10, unparsed_toks));
                continue;
            }
        };

        if wants_parse {
            println!("\n======= PARSER =======\n");
            println!("{:#?}", parsed);
        }

        let codegen_res = parsed.codegen(&mut llvm_ctx, &mut llvm_mod);
        if wants_codegen {
            println!("\n======= CODEGEN =======\n");
            match codegen_res {
                Ok(_) => println!("{:?}", llvm_mod),
                Err(err_str) => println!("Codgen error: {}", err_str),
            }
        }
    }
}
