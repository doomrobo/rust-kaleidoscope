use std::f64;
use std::str::FromStr;
use regex::Regex;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Def,
    Extern,
    Delim,
    OpenParen,
    CloseParen,
    Comma,
    Ident(String),
    Num(f64),
    Op(String),
}

use self::Token::*;

pub fn tokenize(input: &str) -> Vec<Token> {
    let comment_re_str = r"(?m)^\s*#.*$";
    let lang_re_str = concat!(r"\s*",
                              r"(?P<ident>\p{Alphabetic}\w*)|",
                              r"(?P<delim>;)|",
                              r"(?P<open_paren>\()|",
                              r"(?P<close_paren>\))|",
                              r"(?P<comma>,)|",
                              r"(?P<num>\d+\.?\d*)|",
                              r"(?P<op>[\p{P}\p{Sm}]+)|", // Punctuation and math symbols
                              r"\s*");
    let comment_re = Regex::new(comment_re_str).unwrap();
    let lang_re = Regex::new(lang_re_str).unwrap();

    let mut tokens = Vec::new();
    for non_comment in comment_re.split(input) {
        for cap in lang_re.captures_iter(non_comment) {
            let tok = if let Some(ident) = cap.name("ident") {
                match ident {
                    "extern" => Extern,
                    "def" => Def,
                    s => Ident(s.to_string()),
                }
            }
            else if let Some(s) = cap.name("num") {
                Num(f64::from_str(s).unwrap())
            }
            else if let Some(op) = cap.name("op") {
                Op(op.to_string())
            }
            else if cap.name("delim").is_some() { Delim }
            else if cap.name("open_paren").is_some() { OpenParen }
            else if cap.name("close_paren").is_some() { CloseParen }
            else if cap.name("comma").is_some() { Comma }
            else { continue; };

            tokens.push(tok);
        }
    }

    tokens
}

#[test]
fn test_tokenize() {
    let test_str = r"
#hello
    #  world
def hello(int a, int b)
    extern c = a + 3.1415 + b;
    print(c);";

    let expected = vec![Def,
                        Ident("hello".to_string()),
                        OpenParen,
                        Ident("int".to_string()),
                        Ident("a".to_string()),
                        Comma,
                        Ident("int".to_string()),
                        Ident("b".to_string()),
                        CloseParen,
                        Extern,
                        Ident("c".to_string()),
                        Op("=".to_string()),
                        Ident("a".to_string()),
                        Op("+".to_string()),
                        Num(3.1415),
                        Op("+".to_string()),
                        Ident("b".to_string()),
                        Delim,
                        Ident("print".to_string()),
                        OpenParen,
                        Ident("c".to_string()),
                        CloseParen,
                        Delim];
    assert_eq!(tokenize(test_str), expected);
}
