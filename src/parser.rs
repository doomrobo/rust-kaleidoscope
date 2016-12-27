use lexer::Token;
use std::fmt;

pub type ASTForest = Vec<StmtOrExpr>;
type ParserRes<'a, T> = Result<(T, &'a [Token]), String>;
type Parser<'a, T> = Box<Fn(&'a [Token]) -> ParserRes<'a, T>>;

#[derive(Debug)]
pub enum StmtOrExpr {
    Stmt(Stmt),
    Expr(Expr),
}

#[derive(Debug)]
pub enum Stmt {
    Prototype(Prototype),
    Function(Function),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Ident(String),
    NumLit(f64),
    BinaryOp {
        a: Box<Expr>,
        op: String,
        b: Box<Expr>,
    },
    Call {
        func_name: String,
        args: Vec<Expr>
    },
}

#[derive(Debug)]
pub struct Function {
    proto: Prototype,
    body: Expr
}

#[derive(Debug)]
pub struct Prototype {
    name: String,
    args: Vec<String>,
}

impl fmt::Display for StmtOrExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &StmtOrExpr::Stmt(ref stmt) => write!(f, "{}", stmt),
            &StmtOrExpr::Expr(ref expr) => write!(f, "{}", expr),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Stmt::Prototype(ref proto) => write!(f, "extern {}", proto),
            &Stmt::Function(ref func) => {
                write!(f, "def {} {{\n", func.proto)?;
                write!(f, "    {}", func.body)
            }
        }
    }
}

impl fmt::Display for Prototype {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} (", self.name)?;
        let mut arg_it = self.args.iter();
        if let Some(first) = arg_it.next() {
            write!(f, "{}", first)?;
            for arg in arg_it {
                write!(f, ", {}", arg)?;
            }
        }
        f.write_str(")")
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Expr::Ident(ref s) => f.write_str(s),
            &Expr::NumLit(n) => write!(f, "{}", n),
            &Expr::BinaryOp { ref a, ref op, ref b } => {
                if let &Expr::BinaryOp {..} = a.as_ref() {
                    write!(f, "({})", a)?;
                }
                else {
                    write!(f, "{}", a)?;
                }

                write!(f, " {} ", op)?;

                if let &Expr::BinaryOp {..} = b.as_ref() {
                    write!(f, "({})", b)
                }
                else {
                    write!(f, "{}", b)
                }
            }
            &Expr::Call { ref func_name, ref args } => {
                write!(f, "{}(", func_name)?;
                let mut arg_it = args.iter();
                if let Some(first) = arg_it.next() {
                    write!(f, "{}", first)?;
                    for arg in arg_it {
                        write!(f, ", {}", arg)?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

macro_rules! pop {
    ( $input:ident ) => {
        let $input = &$input[1..];
    }
}

macro_rules! read_one {
    ( $tok:expr, $input:ident ) => {
        if peek($input) == &$tok {
            Some(())
        }
        else {
            None
        }
    }
}

macro_rules! eat {
    ( $tok:expr, $input:ident) => {
        read_one!($tok, $input).expect(&*format!("expected {:?}, got {:?}", $tok, $input[0]));
        pop!($input);
    }
}

fn peek(input: &[Token]) -> &Token {
    if input.len() == 0 {
        panic!("Unexpected end of file");
    }
    else {
        &input[0]
    }
}

fn parse_delimited_helper<'a, T>(item_parser: Parser<'a, T>, delimiter: Token, input: &'a [Token],
                                 mut acc: Vec<T>) -> ParserRes<'a, Vec<T>> {
    if peek(input) != &delimiter {
        return Ok((acc, input));
    }

    pop!(input);

    if let Ok((val, input)) = item_parser(input) {
        acc.push(val);
        parse_delimited_helper(item_parser, delimiter, input, acc)
    }
    else {
        Ok((acc, input))
    }
}

fn parse_delimited<'a, T>(item_parser: Parser<'a, T>, delimiter: Token, input: &'a [Token])
   -> ParserRes<'a, Vec<T>> {
    let mut coll = Vec::new();

    // Get the first value
    if let Ok((val, input)) = item_parser(input) {
        coll.push(val);
        let (coll, input) = parse_delimited_helper(item_parser, delimiter, input, coll)?;
        Ok((coll, input))
    }
    else {
        Ok((coll, input))
    }
}

fn parse_ident(input: &[Token]) -> ParserRes<String> {
    if let Token::Ident(ref s) = input[0] {
        Ok((s.to_string(), &input[1..]))
    }
    else {
        Err(format!("Expected identifier. Got {:?}.", input[0]))
    }
}

fn parse_proto(input: &[Token]) -> ParserRes<Prototype> {
    let (name, input) = parse_ident(input)?;
    eat!(Token::OpenParen, input);
    let (args, input) = parse_delimited(Box::new(parse_ident), Token::Comma, input)?;
    eat!(Token::CloseParen, input);
    let proto = Prototype {
        name: name,
        args: args,
    };
    Ok((proto, input))
}

fn parse_parenthesized_expr(input: &[Token]) -> ParserRes<Expr> {
    eat!(Token::OpenParen, input);
    let (expr, input) = parse_expr(input)?;
    eat!(Token::CloseParen, input);
    Ok((expr, input))
}

fn parse_primary_expr(input: &[Token]) -> ParserRes<Expr> {
    let tok = peek(input);
    match tok {
        &Token::OpenParen => parse_parenthesized_expr(input),
        &Token::Num(n) => {
            pop!(input);
            Ok((Expr::NumLit(n), input))
        },
        &Token::Ident(ref ident) => {
            pop!(input);
            // It's a function call
            if peek(input) == &Token::OpenParen {
                pop!(input);
                let (args, input) = parse_delimited(Box::new(parse_expr),
                                                    Token::Comma, input)?;
                eat!(Token::CloseParen, input);
                let call = Expr::Call {
                    func_name: ident.to_string(),
                    args: args,
                };
                Ok((call, input))
            }
            else {
                Ok((Expr::Ident(ident.to_string()), input))
            }
        }
        _ => Err(format!("expected an expression, got {:?}", tok))
    }
}

fn parse_expr(input: &[Token]) -> ParserRes<Expr> {
    let (expr_a, input) = parse_primary_expr(input)?;
    if input.len() == 0 {
        return Ok((expr_a, input));
    }

    match peek(input) {
        &Token::Op(ref op) => {
            pop!(input);
            let (expr_b, input) = parse_expr(input)?;
            let bin_op = Expr::BinaryOp {
                a: Box::new(expr_a),
                op: op.to_string(),
                b: Box::new(expr_b),
            };
            Ok((bin_op, input))
        }
        _ => Ok((expr_a, input))
    }
}

fn parse_def(input: &[Token]) -> ParserRes<Function> {
    eat!(Token::Def, input);
    let (proto, input) = parse_proto(input)?;
    let (body, input) = parse_expr(input)?;
    let func = Function {
        proto: proto,
        body: body,
    };

    Ok((func, input))
}

fn parse_extern(input: &[Token]) -> ParserRes<Prototype> {
    eat!(Token::Extern, input);
    parse_proto(input)
}

fn parse_helper(input: &[Token], mut prog: ASTForest) -> ParserRes<ASTForest> {
    if input.len() == 0 {
        return Ok((prog, input));
    }
    match peek(input) {
        &Token::Def => {
            let (func, input) = parse_def(input)?;
            prog.push(StmtOrExpr::Stmt(Stmt::Function(func)));
            parse_helper(input, prog)
        }
        &Token::Extern => {
            let (proto, input) = parse_extern(input)?;
            prog.push(StmtOrExpr::Stmt(Stmt::Prototype(proto)));
            parse_helper(input, prog)
        }
        _ => {
            let (expr, input) = parse_expr(input)?;
            prog.push(StmtOrExpr::Expr(expr));
            eat!(Token::Delim, input);
            parse_helper(input, prog)
        }
    }
}

pub fn parse(input: &[Token]) -> ParserRes<ASTForest> {
    let prog = Vec::new();
    parse_helper(input, prog)
}

#[test]
fn test_parse_proto() {
    parse_proto(&[Token::Ident("hello".to_string()), Token::OpenParen,
                  Token::Ident("a".to_string()), Token::Comma, Token::Ident("b".to_string()),
                  Token::Comma, Token::Ident("c".to_string()), Token::Comma,
                  Token::CloseParen]).unwrap();
}

#[test]
fn test_parse_expr() {
    // This is the string "1 + hello + (3.4 / 5) * 2 ** f(50, 1.1, g(2.2)) + 4"
    let expr = {
        use lexer::Token::*;
        &[Num(1.), Op("+".to_string()), Ident("hello".to_string()), Op("+".to_string()), OpenParen,
          Num(3.4), Op("/".to_string()), Num(5.), CloseParen, Op("*".to_string()), Num(2.),
          Op("**".to_string()), Ident("f".to_string()), OpenParen, Num(50.), Comma, Num(1.1),
          Comma, Ident("g".to_string()), OpenParen, Num(2.2), Op("-".to_string()), Num(1.1),
          CloseParen, CloseParen, Op("+".to_string()), Num(4.)]
    };
    let expected = {
        use self::Expr::*;

        BinaryOp {
          a: Box::new(NumLit(1.)),
          op: "+".to_string(),
          b: Box::new(BinaryOp {
            a: Box::new(Ident("hello".to_string())),
            op: "+".to_string(),
            b: Box::new(BinaryOp {
              a: Box::new(BinaryOp {
                a: Box::new(NumLit(3.4)),
                op: "/".to_string(),
                b: Box::new(NumLit(5.))
              }),
              op: "*".to_string(),
              b: Box::new(BinaryOp {
                a: Box::new(NumLit(2.)),
                op: "**".to_string(),
                b: Box::new(BinaryOp {
                  a: Box::new(Call {
                    func_name: "f".to_string(),
                    args: vec![
                      NumLit(50.),
                      NumLit(1.1),
                      Call {
                        func_name: "g".to_string(),
                        args: vec![
                          BinaryOp {
                            a: Box::new(NumLit(2.2)),
                            op: "-".to_string(),
                            b: Box::new(NumLit(1.1))
                          }
                        ]
                      }
                    ]
                  }),
                  op: "+".to_string(),
                  b: Box::new(NumLit(4.))
                })
              })
            })
          })
        }
    };

    let (parsed, _) = parse_expr(expr).unwrap();
    assert_eq!(parsed, expected);
}
