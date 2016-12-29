use lexer::Token;

use std::fmt;
use std::collections::HashMap;

lazy_static! {
    // Operator precedence table. The higher the number the tighter it binds
    static ref OP_PRECEDENCE: HashMap<String, usize> = {
        let mut m = HashMap::new();
        m.insert("<".to_string(), 10);
        m.insert("+".to_string(), 20);
        m.insert("-".to_string(), 20);
        m.insert("*".to_string(), 40);
        m.insert("/".to_string(), 40);
        m.insert("**".to_string(), 50);
        m
    };
}

pub type ASTForest = Vec<StmtOrExpr>;
// Returns either (AST node, rest of input) or (error str, rest of input)
type ParserRes<'a, T> = Result<(T, &'a [Token]), (String, &'a [Token])>;
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

// Wrapper type around Option<&Token> so that we can impl Display for it.
struct TokOptWrapper<'a>(Option<&'a Token>);
impl<'a> From<Option<&'a Token>> for TokOptWrapper<'a> {
    fn from(t: Option<&'a Token>) -> TokOptWrapper<'a> {
        TokOptWrapper(t)
    }
}

// Will display the Debug form of the token when it is not None. Otherwise it will display "EOF"
impl<'a> fmt::Display for TokOptWrapper<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Some(ref tok) => write!(f, "{:?}", tok),
            None => f.write_str("EOF"),
        }
    }
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
            // Binary operations will have parentheses around the operands iff the operands are
            // binary operations themselves
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

// Ratchets input forward by one token
macro_rules! pop {
    ( $input:ident ) => {
        let $input = &$input[1..];
    }
}

// pop the input and expect the given token. If it's not what was expected, panic
macro_rules! eat {
    ( $tok:expr, $input:ident) => {
        if !read_one(&$tok, $input) {
            return Err((format!("Expected {:?}. Got {}.",
                                $tok,
                                TokOptWrapper::from($input.get(0))),
                        $input));
        }
        pop!($input);
    }
}

// Returns true if the expected token was read. false otherwise.
fn read_one(tok: &Token, input: &[Token]) -> bool {
    match peek_opt(input) {
        Some(next) => next == tok,
        None => false
    }
}

fn peek_opt(input: &[Token]) -> Option<&Token> {
    if input.len() == 0 {
        None
    }
    else {
        Some(&input[0])
    }
}

fn peek(input: &[Token]) -> &Token {
    peek_opt(input).expect("Unexpected end of file")
}

// Parses a sequence of the form [<delim> <item>]* <delim> ?
fn parse_delimited_helper<'a, T>(item_parser: Parser<'a, T>, delimiter: Token, input: &'a [Token],
                                 mut acc: Vec<T>) -> ParserRes<'a, Vec<T>> {
    // If there's no delimiter, we're done
    if peek_opt(input) != Some(&delimiter) {
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

// Parses 0 or more nodes using item_parser, delimited by the given delimiter
fn parse_delimited<'a, T>(item_parser: Parser<'a, T>, delimiter: Token, input: &'a [Token])
   -> ParserRes<'a, Vec<T>> {
    let mut coll = Vec::new();

    // Get the first value. If there is none, return an empty vec
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
    let tok = peek_opt(input);
    if let Some(&Token::Ident(ref s)) = tok {
        pop!(input);
        Ok((s.to_string(), input))
    }
    else {
        Err((format!("Expected identifier. Got {}.", TokOptWrapper::from(tok)), input))
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
    match peek_opt(input) {
        Some(&Token::OpenParen) => parse_parenthesized_expr(input),
        Some(&Token::Num(n)) => {
            pop!(input);
            Ok((Expr::NumLit(n), input))
        },
        Some(&Token::Ident(ref ident)) => {
            pop!(input);
            // It's a function call
            if read_one(&Token::OpenParen, input) {
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
        tok => Err((format!("Expected expression. Got {}.", TokOptWrapper::from(tok)), input))
    }
}

fn apply_precedence_rules(mut operands: Vec<Expr>, mut operators: Vec<String>)
   -> Result<Expr, String> {
    // Should only happen when there is exactly one element in operands
    if operators.is_empty() {
        return Ok(operands.pop().expect("Unexpected error in binary op parsing"));
    }

    let mut winning_op_idx = 0usize;
    let mut max_prec = 0usize;
    for (i, op) in operators.iter().enumerate() {
        // We can unwrap here because the condition is checked in parse_expr_helper
        let p = *OP_PRECEDENCE.get(op).unwrap();
        if p > max_prec {
            max_prec = p;
            winning_op_idx = i;
        }
    }

    // Group together the tighest-binding operands, and put them back in the vector as a BinOp
    let op = operators.remove(winning_op_idx);
    let expr_a = operands.remove(winning_op_idx);
    let expr_b = operands.remove(winning_op_idx);
    let expr = Expr::BinaryOp {
        a: Box::new(expr_a),
        op: op,
        b: Box::new(expr_b),
    };

    operands.insert(winning_op_idx, expr);
    apply_precedence_rules(operands, operators)
}

// Breaks up expressions into binops and primary expressions. Then applies precedence rules
fn parse_expr_helper(input: &[Token], mut operands: Vec<Expr>, mut operators: Vec<String>)
   -> ParserRes<Expr> {
    match peek_opt(input) {
        // Read an operator and a primary expression
        Some(&Token::Op(ref op)) => {
            if OP_PRECEDENCE.get(op).is_none() {
                return Err((format!("Unknown operator '{}'", op), input));
            }

            pop!(input);
            let (expr, input) = parse_primary_expr(input)?;
            operands.push(expr);
            operators.push(op.to_string());
            parse_expr_helper(input, operands, operators)
        }
        // Otherwise we're done
        _ => match apply_precedence_rules(operands, operators) {
                Ok(expr) => Ok((expr, input)),
                Err(s) => Err((s, input))
        }
    }
}

// Exprs are of the form <primary_expr> [<op> <primary_expr>]*
// So parse the first one, and then collect the rest, then apply to the global operator precedence
// rules to turn it into a single BinOp expression
fn parse_expr(input: &[Token]) -> ParserRes<Expr> {
    let (first_primary, input) = parse_primary_expr(input)?;
    let operands = vec![first_primary];
    let operators = Vec::new();
    parse_expr_helper(input, operands, operators)
}

fn parse_def(input: &[Token]) -> ParserRes<Function> {
    eat!(Token::Def, input);
    let (proto, input) = parse_proto(input)?;
    // Function bodies are a single expr, I suppose
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

// Top-level parser
fn parse_helper(input: &[Token], mut prog: ASTForest) -> ParserRes<ASTForest> {
    // Top-level nodes are either statements (def or extern) or expressions
    match peek_opt(input) {
        Some(&Token::Def) => {
            let (func, input) = parse_def(input)?;
            prog.push(StmtOrExpr::Stmt(Stmt::Function(func)));
            parse_helper(input, prog)
        }
        Some(&Token::Extern) => {
            let (proto, input) = parse_extern(input)?;
            prog.push(StmtOrExpr::Stmt(Stmt::Prototype(proto)));
            parse_helper(input, prog)
        }
        // If it's not a def or an extern prototype, assume it's an expression
        Some(_) => {
            let (expr, input) = parse_expr(input)?;
            prog.push(StmtOrExpr::Expr(expr));
            eat!(Token::Delim, input);
            parse_helper(input, prog)
        }
        // EOF, return what we have
        None => {
            Ok((prog, input))
        }
    }
}

// Makes an empty vector for the top-level forest and passes it to parse_helper
pub fn parse(input: &[Token]) -> ParserRes<ASTForest> {
    let prog = Vec::new();
    parse_helper(input, prog)
}

#[test]
fn test_parse_proto() {
    let proto = &[Token::Ident("hello".to_string()), Token::OpenParen,
                  Token::Ident("a".to_string()), Token::Comma, Token::Ident("b".to_string()),
                  Token::Comma, Token::Ident("c".to_string()), Token::Comma, Token::CloseParen];
    let expected_pretty = "hello (a, b, c)";
    let (parsed, _) = parse_proto(proto).unwrap();

    println!("parsed == {:#?}", parsed);
    assert_eq!(&*format!("{}", parsed), expected_pretty);
}

#[test]
fn test_parse_expr() {
    // This is the string "1 + hello + (3.4 / 5) * 2 ** f(50, 1.1, g(2.2 - 1.1)) + 4"
    let expr = {
        use lexer::Token::*;
        &[Num(1.), Op("+".to_string()), Ident("hello".to_string()), Op("+".to_string()), OpenParen,
          Num(3.4), Op("/".to_string()), Num(5.), CloseParen, Op("*".to_string()), Num(2.),
          Op("**".to_string()), Ident("f".to_string()), OpenParen, Num(50.), Comma, Num(1.1),
          Comma, Ident("g".to_string()), OpenParen, Num(2.2), Op("-".to_string()), Num(1.1),
          CloseParen, CloseParen, Op("+".to_string()), Num(4.)]
    };
    let expected_pretty = "((1 + hello) + ((3.4 / 5) * (2 ** f(50, 1.1, g(2.2 - 1.1))))) + 4";

    let (parsed, _) = parse_expr(expr).unwrap();
    assert_eq!(&*format!("{}", parsed), expected_pretty);
}
