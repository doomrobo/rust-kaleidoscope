#[macro_use]
extern crate lazy_static;
extern crate llvm;
extern crate llvm_sys;
extern crate regex;

pub mod lexer;
pub mod parser;
pub mod codegen;
