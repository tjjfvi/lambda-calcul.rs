mod expr;
mod lexer;
mod parser;

use crate::parser::parse;
use std::time::Instant;

static STDLIB: &'static str = include_str!("./stdlib");

fn main() {
  let input = String::from(STDLIB) + "num.factorial(6)";
  let mut expr = parse(input).expect("");
  println!("{}", expr);
  let start = Instant::now();
  expr.reduce();
  let duration = start.elapsed();
  println!("{}", expr);
  println!("{:?}", duration)
}
