use rand::random;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::{Rc, Weak};
use std::time::Instant;

static STDLIB: &'static str = include_str!("./stdlib");

#[derive(Debug)]
pub struct WrappedExpr(Rc<RefCell<Expr>>);

impl WrappedExpr {
  pub fn wrap_data(data: ExprData) -> WrappedExpr {
    WrappedExpr(Rc::new(RefCell::new(Expr {
      data,
      cloned_value: None,
    })))
  }
  pub fn wrap(expr: Expr) -> WrappedExpr {
    WrappedExpr(Rc::new(RefCell::new(expr)))
  }
}

#[derive(Debug)]
pub struct Expr {
  data: ExprData,
  cloned_value: Option<(u32, Weak<RefCell<Expr>>)>,
}

#[derive(Debug)]
pub enum ExprData {
  Placeholder(u32),
  Wrapper(WrappedExpr),
  Lambda(WrappedExpr, WrappedExpr),
  Call(WrappedExpr, WrappedExpr),
}

impl WrappedExpr {
  pub fn reduce(&mut self) {
    self._reduce(0)
  }
  fn _reduce(&mut self, num: u32) {
    let mut current = WrappedExpr(Rc::clone(&self.0));
    let mut calls: Vec<WrappedExpr> = vec![];
    loop {
      current = {
        let mut current_expr = current.0.borrow_mut();
        match &mut current_expr.data {
          ExprData::Call(fun, arg) => {
            calls.push(WrappedExpr(Rc::clone(&arg.0)));
            WrappedExpr(Rc::clone(&fun.0))
          }
          ExprData::Placeholder(_) => break,
          ExprData::Wrapper(val) => WrappedExpr(Rc::clone(&val.0)),
          ExprData::Lambda(param, body) => match calls.pop() {
            Some(arg) => {
              if Rc::strong_count(&current.0) > 1 {
                let id = random::<u32>();
                {
                  let mut param_expr = param.0.borrow_mut();
                  param_expr.cloned_value = Some((id, Rc::downgrade(&arg.0)));
                }
                body
                  ._clone(id, false)
                  .unwrap_or(WrappedExpr(Rc::clone(&body.0)))
              } else {
                *param.0.borrow_mut() = Expr {
                  data: ExprData::Wrapper(WrappedExpr(Rc::clone(&arg.0))),
                  cloned_value: None,
                };
                WrappedExpr(Rc::clone(&body.0))
              }
            }
            None => {
              body._reduce(num + 1);
              break;
            }
          },
        }
      }
    }
    loop {
      match calls.pop() {
        Some(mut arg) => {
          arg.reduce();
          current = WrappedExpr::wrap_data(ExprData::Call(current, arg))
        }
        None => break,
      }
    }
    self.0 = current.0
  }
  pub fn _clone(&self, id: u32, clone_placeholder: bool) -> Option<WrappedExpr> {
    {
      let expr = self.0.borrow();
      if let ExprData::Wrapper(val) = &expr.data {
        return val._clone(id, clone_placeholder);
      }
      if let Some((id2, val)) = &expr.cloned_value {
        if id == *id2 {
          if let Some(val) = val.upgrade() {
            return Some(WrappedExpr(Rc::clone(&val)));
          }
        }
      }
      if matches!(&expr.data, ExprData::Placeholder(_)) && !clone_placeholder {
        return None;
      }
    }
    let mut expr = self.0.borrow_mut();
    let cloned_expr = Expr {
      data: match &expr.data {
        ExprData::Wrapper(_) => panic!("Unreachable"),
        ExprData::Placeholder(x) => Some(ExprData::Placeholder(*x)),
        ExprData::Lambda(a, b) => match (a._clone(id, true), b._clone(id, false)) {
          (_, None) => None,
          (None, _) => panic!("Unreachable"),
          (Some(a), Some(b)) => Some(ExprData::Lambda(a, b)),
        },
        ExprData::Call(a, b) => match (a._clone(id, false), b._clone(id, false)) {
          (None, None) => None,
          (a2, b2) => Some(ExprData::Call(
            a2.unwrap_or(WrappedExpr(Rc::clone(&a.0))),
            b2.unwrap_or(WrappedExpr(Rc::clone(&b.0))),
          )),
        },
      }?,
      cloned_value: None,
    };
    let clone = WrappedExpr(Rc::new(RefCell::new(cloned_expr)));
    expr.cloned_value = Some((id, Rc::downgrade(&clone.0)));
    Some(clone)
  }
  fn _fmt(&self, f: &mut fmt::Formatter<'_>, num: u32, parens: bool) -> fmt::Result {
    let expr = self.0.borrow();
    match &expr.data {
      ExprData::Placeholder(id) => write!(f, "{}", id),
      ExprData::Wrapper(val) => val._fmt(f, num, parens),
      ExprData::Call(fun, arg) => {
        fun._fmt(f, num, true)?;
        write!(f, "(")?;
        arg._fmt(f, num, false)?;
        write!(f, ")")?;
        Ok(())
      }
      ExprData::Lambda(arg, body) => {
        {
          let mut arg_expr = arg.0.borrow_mut();
          if !matches!(arg_expr.data, ExprData::Placeholder(_)) {
            panic!("Lambda expression has non-placeholder parameter")
          }
          arg_expr.data = ExprData::Placeholder(num);
          if parens {
            write!(f, "(")?;
          }
          write!(f, "{} => ", num)?;
        }
        body._fmt(f, num + 1, false)?;
        if parens {
          write!(f, ")")?;
        }
        Ok(())
      }
    }
  }
  fn _compute_format<'a>(&'a self, num: u32) -> NumberedWrappedExpr<'a> {
    NumberedWrappedExpr(num, self)
  }
  fn _hash<H: Hasher>(&self, hasher: &mut H, num: u32) {
    let expr = self.0.borrow();
    match &expr.data {
      ExprData::Placeholder(id) => {
        (0).hash(hasher);
        (id).hash(hasher);
      }
      ExprData::Wrapper(val) => val._hash(hasher, num),
      ExprData::Call(fun, arg) => {
        (1).hash(hasher);
        fun._hash(hasher, num);
        arg._hash(hasher, num);
      }
      ExprData::Lambda(arg, body) => {
        (2).hash(hasher);
        {
          let mut arg_expr = arg.0.borrow_mut();
          if !matches!(arg_expr.data, ExprData::Placeholder(_)) {
            panic!("Lambda expression has non-placeholder parameter")
          }
          arg_expr.data = ExprData::Placeholder(num);
          num.hash(hasher)
        }
        body._hash(hasher, num + 1);
      }
    }
  }
}

impl Hash for WrappedExpr {
  fn hash<H: Hasher>(&self, hasher: &mut H) {
    self._hash(hasher, 0)
  }
}

impl fmt::Display for WrappedExpr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self._fmt(f, 0, false)
  }
}

pub enum Token {
  OpenParen,
  CloseParen,
  Identifier(String),
  Arrow,
  Equal,
  Semicolon,
}

fn lex(input: &String) -> Result<Vec<Token>, String> {
  let mut result = Vec::new();
  let mut iterator = input.chars().peekable();
  while let Some(char) = iterator.next() {
    result.push(match char {
      ' ' | '\n' => continue,
      '(' => Token::OpenParen,
      ')' => Token::CloseParen,
      x if is_word_char(x) => {
        let mut str = String::new();
        str.push(char);
        while let Some(&char) = iterator.peek() {
          if is_word_char(char) {
            iterator.next();
            str.push(char)
          } else {
            break;
          }
        }
        Token::Identifier(str)
      }
      '='
        if iterator
          .peek()
          .and_then(|x: &char| -> Option<()> {
            if *x == '>' {
              Some(())
            } else {
              None
            }
          })
          .is_some() =>
      {
        iterator.next();
        Token::Arrow
      }
      '=' => Token::Equal,
      ';' => Token::Semicolon,
      _ => return Err(format!("Invalid character {}", char)),
    })
  }
  Ok(result)
}

fn _parse<'a>(
  tokens: &'a Vec<Token>,
  index: usize,
  scope: &mut HashMap<&'a str, WrappedExpr>,
) -> Result<(WrappedExpr, usize), String> {
  let result = match tokens.get(index) {
    Some(token) => match token {
      Token::OpenParen => {
        _parse(tokens, index + 1, scope).and_then(|(expr, index)| match tokens.get(index) {
          Some(Token::CloseParen) => Ok((expr, index + 1)),
          _ => Err(format!("Expected ')' at position {}", index)),
        })
      }
      Token::Identifier(id) => match tokens.get(index + 1) {
        Some(Token::Arrow) => {
          let arg = WrappedExpr::wrap_data(ExprData::Placeholder(u32::MAX));
          let old = scope
            .get(&id[..])
            .and_then(|x| Some(WrappedExpr(Rc::clone(&x.0))));
          scope.insert(id, WrappedExpr(Rc::clone(&arg.0)));
          let result = _parse(tokens, index + 2, scope).and_then(|(expr, index)| {
            Ok((WrappedExpr::wrap_data(ExprData::Lambda(arg, expr)), index))
          });
          match old {
            Some(x) => scope.insert(id, WrappedExpr(Rc::clone(&x.0))),
            None => scope.remove(&id[..]),
          };
          result
        }
        Some(Token::Equal) => {
          _parse(tokens, index + 2, scope).and_then(|(val, index)| match tokens.get(index) {
            Some(Token::Semicolon) => {
              let old = scope
                .get(&id[..])
                .and_then(|x| Some(WrappedExpr(Rc::clone(&x.0))));
              scope.insert(id, WrappedExpr(Rc::clone(&val.0)));
              let result = _parse(tokens, index + 1, scope);
              match old {
                Some(x) => scope.insert(id, WrappedExpr(Rc::clone(&x.0))),
                None => scope.remove(&id[..]),
              };
              result
            }
            _ => Err(format!("Expected ';' at position {}", index)),
          })
        }
        _ => match scope.get(&id[..]) {
          Some(expr) => Ok((WrappedExpr(Rc::clone(&expr.0)), index + 1)),
          None => Err(format!("Unbound identifier {}", id)),
        },
      },
      _ => Err(format!("Unexpected token at position {}", index)),
    },
    None => Err(format!("Unexpected EOF")),
  };
  match result {
    Ok((mut expr, mut index)) => {
      while let Some(Token::OpenParen) = tokens.get(index) {
        let tup =
          match _parse(tokens, index + 1, scope).and_then(|(arg, index)| match tokens.get(index) {
            Some(Token::CloseParen) => {
              let val = WrappedExpr::wrap_data(ExprData::Call(expr, arg));
              Ok((val, index + 1))
            }
            _ => Err(format!("Expected ')' at position {}", index)),
          }) {
            Ok(x) => x,
            x => return x,
          };
        expr = tup.0;
        index = tup.1;
      }
      Ok((expr, index))
    }
    x => x,
  }
}

fn parse(source: String) -> Result<WrappedExpr, String> {
  let tokens = match lex(&source) {
    Ok(x) => x,
    Err(x) => return Err(x),
  };
  let (expr, index) = match _parse(&tokens, 0, &mut HashMap::new()) {
    Ok(x) => x,
    Err(x) => return Err(x),
  };
  if index != tokens.len() {
    return Err(format!("Expected EOF at position {}", index));
  }
  Ok(expr)
}

fn is_word_char(char: char) -> bool {
  match char {
    'a'..='z' | 'A'..='Z' | '0'..='9' | '\'' | '_' | '.' => true,
    _ => false,
  }
}

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

struct NumberedWrappedExpr<'a>(u32, &'a WrappedExpr);

impl<'a> fmt::Display for NumberedWrappedExpr<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.1._fmt(f, self.0, false)
  }
}
