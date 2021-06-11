use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};

static STDLIB: &'static str = include_str!("./stdlib");

#[derive(Eq)]
pub enum Expr {
  Identifier(String),
  Lambda(String, Box<Expr>),
  Call(Box<Expr>, Box<Expr>),
}

thread_local!(static CURRENT_REDUCING_EXPRS: RefCell<HashSet<Expr>> = RefCell::new(HashSet::new()));

impl Expr {
  pub fn reduce(self, full: bool) -> Expr {
    CURRENT_REDUCING_EXPRS.with(|cre_cell| {
      {
        let mut current_reducing_exprs = cre_cell.borrow_mut();
        println!("{} {}", current_reducing_exprs.len(), self);
        if current_reducing_exprs.contains(&self) {
          println!("dejavu");
          return self.clone();
        }
        current_reducing_exprs.insert(self.clone());
      }
      let result = match self.clone() {
        Expr::Identifier(id) => Expr::Identifier(id),
        Expr::Lambda(arg, body) => Expr::Lambda(arg, Box::new(body.reduce(full))),
        Expr::Call(fun, arg) => match *fun {
          Expr::Lambda(name, body) => {
            let bound = body.bind(name, arg);
            if full {
              bound.reduce(full)
            } else {
              bound
            }
          }
          _ => {
            let same = fun == arg;
            let new_fun = fun.reduce(false);
            match new_fun.clone() {
              Expr::Lambda(name, body) => {
                let bound = body.bind(name, if same { Box::new(new_fun) } else { arg });
                if full {
                  bound.reduce(full)
                } else {
                  bound
                }
              }
              new_fun2 => Expr::Call(Box::new(new_fun2.reduce(true)), Box::new(arg.reduce(true))),
            }
          }
        },
      };
      {
        let mut current_reducing_exprs = cre_cell.borrow_mut();
        current_reducing_exprs.remove(&self);
      }
      result
    })
  }
  pub fn bind(&self, name: String, value: Box<Expr>) -> Expr {
    match self {
      Expr::Identifier(id) => {
        if *id == name {
          *value
        } else {
          Expr::Identifier(id.clone())
        }
      }
      Expr::Call(fun, arg) => Expr::Call(
        Box::new(fun.bind(name.clone(), value.clone())),
        Box::new(arg.bind(name, value)),
      ),
      Expr::Lambda(arg, body) => {
        if value.uses(&arg[..]) {
          let new_arg = arg.clone() + "'";
          let body = Box::new(body.bind(arg.clone(), Box::new(Expr::Identifier(new_arg.clone()))));
          Expr::Lambda(new_arg, body).bind(name, value)
        } else {
          Expr::Lambda(arg.clone(), Box::new(body.bind(name, value)))
        }
      }
    }
  }
  pub fn uses(&self, name: &str) -> bool {
    match self {
      Expr::Identifier(id) => *id == name,
      Expr::Call(fun, arg) => fun.uses(name) || arg.uses(name),
      Expr::Lambda(arg, body) => *arg != name && body.uses(name),
    }
  }
  pub fn _hash<H: Hasher>(&self, state: &mut H, num: u32) {
    match self {
      Expr::Identifier(id) => {
        (0).hash(state);
        id.hash(state);
      }
      Expr::Call(fun, arg) => {
        (1).hash(state);
        fun._hash(state, num);
        arg._hash(state, num);
      }
      Expr::Lambda(arg, body) => {
        let new_arg = num.to_string();
        (2).hash(state);
        new_arg.hash(state);
        let new_body = body.bind(arg.clone(), Box::new(Expr::Identifier(new_arg)));
        new_body._hash(state, num);
      }
    }
  }
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expr::Identifier(id) => write!(f, "{}", id),
      Expr::Call(fun, arg) => match &**fun {
        Expr::Lambda(name, body) => write!(f, "({} => {})({})", name, body, arg),
        _ => write!(f, "{}({})", fun, arg),
      },
      Expr::Lambda(arg, body) => write!(f, "{} => {}", arg, body),
    }
  }
}

impl Hash for Expr {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self._hash(state, 0)
  }
}

impl Clone for Expr {
  fn clone(&self) -> Expr {
    match self {
      Expr::Identifier(id) => Expr::Identifier(id.clone()),
      Expr::Call(fun, arg) => Expr::Call(fun.clone(), arg.clone()),
      Expr::Lambda(arg, body) => Expr::Lambda(arg.clone(), body.clone()),
    }
  }
}

impl PartialEq for Expr {
  fn eq(&self, other: &Expr) -> bool {
    match (self, other) {
      (Expr::Identifier(id1), Expr::Identifier(id2)) => id1 == id2,
      (Expr::Call(fun1, arg1), Expr::Call(fun2, arg2)) => fun1 == fun2 && arg1 == arg2,
      (Expr::Lambda(arg1, body1), Expr::Lambda(arg2, body2)) if arg1 == arg2 => body1 == body2,
      (Expr::Lambda(arg1, body1), Expr::Lambda(arg2, body2)) => {
        **body1
          == body2
            .clone()
            .bind(arg2.clone(), Box::new(Expr::Identifier(arg1.clone())))
      }
      _ => false,
    }
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

fn _parse(tokens: &Vec<Token>, index: usize) -> Result<(Expr, usize), String> {
  let result = match tokens.get(index) {
    Some(token) => match token {
      Token::OpenParen => {
        _parse(tokens, index + 1).and_then(|(expr, index)| match tokens.get(index) {
          Some(Token::CloseParen) => Ok((expr, index + 1)),
          _ => Err(format!("Expected ')' at position {}", index)),
        })
      }
      Token::Identifier(id) => match tokens.get(index + 1) {
        Some(Token::Arrow) => _parse(tokens, index + 2)
          .and_then(|(expr, index)| Ok((Expr::Lambda(id.clone(), Box::new(expr)), index))),
        Some(Token::Equal) => {
          _parse(tokens, index + 2).and_then(|(val, index)| match tokens.get(index) {
            Some(Token::Semicolon) => _parse(tokens, index + 1).and_then(|(body, index)| {
              Ok((
                Expr::Call(
                  Box::new(Expr::Lambda(id.clone(), Box::new(body))),
                  Box::new(val),
                ),
                index,
              ))
            }),
            _ => Err(format!("Expected ';' at position {}", index)),
          })
        }
        _ => Ok((Expr::Identifier(id.clone()), index + 1)),
      },
      _ => Err(format!("Unexpected token at position {}", index)),
    },
    None => Err(format!("Unexpected EOF")),
  };
  match result {
    Ok((mut expr, mut index)) => {
      while let Some(Token::OpenParen) = tokens.get(index) {
        let tup = match _parse(tokens, index + 1).and_then(|(arg, index)| match tokens.get(index) {
          Some(Token::CloseParen) => {
            let val = Expr::Call(Box::new(expr), Box::new(arg));
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

fn parse(source: String) -> Result<Expr, String> {
  let tokens = match lex(&source) {
    Ok(x) => x,
    Err(x) => return Err(x),
  };
  let (expr, index) = match _parse(&tokens, 0) {
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
  let input = String::from(STDLIB) + "y(f => x => num.is0(x)(end)(_ => f(num.dec(x))))(2)";
  // let input = String::from("(x => (y => x => y(x))(x))");
  let expr = parse(input).expect("");
  println!("{}", expr.reduce(true));
}
