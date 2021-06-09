use std::fmt;

pub enum Expr {
  Identifier(String),
  Lambda(String, Box<Expr>),
  Call(Box<Expr>, Box<Expr>),
}

impl Expr {
  pub fn reduce(self) -> Expr {
    match self {
      Expr::Identifier(id) => Expr::Identifier(id),
      Expr::Lambda(arg, body) => Expr::Lambda(arg, Box::new(body.reduce())),
      Expr::Call(fun, arg) => {
        let fun = fun.reduce();
        match fun {
          Expr::Lambda(name, body) => body.bind(name, arg).reduce(),
          _ => Expr::Call(Box::new(fun), Box::new(arg.reduce())),
        }
      }
    }
  }
  pub fn bind(self, name: String, value: Box<Expr>) -> Expr {
    match self {
      Expr::Identifier(id) => {
        if id == name {
          *value
        } else {
          Expr::Identifier(id)
        }
      }
      Expr::Call(fun, arg) => Expr::Call(
        Box::new(fun.bind(name.clone(), value.clone())),
        Box::new(arg.bind(name, value)),
      ),
      Expr::Lambda(arg, body) => {
        if value.uses(&arg[..]) {
          let new_arg = arg.clone() + "'";
          let body = Box::new(body.bind(arg, Box::new(Expr::Identifier(new_arg.clone()))));
          Expr::Lambda(new_arg, body).bind(name, value)
        } else {
          Expr::Lambda(arg, Box::new(body.bind(name, value)))
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
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expr::Identifier(id) => write!(f, "{}", id),
      Expr::Call(fun, arg) => match **fun {
        Expr::Lambda(_, _) => write!(f, "({})({})", fun, arg),
        _ => write!(f, "{}({})", fun, arg),
      },
      Expr::Lambda(arg, body) => {
        write!(f, "{} => {}", arg, body)
      }
    }
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

pub enum Token {
  OpenParen,
  CloseParen,
  Identifier(String),
  Arrow,
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
        _ => Ok((Expr::Identifier(id.clone()), index + 1)),
      },
      Token::Arrow => Err(format!("Unexpected '=>' at position {}", index)),
      Token::CloseParen => Err(format!("Unexpected ')' at position {}", index)),
    },
    None => Err(format!("Unexpected EOF")),
  };
  match result {
    Ok((mut expr, mut index)) => {
      while let Some(Token::OpenParen) = tokens.get(index) {
        let tup = match _parse(tokens, index + 1).and_then(|(arg, index)| match tokens.get(index) {
          Some(Token::CloseParen) => Ok((Expr::Call(Box::new(expr), Box::new(arg)), index + 1)),
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
    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
    _ => false,
  }
}

fn main() {
  let input = String::from("(add => (0 => (1 => (2 => (mult =>  mult(add(2)(2))(add(2)(1))  )(a => b => i => z => a(b(i))(z)))(i => z => i(i(z))))(i => z => i(z)))(i => z => z) )(a => b => i => z => a(i)(b(i)(z)))");
  // let input = String::from("(x => (y => x => y(x))(x))");
  let expr = parse(input).expect("");
  println!("{}", expr);
  println!("{}", expr.reduce());
}
