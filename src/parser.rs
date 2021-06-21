use crate::expr::{ExprData, WrappedExpr};
use crate::lexer::{lex, Token};
use std::collections::HashMap;

pub fn parse(source: String) -> Result<WrappedExpr, String> {
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
          let old = scope.get(&id[..]).and_then(|x| Some(WrappedExpr::clone(x)));
          scope.insert(id, WrappedExpr::clone(&arg));
          let result = _parse(tokens, index + 2, scope).and_then(|(expr, index)| {
            Ok((WrappedExpr::wrap_data(ExprData::Lambda(arg, expr)), index))
          });
          match old {
            Some(x) => scope.insert(id, WrappedExpr::clone(&x)),
            None => scope.remove(&id[..]),
          };
          result
        }
        Some(Token::Equal) => {
          _parse(tokens, index + 2, scope).and_then(|(val, index)| match tokens.get(index) {
            Some(Token::Semicolon) => {
              let old = scope.get(&id[..]).and_then(|x| Some(WrappedExpr::clone(x)));
              scope.insert(id, WrappedExpr::clone(&val));
              let result = _parse(tokens, index + 1, scope);
              match old {
                Some(x) => scope.insert(id, WrappedExpr::clone(&x)),
                None => scope.remove(&id[..]),
              };
              result
            }
            _ => Err(format!("Expected ';' at position {}", index)),
          })
        }
        _ => match scope.get(&id[..]) {
          Some(expr) => Ok((WrappedExpr::clone(expr), index + 1)),
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
