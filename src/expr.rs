use rand::random;
use std::cell::RefCell;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::{Rc, Weak};

#[derive(Debug)]
pub struct WrappedExpr(pub Rc<RefCell<Expr>>);

impl WrappedExpr {
  pub fn wrap_data(data: ExprData) -> WrappedExpr {
    WrappedExpr(Rc::new(RefCell::new(Expr {
      data,
      cloned_value: None,
    })))
  }
  pub fn clone(this: &WrappedExpr) -> WrappedExpr {
    WrappedExpr(Rc::clone(&this.0))
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
    let mut current = WrappedExpr::clone(self);
    let mut calls: Vec<WrappedExpr> = vec![];
    loop {
      current = {
        let mut current_expr = current.0.borrow_mut();
        match &mut current_expr.data {
          ExprData::Call(fun, arg) => {
            calls.push(WrappedExpr::clone(arg));
            WrappedExpr::clone(fun)
          }
          ExprData::Placeholder(_) => break,
          ExprData::Wrapper(val) => WrappedExpr::clone(val),
          ExprData::Lambda(param, body) => match calls.pop() {
            Some(arg) => {
              if Rc::strong_count(&current.0) > 1 {
                let id = random::<u32>();
                {
                  let mut param_expr = param.0.borrow_mut();
                  param_expr.cloned_value = Some((id, Rc::downgrade(&arg.0)));
                }
                body._clone(id, false).unwrap_or(WrappedExpr::clone(body))
              } else {
                *param.0.borrow_mut() = Expr {
                  data: ExprData::Wrapper(WrappedExpr::clone(&arg)),
                  cloned_value: None,
                };
                WrappedExpr::clone(body)
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
          arg._reduce(num);
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
    let clone = WrappedExpr::wrap_data(match &expr.data {
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
          a2.unwrap_or(WrappedExpr::clone(a)),
          b2.unwrap_or(WrappedExpr::clone(b)),
        )),
      },
    }?);
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
