pub enum Token {
  OpenParen,
  CloseParen,
  Identifier(String),
  Arrow,
  Equal,
  Semicolon,
}

pub fn lex(input: &String) -> Result<Vec<Token>, String> {
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

fn is_word_char(char: char) -> bool {
  match char {
    'a'..='z' | 'A'..='Z' | '0'..='9' | '\'' | '_' | '.' => true,
    _ => false,
  }
}
