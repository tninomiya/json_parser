/// A list specifing general categories of the error possibly shown in lexer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LexErrorKind {
    InvalidChar(char),
    Eof,
}

use crate::annot::{Annot, Loc};
use crate::token::Token;

type LexError = Annot<LexErrorKind>;

impl LexError {
    fn invalid_char(c: char, loc: Loc) -> Self {
        Self::new(LexErrorKind::InvalidChar(c), loc)
    }
    fn eof(loc: Loc) -> Self {
        Self::new(LexErrorKind::Eof, loc)
    }
}

fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let input = input.as_bytes();

    let mut tokens = Vec::new();
    let mut pos = 0;
    macro_rules! lex_a_token {
        ($lexer:expr) => {{
            let (tok, p) = $lexer?;
            tokens.push(tok);
            pos = p;
        }};
    }

    while pos < input.len() {
        match input[pos] {
            b'{' => lex_a_token!(lex_lbrace(input, pos)),
            b'}' => lex_a_token!(lex_rbrace(input, pos)),
            _ => unimplemented!(),
        }
    }
    Ok(tokens)
}

fn consume_bytes(input: &[u8], pos: usize, b: u8) -> Result<(u8, usize), LexError> {
    if input.len() <= pos {
        return Err(LexError::eof(Loc::new(pos, pos)));
    }
    if input[pos] != b {
        return Err(LexError::invalid_char(
            input[pos] as char,
            Loc::new(pos, pos + 1),
        ));
    }
    Ok((b, pos + 1))
}

fn lex_lbrace(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_bytes(input, start, b'{').map(|(_, end)| (Token::lbrace(Loc::new(start, end)), end))
}

fn lex_rbrace(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_bytes(input, start, b'}').map(|(_, end)| (Token::rbrace(Loc::new(start, end)), end))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::annot::Loc;
    #[test]
    fn text_lexer_empty() {
        assert_eq!(
            lex("{}"),
            Ok(vec![
                Token::lbrace(Loc::new(0, 1)),
                Token::rbrace(Loc::new(1, 2)),
            ])
        )
    }
}
