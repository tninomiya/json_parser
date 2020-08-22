/// A list specifing general categories of the error possibly shown in lexer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LexErrorKind {
    InvalidChar(char),
    UnknownToken(String),
    Eof,
}

use crate::annot::{Annot, Loc};
use crate::token::Token;

/// LexError represents errors for lexer with its position information.
pub type LexError = Annot<LexErrorKind>;

impl LexError {
    fn invalid_char(c: char, loc: Loc) -> Self {
        Self::new(LexErrorKind::InvalidChar(c), loc)
    }
    fn unknown_token(s: String, loc: Loc) -> Self {
        Self::new(LexErrorKind::UnknownToken(s), loc)
    }
    fn eof(loc: Loc) -> Self {
        Self::new(LexErrorKind::Eof, loc)
    }
}

// Converts a input to Tokens.
pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
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
            b'0'..=b'9' => lex_a_token!(lex_number(input, pos)),
            b'\'' | b'"' => lex_a_token!(lex_literal(input, pos)),
            b':' => lex_a_token!(lex_colon(input, pos)),
            b',' => lex_a_token!(lex_comma(input, pos)),
            b'{' => lex_a_token!(lex_lbrace(input, pos)),
            b'}' => lex_a_token!(lex_rbrace(input, pos)),
            b'[' => lex_a_token!(lex_lbracket(input, pos)),
            b']' => lex_a_token!(lex_rbracket(input, pos)),
            b'a'..=b'z' | b'A'..=b'Z' => lex_a_token!(lex_keyword(input, pos)),
            b' ' | b'\n' | b'\t' => {
                let ((), p) = skip_spaces(input, pos)?;
                pos = p;
            }
            b => return Err(LexError::invalid_char(b as char, Loc::new(pos, pos + 1))),
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

fn recognize_sequence(input: &[u8], mut pos: usize, mut f: impl FnMut(u8) -> bool) -> usize {
    while pos < input.len() && f(input[pos]) {
        pos += 1;
    }
    pos
}

fn lex_number(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;

    let start = pos;
    let end = recognize_sequence(input, start, |b| b"1234567890".contains(&b));

    let n = from_utf8(&input[start..end]).unwrap().parse().unwrap();
    Ok((Token::number(n, Loc::new(start, end)), end))
}

fn lex_literal(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;

    if input[pos] != b'\'' && input[pos] != b'"' {
        return Err(LexError::invalid_char(
            input[pos] as char,
            Loc::new(pos, pos + 1),
        ));
    }

    // handle first quotation
    let quotation = input[pos];
    let (_, start) = consume_bytes(input, pos, quotation)?;

    // handle content
    let end = recognize_sequence(input, start, |b| quotation != b);

    // handle last quotation
    consume_bytes(input, end, quotation)?;

    let s = from_utf8(&input[start..end]).unwrap().into();
    Ok((Token::literal(s, Loc::new(pos, end + 1)), end + 1))
}

fn lex_colon(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_bytes(input, start, b':').map(|(_, end)| (Token::colon(Loc::new(start, end)), end))
}

fn lex_comma(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_bytes(input, start, b',').map(|(_, end)| (Token::comma(Loc::new(start, end)), end))
}

fn lex_lbrace(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_bytes(input, start, b'{').map(|(_, end)| (Token::lbrace(Loc::new(start, end)), end))
}

fn lex_rbrace(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_bytes(input, start, b'}').map(|(_, end)| (Token::rbrace(Loc::new(start, end)), end))
}

fn lex_lbracket(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_bytes(input, start, b'[').map(|(_, end)| (Token::lbracket(Loc::new(start, end)), end))
}

fn lex_rbracket(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_bytes(input, start, b']').map(|(_, end)| (Token::rbracket(Loc::new(start, end)), end))
}

fn lex_keyword(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;

    let end = recognize_sequence(input, start, |b| (b'a'..=b'z').contains(&b) || (b'A'..=b'Z').contains(&b));
    if input.len() <= end {
        return Err(LexError::eof(Loc::new(start, start)));
    }

    let s = from_utf8(&input[start..end]).unwrap();
    
    if let Some(token) = Token::get_keyword(&s, Loc::new(start, end)) {
        return Ok((token, end));
    } else {
        return Err(LexError::unknown_token(s.to_string(), Loc::new(start, end)));
    }
}

fn skip_spaces(input: &[u8], pos: usize) -> Result<((), usize), LexError> {
    let pos = recognize_sequence(input, pos, |b| b" \n\t".contains(&b));
    Ok(((), pos))
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

    #[test]
    fn text_lexer_flat() {
        assert_eq!(
            lex("{'key1': 'value1', 'key2': 'value2'}"),
            Ok(vec![
                Token::lbrace(Loc::new(0, 1)),
                Token::literal("key1".to_string(), Loc::new(1, 7)),
                Token::colon(Loc::new(7, 8)),
                Token::literal("value1".to_string(), Loc::new(9, 17)),
                Token::comma(Loc::new(17, 18)),
                Token::literal("key2".to_string(), Loc::new(19, 25)),
                Token::colon(Loc::new(25, 26)),
                Token::literal("value2".to_string(), Loc::new(27, 35)),
                Token::rbrace(Loc::new(35, 36)),
            ])
        )
    }

    #[test]
    fn text_lexer() {
        assert_eq!(
            lex("{'key1': 'value1', 'array': ['key2-1': 'value2-1', 'key2-2': true], 'object': {'key3-1': 'value3-1', 'key3-2': false}, 'key4': null}"),
            Ok(vec![
                Token::lbrace(Loc::new(0, 1)),
                Token::literal("key1".to_string(), Loc::new(1, 7)),
                Token::colon(Loc::new(7, 8)),
                Token::literal("value1".to_string(), Loc::new(9, 17)),
                Token::comma(Loc::new(17, 18)),
                
                // array
                Token::literal("array".to_string(), Loc::new(19, 26)),
                Token::colon(Loc::new(26, 27)),
                Token::lbracket(Loc::new(28, 29)),
                Token::literal("key2-1".to_string(), Loc::new(29, 37)),
                Token::colon(Loc::new(37, 38)),
                Token::literal("value2-1".to_string(), Loc::new(39, 49)),
                Token::comma(Loc::new(49, 50)),
                Token::literal("key2-2".to_string(), Loc::new(51, 59)),
                Token::colon(Loc::new(59, 60)),
                Token::true_value(Loc::new(61, 65)),
                Token::rbracket(Loc::new(65,66)),
                Token::comma(Loc::new(66, 67)),
                
                // object
                Token::literal("object".to_string(), Loc::new(68, 76)),
                Token::colon(Loc::new(76, 77)),
                Token::lbrace(Loc::new(78 ,79)),
                Token::literal("key3-1".to_string(), Loc::new(79, 87)),
                Token::colon(Loc::new(87, 88)),
                Token::literal("value3-1".to_string(), Loc::new(89, 99)),
                Token::comma(Loc::new(99, 100)),
                Token::literal("key3-2".to_string(), Loc::new(101, 109)),
                Token::colon(Loc::new(109, 110)),
                Token::false_value(Loc::new(111, 116)),
                Token::rbrace(Loc::new(116,117)),
                Token::comma(Loc::new(117, 118)),

                Token::literal("key4".to_string(), Loc::new(119, 125)),
                Token::colon(Loc::new(125, 126)),
                Token::null(Loc::new(127, 131)),
                Token::rbrace(Loc::new(131, 132)),
            ])
        )
    }

    #[test]
    fn text_lexer_invalid_string() {
        assert_eq!(lex("{'key}"), Err(LexError::eof(Loc::new(6, 6))))
    }

    #[test]
    fn text_lexer_invalid_keyword() {
        assert_eq!(lex("{invalid}"), Err(LexError::unknown_token("invalid".to_string(), Loc::new(1, 8))))
    }

}
