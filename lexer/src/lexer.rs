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
            b'0'..=b'9' => lex_a_token!(lex_number(input, pos)),
            b'\'' | b'"' => lex_a_token!(lex_literal(input, pos)),
            b':' => lex_a_token!(lex_colon(input, pos)),
            b',' => lex_a_token!(lex_comma(input, pos)),
            b'{' => lex_a_token!(lex_lbrace(input, pos)),
            b'}' => lex_a_token!(lex_rbrace(input, pos)),
            b'[' => lex_a_token!(lex_lbracket(input, pos)),
            b']' => lex_a_token!(lex_rbracket(input, pos)),
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
            lex("{'key1': 'value1', 'array': ['key2-1': 'value2-1', 'key2-2': 'value2-2'], 'object': {'key3-1': 'value3-1', 'key3-2': 'value3-2'}}"),
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
                Token::literal("value2-2".to_string(), Loc::new(61, 71)),
                Token::rbracket(Loc::new(71,72)),
                Token::comma(Loc::new(72, 73)),
                // object
                Token::literal("object".to_string(), Loc::new(74, 82)),
                Token::colon(Loc::new(82, 83)),
                Token::lbrace(Loc::new(84 ,85)),
                Token::literal("key3-1".to_string(), Loc::new(85, 93)),
                Token::colon(Loc::new(93, 94)),
                Token::literal("value3-1".to_string(), Loc::new(95, 105)),
                Token::comma(Loc::new(105, 106)),
                Token::literal("key3-2".to_string(), Loc::new(107, 115)),
                Token::colon(Loc::new(115, 116)),
                Token::literal("value3-2".to_string(), Loc::new(117, 127)),
                Token::rbrace(Loc::new(127,128)),

                Token::rbrace(Loc::new(128, 129)),
            ])
        )
    }
    #[test]
    fn text_lexer_invalid_string() {
        assert_eq!(lex("{'key}"), Err(LexError::eof(Loc::new(6, 6))))
    }
}
