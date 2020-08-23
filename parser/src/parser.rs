use crate::ast::{Ast, Element, Elements, Member, Members, Value};
use lexer::annot::Loc;
use lexer::token::{Token, TokenKind};

/// A list specifing general categories of the error possibly shown in parser.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    UnexpectedToken(Token),
    InvalidJson,
    RedundantExpression(Token),
    Eof,
}

/// Construct AST.
pub fn parse(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
    let ret = parse_json(&mut tokens)?;
    match tokens.next() {
        Some(tok) => Err(ParseError::RedundantExpression(tok)),
        None => Ok(ret),
    }
}

use std::iter::Peekable;

fn parse_json<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let genesis_token = tokens
        .peek()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::LBracket | TokenKind::LBrace => Ok(tok.value.clone()),
            _ => Err(ParseError::UnexpectedToken(tok.clone())),
        })?;

    match genesis_token {
        TokenKind::LBracket => {
            let arr = parse_array(tokens)?;
            let loc = arr.loc().clone();
            tokens.next();
            Ok(Ast::array(arr, loc))
        }
        TokenKind::LBrace => {
            let obj = parse_object(tokens)?;
            let loc = obj.loc().clone();
            tokens.next();
            Ok(Ast::object(obj, loc))
        }
        _ => unreachable!(),
    }
}

fn parse_members<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Members, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("members");
    let mut members = Vec::new();

    let member = match parse_member(tokens) {
        Ok(e) => Ok(e),
        Err(e) => return Err(e),
    }?;
    let mut acc = member.loc().clone();
    members.push(member);
    acc = loop {
        acc = match tokens.peek() {
            Some(tok) if has_next(tok) => {
                let loc = parse_comma(tokens)?;
                acc.merge(&loc)
            }
            _ => break acc,
        };
        let member = match parse_member(tokens) {
            Ok(e) => Ok(e),
            Err(e) => return Err(e),
        }?;
        let loc = member.loc().clone();
        members.push(member);
        acc = acc.merge(&loc);
    };

    return Ok(Members::new(members, acc.clone()));
}

fn parse_member<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Member, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("member");

    let key = parse_literal(tokens)?;
    let loc = key.loc().clone();
    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::Colon => Ok(tok),
            _ => Err(ParseError::UnexpectedToken(tok)),
        })?;

    let value = parse_element(tokens)?;
    let loc = loc.merge(value.loc());
    Ok(Member::new(key, value, loc))
}

fn parse_elements<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Elements, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("elements");

    let mut elements = Vec::new();

    let element = match parse_element(tokens) {
        Ok(e) => Ok(e),
        Err(e) => return Err(e),
    }?;
    let mut acc = element.loc().clone();
    elements.push(element);
    acc = loop {
        acc = match tokens.peek() {
            Some(tok) if has_next(tok) => {
                let loc = parse_comma(tokens)?;
                acc.merge(&loc)
            }
            _ => break acc,
        };
        let element = match parse_element(tokens) {
            Ok(e) => Ok(e),
            Err(e) => return Err(e),
        }?;
        let loc = element.loc().clone();
        elements.push(element);
        acc = acc.merge(&loc);
    };

    return Ok(Elements::new(elements, acc.clone()));
}

fn has_next(next: &Token) -> bool {
    match next.value {
        TokenKind::Comma => true,
        _ => false,
    }
}

fn parse_element<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Element, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("element");

    let value = parse_value(tokens)?;
    let loc = value.loc().clone();
    Ok(Element::new(value, loc))
}

fn parse_value<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Value, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("value");

    let next = tokens.peek().ok_or(ParseError::Eof)?;
    match next.value {
        TokenKind::LBrace => parse_object(tokens),
        TokenKind::LBracket => parse_array(tokens),
        TokenKind::Literal(_) => parse_literal(tokens),
        TokenKind::Number(_) => parse_number(tokens),
        TokenKind::True => parse_true(tokens),
        TokenKind::False => parse_false(tokens),
        TokenKind::Null => parse_null(tokens),
        _ => Err(ParseError::UnexpectedToken(next.clone())),
    }
}

fn parse_object<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Value, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("object");
    let acc = tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::LBrace => Ok(tok.loc.clone()),
            _ => Err(ParseError::UnexpectedToken(tok)),
        })?;

    let is_empty = tokens
        .peek()
        .ok_or(ParseError::InvalidJson)
        .map(|tok| match tok.value {
            TokenKind::RBrace => true,
            _ => false,
        })?;
    if is_empty {
        let acc = tokens
            .next()
            .ok_or(ParseError::Eof)
            .and_then(|tok| match tok.value {
                TokenKind::RBrace => {
                    let acc = acc.merge(&tok.loc);
                    Ok(acc)
                }
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;

        return Ok(Value::empty_object(acc));
    }
    let members = parse_members(tokens)?;

    let acc = tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::RBrace => {
                let acc = acc.merge(&tok.loc);
                Ok(acc)
            }
            _ => Err(ParseError::UnexpectedToken(tok)),
        })?;

    Ok(Value::object_value(members, acc))
}

fn parse_array<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Value, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("array");
    let acc = tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::LBracket => Ok(tok.loc),
            _ => Err(ParseError::UnexpectedToken(tok)),
        })?;

    let is_empty = tokens
        .peek()
        .ok_or(ParseError::InvalidJson)
        .map(|tok| match tok.value {
            TokenKind::RBracket => true,
            _ => false,
        })?;
    if is_empty {
        let acc = tokens
            .next()
            .ok_or(ParseError::Eof)
            .and_then(|tok| match tok.value {
                TokenKind::RBracket => {
                    let acc = acc.merge(&tok.loc);
                    Ok(acc)
                }
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;

        return Ok(Value::empty_array(acc));
    }
    let elements = parse_elements(tokens)?;

    let acc = tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::RBracket => {
                let acc = acc.merge(&tok.loc);
                Ok(acc)
            }
            _ => Err(ParseError::UnexpectedToken(tok)),
        })?;

    Ok(Value::array_value(elements, acc))
}

fn parse_literal<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Value, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("--literal");

    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::Literal(s) => Ok(Value::literal_value(s, tok.loc)),
            _ => Err(ParseError::UnexpectedToken(tok)),
        })
}

fn parse_number<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Value, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("--number");

    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::Number(n) => Ok(Value::number_value(n, tok.loc)),
            _ => Err(ParseError::UnexpectedToken(tok)),
        })
}

fn parse_true<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Value, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("--true");

    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::True => Ok(Value::true_value(tok.loc)),
            _ => Err(ParseError::UnexpectedToken(tok)),
        })
}

fn parse_false<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Value, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("--false");

    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::False => Ok(Value::false_value(tok.loc)),
            _ => Err(ParseError::UnexpectedToken(tok)),
        })
}

fn parse_null<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Value, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("--null");

    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::Null => Ok(Value::null_value(tok.loc)),
            _ => Err(ParseError::UnexpectedToken(tok)),
        })
}

fn parse_colon<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Loc, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("--colon");

    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::Colon => return Ok(tok.loc),
            _ => Err(ParseError::UnexpectedToken(tok)),
        })
}

fn parse_comma<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Loc, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    println!("--comma");
    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::Comma => return Ok(tok.loc),
            _ => Err(ParseError::UnexpectedToken(tok)),
        })
}
#[cfg(test)]
mod tests {
    use super::*;

    // {}
    #[test]
    fn text_parse_empty_object() {
        let tokens = vec![Token::lbrace(Loc::new(0, 1)), Token::rbrace(Loc::new(1, 2))];
        assert_eq!(
            parse(tokens),
            Ok(Ast::object(
                Value::object_value(Members::new(Vec::new(), Loc::new(0, 2)), Loc::new(0, 2)),
                Loc::new(0, 2)
            ))
        )
    }

    // []
    #[test]
    fn text_parse_empty_array() {
        let tokens = vec![
            Token::lbracket(Loc::new(0, 1)),
            Token::rbracket(Loc::new(1, 2)),
        ];
        assert_eq!(
            parse(tokens),
            Ok(Ast::array(
                Value::array_value(Elements::new(Vec::new(), Loc::new(0, 2)), Loc::new(0, 2)),
                Loc::new(0, 2)
            ))
        )
    }

    // {'key1': 'value1', 'key2': 'value2'}
    #[test]
    fn text_parse_flat() {
        let tokens = vec![
            Token::lbrace(Loc::new(0, 1)),
            Token::literal("key1".to_string(), Loc::new(1, 7)),
            Token::colon(Loc::new(7, 8)),
            Token::literal("value1".to_string(), Loc::new(9, 17)),
            Token::comma(Loc::new(17, 18)),
            Token::literal("key2".to_string(), Loc::new(19, 25)),
            Token::colon(Loc::new(25, 26)),
            Token::literal("value2".to_string(), Loc::new(27, 35)),
            Token::rbrace(Loc::new(35, 36)),
        ];
        assert_eq!(
            parse(tokens),
            Ok(Ast::object(
                Value::object_value(
                    Members::new(
                        vec![
                            Member::new(
                                Value::literal_value("key1".to_string(), Loc::new(1, 7)),
                                Element::new(
                                    Value::literal_value("value1".to_string(), Loc::new(9, 17)),
                                    Loc::new(9, 17),
                                ),
                                Loc::new(1, 17),
                            ),
                            Member::new(
                                Value::literal_value("key2".to_string(), Loc::new(19, 25)),
                                Element::new(
                                    Value::literal_value("value2".to_string(), Loc::new(27, 35)),
                                    Loc::new(27, 35),
                                ),
                                Loc::new(19, 35),
                            ),
                        ],
                        Loc::new(1, 35)
                    ),
                    Loc::new(0, 36)
                ),
                Loc::new(0, 36)
            ))
        )
    }

    // 0         1         2         3         4         5         6         7         8         9         10        11        12        13        14
    // 012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
    // {'key1': 'value1', 'array': [{'key2-1': 'value2-1'}, {'key2-2': true}], 'object': {'key3-1': 'value3-1', 'key3-2': false}, 'key4': null}
    #[test]
    fn text_parse() {
        let tokens = vec![
            Token::lbrace(Loc::new(0, 1)),
            Token::literal("key1".to_string(), Loc::new(1, 7)),
            Token::colon(Loc::new(7, 8)),
            Token::literal("value1".to_string(), Loc::new(9, 17)),
            Token::comma(Loc::new(17, 18)),
            // array
            Token::literal("array".to_string(), Loc::new(19, 26)),
            Token::colon(Loc::new(26, 27)),
            Token::lbracket(Loc::new(28, 29)),
            Token::lbrace(Loc::new(29, 30)),
            Token::literal("key2-1".to_string(), Loc::new(30, 38)),
            Token::colon(Loc::new(38, 39)),
            Token::literal("value2-1".to_string(), Loc::new(40, 50)),
            Token::rbrace(Loc::new(50, 51)),
            Token::comma(Loc::new(51, 52)),
            Token::lbrace(Loc::new(53, 54)),
            Token::literal("key2-2".to_string(), Loc::new(54, 62)),
            Token::colon(Loc::new(62, 63)),
            Token::true_value(Loc::new(64, 68)),
            Token::rbrace(Loc::new(68, 69)),
            Token::rbracket(Loc::new(69, 70)),
            Token::comma(Loc::new(70, 71)),
            // object
            Token::literal("object".to_string(), Loc::new(72, 80)),
            Token::colon(Loc::new(80, 81)),
            Token::lbrace(Loc::new(82, 83)),
            Token::literal("key3-1".to_string(), Loc::new(83, 91)),
            Token::colon(Loc::new(91, 92)),
            Token::literal("value3-1".to_string(), Loc::new(93, 103)),
            Token::comma(Loc::new(103, 104)),
            Token::literal("key3-2".to_string(), Loc::new(105, 113)),
            Token::colon(Loc::new(113, 114)),
            Token::false_value(Loc::new(115, 120)),
            Token::rbrace(Loc::new(120, 121)),
            Token::comma(Loc::new(121, 122)),
            Token::literal("key4".to_string(), Loc::new(123, 129)),
            Token::colon(Loc::new(129, 130)),
            Token::null(Loc::new(131, 135)),
            Token::rbrace(Loc::new(135, 136)),
        ];
        assert_eq!(
            parse(tokens),
            Ok(Ast::object(
                Value::object_value(
                    Members::new(
                        vec![
                            Member::new(
                                Value::literal_value("key1".to_string(), Loc::new(1, 7)),
                                Element::new(
                                    Value::literal_value("value1".to_string(), Loc::new(9, 17)),
                                    Loc::new(9, 17),
                                ),
                                Loc::new(1, 17),
                            ),
                            Member::new(
                                Value::literal_value("array".to_string(), Loc::new(19, 26)),
                                Element::new(
                                    Value::array_value(
                                        Elements::new(
                                            vec![
                                                Element::new(
                                                    Value::object_value(
                                                        Members::new(
                                                            vec![Member::new(
                                                                Value::literal_value(
                                                                    "key2-1".to_string(),
                                                                    Loc::new(30, 38)
                                                                ),
                                                                Element::new(
                                                                    Value::literal_value(
                                                                        "value2-1".to_string(),
                                                                        Loc::new(40, 50)
                                                                    ),
                                                                    Loc::new(40, 50),
                                                                ),
                                                                Loc::new(30, 50),
                                                            )],
                                                            Loc::new(30, 50),
                                                        ),
                                                        Loc::new(29, 51),
                                                    ),
                                                    Loc::new(29, 51),
                                                ),
                                                Element::new(
                                                    Value::object_value(
                                                        Members::new(
                                                            vec![Member::new(
                                                                Value::literal_value(
                                                                    "key2-2".to_string(),
                                                                    Loc::new(54, 62)
                                                                ),
                                                                Element::new(
                                                                    Value::true_value(Loc::new(
                                                                        64, 68
                                                                    )),
                                                                    Loc::new(64, 68),
                                                                ),
                                                                Loc::new(54, 68),
                                                            )],
                                                            Loc::new(54, 68),
                                                        ),
                                                        Loc::new(53, 69),
                                                    ),
                                                    Loc::new(53, 69),
                                                )
                                            ],
                                            Loc::new(29, 69),
                                        ),
                                        Loc::new(28, 70),
                                    ),
                                    Loc::new(28, 70),
                                ),
                                Loc::new(19, 70)
                            ),
                            Member::new(
                                Value::literal_value("object".to_string(), Loc::new(72, 80)),
                                Element::new(
                                    Value::object_value(
                                        Members::new(
                                            vec![
                                                Member::new(
                                                    Value::literal_value(
                                                        "key3-1".to_string(),
                                                        Loc::new(83, 91)
                                                    ),
                                                    Element::new(
                                                        Value::literal_value(
                                                            "value3-1".to_string(),
                                                            Loc::new(93, 103)
                                                        ),
                                                        Loc::new(93, 103),
                                                    ),
                                                    Loc::new(83, 103),
                                                ),
                                                Member::new(
                                                    Value::literal_value(
                                                        "key3-2".to_string(),
                                                        Loc::new(105, 113)
                                                    ),
                                                    Element::new(
                                                        Value::false_value(Loc::new(115, 120)),
                                                        Loc::new(115, 120),
                                                    ),
                                                    Loc::new(105, 120),
                                                )
                                            ],
                                            Loc::new(83, 120),
                                        ),
                                        Loc::new(82, 121),
                                    ),
                                    Loc::new(82, 121),
                                ),
                                Loc::new(72, 121)
                            ),
                            Member::new(
                                Value::literal_value("key4".to_string(), Loc::new(123, 129)),
                                Element::new(
                                    Value::null_value(Loc::new(131, 135)),
                                    Loc::new(131, 135),
                                ),
                                Loc::new(123, 135),
                            ),
                        ],
                        // TODO
                        Loc::new(1, 135)
                    ),
                    Loc::new(0, 136)
                ),
                Loc::new(0, 136)
            ))
        )
    }

    // {'key1': 'value1',}
    #[test]
    fn text_parse_invalid() {
        let tokens = vec![
            Token::lbrace(Loc::new(0, 1)),
            Token::literal("key1".to_string(), Loc::new(1, 7)),
            Token::colon(Loc::new(7, 8)),
            Token::literal("value1".to_string(), Loc::new(9, 17)),
            Token::comma(Loc::new(17, 18)),
            Token::rbrace(Loc::new(35, 36)),
        ];
        assert_eq!(
            parse(tokens),
            Err(ParseError::UnexpectedToken(Token::rbrace(Loc::new(35, 36))))
        )
    }
}
