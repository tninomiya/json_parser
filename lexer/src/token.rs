/// Enumeration of possible tokens recognized in JSON format.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// [0-9][0-9]*
    Number(i64),
    /// any unicord characters
    Literal(String),
    /// :
    Colon,
    /// ,
    Comma,
    /// {
    LBrace,
    /// }
    RBrace,
    /// [
    LBracket,
    /// ]
    RBracket,
    /// true
    True,
    /// false
    False,
    /// null
    Null,
}

use crate::annot::{Annot, Loc};

/// Token represents token with its position information.
pub type Token = Annot<TokenKind>;

impl Token {
    pub fn number(n: i64, loc: Loc) -> Self {
        Self::new(TokenKind::Number(n), loc)
    }
    pub fn literal(s: String, loc: Loc) -> Self {
        Self::new(TokenKind::Literal(s), loc)
    }
    pub fn colon(loc: Loc) -> Self {
        Self::new(TokenKind::Colon, loc)
    }
    pub fn comma(loc: Loc) -> Self {
        Self::new(TokenKind::Comma, loc)
    }
    pub fn lbrace(loc: Loc) -> Self {
        Self::new(TokenKind::LBrace, loc)
    }
    pub fn rbrace(loc: Loc) -> Self {
        Self::new(TokenKind::RBrace, loc)
    }
    pub fn lbracket(loc: Loc) -> Self {
        Self::new(TokenKind::LBracket, loc)
    }
    pub fn rbracket(loc: Loc) -> Self {
        Self::new(TokenKind::RBracket, loc)
    }
    pub fn true_value(loc: Loc) -> Self {
        Self::new(TokenKind::True, loc)
    }
    pub fn false_value(loc: Loc) -> Self {
        Self::new(TokenKind::False, loc)
    }
    pub fn null(loc: Loc) -> Self {
        Self::new(TokenKind::Null, loc)
    }
}
