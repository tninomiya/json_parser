fn main() {
    println!("Hello, world!");
}

/// Loc describes a position from Loc.0 to Loc.1.
/// As of now it does not support line number.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Loc(usize, usize);

impl Loc {
    fn merge(&self, other: &Loc) -> Loc {
        use std::cmp::{max, min};
        Loc(min(self.0, other.0), max(self.1, other.1))
    }
}

/// Annot combines values and its location.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Annot<T> {
    value: T,
    loc: Loc,
}

impl<T> Annot<T> {
    fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}

/// Enumeration of possible tokens recognized in JSON format.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TokenKind {
    /// [0-9][0-9]*
    Number(i64),
    /// any unicord characters
    Literal(&str),
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

/// Token represents token with its position information.
type Token = Annot<TokenKind>;

impl Token {
    fn number(n: i64, loc: Loc) {
        Self::new(TokenKind::Number(n), loc)
    }
    fn literal(s: &str, loc: Loc) {
        Self::new(TokenKind::Literal(s), loc)
    }
    fn colon(loc: Loc) {
        Self::new(TokenKind::Colon(loc))
    }
    fn comma(loc: Loc) {
        Self::new(TokenKind::Comma(loc))
    }
    fn lbrace(loc: Loc) {
        Self::new(TokenKind::LBrace(loc))
    }
    fn rbrace(loc: Loc) {
        Self::new(TokenKind::RBrace(loc))
    }
    fn lbracket(loc: Loc) {
        Self::new(TokenKind::LBracket(loc))
    }
    fn trueValue(loc: Loc) {
        Self::new(TokenKind::True(loc))
    }
    fn falseValue(loc: Loc) {
        Self::new(TokenKind::False(loc))
    }
    fn null(loc: Loc) {
        Self::new(TokenKind::Null(loc))
    }
}
