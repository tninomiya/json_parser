use lexer::annot::{Annot, Loc};

/// A list specifing categories of ast element in AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Root(Box<Value>),
}
/// Ast is composed of AstKind and its location.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ast(Annot<AstKind>);

impl Ast {
    pub fn loc(&self) -> &Loc {
        &self.0.loc
    }
    pub fn object(v: Value, loc: Loc) -> Self {
        Ast(Annot {
            value: AstKind::Root(Box::new(v)),
            loc,
        })
    }
    pub fn array(v: Value, loc: Loc) -> Self {
        Ast(Annot {
            value: AstKind::Root(Box::new(v)),
            loc,
        })
    }
}

/// Rerepsentation of a single Key-Value pair in JSON.
pub type MemberData = (Value, Box<Element>);
/// Member is composed of MemberData and its location.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Member(Annot<MemberData>);
/// Members owns a list of Member.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Members(Annot<Vec<Member>>);
impl Member {
    pub fn new(k: Value, v: Element, loc: Loc) -> Self {
        Member(Annot {
            value: (k, Box::new(v)),
            loc,
        })
    }
    pub fn loc(&self) -> &Loc {
        &self.0.loc
    }
    pub fn value(&self) -> &MemberData {
        &self.0.value
    }
}
impl Members {
    pub fn new(v: Vec<Member>, loc: Loc) -> Self {
        Members(Annot { value: v, loc })
    }
    pub fn loc(&self) -> &Loc {
        &self.0.loc
    }
}

/// Rerepsentation of a single element in JSON.
pub type ElementData = Value;
/// Element is composed of ElementData and its location.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Element(Annot<Box<ElementData>>);
/// Elements owns a list of Element.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Elements(Annot<Vec<Element>>);

impl Element {
    pub fn new(e: ElementData, loc: Loc) -> Self {
        Element(Annot {
            value: Box::new(e),
            loc,
        })
    }
    pub fn loc(&self) -> &Loc {
        &self.0.loc
    }
}
impl Elements {
    pub fn new(v: Vec<Element>, loc: Loc) -> Self {
        Elements(Annot { value: v, loc })
    }
    pub fn loc(&self) -> &Loc {
        &self.0.loc
    }
}

/// A list specifing categories of atomic value in AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Object(Members),
    Array(Elements),
    Literal(String),
    Number(i64),
    True,
    False,
    Null,
}
/// Value is composed of ValueKind and its location.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Value(pub Annot<ValueKind>);

impl Value {
    pub fn loc(&self) -> &Loc {
        &self.0.loc
    }
    pub fn value(&self) -> &ValueKind {
        &self.0.value
    }

    pub fn object_value(ms: Members, loc: Loc) -> Self {
        Value(Annot {
            value: ValueKind::Object(ms),
            loc,
        })
    }

    pub fn empty_object(loc: Loc) -> Self {
        let ms = Members::new(Vec::new(), loc.clone());
        Value(Annot {
            value: ValueKind::Object(ms),
            loc,
        })
    }

    pub fn array_value(es: Elements, loc: Loc) -> Self {
        Value(Annot {
            value: ValueKind::Array(es),
            loc,
        })
    }

    pub fn empty_array(loc: Loc) -> Self {
        let es = Elements::new(Vec::new(), loc.clone());
        Value(Annot {
            value: ValueKind::Array(es),
            loc,
        })
    }

    pub fn literal_value(literal: String, loc: Loc) -> Self {
        Value(Annot {
            value: ValueKind::Literal(literal),
            loc,
        })
    }
    pub fn number_value(n: i64, loc: Loc) -> Self {
        Value(Annot {
            value: ValueKind::Number(n),
            loc,
        })
    }
    pub fn true_value(loc: Loc) -> Self {
        Value(Annot {
            value: ValueKind::True,
            loc,
        })
    }
    pub fn false_value(loc: Loc) -> Self {
        Value(Annot {
            value: ValueKind::False,
            loc,
        })
    }
    pub fn null_value(loc: Loc) -> Self {
        Value(Annot {
            value: ValueKind::Null,
            loc,
        })
    }
}
