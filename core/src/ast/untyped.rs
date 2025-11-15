use crate::Spanned;

#[derive(Debug, PartialEq)]
pub struct UntypedFile {
    pub definitions: Vec<Spanned<UntypedDefinition>>,
}

#[derive(Default, PartialEq, Debug)]
pub struct UntypedDefinition {
    pub lhs: Spanned<String>,
    pub rhs: Spanned<UntypedExpression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UntypedExpression {
    FunctionCall {
        function: Box<Spanned<UntypedExpression>>,
        arguments: Vec<Spanned<UntypedExpression>>,
    },
    Literal(UntypedLiteral),
    Ident(String),
}

impl Default for UntypedExpression {
    fn default() -> Self {
        Self::Literal(UntypedLiteral::Int(0))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct UntypedAnnotatedIdent {
    pub ident: Spanned<String>,
    pub annotation: Spanned<String>,
}
#[derive(PartialEq, Debug, Clone)]
pub enum UntypedLiteral {
    String(String),
    Int(i64),
    Float(f64),
    Unit,
    Boolean(bool),
    Function {
        arguments: Vec<UntypedAnnotatedIdent>,
        body: Vec<Spanned<UntypedExpression>>,
    },
}
