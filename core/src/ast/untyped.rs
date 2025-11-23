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
    BinaryExpression {
        lhs: Box<Spanned<UntypedExpression>>,
        operator: BinaryOperator,
        rhs: Box<Spanned<UntypedExpression>>,
    },
    Definition {
        mutable: Spanned<bool>,
        lhs: Spanned<UntypedLValue>,
        rhs: Box<Spanned<UntypedExpression>>,
    },
    Assignment {
        lhs: Spanned<String>,
        rhs: Box<Spanned<UntypedExpression>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum UntypedLValue {
    Ident(Spanned<String>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    AddInt,
    MultiplyInt,
    DivideInt,
    SubtractInt,
    AddFloat,
    MultiplyFloat,
    DivideFloat,
    SubtractFloat,
}

impl BinaryOperator {
    pub fn display(&self) -> String {
        (match self {
            BinaryOperator::AddInt => "+",
            BinaryOperator::MultiplyInt => "*",
            BinaryOperator::DivideInt => "/",
            BinaryOperator::SubtractInt => "-",
            BinaryOperator::AddFloat => "+.",
            BinaryOperator::MultiplyFloat => "*.",
            BinaryOperator::DivideFloat => "/.",
            BinaryOperator::SubtractFloat => "-.",
        })
        .to_string()
    }
}

impl Default for UntypedExpression {
    fn default() -> Self {
        Self::Literal(UntypedLiteral::Int(0))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct UntypedAnnotatedIdent {
    pub ident: Spanned<String>,
    pub annotation: Option<Spanned<String>>,
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
    Array(Vec<Spanned<UntypedExpression>>),
}
