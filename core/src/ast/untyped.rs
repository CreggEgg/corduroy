use crate::{Span, Spanned};

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
    Match {
        target: Box<Spanned<UntypedExpression>>,
        arms: Vec<UntypedMatchArm>,
    },
    Block(UntypedBlock),
}

#[derive(Debug, PartialEq, Clone)]
pub struct UntypedMatchArm {
    pub lhs: UntypedLValue,
    pub rhs: Spanned<UntypedExpression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UntypedLValue {
    Ident(Spanned<String>),
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Array(Vec<Spanned<UntypedLValue>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOperator(pub String);
// #[derive(Debug, PartialEq, Clone, Copy)]
// pub enum BinaryOperator {
//     AddInt,
//     MultiplyInt,
//     DivideInt,
//     SubtractInt,
//     AddFloat,
//     MultiplyFloat,
//     DivideFloat,
//     SubtractFloat,
//     GreaterThan,
//     LessThan,
//     LessThanOrEqual,
//     GreaterThanOrEqual,
//     Equal,
// }

impl BinaryOperator {
    pub fn display(&self) -> String {
        self.0.clone()
        // (match self {
        //     BinaryOperator::AddInt => "+",
        //     BinaryOperator::MultiplyInt => "*",
        //     BinaryOperator::DivideInt => "/",
        //     BinaryOperator::SubtractInt => "-",
        //     BinaryOperator::AddFloat => "+.",
        //     BinaryOperator::MultiplyFloat => "*.",
        //     BinaryOperator::DivideFloat => "/.",
        //     BinaryOperator::SubtractFloat => "-.",
        //     BinaryOperator::GreaterThan => ">",
        //     BinaryOperator::LessThan => "<",
        //     BinaryOperator::LessThanOrEqual => "<=",
        //     BinaryOperator::GreaterThanOrEqual => ">=",
        //     BinaryOperator::Equal => "==",
        // })
        // .to_string()
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
        body: UntypedBlock,
    },
    Array(Vec<Spanned<UntypedExpression>>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct UntypedBlock(pub Vec<Spanned<UntypedExpression>>);

impl From<Vec<Spanned<UntypedExpression>>> for UntypedBlock {
    fn from(value: Vec<Spanned<UntypedExpression>>) -> Self {
        Self(value)
    }
}
