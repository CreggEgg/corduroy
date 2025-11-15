use crate::Spanned;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Function {
        args: Vec<Type>,
        return_type: Box<Type>,
    },
    Int,
    Float,
    String,
    Unit,
    Boolean,
}

#[derive(Debug, PartialEq)]
pub struct File {
    pub definitions: Vec<Spanned<Definition>>,
}

#[derive(Debug, PartialEq)]
pub struct Definition {
    pub lhs: Spanned<String>,
    pub rhs: Spanned<TypedExpression>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedExpression {
    pub expression: Expression,
    pub evaluates_to: Type,
}
impl TypedExpression {
    pub fn new(expression: Expression, r#type: Type) -> Self {
        Self {
            expression,
            evaluates_to: r#type,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    FunctionCall {
        function: Box<Spanned<TypedExpression>>,
        arguments: Vec<Spanned<TypedExpression>>,
    },
    Literal(Literal),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Unit,
    Boolean(bool),
    Function {
        arguments: Vec<AnnotatedIdent>,
        body: Vec<Spanned<TypedExpression>>,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub struct AnnotatedIdent {
    pub ident: Spanned<String>,
    pub annotation: Spanned<Type>,
}
