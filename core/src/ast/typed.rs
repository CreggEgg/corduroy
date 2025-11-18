use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{Spanned, inference::TypeVariables};

static TYPE_VARIABLE_COUNTER: AtomicUsize = AtomicUsize::new(0);

impl Type {
    pub fn reset_type_variable_counter() {
        TYPE_VARIABLE_COUNTER.fetch_min(0, Ordering::SeqCst);
    }

    pub fn new_type_variable() -> Self {
        Self::TypeVariable(TYPE_VARIABLE_COUNTER.fetch_add(1, Ordering::SeqCst))
    }

    pub fn resolve_comparsion(&self, other: &Self, type_variables: &mut TypeVariables) -> bool {
        match (self, other) {
            (
                Type::Function { args, return_type },
                Type::Function {
                    args: args2,
                    return_type: return_type2,
                },
            ) => {
                if args.len() != args2.len() {
                    return false;
                };
                if !return_type.resolve_comparsion(return_type2, type_variables) {
                    return false;
                }
                args.iter()
                    .enumerate()
                    .all(|(idx, argument)| argument.resolve_comparsion(&args2[idx], type_variables))
            }
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::String, Type::String) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::TypeVariable(id), Type::Function { args, return_type })
            | (Type::Function { args, return_type }, Type::TypeVariable(id)) => {
                type_variables.insert(
                    *id,
                    Type::Function {
                        args: args.clone(),
                        return_type: return_type.clone(),
                    },
                );
                true
            }
            (Type::TypeVariable(id), Type::Int) | (Type::Int, Type::TypeVariable(id)) => {
                type_variables.insert(*id, Type::Int);
                true
            }
            (Type::TypeVariable(id), Type::Float) | (Type::Float, Type::TypeVariable(id)) => {
                type_variables.insert(*id, Type::Float);
                true
            }
            (Type::TypeVariable(id), Type::String) | (Type::String, Type::TypeVariable(id)) => {
                type_variables.insert(*id, Type::String);
                true
            }
            (Type::TypeVariable(id), Type::Unit) | (Type::Unit, Type::TypeVariable(id)) => {
                type_variables.insert(*id, Type::Unit);
                true
            }
            (Type::TypeVariable(id), Type::Boolean) | (Type::Boolean, Type::TypeVariable(id)) => {
                type_variables.insert(*id, Type::Boolean);
                true
            }
            (Type::TypeVariable(id), Type::TypeVariable(id2)) => id == id2,
            _ => false,
        }
    }

    pub fn try_get_function_return_type(&self) -> Option<Type> {
        let Type::Function { return_type, .. } = self else {
            return None;
        };
        Some(*return_type.clone())
    }

    pub fn display(&self) -> String {
        match self {
            Type::Function { args, return_type } => format!(
                "({}) -> {}",
                args.iter()
                    .map(|arg| arg.display())
                    .collect::<Vec<_>>()
                    .join(", "),
                return_type.display()
            ),
            Type::Int => "int".into(),
            Type::Float => "float".into(),
            Type::String => "string".into(),
            Type::Unit => "()".into(),
            Type::Boolean => "bool".into(),
            Type::TypeVariable(id) => format!("'{id}"),
        }
    }
}

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
    TypeVariable(usize),
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
    Ident(String),
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
