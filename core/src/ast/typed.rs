use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{Span, Spanned, inference::TypeVariables};

impl Type {
    pub fn new_type_variable(used_type_variables: &mut Vec<TypeVariableId>) -> Self {
        let new_id = TypeVariableId(used_type_variables.len());

        used_type_variables.push(new_id);
        Self::TypeVariable(new_id)
    }

    pub(crate) fn unify(
        &self,
        other: &Type,
        resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
        resolved_generics: &mut HashMap<GenericId, Type>,
    ) -> bool {
        match (self, other) {
            (
                Type::Function { args, return_type },
                Type::Function {
                    args: other_args,
                    return_type: other_return_type,
                },
            ) => {
                args.iter().enumerate().all(|(i, arg)| {
                    arg.unify(&other_args[i], resolved_type_variables, resolved_generics)
                }) && return_type.unify(
                    other_return_type,
                    resolved_type_variables,
                    resolved_generics,
                )
            }
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::String, Type::String) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Array(item_type, len), Type::Array(other_item_type, other_len)) => {
                item_type.unify(&other_item_type, resolved_type_variables, resolved_generics)
                    && len == other_len
            }
            (Type::TypeVariable(id), Type::Function { args, return_type })
            | (Type::Function { args, return_type }, Type::TypeVariable(id)) => {
                if let Some(resolved_type) =
                    resolve_type_variable_and_simplify(*id, resolved_type_variables)
                {
                    match resolved_type {
                        Type::Function {
                            args: other_args,
                            return_type: other_return_type,
                        } => {
                            args == other_args
                                && return_type.unify(
                                    &other_return_type.clone(),
                                    resolved_type_variables,
                                    resolved_generics,
                                )
                        }
                        _ => {
                            resolved_type_variables.insert(
                                *id,
                                Type::Function {
                                    args: args.clone(),
                                    return_type: return_type.clone(),
                                },
                            );
                            true
                        }
                    }
                } else {
                    false
                }
            }
            (Type::TypeVariable(id), Type::Array(element_type, len))
            | (Type::Array(element_type, len), Type::TypeVariable(id)) => {
                if let Some(resolved_type) =
                    resolve_type_variable_and_simplify(*id, resolved_type_variables)
                {
                    match resolved_type {
                        Type::Array(other_element_type, other_len) => {
                            len == other_len
                                && element_type.unify(
                                    &other_element_type.clone(),
                                    resolved_type_variables,
                                    resolved_generics,
                                )
                        }
                        _ => {
                            resolved_type_variables
                                .insert(*id, Type::Array(element_type.clone(), *len));
                            true
                        }
                    }
                } else {
                    false
                }
            }
            (Type::TypeVariable(id), Type::Int) | (Type::Int, Type::TypeVariable(id)) => {
                if let Some(Type::Int) =
                    resolve_type_variable_and_simplify(*id, resolved_type_variables)
                {
                    resolved_type_variables.insert(*id, Type::Int);
                    true
                } else {
                    false
                }
            }
            (Type::TypeVariable(id), Type::Float) | (Type::Float, Type::TypeVariable(id)) => {
                if let Some(Type::Float) =
                    resolve_type_variable_and_simplify(*id, resolved_type_variables)
                {
                    resolved_type_variables.insert(*id, Type::Float);
                    true
                } else {
                    false
                }
            }
            (Type::TypeVariable(id), Type::String) | (Type::String, Type::TypeVariable(id)) => {
                if let Some(Type::String) =
                    resolve_type_variable_and_simplify(*id, resolved_type_variables)
                {
                    resolved_type_variables.insert(*id, Type::String);
                    true
                } else {
                    false
                }
            }
            (Type::TypeVariable(id), Type::Unit) | (Type::Unit, Type::TypeVariable(id)) => {
                if let Some(Type::Unit) =
                    resolve_type_variable_and_simplify(*id, resolved_type_variables)
                {
                    resolved_type_variables.insert(*id, Type::Unit);
                    true
                } else {
                    false
                }
            }
            (Type::TypeVariable(id), Type::Boolean) | (Type::Boolean, Type::TypeVariable(id)) => {
                if let Some(Type::Boolean) =
                    resolve_type_variable_and_simplify(*id, resolved_type_variables)
                {
                    resolved_type_variables.insert(*id, Type::Boolean);
                    true
                } else {
                    false
                }
            }
            (Type::TypeVariable(id), Type::TypeVariable(other_id)) => {
                match (
                    resolve_type_variable(*id, resolved_type_variables),
                    resolve_type_variable(*other_id, resolved_type_variables),
                ) {
                    (None, None) => {
                        resolved_type_variables.insert(*id, Type::TypeVariable(*other_id));
                        true
                    }
                    (Some(r#type), None) | (None, Some(r#type)) => {
                        resolved_type_variables.insert(*id, r#type.clone());
                        true
                    }
                    (Some(r#type), Some(other_type)) => r#type.clone().unify(
                        &other_type.clone(),
                        resolved_type_variables,
                        resolved_generics,
                    ),
                }
            }
            (Type::Generic(id), Type::Function { args, return_type })
            | (Type::Function { args, return_type }, Type::Generic(id)) => {
                if let Some(resolved_type) =
                    resolve_generic_and_simplify(*id, resolved_type_variables, resolved_generics)
                {
                    match resolved_type {
                        Type::Function {
                            args: other_args,
                            return_type: other_return_type,
                        } => {
                            args == other_args
                                && return_type.unify(
                                    &other_return_type.clone(),
                                    resolved_type_variables,
                                    resolved_generics,
                                )
                        }
                        _ => {
                            resolved_generics.insert(
                                *id,
                                Type::Function {
                                    args: args.clone(),
                                    return_type: return_type.clone(),
                                },
                            );
                            true
                        }
                    }
                } else {
                    false
                }
            }
            (Type::Generic(id), Type::Array(_, _)) | (Type::Array(_, _), Type::Generic(id)) => {
                todo!()
            }
            (Type::Generic(id), Type::Int) | (Type::Int, Type::Generic(id)) => {
                if let Some(Type::Int) =
                    resolve_generic_and_simplify(*id, resolved_type_variables, resolved_generics)
                {
                    resolved_generics.insert(*id, Type::Int);
                    true
                } else {
                    false
                }
            }
            (Type::Generic(id), Type::Float) | (Type::Float, Type::Generic(id)) => {
                if let Some(Type::Float) =
                    resolve_generic_and_simplify(*id, resolved_type_variables, resolved_generics)
                {
                    resolved_generics.insert(*id, Type::Float);
                    true
                } else {
                    false
                }
            }
            (Type::Generic(id), Type::String) | (Type::String, Type::Generic(id)) => {
                if let Some(Type::String) =
                    resolve_generic_and_simplify(*id, resolved_type_variables, resolved_generics)
                {
                    resolved_generics.insert(*id, Type::String);
                    true
                } else {
                    false
                }
            }
            (Type::Generic(id), Type::Unit) | (Type::Unit, Type::Generic(id)) => {
                if let Some(Type::Unit) =
                    resolve_generic_and_simplify(*id, resolved_type_variables, resolved_generics)
                {
                    resolved_generics.insert(*id, Type::Unit);
                    true
                } else {
                    false
                }
            }
            (Type::Generic(id), Type::Boolean) | (Type::Boolean, Type::Generic(id)) => {
                if let Some(Type::Boolean) =
                    resolve_generic_and_simplify(*id, resolved_type_variables, resolved_generics)
                {
                    resolved_generics.insert(*id, Type::Boolean);
                    true
                } else {
                    false
                }
            }
            (Type::TypeVariable(type_variable_id), Type::Generic(id))
            | (Type::Generic(id), Type::TypeVariable(type_variable_id)) => {
                match (
                    resolve_generic(*id, resolved_type_variables, resolved_generics),
                    resolve_type_variable(*type_variable_id, resolved_type_variables),
                ) {
                    (None, other) => {
                        resolved_generics.insert(
                            *id,
                            other
                                .cloned()
                                .unwrap_or(Type::TypeVariable(*type_variable_id)),
                        );
                        true
                    }
                    (Some(r#type), other) => r#type.clone().unify(
                        &other
                            .cloned()
                            .unwrap_or(Type::TypeVariable(*type_variable_id)),
                        resolved_type_variables,
                        resolved_generics,
                    ),
                }
            }
            (Type::Generic(id), Type::Generic(id2)) => unreachable!(),
            _ => false,
        }
    }
}

pub fn resolve_type_variable(
    id: TypeVariableId,
    resolved_type_variables: &HashMap<TypeVariableId, Type>,
) -> Option<&Type> {
    match resolved_type_variables.get(&id) {
        Some(Type::TypeVariable(id)) => resolve_type_variable(*id, resolved_type_variables),
        Some(r#type) => Some(r#type),
        None => None,
    }
}
pub fn resolve_type_variable_and_simplify(
    id: TypeVariableId,
    resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
) -> Option<&Type> {
    fn aux(
        id: TypeVariableId,
        resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
        is_indirect: bool,
    ) -> Option<&Type> {
        match resolved_type_variables.get(&id) {
            Some(Type::TypeVariable(id)) => aux(*id, resolved_type_variables, true),
            Some(r#type) => {
                if is_indirect {
                    resolved_type_variables.insert(id, r#type.clone());
                }
                Some(resolved_type_variables.get(&id).unwrap()) // PERF I think this could be made
                // unchecked / this whole function could be more optimized
            }
            None => None,
        }
    }
    aux(id, resolved_type_variables, false)
}
pub fn resolve_generic<'a>(
    id: GenericId,
    resolved_type_variables: &'a HashMap<TypeVariableId, Type>,
    resolved_generics: &'a HashMap<GenericId, Type>,
) -> Option<&'a Type> {
    match resolved_generics.get(&id) {
        Some(Type::TypeVariable(id)) => resolve_type_variable(*id, resolved_type_variables),
        Some(Type::Generic(id)) => resolve_generic(*id, resolved_type_variables, resolved_generics),
        Some(r#type) => Some(r#type),
        None => None,
    }
}
pub fn resolve_generic_and_simplify<'a>(
    id: GenericId,
    resolved_type_variables: &'a mut HashMap<TypeVariableId, Type>,
    resolved_generics: &'a mut HashMap<GenericId, Type>,
) -> Option<&'a Type> {
    fn aux<'a>(
        id: GenericId,
        resolved_type_variables: &'a mut HashMap<TypeVariableId, Type>,
        resolved_generics: &'a mut HashMap<GenericId, Type>,
        is_indirect: bool,
    ) -> Option<&'a Type> {
        match resolved_generics.get(&id) {
            Some(Type::TypeVariable(id)) => {
                resolve_type_variable_and_simplify(*id, resolved_type_variables)
            }
            Some(Type::Generic(id)) => {
                resolve_generic(*id, resolved_type_variables, resolved_generics)
            }
            Some(r#type) => {
                if is_indirect {
                    resolved_generics.insert(id, r#type.clone());
                }
                Some(resolved_generics.get(&id).unwrap()) // PERF I think this could be made
                // unchecked / this whole function could be more optimized
            }
            None => None,
        }
    }
    aux(id, resolved_type_variables, resolved_generics, false)
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
    TypeVariable(TypeVariableId),
    Generic(GenericId),
    Array(Box<Type>, usize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeVariableId(usize);

impl Deref for TypeVariableId {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for TypeVariableId {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct GenericId(usize);

impl Deref for GenericId {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for GenericId {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
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
    pub expression: Box<Expression>,
    pub evaluates_to: Type,
}
impl TypedExpression {
    pub fn new(expression: Expression, r#type: Type) -> Self {
        Self {
            expression: Box::new(expression),
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
    // BinaryExpression {
    //     lhs: Box<Spanned<TypedExpression>>,
    //     operator: BinaryOperator,
    //     rhs: Box<Spanned<TypedExpression>>,
    // },
    Definition {
        lhs: Spanned<LValue>,
        rhs: Box<Spanned<TypedExpression>>,
        mutable: bool,
    },
    Assignment {
        lhs: Spanned<String>,
        rhs: Box<Spanned<TypedExpression>>,
    },
    Block(Block),
    Match {
        target: Box<Spanned<TypedExpression>>,
        arms: Vec<MatchArm>,
    },
    BinaryExpression {
        operator: String,
        lhs: (TypedExpression, Span),
        rhs: (TypedExpression, Span),
    },
}

#[derive(PartialEq, Debug, Clone)]
pub struct MatchArm {
    pub lhs: LValue,
    pub rhs: Spanned<TypedExpression>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum LValue {
    Ident(Spanned<String>),
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Array(Vec<Spanned<LValue>>),
}

// #[derive(Debug, PartialEq, Clone, Copy)]
// pub enum BinaryOperator {
//     Add,
//     Multiply,
//     Divide,
//     Subtract,
//     GreaterThan,
//     LessThan,
//     LessThanOrEqual,
//     GreaterThanOrEqual,
//     Equal,
// }

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Unit,
    Boolean(bool),
    Function {
        arguments: Vec<AnnotatedIdent>,
        body: Block,
    },
    Array(Vec<Spanned<TypedExpression>>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct AnnotatedIdent {
    pub ident: Spanned<String>,
    pub annotation: Spanned<Type>,
}
#[derive(PartialEq, Debug, Clone)]
pub struct Block(pub Vec<Spanned<TypedExpression>>);

impl From<Vec<Spanned<TypedExpression>>> for Block {
    fn from(value: Vec<Spanned<TypedExpression>>) -> Self {
        Self(value)
    }
}
