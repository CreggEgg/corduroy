use std::collections::HashMap;

use crate::{
    Scope, Span, Spanned,
    ast::{
        typed::{AnnotatedIdent, Definition, Expression, File, Literal, Type, TypedExpression},
        untyped::{
            UntypedAnnotatedIdent, UntypedDefinition, UntypedExpression, UntypedFile,
            UntypedLiteral,
        },
    },
};

pub type TypeVariables = HashMap<usize, Type>;

#[derive(Debug, PartialEq)]
pub enum TypeError {
    IncorrectType {
        expected: Type,
        got: Type,
        span: Span,
    },
    Undefined {
        ident: String,
        span: Span,
    },
}

pub fn infer_ast(mut untyped_ast: UntypedFile, mut scope: Scope<Type>) -> Result<File, TypeError> {
    let mut type_variables: TypeVariables = HashMap::new();
    let definitions = untyped_ast
        .definitions
        .drain(0..)
        .map(|(definition, span)| {
            Ok((
                infer_definition(definition, &mut scope, &mut type_variables)?,
                span.to_owned(),
            ))
        });
    Ok(File {
        definitions: definitions.collect::<Result<Vec<_>, TypeError>>()?,
    })
}

fn infer_definition(
    definiton: UntypedDefinition,
    scope: &mut Scope<Type>,
    type_variables: &mut TypeVariables,
) -> Result<Definition, TypeError> {
    let UntypedDefinition {
        lhs,
        rhs: (rhs, rhs_span),
    } = definiton;
    Ok(Definition {
        lhs,
        rhs: (
            infer_expr(
                rhs,
                scope,
                &Type::new_type_variable(),
                type_variables,
                rhs_span,
            )?,
            rhs_span,
        ),
    })
}
fn infer_expr(
    expression: UntypedExpression,
    scope: &mut Scope<Type>,
    expected_evaluates_to: &Type,
    type_variables: &mut TypeVariables,
    span: Span,
) -> Result<TypedExpression, TypeError> {
    let typed_expression = match expression {
        UntypedExpression::FunctionCall {
            function,
            mut arguments,
        } => {
            let arguments = arguments
                .drain(0..)
                .map(|(argument, span)| {
                    Ok((
                        infer_expr(
                            argument,
                            scope,
                            &Type::new_type_variable(),
                            type_variables,
                            span,
                        )?,
                        span,
                    ))
                })
                .collect::<Result<Vec<Spanned<TypedExpression>>, TypeError>>()?;
            let (function, function_span) = *function;
            let expected = Type::Function {
                args: arguments
                    .clone()
                    .drain(0..)
                    .map(|(expr, _)| expr.evaluates_to)
                    .collect::<Vec<_>>(), // TODO - THIS IS SO ICKY
                return_type: Box::new(expected_evaluates_to.clone()),
            };

            let function_type =
                infer_expr(function, scope, &expected, type_variables, function_span)?;
            if !function_type
                .evaluates_to
                .resolve_comparsion(&expected, scope, type_variables)
            {
                return Err(TypeError::IncorrectType {
                    expected,
                    got: function_type.evaluates_to,
                    span: function_span,
                });
            }

            let return_type = function_type
                    .evaluates_to
                    .try_get_function_return_type()
                    .unwrap()/* We can unwrap here because resolve comparison ensures it is a function */;

            Ok(TypedExpression::new(
                Expression::FunctionCall {
                    function: Box::new((function_type, function_span)),
                    arguments,
                },
                return_type,
            ))
        }
        UntypedExpression::Literal(literal) => infer_literal(literal, scope, type_variables),
        UntypedExpression::Ident(ident) => Ok(TypedExpression::new(
            Expression::Ident(ident.clone()),
            scope
                .get(&ident)
                .ok_or(TypeError::Undefined { ident, span })?
                .clone(),
        )),
    }?;
    if let Type::TypeVariable(id) = expected_evaluates_to {
        type_variables.insert(*id, typed_expression.evaluates_to.clone());
    }
    Ok(typed_expression)
}

fn infer_literal(
    literal: UntypedLiteral,
    scope: &mut Scope<Type>,
    type_variables: &mut TypeVariables,
) -> Result<TypedExpression, TypeError> {
    Ok(match literal {
        UntypedLiteral::String(string) => {
            TypedExpression::new(Expression::Literal(Literal::String(string)), Type::String)
        }
        UntypedLiteral::Int(int) => {
            TypedExpression::new(Expression::Literal(Literal::Int(int)), Type::Int)
        }
        UntypedLiteral::Float(float) => {
            TypedExpression::new(Expression::Literal(Literal::Float(float)), Type::Float)
        }
        UntypedLiteral::Unit => {
            TypedExpression::new(Expression::Literal(Literal::Unit), Type::Unit)
        }
        UntypedLiteral::Boolean(boolean) => TypedExpression::new(
            Expression::Literal(Literal::Boolean(boolean)),
            Type::Boolean,
        ),
        UntypedLiteral::Function { arguments, body } => {
            infer_function(arguments, body, scope, type_variables)?
        }
    })
}

fn infer_function(
    mut arguments: Vec<UntypedAnnotatedIdent>,
    mut body: Vec<Spanned<UntypedExpression>>,
    scope: &mut Scope<Type>,
    type_variables: &mut TypeVariables,
) -> Result<TypedExpression, TypeError> {
    let arguments = arguments
        .drain(0..)
        .map(|argument| infer_annotated_ident(argument, scope))
        .collect::<Vec<_>>();
    let mut internal_scope = scope.clone();
    for argument in &arguments {
        internal_scope.insert(argument.ident.0.clone(), argument.annotation.0.clone());
    }

    let body = body
        .drain(0..)
        .map(|(expression, span)| {
            Ok((
                infer_expr(
                    expression,
                    &mut internal_scope,
                    &Type::new_type_variable(),
                    type_variables,
                    span,
                )?,
                span,
            ))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let return_type = body
        .last()
        .map(|(last_expression, _)| last_expression.evaluates_to.clone())
        .unwrap_or(Type::Unit);

    let argument_types = arguments
        .iter()
        .map(
            |AnnotatedIdent {
                 annotation: (annotation, _),
                 ..
             }| { annotation.clone() },
        )
        .collect();

    Ok(TypedExpression::new(
        Expression::Literal(Literal::Function { arguments, body }),
        Type::Function {
            args: argument_types,
            return_type: Box::new(return_type),
        },
    ))
}

fn infer_annotated_ident(
    argument: UntypedAnnotatedIdent,
    scope: &mut Scope<Type>,
) -> AnnotatedIdent {
    todo!()
}
