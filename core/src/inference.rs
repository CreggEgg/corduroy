use std::collections::HashMap;

use crate::{
    Scope, Spanned,
    ast::{
        typed::{AnnotatedIdent, Definition, Expression, File, Literal, Type, TypedExpression},
        untyped::{
            UntypedAnnotatedIdent, UntypedDefinition, UntypedExpression, UntypedFile,
            UntypedLiteral,
        },
    },
};

pub fn infer_ast(mut untyped_ast: UntypedFile, mut scope: Scope<Type>) -> File {
    let definitions = untyped_ast
        .definitions
        .drain(0..)
        .map(|(definition, span)| (infer_definition(definition, &mut scope), span.to_owned()));
    File {
        definitions: definitions.collect::<Vec<_>>(),
    }
}

fn infer_definition(definiton: UntypedDefinition, scope: &mut Scope<Type>) -> Definition {
    let UntypedDefinition {
        lhs,
        rhs: (rhs, rhs_span),
    } = definiton;
    Definition {
        lhs,
        rhs: (infer_expr(rhs, scope), rhs_span),
    }
}
fn infer_expr(expression: UntypedExpression, scope: &mut Scope<Type>) -> TypedExpression {
    match expression {
        UntypedExpression::FunctionCall {
            function,
            arguments,
        } => todo!(),
        UntypedExpression::Literal(literal) => infer_literal(literal, scope),
        UntypedExpression::Ident(_) => todo!(),
    }
}

fn infer_literal(literal: UntypedLiteral, scope: &mut Scope<Type>) -> TypedExpression {
    match literal {
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
        UntypedLiteral::Function { arguments, body } => infer_function(arguments, body, scope),
    }
}

fn infer_function(
    mut arguments: Vec<UntypedAnnotatedIdent>,
    mut body: Vec<Spanned<UntypedExpression>>,
    scope: &mut Scope<Type>,
) -> TypedExpression {
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
        .map(|(expression, span)| (infer_expr(expression, &mut internal_scope), span))
        .collect::<Vec<_>>();

    let return_type = body
        .last()
        .map(|(last_expression, _)| last_expression.evaluates_to.clone())
        .unwrap_or(Type::Unit);

    TypedExpression::new(
        Expression::Literal(Literal::Function { arguments, body }),
        return_type,
    )
}

fn infer_annotated_ident(
    argument: UntypedAnnotatedIdent,
    scope: &mut Scope<Type>,
) -> AnnotatedIdent {
    todo!()
}
