use std::{collections::HashMap, usize};

use crate::{
    Span, Spanned,
    ast::{
        typed::{
            AnnotatedIdent, Block, Definition, Expression, File, GenericId, LValue, Literal, Type,
            TypeVariableId, TypedExpression,
        },
        untyped::{
            UntypedAnnotatedIdent, UntypedBlock, UntypedDefinition, UntypedExpression, UntypedFile,
            UntypedLValue, UntypedLiteral,
        },
    },
};

mod finalizer;

pub type TypeVariables = HashMap<usize, Type>;

#[derive(Clone, Debug)]
pub struct InferenceMetadata<T>
where
    T: std::fmt::Debug,
{
    pub inner: T,
    pub definition_type: DefinitionType,
}

#[derive(Clone, Debug)]
pub enum DefinitionType {
    TopLevel(Span),
    MutableLocal(Span),
    Immutable(Span),
    CompilerProvided,
    Argument(Span),
}

type Scope = crate::Scope<InferenceMetadata<Type>>;
type NamedTypes = crate::Scope<Type>;

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
    WrongNumberOfArguments {
        expected: usize,
        got: usize,
    },
    MismatchedBinaryExpression {
        lhs: Type,
        lhs_span: Span,
        rhs: Type,
        rhs_span: Span,
        operator: crate::ast::untyped::BinaryOperator,
        lhs_expectation: Type,
        rhs_expectation: Type,
    },
    CannotMutate {
        target_span: Span,
        immutable_reason: ImmutableReason,
    },
    StructuresDoNotMatch {
        pattern_span: Span,
        value_span: Span,
        strucuture_mismatch_type: StructureMismatchType,
    },
}

#[derive(Debug, PartialEq)]
pub enum StructureMismatchType {
    ArrayLength {
        pattern_length: usize,
        value_length: usize,
    },
    DifferentTypes {
        pattern_type: Type,
        value_type: Type,
    },
}

#[derive(Debug, PartialEq)]
pub enum ImmutableReason {
    TopLevel,
    NotDeclaredAsMutable,
    Argument,
    CompilerProvided,
}

pub fn infer_ast(
    mut untyped_ast: UntypedFile,
    mut scope: Scope,
    mut named_types: NamedTypes,
) -> Result<File, Box<TypeError>> {
    eprintln!("75 - starting to infer ast");
    let definitions = untyped_ast
        .definitions
        .drain(0..)
        .map(|(definition, span)| {
            let mut resolved_type_variables = HashMap::new();
            let mut used_type_variables = Vec::new();
            let inferred_definition = infer_definition(
                definition,
                &mut scope,
                &mut resolved_type_variables,
                &mut used_type_variables,
                &named_types,
            )?;
            let finalized_definition = finalize_definition(inferred_definition)?;
            Ok((finalized_definition, span.to_owned()))
        });

    Ok(File {
        definitions: definitions.collect::<Result<Vec<_>, Box<TypeError>>>()?,
    })
}

fn infer_definition(
    definition: UntypedDefinition,
    scope: &mut Scope,
    resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
    used_type_variables: &mut Vec<TypeVariableId>,
    named_types: &NamedTypes,
) -> Result<Definition, Box<TypeError>> {
    let definition_type = Type::new_type_variable(used_type_variables);
    let rhs = infer_expression(
        definition.rhs.0,
        scope,
        resolved_type_variables,
        used_type_variables,
        definition.rhs.1,
        definition_type,
        named_types,
        &mut HashMap::new(),
    )?;
    let rhs_span = definition.rhs.1;

    Ok(Definition {
        lhs: definition.lhs,
        rhs: (rhs, rhs_span),
    })
}

fn infer_expression(
    expression: UntypedExpression,
    scope: &mut Scope,
    resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
    used_type_variables: &mut Vec<TypeVariableId>,
    span: Span,
    expected_return_type: Type,
    named_types: &NamedTypes,
    resolved_generics: &mut HashMap<GenericId, Type>,
) -> Result<TypedExpression, Box<TypeError>> {
    let (inferred_expression, expression_type) = match expression {
        UntypedExpression::FunctionCall {
            function,
            arguments,
        } => infer_function_call(
            function,
            arguments,
            scope,
            resolved_type_variables,
            used_type_variables,
            &expected_return_type,
            named_types,
            resolved_generics,
        )?,
        UntypedExpression::Literal(untyped_literal) => {
            //TODO MAY EVENTUALLY NEED TO INTRODUCE A GENERIC SCOPE when i add generic data types
            let (literal, literal_type) = infer_literal(
                untyped_literal,
                scope,
                resolved_type_variables,
                used_type_variables,
                named_types,
                resolved_generics,
            )?;
            (Expression::Literal(literal), literal_type)
        }
        UntypedExpression::Ident(ident) => {
            let Some(InferenceMetadata {
                inner: type_of_value,
                ..
            }) = scope.get(&ident)
            else {
                return Err(Box::new(TypeError::Undefined { ident, span }));
            };
            (Expression::Ident(ident), type_of_value.clone())
        }
        UntypedExpression::BinaryExpression { lhs, operator, rhs } => {
            let (lhs, lhs_span) = *lhs;
            let (rhs, rhs_span) = *rhs;

            let Some(InferenceMetadata {
                inner: operator_type,
                ..
            }) = scope.get(&operator.0)
            else {
                return Err(Box::new(TypeError::Undefined {
                    ident: operator.0,
                    span,
                }));
            };

            let Type::Function { args, return_type } = operator_type else {
                unreachable!();
            };

            let lhs_expectation = args[0].clone();
            let rhs_expectation = args[1].clone();
            let return_type = *return_type.clone();

            let lhs = infer_expression(
                lhs,
                scope,
                resolved_type_variables,
                used_type_variables,
                lhs_span,
                lhs_expectation,
                named_types,
                resolved_generics,
            )?;
            let rhs = infer_expression(
                rhs,
                scope,
                resolved_type_variables,
                used_type_variables,
                rhs_span,
                rhs_expectation,
                named_types,
                resolved_generics,
            )?;

            (
                Expression::BinaryExpression {
                    lhs: (lhs, lhs_span),
                    operator: operator.0,
                    rhs: (rhs, rhs_span),
                },
                return_type,
            )
        }
        UntypedExpression::Definition { mutable, lhs, rhs } => {
            let definition_type = Type::new_type_variable(used_type_variables);
            let value = infer_expression(
                rhs.0,
                scope,
                resolved_type_variables,
                used_type_variables,
                rhs.1,
                definition_type,
                named_types,
                resolved_generics,
            )?;
            let (lvalue, scope_diff) = infer_lvalue(&lhs, &(value.evaluates_to.clone(), rhs.1))?;
            for (key, value) in scope_diff {
                scope.insert(
                    key,
                    InferenceMetadata {
                        inner: value,
                        definition_type: if mutable.0 {
                            DefinitionType::MutableLocal(mutable.1)
                        } else {
                            DefinitionType::Immutable(mutable.1)
                        },
                    },
                );
            }

            (
                Expression::Definition {
                    lhs: (lvalue, lhs.1),
                    rhs: Box::new((value, rhs.1)),
                    mutable: mutable.0,
                },
                Type::Unit,
            )
        }
        UntypedExpression::Assignment { lhs, rhs } => todo!(),
        UntypedExpression::Match { target, arms } => todo!(),
        UntypedExpression::Block(untyped_block) => {
            let (block, return_type) = infer_block(
                untyped_block,
                scope,
                resolved_type_variables,
                used_type_variables,
                named_types,
                resolved_generics,
            )?;
            (Expression::Block(block), return_type)
        }
    };

    if !expression_type.unify(
        &expected_return_type,
        resolved_type_variables,
        resolved_generics,
    ) {
        return Err(Box::new(TypeError::IncorrectType {
            expected: expected_return_type,
            got: expression_type,
            span,
        }));
    }

    Ok(TypedExpression::new(inferred_expression, expression_type))
}

fn infer_function_call(
    function: Box<Spanned<UntypedExpression>>,
    mut arguments: Vec<Spanned<UntypedExpression>>,
    scope: &mut HashMap<String, InferenceMetadata<Type>>,
    resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
    used_type_variables: &mut Vec<TypeVariableId>,
    expected_return_type: &Type,
    named_types: &HashMap<String, Type>,
    resolved_generics: &mut HashMap<GenericId, Type>,
) -> Result<(Expression, Type), Box<TypeError>> {
    let mut resolved_generics = resolved_generics.clone();

    let expected_arguments = arguments
        .iter()
        .map(|_| Type::new_type_variable(used_type_variables))
        .collect::<Vec<_>>();
    let expected_function_type = Type::Function {
        args: expected_arguments.clone(),
        return_type: Box::new(expected_return_type.clone()),
    };

    let (function, function_span) = *function;
    let TypedExpression {
        expression: function_expression,
        evaluates_to: function_type,
    } = infer_expression(
        function,
        scope,
        resolved_type_variables,
        used_type_variables,
        function_span,
        expected_return_type.clone(), //PERF yuck! perf
        named_types,
        &mut resolved_generics,
    )?;
    //TODO allow for improved error reporting here. Rn it will say the function is invalid if the
    //arguments are.

    if expected_arguments.len() != arguments.len() {
        return Err(Box::new(TypeError::WrongNumberOfArguments {
            expected: expected_arguments.len(),
            got: arguments.len(),
        }));
    }

    let mut inferred_arguments = Vec::with_capacity(arguments.len());
    for (idx, argument) in arguments.drain(0..).enumerate() {
        inferred_arguments.push((
            infer_expression(
                argument.0,
                scope,
                resolved_type_variables,
                used_type_variables,
                argument.1,
                expected_arguments[idx].clone(), //PERF yuck! perf
                named_types,
                &mut resolved_generics,
            )?,
            argument.1,
        ));
    }

    Ok((
        Expression::FunctionCall {
            function: Box::new((
                TypedExpression::new(*function_expression, function_type),
                function_span,
            )),
            arguments: inferred_arguments,
        },
        expected_return_type.clone(),
    ))
}

fn infer_lvalue(
    lvalue: &Spanned<UntypedLValue>,
    rhs: &Spanned<Type>,
) -> Result<(LValue, crate::Scope<Type>), Box<TypeError>> {
    let mut scope_diff = HashMap::new();
    Ok((
        match (&lvalue.0, &rhs.0) {
            (UntypedLValue::Ident(ident), other) => {
                scope_diff.insert(ident.0.clone(), other.clone());
                LValue::Ident(ident.clone())
            }
            (UntypedLValue::Int(x), Type::Int) => LValue::Int(*x),
            (UntypedLValue::Float(x), Type::Float) => LValue::Float(*x),
            (UntypedLValue::Boolean(x), Type::Boolean) => LValue::Boolean(*x),
            (UntypedLValue::String(x), Type::String) => LValue::String(x.clone()),
            (UntypedLValue::Array(items), Type::Array(item_type, length)) => {
                if items.len() != *length {
                    return Err(Box::new(TypeError::StructuresDoNotMatch {
                        pattern_span: lvalue.1,
                        value_span: rhs.1,
                        strucuture_mismatch_type: StructureMismatchType::ArrayLength {
                            pattern_length: items.len(),
                            value_length: *length,
                        },
                    }));
                }

                let mut items = items
                    .iter()
                    .map(|item| Ok((infer_lvalue(&item, rhs)?, item.1)))
                    .collect::<Result<Vec<_>, Box<TypeError>>>()?;
                for (key, value) in items
                    .iter()
                    .map(|((_, scope_diff), _)| scope_diff)
                    .flatten()
                {
                    scope_diff.insert(key.clone(), value.clone());
                }

                LValue::Array(
                    items
                        .drain(0..)
                        .map(|((item, _), span)| (item, span))
                        .collect(),
                )
            }
            _ => {
                todo!("tried to match a pattern that doesn't match the actual value");
            }
        },
        scope_diff,
    ))
}

fn infer_literal(
    literal: UntypedLiteral,
    scope: &mut Scope,
    resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
    used_type_variables: &mut Vec<TypeVariableId>,
    named_types: &NamedTypes,
    resolved_generics: &mut HashMap<GenericId, Type>,
) -> Result<(Literal, Type), Box<TypeError>> {
    Ok(match literal {
        UntypedLiteral::String(x) => (Literal::String(x), Type::String),
        UntypedLiteral::Int(x) => (Literal::Int(x), Type::Int),
        UntypedLiteral::Float(x) => (Literal::Float(x), Type::Float),
        UntypedLiteral::Unit => (Literal::Unit, Type::Unit),
        UntypedLiteral::Boolean(x) => (Literal::Boolean(x), Type::Boolean),
        UntypedLiteral::Function { arguments, body } => infer_function_literal(
            arguments,
            body,
            scope,
            named_types,
            resolved_type_variables,
            used_type_variables,
            resolved_generics,
        )?,
        UntypedLiteral::Array(elements) => infer_array_literal(
            elements,
            scope,
            resolved_type_variables,
            used_type_variables,
            named_types,
            resolved_generics,
        )?,
    })
}
fn infer_array_literal(
    mut elements: Vec<Spanned<UntypedExpression>>,
    scope: &mut Scope,
    resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
    used_type_variables: &mut Vec<TypeVariableId>,
    named_types: &NamedTypes,
    resolved_generics: &mut HashMap<GenericId, Type>,
) -> Result<(Literal, Type), Box<TypeError>> {
    let element_type = Type::new_type_variable(used_type_variables);

    let elements = elements
        .drain(0..)
        .map(|(element, span)| {
            Ok((
                infer_expression(
                    element,
                    scope,
                    resolved_type_variables,
                    used_type_variables,
                    span,
                    element_type.clone(),
                    named_types,
                    resolved_generics,
                )?,
                span,
            ))
        })
        .collect::<Result<Vec<Spanned<TypedExpression>>, Box<TypeError>>>()?;

    Ok((
        Literal::Array(elements.clone()),
        Type::Array(Box::new(element_type), elements.len()),
    ))
}

fn infer_function_literal(
    mut arguments: Vec<UntypedAnnotatedIdent>,
    body: UntypedBlock,
    scope: &mut Scope,
    named_types: &NamedTypes,
    resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
    used_type_variables: &mut Vec<TypeVariableId>,
    resolved_generics: &mut HashMap<GenericId, Type>,
) -> Result<(Literal, Type), Box<TypeError>> {
    let arguments = arguments
        .drain(0..)
        .map(|argument| infer_annotated_ident(argument, named_types, used_type_variables))
        .collect::<Result<Vec<_>, Box<TypeError>>>()?;
    let (body, return_type) = infer_block(
        body,
        scope,
        resolved_type_variables,
        used_type_variables,
        named_types,
        resolved_generics,
    )?;

    let function_type = Type::Function {
        args: arguments
            .iter()
            .map(|it| it.annotation.0.clone())
            .collect::<Vec<_>>(),
        return_type: Box::new(return_type),
    };

    Ok((Literal::Function { arguments, body }, function_type))
}

fn infer_block(
    mut block: UntypedBlock,
    scope: &mut Scope,
    resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
    used_type_variables: &mut Vec<TypeVariableId>,
    named_types: &NamedTypes,
    resolved_generics: &mut HashMap<GenericId, Type>,
) -> Result<(Block, Type), Box<TypeError>> {
    let lines = block
        .0
        .drain(0..)
        .map(|(line, span)| {
            let line_type = Type::new_type_variable(used_type_variables);
            Ok((
                infer_expression(
                    line,
                    scope,
                    resolved_type_variables,
                    used_type_variables,
                    span,
                    line_type,
                    named_types,
                    resolved_generics,
                )?,
                span,
            ))
        })
        .collect::<Result<Vec<_>, Box<TypeError>>>()?;

    let return_type = lines
        .last()
        .map(|line| line.0.evaluates_to.clone())
        .unwrap_or(Type::Unit);

    Ok((Block(lines), return_type))
}

fn infer_annotated_ident(
    annotated_ident: UntypedAnnotatedIdent,
    named_types: &NamedTypes,
    used_type_variables: &mut Vec<TypeVariableId>,
) -> Result<AnnotatedIdent, Box<TypeError>> {
    let (annotation_type, annotation_span) = match annotated_ident.annotation {
        Some((annotation, annotation_span)) => {
            let Some(annotation_type) = named_types.get(&annotation) else {
                return Err(Box::new(TypeError::Undefined {
                    ident: annotation,
                    span: annotation_span,
                }));
            };

            (annotation_type.clone(), annotation_span)
        }
        None => (
            Type::new_type_variable(used_type_variables),
            annotated_ident.ident.1,
        ),
    };

    Ok(AnnotatedIdent {
        ident: annotated_ident.ident,
        annotation: (annotation_type, annotation_span),
    })
}
