use std::collections::HashMap;

use crate::{
    Span, Spanned,
    ast::{
        typed::{
            AnnotatedIdent, Definition, Expression, File, LValue, Literal, Type, TypedExpression,
        },
        untyped::{
            UntypedAnnotatedIdent, UntypedDefinition, UntypedExpression, UntypedFile,
            UntypedLValue, UntypedLiteral,
        },
    },
};

pub type TypeVariables = HashMap<usize, Type>;

#[derive(Clone)]
pub struct InferenceMetadata<T> {
    pub inner: T,
    pub definition_type: DefinitionType,
}

#[derive(Clone)]
pub enum DefinitionType {
    TopLevel(Span),
    MutableLocal(Span),
    Immutable(Span),
    CompilerProvided,
    Argument(Span),
}

type Scope = crate::Scope<InferenceMetadata<Type>>;

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
        operator_expectation: Type,
    },
    CannotMutate {
        target_span: Span,
        immutable_reason: ImmutableReason,
    },
}

#[derive(Debug, PartialEq)]
pub enum ImmutableReason {
    TopLevel,
    NotDeclaredAsMutable,
    Argument,
    CompilerProvided,
}

pub fn infer_ast(mut untyped_ast: UntypedFile, mut scope: Scope) -> Result<File, Box<TypeError>> {
    let definitions = untyped_ast
        .definitions
        .drain(0..)
        .map(|(definition, span)| {
            let mut type_variables: TypeVariables = HashMap::new();
            let inferred_definition =
                infer_definition(definition, &mut scope, &mut type_variables)?;
            dbg!(&type_variables);
            let finalized_definition =
                inferred_definition.substitute_type_variables(&type_variables);
            Ok((finalized_definition, span.to_owned()))
        });

    Ok(File {
        definitions: definitions.collect::<Result<Vec<_>, Box<TypeError>>>()?,
    })
}

fn infer_definition(
    definiton: UntypedDefinition,
    scope: &mut Scope,
    type_variables: &mut TypeVariables,
) -> Result<Definition, Box<TypeError>> {
    let UntypedDefinition {
        lhs,
        rhs: (rhs, rhs_span),
    } = definiton;
    let rhs = infer_expr(
        rhs,
        scope,
        &Type::new_type_variable(),
        type_variables,
        rhs_span,
    )?;
    scope.insert(
        lhs.0.clone(),
        InferenceMetadata {
            inner: rhs.evaluates_to.clone(),
            definition_type: DefinitionType::TopLevel({
                use chumsky::span::Span;
                (lhs.1.start()..rhs_span.end()).into()
            }),
        },
    );
    Ok(Definition {
        lhs,
        rhs: (rhs, rhs_span),
    })
}
fn infer_expr(
    expression: UntypedExpression,
    scope: &mut Scope,
    expected_evaluates_to: &Type,
    type_variables: &mut TypeVariables,
    span: Span,
) -> Result<TypedExpression, Box<TypeError>> {
    let typed_expression = match expression {
        UntypedExpression::FunctionCall {
            function,
            mut arguments,
        } => {
            let (function, function_span) = *function;
            let function_type = infer_expr(
                function,
                scope,
                &Type::new_type_variable(),
                type_variables,
                function_span,
            )?;
            let Type::Function {
                args: ref expected_arguments,
                ref return_type,
            } = function_type.evaluates_to
            else {
                return Err(Box::new(TypeError::IncorrectType {
                    expected: Type::Function {
                        args: (0..arguments.len())
                            .map(|_| Type::new_type_variable())
                            .collect(),
                        return_type: Box::new(expected_evaluates_to.clone()),
                    },
                    got: function_type.evaluates_to,
                    span: function_span,
                }));
            };

            if arguments.len() != expected_arguments.len() {
                return Err(Box::new(TypeError::WrongNumberOfArguments {
                    expected: expected_arguments.len(),
                    got: arguments.len(),
                }));
            };

            let arguments = arguments
                .drain(0..)
                .enumerate()
                .map(|(idx, (argument, span))| {
                    let inferred = infer_expr(
                        argument,
                        scope,
                        &Type::new_type_variable(),
                        type_variables,
                        span,
                    )?;
                    if inferred
                        .evaluates_to
                        .resolve_comparsion(&expected_arguments[idx], type_variables)
                    {
                        Ok((inferred, span))
                    } else {
                        Err(Box::new(TypeError::IncorrectType {
                            expected: expected_arguments[idx].clone(),
                            got: inferred.evaluates_to,
                            span,
                        }))
                    }
                })
                .collect::<Result<Vec<Spanned<TypedExpression>>, Box<TypeError>>>()?;
            // let expected = Type::Function {
            //     args: arguments
            //         .clone()
            //         .drain(0..)
            //         .map(|(expr, _)| expr.evaluates_to)
            //         .collect::<Vec<_>>(), // TODO - THIS IS SO ICKY
            //     return_type: Box::new(expected_evaluates_to.clone()),
            // };

            // if !function_type
            //     .evaluates_to
            //     .resolve_comparsion(&expected, type_variables)
            // {
            //     return Err(TypeError::IncorrectType {
            //         expected,
            //         got: function_type.evaluates_to,
            //         span: function_span,
            //     });
            // }

            // let return_type = function_type
            //         .evaluates_to
            //         .try_get_function_return_type()
            //         .unwrap()/* We can unwrap here because resolve comparison ensures it is a function */;

            Ok(TypedExpression::new(
                Expression::FunctionCall {
                    function: Box::new((function_type.clone(), function_span)),
                    arguments,
                },
                *return_type.clone(),
            ))
        }
        UntypedExpression::Literal(literal) => infer_literal(literal, scope, type_variables),
        UntypedExpression::Ident(ident) => Ok(TypedExpression::new(
            Expression::Ident(ident.clone()),
            scope
                .get(&ident)
                .map(|value| value.inner.clone())
                .ok_or(TypeError::Undefined { ident, span })?,
        )),
        UntypedExpression::BinaryExpression { lhs, operator, rhs } => {
            let (lhs, lhs_span) = *lhs;
            let (rhs, rhs_span) = *rhs;
            let (expected_operand_type, typed_operator) = {
                use crate::ast::typed::BinaryOperator::*;
                use Type::*;
                match operator {
                    crate::ast::untyped::BinaryOperator::AddInt => (Int, Add),
                    crate::ast::untyped::BinaryOperator::MultiplyInt => (Int, Multiply),
                    crate::ast::untyped::BinaryOperator::DivideInt => (Int, Divide),
                    crate::ast::untyped::BinaryOperator::SubtractInt => (Int, Subtract),
                    crate::ast::untyped::BinaryOperator::AddFloat => (Float, Add),
                    crate::ast::untyped::BinaryOperator::MultiplyFloat => (Float, Multiply),
                    crate::ast::untyped::BinaryOperator::DivideFloat => (Float, Divide),
                    crate::ast::untyped::BinaryOperator::SubtractFloat => (Float, Subtract),
                }
            };
            let lhs = infer_expr(lhs, scope, &expected_operand_type, type_variables, lhs_span)?;
            let rhs = infer_expr(rhs, scope, &expected_operand_type, type_variables, rhs_span)?;
            if !(lhs
                .evaluates_to
                .resolve_comparsion(&expected_operand_type, type_variables)
                && rhs
                    .evaluates_to
                    .resolve_comparsion(&expected_operand_type, type_variables))
            {
                let mut lhs = lhs;
                let mut rhs = rhs;
                lhs.evaluates_to
                    .substitute_type_variables(type_variables, &mut Vec::new());
                rhs.evaluates_to
                    .substitute_type_variables(type_variables, &mut Vec::new());
                Err(Box::new(TypeError::MismatchedBinaryExpression {
                    lhs: lhs.evaluates_to,
                    lhs_span,
                    rhs: rhs.evaluates_to,
                    rhs_span,
                    operator,
                    operator_expectation: expected_operand_type,
                }))
            } else {
                Ok(TypedExpression::new(
                    Expression::BinaryExpression {
                        lhs: Box::new((lhs, lhs_span)),
                        operator: typed_operator,
                        rhs: Box::new((rhs, rhs_span)),
                    },
                    expected_operand_type,
                ))
            }
        }
        UntypedExpression::Definition { lhs, rhs, mutable } => {
            let value = infer_expr(rhs.0, scope, expected_evaluates_to, type_variables, rhs.1)?;
            let lhs = match lhs.0 {
                UntypedLValue::Ident((ident, span)) => {
                    use chumsky::span::Span;
                    scope.insert(
                        ident.clone(),
                        InferenceMetadata {
                            inner: value.evaluates_to.clone(),
                            definition_type: if mutable.0 {
                                DefinitionType::MutableLocal(
                                    (mutable.1.start()..rhs.1.end()).into(),
                                )
                            } else {
                                DefinitionType::Immutable((mutable.1.start()..rhs.1.end()).into())
                            },
                        },
                    );

                    (LValue::Ident((ident, span)), span)
                }
            };
            Ok(TypedExpression::new(
                Expression::Definition {
                    lhs,
                    rhs: Box::new((value, rhs.1)),
                    mutable: mutable.0,
                },
                Type::Unit,
            ))
        }
        UntypedExpression::Assignment { lhs, rhs } => {
            let (rhs, rhs_span) = *rhs;
            let current = scope
                .get(&lhs.0)
                .ok_or_else(|| TypeError::Undefined {
                    ident: lhs.0.clone(),
                    span,
                })?
                .clone();
            match current.definition_type {
                DefinitionType::TopLevel(_) => {
                    return Err(Box::new(TypeError::CannotMutate {
                        target_span: lhs.1,
                        immutable_reason: ImmutableReason::TopLevel,
                    }));
                }
                DefinitionType::Immutable(_) => {
                    return Err(Box::new(TypeError::CannotMutate {
                        target_span: lhs.1,
                        immutable_reason: ImmutableReason::NotDeclaredAsMutable,
                    }));
                }
                DefinitionType::CompilerProvided => {
                    return Err(Box::new(TypeError::CannotMutate {
                        target_span: lhs.1,
                        immutable_reason: ImmutableReason::CompilerProvided,
                    }));
                }
                DefinitionType::Argument(_) => {
                    return Err(Box::new(TypeError::CannotMutate {
                        target_span: lhs.1,
                        immutable_reason: ImmutableReason::Argument,
                    }));
                }
                DefinitionType::MutableLocal(_) => {}
            };

            let rhs = infer_expr(rhs, scope, expected_evaluates_to, type_variables, rhs_span)?;
            if !rhs
                .evaluates_to
                .resolve_comparsion(&current.inner, type_variables)
            {
                return Err(Box::new(TypeError::IncorrectType {
                    expected: current.inner,
                    got: rhs.evaluates_to,
                    span: rhs_span,
                }));
            }

            Ok(TypedExpression::new(
                Expression::Assignment {
                    lhs,
                    rhs: Box::new((rhs, rhs_span)),
                },
                Type::Unit,
            ))
        }
    }?;
    if let Type::TypeVariable(id) = expected_evaluates_to {
        type_variables.insert(*id, typed_expression.evaluates_to.clone());
    }
    Ok(typed_expression)
}

fn infer_literal(
    literal: UntypedLiteral,
    scope: &mut Scope,
    type_variables: &mut TypeVariables,
) -> Result<TypedExpression, Box<TypeError>> {
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
        UntypedLiteral::Array(mut elements) => {
            let element_type = Type::new_type_variable();
            let elements = elements
                .drain(0..)
                .map(|(element, span)| {
                    let element = infer_expr(element, scope, &element_type, type_variables, span)?;
                    if !element
                        .evaluates_to
                        .resolve_comparsion(&element_type, type_variables)
                    {
                        Err(Box::new(TypeError::IncorrectType {
                            expected: element_type.clone(),
                            got: element.evaluates_to,
                            span,
                        }))
                    } else {
                        Ok((element, span))
                    }
                })
                .collect::<Result<Vec<_>, Box<TypeError>>>()?;
            let len = elements.len();
            TypedExpression::new(
                Expression::Literal(Literal::Array(elements)),
                Type::Array(Box::new(element_type), len),
            )
        }
    })
}

fn infer_function(
    mut arguments: Vec<UntypedAnnotatedIdent>,
    mut body: Vec<Spanned<UntypedExpression>>,
    scope: &mut Scope,
    type_variables: &mut TypeVariables,
) -> Result<TypedExpression, Box<TypeError>> {
    let arguments = arguments
        .drain(0..)
        .map(|argument| infer_annotated_ident(argument, scope))
        .collect::<Result<Vec<_>, _>>()?;
    let mut internal_scope = scope.clone();
    for argument in &arguments {
        internal_scope.insert(
            argument.ident.0.clone(),
            InferenceMetadata {
                inner: argument.annotation.0.clone(),
                definition_type: DefinitionType::Argument({
                    use chumsky::span::Span;
                    (argument.ident.1.start()..argument.annotation.1.end()).into()
                }),
            },
        );
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
        .collect::<Result<Vec<_>, Box<TypeError>>>()?;

    let return_type = body
        .last()
        .map(|(last_expression, _)| last_expression.evaluates_to.clone())
        .unwrap_or(Type::Unit);

    let argument_types = arguments
        .iter()
        .map(|AnnotatedIdent { annotation, .. }| annotation.0.clone())
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
    scope: &mut Scope,
) -> Result<AnnotatedIdent, Box<TypeError>> {
    let span = argument.ident.1;
    Ok(AnnotatedIdent {
        ident: argument.ident,
        annotation: match argument.annotation {
            Some((type_name, span)) => (
                scope
                    .get(&type_name)
                    .map(|value| value.inner.clone())
                    .ok_or(TypeError::Undefined {
                        ident: type_name,
                        span,
                    })?,
                span,
            ),
            None => (Type::new_type_variable(), (span)),
        },
    })
}

impl Definition {
    fn substitute_type_variables(mut self, type_variables: &TypeVariables) -> Self {
        self.rhs
            .0
            .substitute_type_variables(type_variables, &mut Vec::new());
        self
    }
}

impl TypedExpression {
    fn substitute_type_variables(
        &mut self,
        type_variables: &TypeVariables,
        used_type_variables: &mut Vec<usize>,
    ) {
        self.evaluates_to
            .substitute_type_variables(type_variables, used_type_variables);

        match &mut self.expression {
            Expression::FunctionCall {
                function,
                arguments,
            } => {
                function
                    .0
                    .substitute_type_variables(type_variables, used_type_variables);
                for argument in arguments {
                    argument
                        .0
                        .substitute_type_variables(type_variables, used_type_variables);
                }
            }
            Expression::Literal(literal) => match literal {
                Literal::Function { arguments, body } => {
                    for argument in arguments {
                        argument.substitute_type_variables(type_variables, used_type_variables);
                    }
                    for line in body {
                        line.0
                            .substitute_type_variables(type_variables, used_type_variables);
                    }
                }
                Literal::Array(elements) => {
                    for element in elements {
                        element
                            .0
                            .substitute_type_variables(type_variables, used_type_variables);
                    }
                }
                _ => {}
            },
            Expression::Ident(_) => {}
            Expression::BinaryExpression {
                lhs,
                operator: _,
                rhs,
            } => {
                lhs.0
                    .substitute_type_variables(type_variables, used_type_variables);
                rhs.0
                    .substitute_type_variables(type_variables, used_type_variables);
            }
            Expression::Definition { rhs, .. } => rhs
                .0
                .substitute_type_variables(type_variables, used_type_variables),
            Expression::Assignment { rhs, .. } => rhs
                .0
                .substitute_type_variables(type_variables, used_type_variables),
        }
    }
}

impl AnnotatedIdent {
    fn substitute_type_variables(
        &mut self,
        type_variables: &TypeVariables,
        used_type_variables: &mut Vec<usize>,
    ) {
        self.annotation
            .0
            .substitute_type_variables(type_variables, used_type_variables)
    }
}

fn resolve_type_variable(id: usize, type_variables: &TypeVariables) -> Option<&Type> {
    println!("hi");
    match type_variables.get(&id) {
        Some(Type::TypeVariable(id)) => resolve_type_variable(*id, type_variables),
        Some(r#type) => Some(r#type),
        None => None,
    }
}

impl Type {
    fn substitute_type_variables(
        &mut self,
        type_variables: &TypeVariables,
        used_type_variables: &mut Vec<usize>,
    ) {
        match self {
            Type::Function { args, return_type } => {
                for arg in args {
                    arg.substitute_type_variables(type_variables, used_type_variables);
                }
                return_type.substitute_type_variables(type_variables, used_type_variables);
            }
            Type::Array(element_type, _) => {
                element_type.substitute_type_variables(type_variables, used_type_variables);
            }
            Type::TypeVariable(id) => {
                if let Some(r#type) = resolve_type_variable(*id, type_variables) {
                    *self = r#type.clone();
                } else {
                    let variable_index = used_type_variables.iter().position(|it| it == id);
                    if let Some(position) = variable_index {
                        *id = position;
                    } else {
                        let len = used_type_variables.len();
                        used_type_variables.push(*id);
                        *id = len;
                    }
                }
            }
            _ => {}
        }
    }
}
