use std::{collections::HashMap, usize};

use crate::{
    Span, Spanned,
    ast::{
        typed::{
            AnnotatedIdent, Block, Definition, Expression, File, LValue, Literal, MatchArm, Type,
            TypedExpression,
        },
        untyped::{
            UntypedAnnotatedIdent, UntypedBlock, UntypedDefinition, UntypedExpression, UntypedFile,
            UntypedLValue, UntypedLiteral,
        },
    },
};

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
}

#[derive(Debug, PartialEq)]
pub enum ImmutableReason {
    TopLevel,
    NotDeclaredAsMutable,
    Argument,
    CompilerProvided,
}

pub fn infer_ast(mut untyped_ast: UntypedFile, mut scope: Scope) -> Result<File, Box<TypeError>> {
    eprintln!("75 - starting to infer ast");
    let definitions = untyped_ast
        .definitions
        .drain(0..)
        .map(|(definition, span)| {
            dbg!(&scope);
            eprintln!("inferring a definition");
            let mut type_variables: TypeVariables = HashMap::new();
            let inferred_definition =
                infer_definition(definition, &mut scope, &mut type_variables)?;
            eprintln!("substituting type variables in a definition");
            dbg!(&type_variables);
            let finalized_definition =
                inferred_definition.substitute_type_variables(&type_variables);
            eprintln!("substituted type variables in a definition");
            dbg!(&finalized_definition);
            eprintln!("=================================================\n=================================================\n=================================================\n=================================================\n");
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

    let definition_type = Type::new_type_variable();
    scope.insert(
        lhs.0.clone(),
        InferenceMetadata {
            inner: definition_type.clone(),
            definition_type: DefinitionType::TopLevel({
                use chumsky::span::Span;
                (lhs.1.start()..rhs_span.end()).into()
            }),
        },
    );
    let rhs = infer_expr(rhs, scope, &definition_type, type_variables, rhs_span)?;
    if !rhs
        .evaluates_to
        .resolve_comparsion(&definition_type, type_variables)
    {
        return Err(Box::new(TypeError::IncorrectType {
            expected: definition_type,
            got: rhs.evaluates_to,
            span: rhs_span,
        }));
    }
    scope
        .get_mut(&lhs.0.clone())
        .unwrap()
        .inner
        .substitute_type_variables(type_variables, &mut Vec::new());

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
            arguments,
        } => infer_function_call(
            function,
            arguments,
            expected_evaluates_to,
            scope,
            type_variables,
            span,
        ),
        UntypedExpression::Literal(literal) => infer_literal(literal, scope, type_variables),
        UntypedExpression::Ident(ident) => Ok(TypedExpression::new(
            Expression::Ident(ident.clone()),
            scope
                .get(&ident)
                .map(|value| value.inner.clone())
                .ok_or(TypeError::Undefined { ident, span })?,
        )),
        UntypedExpression::BinaryExpression { lhs, operator, rhs } => {
            infer_binary_expression(lhs, rhs, operator, scope, type_variables)
        }
        UntypedExpression::Definition { lhs, rhs, mutable } => {
            let value = infer_expr(rhs.0, scope, expected_evaluates_to, type_variables, rhs.1)?;
            let lhs = match lhs {
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
                UntypedLValue::Int(_) => todo!(),
                UntypedLValue::Float(_) => todo!(),
                UntypedLValue::Boolean(_) => todo!(),
                UntypedLValue::String(_) => todo!(),
                UntypedLValue::Array(items) => todo!(),
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
            infer_assignment(lhs, rhs, scope, type_variables)
        }
        UntypedExpression::Match { target, mut arms } => {
            let target_type = Type::new_type_variable();
            let (target, target_span) = *target;
            let target = infer_expr(target, scope, &target_type, type_variables, target_span)?;
            if !target
                .evaluates_to
                .resolve_comparsion(&target_type, type_variables)
            {
                return Err(Box::new(TypeError::IncorrectType {
                    expected: target_type,
                    got: target.evaluates_to,
                    span: target_span,
                }));
            }

            let arms = arms
                .drain(0..)
                .map(|arm| {
                    let lhs = infer_lvalue(arm.lhs)?;
                    let rhs = infer_expr(
                        arm.rhs.0,
                        scope,
                        expected_evaluates_to,
                        type_variables,
                        arm.rhs.1,
                    )?;

                    Ok(MatchArm {
                        lhs: lhs,
                        rhs: (rhs, arm.rhs.1),
                    })
                })
                .collect::<Result<Vec<_>, Box<TypeError>>>()?;

            for arm in &arms {
                if !arm
                    .rhs
                    .0
                    .evaluates_to
                    .resolve_comparsion(expected_evaluates_to, type_variables)
                {
                    return Err(Box::new(TypeError::IncorrectType {
                        expected: expected_evaluates_to.clone(),
                        got: arm.rhs.0.evaluates_to.clone(),
                        span: arm.rhs.1,
                    }));
                }
                dbg!(&arm.rhs.0.evaluates_to);
            }
            dbg!(&expected_evaluates_to);
            Ok(TypedExpression::new(
                Expression::Match {
                    target: Box::new((target, target_span)),
                    arms,
                },
                expected_evaluates_to.clone(),
            ))
        }
        UntypedExpression::Block(untyped_block) => {
            let (block, return_type) = infer_block(untyped_block, scope, type_variables)?;
            Ok(TypedExpression::new(Expression::Block(block), return_type))
        }
    }?;

    if !typed_expression
        .evaluates_to
        .resolve_comparsion(expected_evaluates_to, type_variables)
    {
        return Err(Box::new(TypeError::IncorrectType {
            expected: expected_evaluates_to.clone(),
            got: typed_expression.evaluates_to,
            span,
        }));
    }
    if let Type::TypeVariable(id) = expected_evaluates_to {
        if let Type::TypeVariable(id2) = typed_expression.evaluates_to {
            if *id != id2 {
                println!("INSERTING ID{id} = {:?}", typed_expression.evaluates_to);
                type_variables.insert(*id, typed_expression.evaluates_to.clone());
            } else {
                println!("GET WORRIED WE'RE GETTING RECURSIVE");
            }
            // dbg!(typed_expression);
            // panic!("creating recursive thing");
        }
    }
    Ok(typed_expression)
}

fn infer_lvalue(lvalue: UntypedLValue) -> Result<LValue, Box<TypeError>> {
    Ok(match lvalue {
        UntypedLValue::Ident(x) => LValue::Ident(x),
        UntypedLValue::Int(x) => LValue::Int(x),
        UntypedLValue::Float(_) => todo!(),
        UntypedLValue::Boolean(_) => todo!(),
        UntypedLValue::String(_) => todo!(),
        UntypedLValue::Array(items) => todo!(),
    })
}

fn infer_assignment(
    lhs: Spanned<String>,
    rhs: Box<Spanned<UntypedExpression>>,
    scope: &mut Scope,
    type_variables: &mut TypeVariables,
) -> Result<TypedExpression, Box<TypeError>> {
    let (rhs, rhs_span) = *rhs;
    let current = scope
        .get(&lhs.0)
        .ok_or_else(|| TypeError::Undefined {
            ident: lhs.0.clone(),
            span: lhs.1,
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

    let rhs = infer_expr(rhs, scope, &current.inner, type_variables, rhs_span)?;
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

fn infer_binary_expression(
    lhs: Box<Spanned<UntypedExpression>>,
    rhs: Box<Spanned<UntypedExpression>>,
    operator: crate::ast::untyped::BinaryOperator,
    scope: &mut Scope,
    type_variables: &mut TypeVariables,
) -> Result<TypedExpression, Box<TypeError>> {
    let (lhs, lhs_span) = *lhs;
    let (rhs, rhs_span) = *rhs;
    // let (expected_operand_type, return_type, typed_operator) = {
    //     use crate::ast::typed::BinaryOperator::*;
    //     use Type::*;
    //     match operator {
    //         crate::ast::untyped::BinaryOperator::AddInt => (Int, Int, Add),
    //         crate::ast::untyped::BinaryOperator::MultiplyInt => (Int, Int, Multiply),
    //         crate::ast::untyped::BinaryOperator::DivideInt => (Int, Int, Divide),
    //         crate::ast::untyped::BinaryOperator::SubtractInt => (Int, Int, Subtract),
    //         crate::ast::untyped::BinaryOperator::AddFloat => (Float, Float, Add),
    //         crate::ast::untyped::BinaryOperator::MultiplyFloat => (Float, Float, Multiply),
    //         crate::ast::untyped::BinaryOperator::DivideFloat => (Float, Float, Divide),
    //         crate::ast::untyped::BinaryOperator::SubtractFloat => (Float, Float, Subtract),
    //         crate::ast::untyped::BinaryOperator::GreaterThan => (),
    //         crate::ast::untyped::BinaryOperator::LessThan => todo!(),
    //         crate::ast::untyped::BinaryOperator::LessThanOrEqual => todo!(),
    //         crate::ast::untyped::BinaryOperator::GreaterThanOrEqual => todo!(),
    //         crate::ast::untyped::BinaryOperator::Equal => todo!(),
    //     }
    // };
    //
    let (operator_args, return_type) = match scope.get(&operator.0) {
        Some(InferenceMetadata {
            inner: Type::Function { args, return_type },
            ..
        }) => (args.clone(), return_type.clone()),
        Some(InferenceMetadata { .. }) => unreachable!(),
        None => {
            return Err(Box::new(TypeError::Undefined {
                ident: operator.0,
                span: {
                    use chumsky::span::Span;
                    (lhs_span.start()..rhs_span.end()).into()
                },
            }));
        }
    };

    // let Some(Type::Function { args, return_type }) =  else {
    //
    // };
    let lhs = infer_expr(lhs, scope, &operator_args[0], type_variables, lhs_span)?;
    let rhs = infer_expr(rhs, scope, &operator_args[1], type_variables, rhs_span)?;
    if !(lhs
        .evaluates_to
        .resolve_comparsion(&operator_args[0], type_variables)
        && rhs
            .evaluates_to
            .resolve_comparsion(&operator_args[1], type_variables))
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
            lhs_expectation: operator_args[0].clone(),
            rhs_expectation: operator_args[1].clone(),
        }))
    } else {
        Ok(TypedExpression::new(
            Expression::BinaryExpression {
                operator: operator.0,
                lhs: (lhs, lhs_span),
                rhs: (rhs, rhs_span),
            },
            *return_type.clone(),
        ))
    }
}

fn infer_function_call(
    function: Box<Spanned<UntypedExpression>>,
    mut arguments: Vec<Spanned<UntypedExpression>>,
    expected_evaluates_to: &Type,
    scope: &mut Scope,
    type_variables: &mut TypeVariables,
    span: Span,
) -> Result<TypedExpression, Box<TypeError>> {
    println!("407 - starting to infer function call");

    dbg!(&type_variables);
    let (function, function_span) = *function;
    let function = infer_expr(
        function,
        scope,
        &Type::new_type_variable(),
        type_variables,
        function_span,
    )?;
    dbg!(&function);

    let function_details = match function.evaluates_to.clone() {
        Type::Function { args, return_type } => Some((args, return_type)),
        Type::TypeVariable(id) => {
            dbg!(&type_variables);
            let final_type = resolve_type_variable(id, type_variables);
            match final_type {
                Some(Type::Function { args, return_type }) => {
                    Some((args.clone(), return_type.clone()))
                }
                None => None,
                Some(_) => todo!("called on some non-function value"),
            }
        }
        got => {
            todo!()
        }
    };
    dbg!(&type_variables);

    let arguments = arguments
        .drain(0..)
        .map(|(argument, span)| {
            dbg!(&type_variables);
            Ok((
                infer_expr(argument, scope, expected_evaluates_to, type_variables, span)?,
                span,
            ))
        })
        .collect::<Result<Vec<_>, Box<TypeError>>>()?;

    dbg!(&type_variables);
    match function_details {
        Some((expected_arguments, return_type)) => {
            dbg!(&type_variables);
            if expected_arguments.len() != arguments.len() {
                return Err(Box::new(TypeError::WrongNumberOfArguments {
                    expected: expected_arguments.len(),
                    got: arguments.len(),
                }));
            }

            let first_incorrect_argument =
                arguments.iter().enumerate().find(|(idx, (argument, _))| {
                    !argument
                        .evaluates_to
                        .resolve_comparsion(&expected_arguments[*idx], type_variables)
                });

            if let Some((idx, (argument, span))) = first_incorrect_argument {
                return Err(Box::new(TypeError::IncorrectType {
                    expected: expected_arguments[idx].clone(),
                    got: argument.evaluates_to.clone(),
                    span: *span,
                }));
            }

            if !return_type.resolve_comparsion(expected_evaluates_to, type_variables) {
                return Err(Box::new(TypeError::IncorrectType {
                    expected: expected_evaluates_to.clone(),
                    got: *return_type,
                    span: span,
                }));
            }

            Ok(TypedExpression::new(
                Expression::FunctionCall {
                    function: Box::new((function, function_span)),
                    arguments,
                },
                *return_type,
            ))
        }
        None => {
            dbg!(&type_variables);
            let expected = Type::Function {
                args: arguments
                    .iter()
                    .map(|(argument, _)| argument.evaluates_to.clone())
                    .collect(),
                return_type: Box::new(expected_evaluates_to.clone()),
            };

            function
                .evaluates_to
                .resolve_comparsion(&expected, type_variables);

            Ok(TypedExpression::new(
                Expression::FunctionCall {
                    function: Box::new((function, function_span)),
                    arguments,
                },
                expected_evaluates_to.clone(),
            ))
        }
    }
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
    body: UntypedBlock, //Vec<Spanned<UntypedExpression>>,
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

    let (body, return_type) = infer_block(body, &mut internal_scope, type_variables)?;

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

fn infer_block(
    mut body: UntypedBlock,
    scope: &mut HashMap<String, InferenceMetadata<Type>>,
    type_variables: &mut HashMap<usize, Type>,
) -> Result<(Block, Type), Box<TypeError>> {
    let mut scope = scope.clone();
    let body = body
        .0
        .drain(0..)
        .map(|(expression, span)| {
            Ok((
                infer_expr(
                    expression,
                    &mut scope,
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
    Ok((Block(body), return_type))
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
        dbg!(&self);
        self.evaluates_to
            .substitute_type_variables(type_variables, used_type_variables);
        dbg!(&self);

        match &mut *self.expression {
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
                    body.substitute_type_variables(type_variables, used_type_variables);
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
            Expression::Block(block) => {
                block.substitute_type_variables(type_variables, used_type_variables);
            }
            Expression::Match { target, arms } => {
                target
                    .0
                    .substitute_type_variables(type_variables, used_type_variables);

                for arm in arms {
                    arm.substitute_type_variables(type_variables, used_type_variables);
                }
            }
        }
    }
}

impl MatchArm {
    fn substitute_type_variables(
        &mut self,
        type_variables: &TypeVariables,
        used_type_variables: &mut Vec<usize>,
    ) {
        self.rhs
            .0
            .substitute_type_variables(type_variables, used_type_variables);
    }
}

impl Block {
    fn substitute_type_variables(
        &mut self,
        type_variables: &TypeVariables,
        used_type_variables: &mut Vec<usize>,
    ) {
        for line in &mut self.0 {
            line.0
                .substitute_type_variables(type_variables, used_type_variables);
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
    fn aux(id: usize, type_variables: &TypeVariables, i: usize) -> Option<&Type> {
        if i > 99 {
            dbg!(type_variables);
            panic!("WE GOT RECURSIVE WITH id: {id}");
        }
        match type_variables.get(&id) {
            Some(Type::TypeVariable(id)) => aux(*id, type_variables, i + 1),
            Some(r#type) => Some(r#type),
            None => None,
        }
    }

    aux(id, type_variables, 0)
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
                    println!("{id} has been resolved to {:?}", r#type);
                    *self = r#type.clone();
                } else {
                    let variable_index = used_type_variables.iter().position(|it| it == id);
                    if let Some(position) = variable_index {
                        println!("'{id} has been NORMALIZED {position}");
                        *id = position;
                    } else {
                        let len = used_type_variables.len();
                        used_type_variables.push(*id);
                        println!("'{id} has been NORMALIZED {len}");
                        *id = len;
                    }
                }
            }
            _ => {}
        }
    }
}
