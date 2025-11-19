use std::collections::HashMap;

use ast::{
    parser::{self, ParseError},
    untyped::UntypedFile,
};
use chumsky::span::SimpleSpan;

pub mod ast;
pub mod inference;
pub mod tokens;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);
pub type Scope<T> = HashMap<String, T>;

pub fn parse_file(file: &str) -> Result<UntypedFile, ParseError> {
    let tokens = tokens::tokenize(file);

    let ast = parser::parse(tokens, file.len());
    ast
}

#[allow(unused)]
mod example_programs {
    pub const SIMPLE_PROGRAM: &str = r#"
main = 1
"#;
    pub const STARTER_PROGRAM: &str = r#"
main = () -> {
println("hello world")
} 
"#;

    pub const EXAMPLE_PROGRAM: &str = r#"
main = () -> {
    println("hello world");
    let my_string = "hi there";
    creates_reference(my_string);
    println(my_string);
}
creates_reference = (x: string) -> {
    println(x);
}
"#;
    pub const FUNCTION_WITH_ARGUMENTS: &str = r#"
add = (a, b: int) -> {
    a
}
main = () -> {
    add(5, 10)
}
"#;
    pub const BINARY_OP: &str = r#"
my_cool_function = (a, b, c) -> {
    a + b * c
}
main = () -> {
    my_cool_function(5, 10, 3)
}
"#;
}

#[cfg(test)]
mod tests {
    use example_programs::*;

    use super::*;

    #[test]
    fn tokenize() {
        let tokens = tokens::tokenize(EXAMPLE_PROGRAM).collect::<Vec<_>>();

        dbg!(tokens);
    }

    mod untyped {
        use crate::ast::untyped::BinaryOperator;
        use crate::example_programs::*;
        use crate::{
            ast::{
                parser,
                untyped::{
                    UntypedAnnotatedIdent, UntypedDefinition, UntypedExpression, UntypedFile,
                    UntypedLiteral,
                },
            },
            tokens,
        };
        use pretty_assertions::assert_eq;

        #[test]
        fn simple_parse() {
            let tokens = tokens::tokenize(SIMPLE_PROGRAM);

            let ast = parser::parse(tokens, SIMPLE_PROGRAM.len()).unwrap();

            assert_eq!(
                ast,
                UntypedFile {
                    definitions: vec![(
                        UntypedDefinition {
                            lhs: ("main".into(), (1..5).into()),
                            rhs: (
                                UntypedExpression::Literal(UntypedLiteral::Int(1)),
                                (8..9).into()
                            )
                        },
                        (1..9).into()
                    )]
                }
            )
        }

        #[test]
        fn hello_world_parse() {
            let tokens = tokens::tokenize(STARTER_PROGRAM);

            let ast = parser::parse(tokens, STARTER_PROGRAM.len()).unwrap();

            assert_eq!(
                ast,
                UntypedFile {
                    definitions: vec![(
                        UntypedDefinition {
                            lhs: ("main".into(), (1..5).into()),
                            rhs: (
                                UntypedExpression::Literal(UntypedLiteral::Function {
                                    arguments: vec![],
                                    body: vec![(
                                        UntypedExpression::FunctionCall {
                                            function: Box::new((
                                                UntypedExpression::Ident("println".into()),
                                                (16..23).into()
                                            )),
                                            arguments: vec![(
                                                UntypedExpression::Literal(UntypedLiteral::String(
                                                    "hello world".into()
                                                )),
                                                (24..37).into()
                                            )]
                                        },
                                        (16..38).into()
                                    )]
                                }),
                                (8..40).into()
                            )
                        },
                        (1..40).into()
                    )]
                }
            )
        }
        #[test]
        fn arguments_parse() {
            let tokens = tokens::tokenize(FUNCTION_WITH_ARGUMENTS);

            let ast = parser::parse(tokens, FUNCTION_WITH_ARGUMENTS.len()).unwrap();

            assert_eq!(
                ast,
                UntypedFile {
                    definitions: vec![
                        (
                            UntypedDefinition {
                                lhs: ("add".into(), (1..4).into()),
                                rhs: (
                                    UntypedExpression::Literal(UntypedLiteral::Function {
                                        arguments: vec![
                                            (UntypedAnnotatedIdent {
                                                ident: ("a".into(), (8..9).into()),
                                                annotation: None
                                            }),
                                            (UntypedAnnotatedIdent {
                                                ident: ("b".into(), (11..12).into()),
                                                annotation: Some(("int".into(), (14..17).into()))
                                            })
                                        ],
                                        body: vec![(
                                            UntypedExpression::Ident("a".into()),
                                            (28..29).into()
                                        )]
                                    }),
                                    (7..31).into()
                                )
                            },
                            (1..31).into()
                        ),
                        (
                            UntypedDefinition {
                                lhs: ("main".into(), (32..36).into()),
                                rhs: (
                                    UntypedExpression::Literal(UntypedLiteral::Function {
                                        arguments: vec![],
                                        body: vec![(
                                            UntypedExpression::FunctionCall {
                                                function: Box::new((
                                                    UntypedExpression::Ident("add".into()),
                                                    (51..54).into()
                                                )),
                                                arguments: vec![
                                                    (
                                                        UntypedExpression::Literal(
                                                            UntypedLiteral::Int(5)
                                                        ),
                                                        (55..56).into()
                                                    ),
                                                    (
                                                        UntypedExpression::Literal(
                                                            UntypedLiteral::Int(10)
                                                        ),
                                                        (58..60).into()
                                                    )
                                                ]
                                            },
                                            (51..61).into()
                                        )]
                                    }),
                                    (39..63).into()
                                )
                            },
                            (32..63).into()
                        ),
                    ]
                }
            )
        }

        #[test]
        fn parse_binary_op() {
            let tokens = tokens::tokenize(BINARY_OP);

            let ast = parser::parse(tokens, BINARY_OP.len()).unwrap();

            assert_eq!(
                ast,
                UntypedFile {
                    definitions: vec![
                        (
                            UntypedDefinition {
                                lhs: ("my_cool_function".into(), (1..17).into()),
                                rhs: (
                                    UntypedExpression::Literal(UntypedLiteral::Function {
                                        arguments: vec![
                                            (UntypedAnnotatedIdent {
                                                ident: ("a".into(), (21..22).into()),
                                                annotation: None
                                            }),
                                            (UntypedAnnotatedIdent {
                                                ident: ("b".into(), (24..25).into()),
                                                annotation: None
                                            }),
                                            (UntypedAnnotatedIdent {
                                                ident: ("c".into(), (27..28).into()),
                                                annotation: None
                                            })
                                        ],
                                        body: vec![(
                                            UntypedExpression::BinaryExpression {
                                                lhs: Box::new((
                                                    UntypedExpression::Ident("a".into()),
                                                    (39..40).into()
                                                )),
                                                operator: BinaryOperator::AddInt,
                                                rhs: Box::new((
                                                    UntypedExpression::BinaryExpression {
                                                        lhs: Box::new((
                                                            UntypedExpression::Ident("b".into()),
                                                            (43..44).into()
                                                        )),
                                                        operator: BinaryOperator::MultiplyInt,
                                                        rhs: Box::new((
                                                            UntypedExpression::Ident("c".into()),
                                                            (47..48).into()
                                                        ))
                                                    },
                                                    (43..48).into()
                                                ))
                                            },
                                            // UntypedExpression::Ident("a".into()),
                                            (39..48).into()
                                        )]
                                    }),
                                    (20..50).into()
                                )
                            },
                            (1..50).into()
                        ),
                        (
                            UntypedDefinition {
                                lhs: ("main".into(), (51..55).into()),
                                rhs: (
                                    UntypedExpression::Literal(UntypedLiteral::Function {
                                        arguments: vec![],
                                        body: vec![(
                                            UntypedExpression::FunctionCall {
                                                function: Box::new((
                                                    UntypedExpression::Ident(
                                                        "my_cool_function".into()
                                                    ),
                                                    (70..86).into()
                                                )),
                                                arguments: vec![
                                                    (
                                                        UntypedExpression::Literal(
                                                            UntypedLiteral::Int(5)
                                                        ),
                                                        (87..88).into()
                                                    ),
                                                    (
                                                        UntypedExpression::Literal(
                                                            UntypedLiteral::Int(10)
                                                        ),
                                                        (90..92).into()
                                                    ),
                                                    (
                                                        UntypedExpression::Literal(
                                                            UntypedLiteral::Int(3)
                                                        ),
                                                        (94..95).into()
                                                    )
                                                ]
                                            },
                                            (70..96).into()
                                        )]
                                    }),
                                    (58..98).into()
                                )
                            },
                            (51..98).into()
                        ),
                    ]
                }
            );
        }
    }

    mod typed {
        use crate::{
            ast::typed::{AnnotatedIdent, BinaryOperator},
            example_programs::*,
        };
        use pretty_assertions::assert_eq;
        use std::collections::HashMap;

        use crate::{
            ast::{
                parser,
                typed::{Definition, Expression, File, Literal, Type, TypedExpression},
            },
            inference, tokens,
        };

        #[test]
        fn simple_infer() {
            Type::reset_type_variable_counter();
            let tokens = tokens::tokenize(SIMPLE_PROGRAM);

            let ast = parser::parse(tokens, SIMPLE_PROGRAM.len()).unwrap();

            let inferred = inference::infer_ast(ast, HashMap::new()).unwrap();

            assert_eq!(
                inferred,
                File {
                    definitions: vec![(
                        Definition {
                            lhs: ("main".into(), (1..5).into()),
                            rhs: (
                                TypedExpression::new(
                                    Expression::Literal(Literal::Int(1)),
                                    Type::Int
                                ),
                                (8..9).into()
                            )
                        },
                        (1..9).into()
                    )]
                }
            )
        }

        #[test]
        fn hello_world_infer() {
            Type::reset_type_variable_counter();
            let tokens = tokens::tokenize(STARTER_PROGRAM);

            let ast = parser::parse(tokens, STARTER_PROGRAM.len()).unwrap();

            let inferred = inference::infer_ast(
                ast,
                HashMap::from([(
                    "println".into(),
                    Type::Function {
                        args: vec![Type::String],
                        return_type: Box::new(Type::Unit),
                    },
                )]),
            )
            .unwrap();

            assert_eq!(
                inferred,
                File {
                    definitions: vec![(
                        Definition {
                            lhs: ("main".into(), (1..5).into()),
                            rhs: (
                                TypedExpression::new(
                                    Expression::Literal(Literal::Function {
                                        arguments: vec![],
                                        body: vec![(
                                            TypedExpression::new(
                                                Expression::FunctionCall {
                                                    function: Box::new((
                                                        TypedExpression::new(
                                                            Expression::Ident("println".into()),
                                                            Type::Function {
                                                                args: vec![Type::String],
                                                                return_type: Box::new(Type::Unit)
                                                            }
                                                        ),
                                                        (16..23).into()
                                                    )),
                                                    arguments: vec![(
                                                        TypedExpression::new(
                                                            Expression::Literal(Literal::String(
                                                                "hello world".into()
                                                            )),
                                                            Type::String
                                                        ),
                                                        (24..37).into()
                                                    )]
                                                },
                                                Type::Unit
                                            ),
                                            (16..38).into()
                                        )]
                                    }),
                                    Type::Function {
                                        args: vec![],
                                        return_type: Box::new(Type::Unit)
                                    }
                                ),
                                (8..40).into()
                            )
                        },
                        (1..40).into()
                    )]
                }
            )
        }
        #[test]
        fn arguments_infer() {
            Type::reset_type_variable_counter();
            let tokens = tokens::tokenize(FUNCTION_WITH_ARGUMENTS);

            let ast = parser::parse(tokens, FUNCTION_WITH_ARGUMENTS.len()).unwrap();

            let inferred = inference::infer_ast(
                ast,
                HashMap::from([
                    ("int".into(), Type::Int),
                    (
                        "println".into(),
                        Type::Function {
                            args: vec![Type::String],
                            return_type: Box::new(Type::Unit),
                        },
                    ),
                ]),
            )
            .unwrap();
            dbg!(&inferred);

            assert_eq!(
                inferred,
                File {
                    definitions: vec![
                        (
                            Definition {
                                lhs: ("add".into(), (1..4).into()),
                                rhs: (
                                    TypedExpression::new(
                                        Expression::Literal(Literal::Function {
                                            arguments: vec![
                                                AnnotatedIdent {
                                                    ident: ("a".into(), (8..9).into()),
                                                    annotation: (
                                                        Type::TypeVariable(0),
                                                        (8..9).into()
                                                    ),
                                                },
                                                AnnotatedIdent {
                                                    ident: ("b".into(), (11..12).into()),
                                                    annotation: (Type::Int, (14..17).into()),
                                                },
                                            ],
                                            body: vec![(
                                                TypedExpression::new(
                                                    Expression::Ident("a".into()),
                                                    Type::TypeVariable(0),
                                                ),
                                                (28..29).into(),
                                            ),],
                                        },),
                                        Type::Function {
                                            args: vec![Type::TypeVariable(0,), Type::Int,],
                                            return_type: Box::new(Type::TypeVariable(0,)),
                                        },
                                    ),
                                    (7..31).into()
                                ),
                            },
                            (1..31).into(),
                        ),
                        (
                            Definition {
                                lhs: ("main".into(), (32..36).into()),
                                rhs: (
                                    TypedExpression::new(
                                        Expression::Literal(Literal::Function {
                                            arguments: vec![],
                                            body: vec![(
                                                TypedExpression::new(
                                                    Expression::FunctionCall {
                                                        function: Box::new((
                                                            TypedExpression::new(
                                                                Expression::Ident("add".into()),
                                                                Type::Function {
                                                                    args: vec![
                                                                        Type::Int,
                                                                        Type::Int,
                                                                    ],
                                                                    return_type: Box::new(
                                                                        Type::Int
                                                                    ),
                                                                },
                                                            ),
                                                            (51..54).into(),
                                                        )),
                                                        arguments: vec![
                                                            (
                                                                TypedExpression::new(
                                                                    Expression::Literal(
                                                                        Literal::Int(5,),
                                                                    ),
                                                                    Type::Int,
                                                                ),
                                                                (55..56).into(),
                                                            ),
                                                            (
                                                                TypedExpression::new(
                                                                    Expression::Literal(
                                                                        Literal::Int(10,),
                                                                    ),
                                                                    Type::Int,
                                                                ),
                                                                (58..60).into(),
                                                            ),
                                                        ],
                                                    },
                                                    Type::Int,
                                                ),
                                                (51..61).into(),
                                            ),],
                                        },),
                                        Type::Function {
                                            args: vec![],
                                            return_type: Box::new(Type::Int)
                                        },
                                    ),
                                    (39..63).into(),
                                ),
                            },
                            (32..63).into(),
                        ),
                    ],
                }
            )
        }

        #[test]
        fn binary_op_infer() {
            let tokens = tokens::tokenize(BINARY_OP);

            let ast = parser::parse(tokens, BINARY_OP.len()).unwrap();

            let inferred = inference::infer_ast(
                ast,
                HashMap::from([
                    ("int".into(), Type::Int),
                    (
                        "println".into(),
                        Type::Function {
                            args: vec![Type::String],
                            return_type: Box::new(Type::Unit),
                        },
                    ),
                ]),
            )
            .unwrap();

            dbg!(&inferred);
            assert_eq!(
                inferred,
                File {
                    definitions: vec![
                        (
                            Definition {
                                lhs: ("my_cool_function".into(), (1..17).into()),
                                rhs: (
                                    TypedExpression {
                                        expression: Expression::Literal(Literal::Function {
                                            arguments: vec![
                                                AnnotatedIdent {
                                                    ident: ("a".into(), (21..22).into()),
                                                    annotation: (Type::Int, (21..22).into()),
                                                },
                                                AnnotatedIdent {
                                                    ident: ("b".into(), (24..25).into()),
                                                    annotation: (Type::Int, (24..25).into(),),
                                                },
                                                AnnotatedIdent {
                                                    ident: ("c".into(), (27..28).into()),
                                                    annotation: (Type::Int, (27..28).into()),
                                                },
                                            ],
                                            body: vec![(
                                                TypedExpression {
                                                    expression: Expression::BinaryExpression {
                                                        lhs: Box::new((
                                                            TypedExpression {
                                                                expression: Expression::Ident(
                                                                    "a".into()
                                                                ),
                                                                evaluates_to: Type::Int,
                                                            },
                                                            (39..40).into(),
                                                        )),
                                                        operator: BinaryOperator::Add,
                                                        rhs: Box::new((
                                                            TypedExpression {
                                                                expression:
                                                                    Expression::BinaryExpression {
                                                                        lhs: Box::new((
                                                                            TypedExpression {
                                                                                expression: Expression::Ident(
                                                                                    "b".into(),
                                                                                ),
                                                                                evaluates_to: Type::Int,
                                                                            },
                                                                            (43..44).into(),
                                                                        )),
                                                                        operator: BinaryOperator::Multiply,
                                                                        rhs: Box::new((
                                                                            TypedExpression {
                                                                                expression: Expression::Ident(
                                                                                    "c".into(),
                                                                                ),
                                                                                evaluates_to: Type::Int,
                                                                            },
                                                                            (47..48).into(),
                                                                        )),
                                                                    },
                                                                evaluates_to: Type::Int,
                                                            },
                                                            (43..48).into(),
                                                        )),
                                                    },
                                                    evaluates_to: Type::Int,
                                                },
                                                (39..48).into(),
                                            ),],
                                        },),
                                        evaluates_to: Type::Function {
                                            args: vec![Type::Int, Type::Int, Type::Int,],
                                            return_type: Box::new(Type::Int),
                                        },
                                    },
                                    (20..50).into(),
                                ),
                            },
                            (1..50).into(),
                        ),
                        (
                            Definition {
                                lhs: ("main".into(), (51..55).into()),
                                rhs: (
                                    TypedExpression {
                                        expression: Expression::Literal(Literal::Function {
                                            arguments: vec![],
                                            body: vec![(
                                                TypedExpression {
                                                    expression: Expression::FunctionCall {
                                                        function: Box::new((
                                                            TypedExpression {
                                                                expression: Expression::Ident(
                                                                    "my_cool_function".into(),
                                                                ),
                                                                evaluates_to: Type::Function {
                                                                    args: vec![Type::Int, Type::Int, Type::Int,],
                                                                    return_type: Box::new(Type::Int),
                                                                },
                                                            },
                                                            (70..86).into(),
                                                        )),
                                                        arguments: vec![
                                                            (
                                                                TypedExpression {
                                                                    expression: Expression::Literal(Literal::Int(5,),),
                                                                    evaluates_to: Type::Int,
                                                                },
                                                                (87..88).into(),
                                                            ),
                                                            (
                                                                TypedExpression {
                                                                    expression: Expression::Literal(Literal::Int(10,),),
                                                                    evaluates_to: Type::Int,
                                                                },
                                                                (90..92).into(),
                                                            ),
                                                            (
                                                                TypedExpression {
                                                                    expression: Expression::Literal(Literal::Int(3,),),
                                                                    evaluates_to: Type::Int,
                                                                },
                                                                (94..95).into(),
                                                            ),
                                                        ],
                                                    },
                                                    evaluates_to: Type::Int,
                                                },
                                                (70..96).into(),
                                            ),],
                                        },),
                                        evaluates_to: Type::Function {
                                            args: vec![],
                                            return_type: Box::new(Type::Int),
                                        },
                                    },
                                    (58..98).into(),
                                ),
                            },
                            (51..98).into(),
                        ),
                    ],
                }
            )
        }
    }
}
