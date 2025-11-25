use ast::{
    parser::{self, ParseError},
    untyped::UntypedFile,
};
use chumsky::span::SimpleSpan;
use std::collections::HashMap;

pub mod ast;
pub mod inference;
pub mod reporting;
pub mod tokens;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);
pub type Scope<T> = HashMap<String, T>;

pub fn parse_file(file: &str) -> Result<UntypedFile, ParseError<'_>> {
    let tokens = tokens::tokenize(file);

    parser::parse(tokens, file.len())
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
    pub const ARRAYS_AND_VARIABLES: &str = r#"
main = () -> {
    let my_array = [5, 10 + 15];
    mut x = 5;
    x = 10
}
"#;
    pub const CASES_AND_RECURSION: &str = r#"
fib = (n) -> {
    match n {
        0 -> 1,
        1 -> 1,
        n -> fib(n-1) + fib(n-2),
    }
}
main = () -> {
    let x = fib(5) > 10;
    match x {
        true -> {
            println("fib(5) is greater than 10")
        },
        false -> {
            println("fib(5) is not greater than 10")
        }
    }
}
"#;
    pub const RECURSIVE_TYPES_BUG: &str = r#"
fib = (n) -> {
    match n {
        x -> n+1
    }
}
main = () -> {
    fib(5)
}
"#;
}

#[cfg(test)]
mod tests {
    use std::io::BufWriter;

    use example_programs::*;

    use crate::inference::{DefinitionType, InferenceMetadata};

    use super::*;

    fn parse(file: &str, file_content: &str) -> UntypedFile {
        match parse_file(file_content) {
            Ok(ast) => ast,
            Err(error) => {
                let writer = BufWriter::new(std::io::stdout());
                error.report(file, file_content, writer);
                panic!();
            }
        }
    }

    fn infer(file: &str, file_content: &str) -> ast::typed::File {
        let ast = parse(file, file_content);
        use crate::ast::typed::Type;

        Type::reset_type_variable_counter();
        match crate::inference::infer_ast(
            ast,
            HashMap::from([
                (
                    "int".into(),
                    InferenceMetadata {
                        inner: Type::Int,
                        definition_type: DefinitionType::CompilerProvided,
                    },
                ),
                (
                    "println".into(),
                    InferenceMetadata {
                        inner: Type::Function {
                            args: vec![Type::String],
                            return_type: Box::new(Type::Unit),
                        },
                        definition_type: DefinitionType::CompilerProvided,
                    },
                ),
                (
                    "+".into(),
                    InferenceMetadata {
                        inner: Type::Function {
                            args: vec![Type::Int, Type::Int],
                            return_type: Box::new(Type::Int),
                        },
                        definition_type: DefinitionType::CompilerProvided,
                    },
                ),
                (
                    "-".into(),
                    InferenceMetadata {
                        inner: Type::Function {
                            args: vec![Type::Int, Type::Int],
                            return_type: Box::new(Type::Int),
                        },
                        definition_type: DefinitionType::CompilerProvided,
                    },
                ),
                (
                    "*".into(),
                    InferenceMetadata {
                        inner: Type::Function {
                            args: vec![Type::Int, Type::Int],
                            return_type: Box::new(Type::Int),
                        },
                        definition_type: DefinitionType::CompilerProvided,
                    },
                ),
                (
                    "/".into(),
                    InferenceMetadata {
                        inner: Type::Function {
                            args: vec![Type::Int, Type::Int],
                            return_type: Box::new(Type::Int),
                        },
                        definition_type: DefinitionType::CompilerProvided,
                    },
                ),
                (
                    "*.".into(),
                    InferenceMetadata {
                        inner: Type::Function {
                            args: vec![Type::Float, Type::Float],
                            return_type: Box::new(Type::Float),
                        },
                        definition_type: DefinitionType::CompilerProvided,
                    },
                ),
                (
                    "/.".into(),
                    InferenceMetadata {
                        inner: Type::Function {
                            args: vec![Type::Float, Type::Float],
                            return_type: Box::new(Type::Float),
                        },
                        definition_type: DefinitionType::CompilerProvided,
                    },
                ),
            ]),
        ) {
            Ok(ast) => ast,
            Err(error) => {
                let writer = BufWriter::new(std::io::stdout());
                error.report(file, file_content, writer);
                panic!();
            }
        }
    }

    #[test]
    fn tokenize() {
        let tokens = tokens::tokenize(EXAMPLE_PROGRAM).collect::<Vec<_>>();

        dbg!(tokens);
    }

    mod untyped {
        use crate::ast::untyped::{BinaryOperator, UntypedLValue};
        use crate::ast::untyped::{
            UntypedAnnotatedIdent, UntypedDefinition, UntypedExpression, UntypedFile,
            UntypedLiteral,
        };
        use crate::example_programs::*;
        use crate::tests::parse;
        use pretty_assertions::assert_eq;

        #[test]
        fn simple_parse() {
            let ast = parse("SIMPLE_PROGRAM", SIMPLE_PROGRAM);

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
            let ast = parse("STARTER_PROGRAM", STARTER_PROGRAM);

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
                                    .into()
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
            let ast = parse("FUNCTION_WITH_ARGUMENTS", FUNCTION_WITH_ARGUMENTS);

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
                                        .into()
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
                                        .into()
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
            let ast = parse("BINARY_OP", BINARY_OP);

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
                                                operator: BinaryOperator("+".to_string()),
                                                rhs: Box::new((
                                                    UntypedExpression::BinaryExpression {
                                                        lhs: Box::new((
                                                            UntypedExpression::Ident("b".into()),
                                                            (43..44).into()
                                                        )),
                                                        operator: BinaryOperator("*".to_string()),
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
                                        .into()
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
                                        .into()
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
        #[test]
        fn arrays_and_variables_parse() {
            let ast = parse("ARRAYS_AND_VARIABLES", ARRAYS_AND_VARIABLES);

            assert_eq!(
                ast,
                UntypedFile {
                    definitions: vec![(
                        UntypedDefinition {
                            lhs: ("main".into(), (1..5).into()),
                            rhs: (
                                UntypedExpression::Literal(UntypedLiteral::Function {
                                    arguments: vec![],
                                    body: vec![
                                        (
                                            UntypedExpression::Definition {
                                                mutable: (false, (20..23).into()),
                                                lhs: UntypedLValue::Ident((
                                                    "my_array".into(),
                                                    (24..32).into()
                                                )),

                                                rhs: Box::new((
                                                    UntypedExpression::Literal(
                                                        UntypedLiteral::Array(vec![
                                                            (
                                                                UntypedExpression::Literal(
                                                                    UntypedLiteral::Int(5)
                                                                ),
                                                                (36..37).into()
                                                            ),
                                                            (UntypedExpression::BinaryExpression {
                                                                lhs: Box::new((
                                                                    UntypedExpression::Literal(
                                                                        UntypedLiteral::Int(10)
                                                                    ),
                                                                    (39..41).into()
                                                                )),
                                                                operator: BinaryOperator("+".to_string()),
                                                                rhs: Box::new((
                                                                    UntypedExpression::Literal(
                                                                        UntypedLiteral::Int(15)
                                                                    ),
                                                                    (44..46).into()
                                                                ))
                                                            }, (39..46).into())
                                                        ])
                                                    ),
                                                    (35..47).into()
                                                ))
                                            },
                                            (20..47).into()
                                        ),
                                        (
                                            UntypedExpression::Definition {
                                                mutable: (true, (53..56).into()),
                                                lhs: UntypedLValue::Ident((
                                                    "x".into(),
                                                    (57..58).into()
                                                )),

                                                rhs: Box::new((
                                                    UntypedExpression::Literal(
                                                        UntypedLiteral::Int(5)
                                                    ),
                                                    (61..62).into()
                                                ))
                                            },
                                            (53..62).into()
                                        ),
                                        (
                                            UntypedExpression::Assignment {
                                                lhs: ("x".into(), (68..69).into()),
                                                rhs: Box::new((
                                                    UntypedExpression::Literal(
                                                        UntypedLiteral::Int(10)
                                                    ),
                                                    (72..74).into()
                                                ))
                                            },
                                            (68..74).into()
                                        )
                                    ]
                                    .into()
                                }),
                                (8..76).into()
                            )
                        },
                        (1..76).into()
                    )]
                }
            )
        }
    }

    mod typed {
        use crate::{
            ast::typed::{AnnotatedIdent, LValue},
            example_programs::*,
            tests::infer,
        };
        use pretty_assertions::assert_eq;

        use crate::ast::typed::{Definition, Expression, File, Literal, Type, TypedExpression};

        macro_rules! int {
            ($expr:expr) => {
                TypedExpression::new(Expression::Literal(Literal::Int($expr)), Type::Int)
            };
        }

        // macro_rules! function_definition {
        //     ($name:expr,$args:expr,$start:expr,$end:expr) => {};
        // }

        #[test]
        fn simple_infer() {
            Type::reset_type_variable_counter();

            let inferred = infer("SIMPLE_PROGRAM", SIMPLE_PROGRAM);

            assert_eq!(
                inferred,
                File {
                    definitions: vec![(
                        Definition {
                            lhs: ("main".into(), (1..5).into()),
                            rhs: (int!(1), (8..9).into())
                        },
                        (1..9).into()
                    )]
                }
            )
        }

        #[test]
        fn hello_world_infer() {
            Type::reset_type_variable_counter();

            let inferred = infer("STARTER_PROGRAM", STARTER_PROGRAM);

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
                                        .into()
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
            println!("HELP ME DEAR GOD PLEASE");
            let inferred = infer("FUNCTION_WITH_ARGUMENTS", FUNCTION_WITH_ARGUMENTS);
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
                                            ),]
                                            .into(),
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
                                                            (int!(5), (55..56).into(),),
                                                            (int!(10), (58..60).into(),),
                                                        ],
                                                    },
                                                    Type::Int,
                                                ),
                                                (51..61).into(),
                                            ),]
                                            .into(),
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
        fn recursive_types_bug() {
            println!("HELP ME DEAR GOD PLEASE");
            let inferred = infer("RECURSIVE_TYPES_BUG", RECURSIVE_TYPES_BUG);
        }

        #[test]
        fn binary_op_infer() {
            println!("698");
            let inferred = infer("BINARY_OP", BINARY_OP);

            dbg!(&inferred);
            assert_eq!(
                inferred,
                File {
                    definitions: vec![
                        (
                            Definition {
                                lhs: ("my_cool_function".into(), (1..17).into()),
                                rhs: (
                                    TypedExpression::new(
                                        Expression::Literal(Literal::Function {
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
                                                TypedExpression::new(
                                                    Expression::BinaryExpression {
                                                        lhs: (
                                                            TypedExpression::new(
                                                                Expression::Ident("a".into()),
                                                                Type::Int,
                                                            ),
                                                            (39..40).into(),
                                                        ),
                                                        operator: "+".into(),
                                                        rhs: (
                                                            TypedExpression::new(
                                                                Expression::BinaryExpression {
                                                                    lhs: (
                                                                        TypedExpression::new(
                                                                            Expression::Ident(
                                                                                "b".into(),
                                                                            ),
                                                                            Type::Int,
                                                                        ),
                                                                        (43..44).into(),
                                                                    ),
                                                                    operator: "*".into(),
                                                                    rhs: (
                                                                        TypedExpression::new(
                                                                            Expression::Ident(
                                                                                "c".into(),
                                                                            ),
                                                                            Type::Int,
                                                                        ),
                                                                        (47..48).into(),
                                                                    ),
                                                                },
                                                                Type::Int,
                                                            ),
                                                            (43..48).into(),
                                                        ),
                                                    },
                                                    Type::Int,
                                                ),
                                                (39..48).into(),
                                            ),]
                                            .into(),
                                        },),
                                        Type::Function {
                                            args: vec![Type::Int, Type::Int, Type::Int,],
                                            return_type: Box::new(Type::Int),
                                        },
                                    ),
                                    (20..50).into(),
                                ),
                            },
                            (1..50).into(),
                        ),
                        (
                            Definition {
                                lhs: ("main".into(), (51..55).into()),
                                rhs: (
                                    TypedExpression::new(
                                        Expression::Literal(Literal::Function {
                                            arguments: vec![],
                                            body: vec![(
                                                TypedExpression::new(
                                                    Expression::FunctionCall {
                                                        function: Box::new((
                                                            TypedExpression::new(
                                                                Expression::Ident(
                                                                    "my_cool_function".into(),
                                                                ),
                                                                Type::Function {
                                                                    args: vec![
                                                                        Type::Int,
                                                                        Type::Int,
                                                                        Type::Int,
                                                                    ],
                                                                    return_type: Box::new(
                                                                        Type::Int
                                                                    ),
                                                                },
                                                            ),
                                                            (70..86).into(),
                                                        )),
                                                        arguments: vec![
                                                            (int!(5), (87..88).into(),),
                                                            (int!(10), (90..92).into(),),
                                                            (int!(3), (94..95).into(),),
                                                        ],
                                                    },
                                                    Type::Int,
                                                ),
                                                (70..96).into(),
                                            ),]
                                            .into(),
                                        },),
                                        Type::Function {
                                            args: vec![],
                                            return_type: Box::new(Type::Int),
                                        },
                                    ),
                                    (58..98).into(),
                                ),
                            },
                            (51..98).into(),
                        ),
                    ],
                }
            )
        }

        #[test]
        fn arrays_and_variables_infer() {
            let inferred = infer("ARRAYS_AND_VARIABLES", ARRAYS_AND_VARIABLES);
            // dbg!(&inferred);
            //
            let body = vec![
                (
                    TypedExpression::new(
                        Expression::Definition {
                            lhs: (
                                LValue::Ident(("my_array".into(), (24..32).into())),
                                (24..32).into(),
                            ),
                            rhs: Box::new((
                                TypedExpression::new(
                                    Expression::Literal(Literal::Array(vec![
                                        (int!(5), (36..37).into()),
                                        (
                                            TypedExpression::new(
                                                Expression::BinaryExpression {
                                                    lhs: (int!(10), (39..41).into()),
                                                    operator: "+".to_string(),
                                                    rhs: (int!(15), (44..46).into()),
                                                },
                                                Type::Int,
                                            ),
                                            (39..46).into(),
                                        ),
                                    ])),
                                    Type::Array(Box::new(Type::Int), 2),
                                ),
                                (35..47).into(),
                            )),
                            mutable: false,
                        },
                        Type::Unit,
                    ),
                    (20..47).into(),
                ),
                (
                    TypedExpression::new(
                        Expression::Definition {
                            lhs: (
                                LValue::Ident(("x".into(), (57..58).into())),
                                (57..58).into(),
                            ),
                            rhs: Box::new((int!(5), (61..62).into())),
                            mutable: true,
                        },
                        Type::Unit,
                    ),
                    (53..62).into(),
                ),
                (
                    TypedExpression::new(
                        Expression::Assignment {
                            lhs: ("x".into(), (68..69).into()),
                            rhs: Box::new((int!(10), (72..74).into())),
                        },
                        Type::Unit,
                    ),
                    (68..74).into(),
                ),
            ];

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
                                        body: body.into()
                                    }),
                                    Type::Function {
                                        args: vec![],
                                        return_type: Box::new(Type::Unit)
                                    }
                                ),
                                (8..76).into()
                            )
                        },
                        (1..76).into()
                    )]
                }
            );
        }

        #[test]
        fn cases_and_recursion_parse_and_infer() {
            let inferred = infer("CASES_AND_RECURSION", CASES_AND_RECURSION);

            assert_eq!(
                inferred,
                File {
                    definitions: todo!()
                }
            )
        }
    }
}
