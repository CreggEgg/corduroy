use std::collections::HashMap;

use chumsky::span::SimpleSpan;

pub mod ast;
pub mod inference;
pub mod tokens;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);
pub type Scope<T> = HashMap<String, T>;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}
const SIMPLE_PROGRAM: &str = r#"
main = 1
"#;
const STARTER_PROGRAM: &str = r#"
main = () -> {
println("hello world")
} 
"#;

const EXAMPLE_PROGRAM: &str = r#"
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
const FUNCTION_WITH_ARGUMENTS: &str = r#"
main = () -> {
    add(5, 10)
}
add = (a, b: int) -> {
    a
}
"#;

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    #[test]
    fn tokenize() {
        let tokens = tokens::tokenize(EXAMPLE_PROGRAM).collect::<Vec<_>>();

        dbg!(tokens);
    }

    mod untyped {
        use crate::{
            FUNCTION_WITH_ARGUMENTS, SIMPLE_PROGRAM, STARTER_PROGRAM,
            ast::{
                self, parser,
                untyped::{
                    UntypedAnnotatedIdent, UntypedDefinition, UntypedExpression, UntypedFile,
                    UntypedLiteral,
                },
            },
            tokens,
        };
        use pretty_assertions::{assert_eq, assert_ne};

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
                                lhs: ("main".into(), (1..5).into()),
                                rhs: (
                                    UntypedExpression::Literal(UntypedLiteral::Function {
                                        arguments: vec![],
                                        body: vec![(
                                            UntypedExpression::FunctionCall {
                                                function: Box::new((
                                                    UntypedExpression::Ident("add".into()),
                                                    (20..23).into()
                                                )),
                                                arguments: vec![
                                                    (
                                                        UntypedExpression::Literal(
                                                            UntypedLiteral::Int(5)
                                                        ),
                                                        (24..25).into()
                                                    ),
                                                    (
                                                        UntypedExpression::Literal(
                                                            UntypedLiteral::Int(10)
                                                        ),
                                                        (27..29).into()
                                                    )
                                                ]
                                            },
                                            (20..30).into()
                                        )]
                                    }),
                                    (8..32).into()
                                )
                            },
                            (1..32).into()
                        ),
                        (
                            UntypedDefinition {
                                lhs: ("add".into(), (33..36).into()),
                                rhs: (
                                    UntypedExpression::Literal(UntypedLiteral::Function {
                                        arguments: vec![
                                            (UntypedAnnotatedIdent {
                                                ident: ("a".into(), (40..41).into()),
                                                annotation: None
                                            }),
                                            (UntypedAnnotatedIdent {
                                                ident: ("b".into(), (43..44).into()),
                                                annotation: Some(("int".into(), (46..49).into()))
                                            })
                                        ],
                                        body: vec![(
                                            UntypedExpression::Ident("a".into()),
                                            (60..61).into()
                                        )]
                                    }),
                                    (39..63).into()
                                )
                            },
                            (33..63).into()
                        )
                    ]
                }
            )
        }
    }

    mod typed {
        use pretty_assertions::{assert_eq, assert_ne};
        use std::collections::HashMap;

        use crate::{
            SIMPLE_PROGRAM, STARTER_PROGRAM,
            ast::{
                parser,
                typed::{Definition, Expression, File, Literal, Type, TypedExpression},
            },
            inference::{self, TypeError},
            tokens,
        };

        #[test]
        fn simple_infer() {
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
    }
}
