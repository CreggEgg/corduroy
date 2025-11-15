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
    println("hello world");
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
            SIMPLE_PROGRAM, STARTER_PROGRAM,
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
                                                (0..0).into()
                                            )),
                                            arguments: vec![(
                                                UntypedExpression::Literal(UntypedLiteral::String(
                                                    "hello world".into()
                                                )),
                                                (0..0).into()
                                            )]
                                        },
                                        (0..0).into()
                                    )]
                                }),
                                (8..9).into()
                            )
                        },
                        (1..9).into()
                    )]
                }
            )
        }
    }

    mod typed {
        use pretty_assertions::{assert_eq, assert_ne};
        use std::collections::HashMap;

        use crate::{
            SIMPLE_PROGRAM,
            ast::{
                parser,
                typed::{Definition, Expression, File, Literal, Type, TypedExpression},
            },
            inference, tokens,
        };

        #[test]
        fn simple_infer() {
            let tokens = tokens::tokenize(SIMPLE_PROGRAM);

            let ast = parser::parse(tokens, SIMPLE_PROGRAM.len()).unwrap();

            let inferred = inference::infer_ast(ast, HashMap::new());

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
    }
}
