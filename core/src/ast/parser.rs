use chumsky::{
    IterParser, Parser,
    error::Simple,
    extra,
    input::{Input, Stream, ValueInput},
    prelude::{choice, just, recursive},
    select,
};

use crate::{Span, Spanned, tokens::Token};

use super::untyped::{
    BinaryOperator, UntypedAnnotatedIdent, UntypedDefinition, UntypedExpression, UntypedFile,
    UntypedLiteral,
};

type ParserError<'a> = Simple<'a, Token<'a>>;

#[derive(Debug)]
pub enum ParseError<'a> {
    EmptyErr(Vec<ParserError<'a>>),
}

// main = () -> {
//     println("hello world");
//     let my_string = "hi there";
//     creates_reference(my_string);
//     println(my_string);
// }
// creates_reference = (x: string) -> {
//     println(x);
// }

pub fn parse<'a, I>(
    file: I, // Vec<Spanned<Token<'a>>>,
    file_length: usize,
) -> Result<UntypedFile, ParseError<'a>>
where
    I: Iterator<Item = Spanned<Token<'a>>> + 'a,
{
    let token_stream = Stream::from_iter(file)
        // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
        // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
        .map((file_length..file_length).into(), |(t, s): (_, _)| (t, s));
    file_parser()
        .parse(token_stream)
        .into_result()
        .map_err(ParseError::EmptyErr)
}

fn file_parser<'a, I>() -> impl Parser<'a, I, UntypedFile, extra::Err<ParserError<'a>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = Span>,
{
    definition_parser()
        .repeated()
        .collect::<Vec<_>>()
        .map(|definitions| UntypedFile { definitions })
}

fn definition_parser<'a, I>()
-> impl Parser<'a, I, Spanned<UntypedDefinition>, extra::Err<ParserError<'a>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = Span>,
{
    // just(Token::Ident(()))

    // any().map_with(|x, e| UntypedDefinition::default())
    ident_parser()
        .then_ignore(just(Token::DefineEqual))
        .then(expression_parser())
        .map_with(|(ident, expression), e| {
            (
                UntypedDefinition {
                    lhs: ident,
                    rhs: expression,
                },
                e.span(),
            )
        })
}

fn ident_parser<'a, I>() -> impl Parser<'a, I, Spanned<String>, extra::Err<ParserError<'a>>> + Clone
where
    I: ValueInput<'a, Token = Token<'a>, Span = Span>,
{
    let ident = select! {
        Token::Ident(x) => x,
    };
    ident.map_with(|t, e| (t.to_string(), e.span()))
}

fn expression_parser<'a, I>()
-> impl Parser<'a, I, Spanned<UntypedExpression>, extra::Err<ParserError<'a>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = Span>,
{
    recursive(|expr| {
        let literal = {
            let mapping = select! {
                Token::Int(x) => UntypedLiteral::Int(x.parse::<i64>().unwrap()),
                Token::String(x) => UntypedLiteral::String(x[1..x.len() - 1].to_string()),
            };
            //
            let function_literal = annotated_ident()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
                .then_ignore(just(Token::Arrow))
                .then(
                    expr.clone()
                        .separated_by(just(Token::Semicolon))
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LeftCurly), just(Token::RightCurly)),
                )
                .map(|(arguments, body)| UntypedLiteral::Function { arguments, body });

            // let unit_literal = just(Token::LeftBrace)
            //     .ignored()
            //     .then_ignore(just(Token::RightBrace))
            //     .map(|_| UntypedLiteral::Unit);
            // mapping.or(function_literal)
            // let function_literal = annotated_ident()
            //     .separated_by(just(Token::Comma))
            //     .then(expr.clone().repeated())
            //     .map(|_| UntypedLiteral::Unit);
            choice((function_literal /* , unit_literal */, mapping))
        }
        .map(UntypedExpression::Literal);

        let ident = ident_parser().map(|ident| UntypedExpression::Ident(ident.0));

        let function_expression = expr
            .clone()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            .or(ident_parser().map(|(ident, span)| (UntypedExpression::Ident(ident), span)));

        let function_call = function_expression
            .then(
                expr.separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
            )
            .map(|(function, arguments)| UntypedExpression::FunctionCall {
                function: Box::new(function), //Box::new((UntypedExpression::Ident(function.0), function.1)),
                arguments,
            });

        let atom = choice((literal, function_call, ident)).map_with(|t, e| (t, e.span()));

        let product = atom
            .clone()
            .then(
                choice((
                    just(Token::MultiplyInt).map(|_| BinaryOperator::MultiplyInt),
                    just(Token::MultiplyFloat).map(|_| BinaryOperator::MultiplyFloat),
                    just(Token::DivideInt).map(|_| BinaryOperator::DivideInt),
                    just(Token::DivideFloat).map(|_| BinaryOperator::DivideFloat),
                ))
                .then(atom.clone())
                .or_not(),
            )
            .map_with(|(lhs, rhs), e| {
                (
                    match rhs {
                        Some((op, rhs)) => UntypedExpression::BinaryExpression {
                            lhs: Box::new(lhs),
                            operator: op,
                            rhs: Box::new(rhs),
                        },
                        None => lhs.0,
                    },
                    e.span(),
                )
            });
        let sum = product
            .clone()
            .then(
                choice((
                    just(Token::AddInt).map(|_| BinaryOperator::AddInt),
                    just(Token::AddFloat).map(|_| BinaryOperator::AddFloat),
                    just(Token::SubtractInt).map(|_| BinaryOperator::SubtractInt),
                    just(Token::SubtractFloat).map(|_| BinaryOperator::SubtractFloat),
                ))
                .then(product.clone())
                .or_not(),
            )
            .map_with(|(lhs, rhs), e| {
                (
                    match rhs {
                        Some((op, rhs)) => UntypedExpression::BinaryExpression {
                            lhs: Box::new(lhs),
                            operator: op,
                            rhs: Box::new(rhs),
                        },
                        None => lhs.0,
                    },
                    e.span(),
                )
            });
        sum
    })
}

fn annotated_ident<'a, I>()
-> impl Parser<'a, I, UntypedAnnotatedIdent, extra::Err<ParserError<'a>>> + Clone
where
    I: ValueInput<'a, Token = Token<'a>, Span = Span>,
{
    ident_parser()
        .then(just(Token::Colon).ignore_then(ident_parser()).or_not())
        .map(|(lhs, rhs)| UntypedAnnotatedIdent {
            ident: lhs,
            annotation: rhs,
        })
}
