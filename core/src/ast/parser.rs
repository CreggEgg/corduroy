use chumsky::{
    IterParser, Parser,
    error::{EmptyErr, Simple},
    extra,
    input::{Input, Stream, ValueInput},
    prelude::{any, choice, just, recursive},
    select,
};

use crate::{Span, Spanned, tokens::Token};

use super::untyped::{
    UntypedAnnotatedIdent, UntypedDefinition, UntypedExpression, UntypedFile, UntypedLiteral,
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
        .map_err(|err| ParseError::EmptyErr(err))
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

fn ident_parser<'a, I>() -> impl Parser<'a, I, Spanned<String>, extra::Err<ParserError<'a>>>
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
            };

            // let function_literal = annotated_ident()
            //     .separated_by(just(Token::Comma))
            //     .collect::<Vec<_>>()
            //     .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            //     .then_ignore(just(Token::Arrow))
            //     .then(
            //         expr.separated_by(just(Token::Semicolon))
            //             .collect::<Vec<_>>()
            //             .delimited_by(just(Token::LeftCurly), just(Token::RightCurly)),
            //     )
            //     .map(|(arguments, body)| UntypedLiteral::Function { arguments, body });
            mapping //.or(function_literal)
        }
        .map(UntypedExpression::Literal);

        choice((literal,)).map_with(|t, e| (t, e.span()))
    })
}

fn annotated_ident<'a, I>() -> impl Parser<'a, I, UntypedAnnotatedIdent, extra::Err<ParserError<'a>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = Span>,
{
    ident_parser()
        .then_ignore(just(Token::Colon))
        .then(ident_parser())
        .map(|(lhs, rhs)| UntypedAnnotatedIdent {
            ident: lhs,
            annotation: rhs,
        })
}
