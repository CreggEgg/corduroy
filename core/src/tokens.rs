use std::ops::Range;

use logos::Logos;

use crate::Spanned;

/* main = () -> {
    println("hello world");
    let my_string = "hi there";
    creates_reference(my_string);
    println(my_string)
}
creates_reference = (x: string) -> {
    println(x)
} */

#[derive(Debug)]
pub struct TokenError(pub Range<usize>);

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'a> {
    Error,
    #[token(",")]
    Comma,
    #[token("(")]
    LeftBrace,
    #[token(")")]
    RightBrace,
    #[token("{")]
    LeftCurly,
    #[token("}")]
    RightCurly,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("=")]
    DefineEqual,
    #[token("==")]
    EqualComparison,
    #[token("+")]
    AddInt,
    #[token("+.")]
    AddFloat,
    #[token("*")]
    MultiplyInt,
    #[token("*.")]
    MultiplyFloat,
    #[token("-")]
    SubtractInt,
    #[token("-.")]
    SubtractFloat,
    #[token("/")]
    DivideInt,
    #[token("/.")]
    DivideFloat,
    #[token("->")]
    Arrow,
    #[token("let")]
    Let,
    #[regex("[a-z_]+")]
    Ident(&'a str),
    #[regex("\"[^\"]+\"")]
    String(&'a str),
    #[regex("[0-9]+")]
    Int(&'a str),
}

pub fn tokenize(file: &str) -> impl Iterator<Item = Spanned<Token>> {
    //Result<Vec<Spanned<Token>>, TokenError> {
    let lexer = Token::lexer(file);
    lexer.spanned().map(|(token, span)| match token {
        Ok(token) => (token, span.into()),
        Err(_) => (Token::Error, span.into()),
    })
}
