use std::{
    fmt::{Display, Write},
    ops::Range,
};

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

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Error => f.write_str("Error"),
            Token::Comma => f.write_char(','),
            Token::LeftBrace => f.write_char('('),
            Token::RightBrace => f.write_char(')'),
            Token::LeftCurly => f.write_char('{'),
            Token::RightCurly => f.write_char('}'),
            Token::Semicolon => f.write_char(';'),
            Token::Colon => f.write_char(':'),
            Token::DefineEqual => f.write_char('='),
            Token::EqualComparison => f.write_str("=="),
            Token::AddInt => f.write_str("+"),
            Token::AddFloat => f.write_str("+."),
            Token::MultiplyInt => f.write_str("*"),
            Token::MultiplyFloat => f.write_str("*."),
            Token::SubtractInt => f.write_str("-"),
            Token::SubtractFloat => f.write_str("-."),
            Token::DivideInt => f.write_str("/"),
            Token::DivideFloat => f.write_str("/."),
            Token::Arrow => f.write_str("->"),
            Token::Let => f.write_str("let"),
            Token::Ident(ident) => f.write_str(ident),
            Token::String(data) => {
                f.write_char('"')?;
                f.write_str(data)?;
                f.write_char('"')
            }
            Token::Int(int) => f.write_str(&format!("{}", int)),
        }
    }
}

pub fn tokenize(file: &str) -> impl Iterator<Item = Spanned<Token>> {
    //Result<Vec<Spanned<Token>>, TokenError> {
    let lexer = Token::lexer(file);
    lexer.spanned().map(|(token, span)| match token {
        Ok(token) => (token, span.into()),
        Err(_) => (Token::Error, span.into()),
    })
}
