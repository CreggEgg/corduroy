use core::{ast::typed::Type, inference, parse_file};
use std::{
    collections::HashMap,
    fs,
    os::unix::process::CommandExt,
    process::{Command, exit},
};

use ariadne::{ColorGenerator, Label, Report, Source};
use clap::Parser;
use codegen::compile;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    file: String,
}
fn main() {
    let args = Args::parse();

    let file_content = fs::read_to_string(&args.file).unwrap();

    let ast = match parse_file(&file_content) {
        Ok(x) => x,
        Err(err) => match err {
            core::ast::parser::ParseError::EmptyErr(errors) => {
                for error in errors {
                    Report::build(
                        ariadne::ReportKind::Error,
                        (args.file.clone(), error.span().into_range()),
                    )
                    .with_message("Failed to parse".to_string())
                    .with_label(
                        Label::new((args.file.clone(), error.span().into_range()))
                            .with_color(ariadne::Color::Red)
                            .with_message(error.reason().to_string()),
                    )
                    .finish()
                    .print((args.file.clone(), Source::from(&file_content)))
                    .unwrap();
                }
                exit(1);
            }
        },
    };

    let inferred = match inference::infer_ast(
        ast,
        HashMap::from([
            (
                "println".into(),
                Type::Function {
                    args: vec![Type::String],
                    return_type: Box::new(Type::Unit),
                },
            ),
            (
                "printint".into(),
                Type::Function {
                    args: vec![Type::Int],
                    return_type: Box::new(Type::Unit),
                },
            ),
            ("int".into(), Type::Int),
            ("float".into(), Type::Float),
        ]),
    ) {
        Ok(x) => x,
        Err(err) => match err {
            inference::TypeError::IncorrectType {
                expected,
                got,
                span,
            } => {
                Report::build(
                    ariadne::ReportKind::Error,
                    (args.file.clone(), span.into_range()),
                )
                .with_message("Unexpected Type Found")
                .with_label(
                    Label::new((args.file.clone(), span.into_range()))
                        .with_color(ariadne::Color::Red)
                        .with_message(format!(
                            "Expected: {} but got: {}",
                            expected.display(),
                            got.display()
                        )),
                )
                .finish()
                .print((args.file.clone(), Source::from(&file_content)))
                .unwrap();
                exit(1);
            }
            inference::TypeError::Undefined { ident, span } => {
                Report::build(
                    ariadne::ReportKind::Error,
                    (args.file.clone(), span.into_range()),
                )
                .with_message("Undefined identifier")
                .with_label(
                    Label::new((args.file.clone(), span.into_range()))
                        .with_color(ariadne::Color::Red)
                        .with_message(format!("{ident} is undefined at this point",)),
                )
                .finish()
                .print((args.file.clone(), Source::from(&file_content)))
                .unwrap();
                exit(1);
            }
            inference::TypeError::WrongNumberOfArguments { expected, got } => todo!(),
            inference::TypeError::MismatchedBinaryExpression {
                lhs,
                lhs_span,
                rhs,
                rhs_span,
                operator,
                operator_expectation,
            } => {
                let mut color_generator = ColorGenerator::new();
                Report::build(
                    ariadne::ReportKind::Error,
                    (
                        args.file.clone(),
                        lhs_span.into_range().nth(0).unwrap()
                            ..rhs_span.into_range().last().unwrap(),
                    ),
                )
                .with_message("Wrong types for this binary operator")
                .with_label(
                    Label::new((args.file.clone(), lhs_span.into_range()))
                        .with_color(color_generator.next())
                        .with_message(format!("Left side has type {}", lhs.display())),
                )
                .with_label(
                    Label::new((args.file.clone(), rhs_span.into_range()))
                        .with_color(color_generator.next())
                        .with_message(format!("Right side has type {}", rhs.display())),
                )
                .with_note(format!(
                    "{} expects values of type {}",
                    operator.display(),
                    operator_expectation.display()
                ))
                .finish()
                .print((args.file.clone(), Source::from(&file_content)))
                .unwrap();
                exit(1);
            }
        },
    };

    let compiled_object = compile(&inferred);
    let _ = fs::create_dir("./tmp-corduroy-build/");
    fs::write("./tmp-corduroy-build/main.o", compiled_object).unwrap();
    fs::write(
        "./tmp-corduroy-build/std.c",
        include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/test.c")),
    )
    .unwrap();

    Command::new("gcc")
        .args(["./tmp-corduroy-build/main.o", "./tmp-corduroy-build/std.c"])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    fs::remove_dir_all("./tmp-corduroy-build/").unwrap();
}
