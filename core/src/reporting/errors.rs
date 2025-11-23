use std::io::Write;

use ariadne::{ColorGenerator, Label, Report, Source};

use crate::{ast::parser::ParseError, inference::TypeError};

impl ParseError<'_> {
    pub fn report(&self, file: &str, file_content: &str, mut writer: impl Write) {
        match self {
            ParseError::EmptyErr(richs) => {
                for error in richs {
                    Report::build(
                        ariadne::ReportKind::Error,
                        (file, error.span().into_range()),
                    )
                    .with_message("Failed to parse".to_string())
                    .with_label(
                        Label::new((file, error.span().into_range()))
                            .with_color(ariadne::Color::Red)
                            .with_message(error.reason().to_string()),
                    )
                    .finish()
                    .write_for_stdout((file, Source::from(&file_content)), &mut writer)
                    .unwrap();
                }
            }
        }
    }
}

#[allow(unused)]
impl TypeError {
    pub fn report(&self, file: &str, file_content: &str, mut writer: impl Write) {
        match self {
            Self::IncorrectType {
                expected,
                got,
                span,
            } => {
                Report::build(ariadne::ReportKind::Error, (file, span.into_range()))
                    .with_message("Unexpected Type Found")
                    .with_label(
                        Label::new((file, span.into_range()))
                            .with_color(ariadne::Color::Red)
                            .with_message(format!(
                                "Expected: {} but got: {}",
                                expected.display(),
                                got.display()
                            )),
                    )
                    .finish()
                    .write_for_stdout((file, Source::from(&file_content)), &mut writer)
                    .unwrap();
            }
            Self::Undefined { ident, span } => {
                Report::build(ariadne::ReportKind::Error, (file, span.into_range()))
                    .with_message("Undefined identifier")
                    .with_label(
                        Label::new((file, span.into_range()))
                            .with_color(ariadne::Color::Red)
                            .with_message(format!("{ident} is undefined at this point",)),
                    )
                    .finish()
                    .write_for_stdout((file, Source::from(&file_content)), &mut writer)
                    .unwrap();
            }
            Self::WrongNumberOfArguments { expected, got } => todo!(),
            Self::MismatchedBinaryExpression {
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
                        file,
                        lhs_span.into_range().nth(0).unwrap()
                            ..rhs_span.into_range().last().unwrap(),
                    ),
                )
                .with_message("Wrong types for this binary operator")
                .with_label(
                    Label::new((file, lhs_span.into_range()))
                        .with_color(color_generator.next())
                        .with_message(format!("Left side has type {}", lhs.display())),
                )
                .with_label(
                    Label::new((file, rhs_span.into_range()))
                        .with_color(color_generator.next())
                        .with_message(format!("Right side has type {}", rhs.display())),
                )
                .with_note(format!(
                    "{} expects values of type {}",
                    operator.display(),
                    operator_expectation.display()
                ))
                .finish()
                .write_for_stdout((file, Source::from(&file_content)), &mut writer)
                .unwrap();
            }
            Self::CannotMutate {
                target_span,
                immutable_reason,
            } => todo!(),
        }
    }
}
