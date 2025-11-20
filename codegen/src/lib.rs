use core::{
    Scope, Spanned,
    ast::typed::{AnnotatedIdent, Definition, File, Type, TypedExpression},
};
use std::{collections::HashMap, path::Path};

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::{Linkage, Module},
    targets::{InitializationConfig, Target, TargetTriple},
    values::{FunctionValue, IntValue},
};

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    scope: Scope<RuntimeValue<'ctx>>,
}

#[derive(Clone)]
enum RuntimeValue<'a> {
    Value(IntValue<'a>),
    Function(FunctionValue<'a>),
}

pub fn compile(file: &File) {
    let context = Context::create();
    let module = context.create_module("main");
    let mut scope: Scope<RuntimeValue> = HashMap::new();

    let builder = context.create_builder();

    let int = context.i64_type();

    let main_type = int.fn_type(&[], false);
    let function = module.add_function("main", main_type, Some(Linkage::External));

    let basic_block = context.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);

    let mut functions = Vec::new();

    for definition in &file.definitions {
        let expr = &definition.0.rhs.0;
        match expr {
            TypedExpression {
                expression:
                    core::ast::typed::Expression::Literal(core::ast::typed::Literal::Function {
                        arguments,
                        body,
                    }),
                ..
            } => {
                let name = definition.0.lhs.0.clone();
                let return_type = expr.evaluates_to.clone();
                functions.push((name, arguments, body, return_type));
            }
            _ => {
                let name = definition.0.lhs.0.clone();
                let value = compile_expr(expr, &mut scope);
                scope.insert(name, value);
            }
        }
    }
    for (name, arguments, body, return_type) in functions {
        let function = compile_function(
            name.clone(),
            arguments,
            body,
            return_type,
            builder,
            &mut scope,
            &context,
            &mut module,
        );
        scope.insert(name, RuntimeValue::Function(function));
    }

    // let x = function.get_nth_param(0).unwrap().into_int_value();
    // let y = function.get_nth_param(1).unwrap().into_int_value();
    // let sum = codegen.builder.build_int_add(x, y, "sum").unwrap();
    // codegen.builder.build_return(Some(&sum)).unwrap();
    Target::initialize_x86(&InitializationConfig::default());
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64-pc-linux-gnu"),
            "x86-64",
            "+avx2",
            inkwell::OptimizationLevel::Default,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap();

    // std::fs::write(
    //     "./out.o",
    //     codegen
    //         .module
    //         .write_bitcode_to_memory()
    //         .create_object_file()
    //         .unwrap().,
    // );

    target_machine
        .write_to_file(
            &codegen.module,
            inkwell::targets::FileType::Object,
            Path::new("./out.o"),
        )
        .unwrap();
}

fn compile_function<'a>(
    name: String,
    arguments: &'a [AnnotatedIdent],
    body: &'a [Spanned<TypedExpression>],
    return_type: Type,
    builder: Builder<'_>,
    scope: &mut HashMap<String, RuntimeValue<'_>>,
    context: &'a Context,
    module: &'a mut Module<'a>,
) -> FunctionValue<'a> {
    let int = context.i64_type();
    let float = context.f64_type();

    let args = arguments
        .iter()
        .map(|argument| match argument.annotation.0 {
            Type::Float => float.into(),
            _ => int.into(),
        })
        .collect::<Vec<_>>();

    let fn_type = match return_type {
        Type::Float => float.fn_type(&args, false),
        _ => int.fn_type(&args, false),
    };

    let function = module.add_function(&name, fn_type, Some(Linkage::External));

    let basic_block = context.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);

    for (expression, _) in body {
        compile_expr(expression, scope);
    }

    function
}

fn compile_expr<'a>(
    expression: &'a TypedExpression,
    scope: &'a mut Scope<RuntimeValue>,
    builder: Builder<'_>,
    context: &'a Context,
) -> RuntimeValue<'a> {
    match &expression.expression {
        core::ast::typed::Expression::FunctionCall {
            function,
            arguments,
        } => todo!(),
        core::ast::typed::Expression::Literal(literal) => match literal {
            core::ast::typed::Literal::String(_) => todo!(),
            core::ast::typed::Literal::Int(x) => {
                RuntimeValue::Value(context.i64_type().const_int(*x as u64, true))
            }

            core::ast::typed::Literal::Float(x) => {
                let inner_value = context.i64_type().const_int(*x);
                RuntimeValue::Value(inner_value)
            }
            core::ast::typed::Literal::Unit => todo!(),
            core::ast::typed::Literal::Boolean(_) => todo!(),
            core::ast::typed::Literal::Function { arguments, body } => todo!(),
        },
        core::ast::typed::Expression::Ident(ident) => scope.get(ident).unwrap().clone(),
        core::ast::typed::Expression::BinaryExpression { lhs, operator, rhs } => todo!(),
    }
}

// codegen.module.write("./out.o");
