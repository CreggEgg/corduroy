use core::{
    Scope, Spanned,
    ast::typed::{AnnotatedIdent, Definition, File, Type, TypedExpression},
};
use std::{collections::HashMap, path::Path};

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::{Linkage, Module},
    targets::{InitializationConfig, Target, TargetTriple},
    types::PointerType,
    values::{BasicValue, FloatValue, FunctionValue, IntValue, PointerValue},
};

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    scope: Scope<RuntimeValue<'ctx>>,
}

#[derive(Clone)]
enum RuntimeValue<'a> {
    Int(IntValue<'a>),
    Float(FloatValue<'a>),
    Pointer(PointerValue<'a>),
    Function(FunctionValue<'a>),
}

pub fn compile(file: &File) -> Vec<u8> {
    let context = Context::create();

    let module = context.create_module("main");
    let mut scope: Scope<RuntimeValue> = HashMap::new();

    let builder = context.create_builder();

    let int = context.i64_type();

    let printint = {
        let fn_type = int.fn_type(&[int.into()], false);
        module.add_function("printint", fn_type, Some(Linkage::External))
    };
    scope.insert("printint".to_string(), RuntimeValue::Function(printint));
    let println = {
        let fn_type = int.fn_type(&[context.ptr_type(AddressSpace::default()).into()], false);
        module.add_function("println", fn_type, Some(Linkage::External))
    };
    scope.insert("println".to_string(), RuntimeValue::Function(println));

    let main_type = int.fn_type(&[], false);
    let function = module.add_function("main", main_type, Some(Linkage::External));

    let basic_block = context.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);

    let mut functions = Vec::new();

    let mut internal_main = Option::None;

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
                let value = compile_expr(expr, &mut scope, &builder, &context);
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
            &builder,
            &mut scope,
            &context,
            &module,
        );
        println!("building function {name}");
        scope.insert(name.clone(), RuntimeValue::Function(function));
        if name == "main" {
            internal_main = Some(function);
        }
    }

    match internal_main {
        Some(main) => {
            builder.position_at_end(basic_block);
            builder.build_call(main, &[], "internalmain").unwrap();
            builder
                .build_return(Some(&context.i64_type().const_int(0, false)))
                .unwrap();
        }
        None => {}
    };

    // let x = function.get_nth_param(0).unwrap().into_int_value();
    // let y = function.get_nth_param(1).unwrap().into_int_value();
    // let sum = codegen.builder.build_int_add(x, y, "sum").unwrap();
    // codegen.builder.build_return(Some(&sum)).unwrap();
    Target::initialize_x86(&InitializationConfig::default());
    println!("88");
    let target = Target::from_name("x86-64").unwrap();
    println!("90");
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
    println!("101");

    module.print_to_stderr();

    // std::fs::write(
    //     "./out.o",
    //     codegen
    //         .module
    //         .write_bitcode_to_memory()
    //         .create_object_file()
    //         .unwrap().,
    // );
    //
    target_machine
        .write_to_memory_buffer(&module, inkwell::targets::FileType::Object)
        .unwrap()
        .as_slice()
        .to_owned()
}

fn compile_function<'a, 'ctx>(
    name: String,
    arguments: &[AnnotatedIdent],
    body: &[Spanned<TypedExpression>],
    return_type: Type,
    builder: &'a Builder<'ctx>,
    scope: &'a mut HashMap<String, RuntimeValue<'ctx>>,
    context: &'ctx Context,
    module: &Module<'ctx>,
) -> FunctionValue<'ctx> {
    let int = context.i64_type();
    let float = context.f64_type();

    let mut args = arguments
        .iter()
        .map(|it| match it.annotation.0 {
            Type::Float => float.into(),
            _ => int.into(),
        })
        .collect::<Vec<_>>();

    let fn_type = match return_type {
        Type::Float => float.fn_type(&args, false),
        _ => int.fn_type(&args, false),
    };

    let name = match name.as_str() {
        "main" => "_main",
        x => x,
    };

    let function = module.add_function(&name, fn_type, Some(Linkage::External));

    for i in 0..arguments.len() {
        scope.insert(
            arguments[i].ident.0.clone(),
            RuntimeValue::Int(function.get_nth_param(i as u32).unwrap().into_int_value()),
        );
    }

    let basic_block = context.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);

    for (i, (expression, _)) in body.iter().enumerate() {
        println!("building expression {i}");
        let value = compile_expr(expression, scope, builder, context);
        if i == body.len() - 1 && return_type != Type::Unit {
            builder
                .build_return(Some(&match value {
                    RuntimeValue::Int(int_value) => int_value.as_basic_value_enum(),
                    RuntimeValue::Function(function_value) => todo!(),
                    RuntimeValue::Float(float_value) => float_value.as_basic_value_enum(),
                    RuntimeValue::Pointer(pointer_value) => pointer_value.as_basic_value_enum(),
                }))
                .unwrap();
        }
        println!("done building expression {i}");
    }

    if !function.verify(true) {
        function.print_to_stderr();
    }

    function
}

fn compile_expr<'a, 'ctx>(
    expression: &'a TypedExpression,
    scope: &'a mut Scope<RuntimeValue<'ctx>>,
    builder: &Builder<'ctx>,
    context: &'ctx Context,
) -> RuntimeValue<'ctx> {
    match &expression.expression {
        core::ast::typed::Expression::FunctionCall {
            function,
            arguments,
        } => {
            let RuntimeValue::Function(function) =
                compile_expr(&function.0, scope, builder, context)
            else {
                unreachable!();
            };

            let args = arguments
                .iter()
                .map(
                    |(arg, _)| match compile_expr(arg, scope, builder, context) {
                        RuntimeValue::Int(int_value) => int_value.into(),
                        RuntimeValue::Function(function_value) => panic!(),
                        RuntimeValue::Float(float_value) => float_value.into(),
                        RuntimeValue::Pointer(pointer_value) => pointer_value.into(),
                    },
                )
                .collect::<Vec<_>>();

            let ret = builder.build_call(function, &args, "tmpcall").unwrap();
            RuntimeValue::Int(ret.try_as_basic_value().basic().unwrap().into_int_value())
        }
        core::ast::typed::Expression::Literal(literal) => match literal {
            core::ast::typed::Literal::String(string) => {
                let ptr = builder
                    .build_alloca(
                        context.i64_type().array_type(string.len() as u32),
                        "tmpstring",
                    )
                    .unwrap();
                for (i, char) in string.char_indices() {
                    let address = builder
                        .build_int_to_ptr(
                            builder
                                .build_int_add(
                                    ptr.const_to_int(context.i64_type()),
                                    context.i64_type().const_int(i as u64, false),
                                    "tmpindex",
                                )
                                .unwrap(),
                            context.ptr_type(AddressSpace::default()),
                            "tmpaddress",
                        )
                        .unwrap();
                    // .const_to_pointer(context.ptr_type(AddressSpace::default()));
                    builder
                        .build_store(address, context.i64_type().const_int(char as u64, false))
                        .unwrap();
                }

                RuntimeValue::Pointer(ptr)
            }
            core::ast::typed::Literal::Int(x) => {
                println!("creating literal value");
                RuntimeValue::Int(context.i64_type().const_int(*x as u64, false))
            }

            core::ast::typed::Literal::Float(x) => {
                let inner_value = context.i64_type().const_int(*x as u64, false);
                RuntimeValue::Int(inner_value)
            }
            core::ast::typed::Literal::Unit => todo!(),
            core::ast::typed::Literal::Boolean(_) => todo!(),
            core::ast::typed::Literal::Function { arguments, body } => todo!(),
        },
        core::ast::typed::Expression::Ident(ident) => match scope.get(ident) {
            Some(value) => value.clone(),
            None => panic!("Could not find {ident}"),
        },
        core::ast::typed::Expression::BinaryExpression { lhs, operator, rhs } => {
            let RuntimeValue::Int(lhs) = compile_expr(&lhs.0, scope, builder, context) else {
                unreachable!();
            };
            let RuntimeValue::Int(rhs) = compile_expr(&rhs.0, scope, builder, context) else {
                unreachable!();
            };

            RuntimeValue::Int(match operator {
                core::ast::typed::BinaryOperator::Add => {
                    builder.build_int_add(lhs, rhs, "tmpadd").unwrap()
                }
                core::ast::typed::BinaryOperator::Multiply => {
                    builder.build_int_mul(lhs, rhs, "tmpmul").unwrap()
                }
                core::ast::typed::BinaryOperator::Divide => {
                    builder.build_int_signed_div(lhs, rhs, "tmpdiv").unwrap()
                }
                core::ast::typed::BinaryOperator::Subtract => {
                    builder.build_int_sub(lhs, rhs, "tmpsub").unwrap()
                }
            })
        }
    }
}

// codegen.module.write("./out.o");
