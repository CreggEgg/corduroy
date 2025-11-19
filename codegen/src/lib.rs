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
    values::FunctionValue,
};

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    scope: Scope<RuntimeValue<'ctx>>
}

enum RuntimeValue<'a> {
    Value(),
    Function(FunctionValue<'a>),
}

pub fn compile(file: &File) {
    let context = Context::create();
    let module = context.create_module("main");
    let mut codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        scope: HashMap::new()
    };

    let int = context.i64_type();

    let main_type = int.fn_type(&[], false);
    let function = codegen
        .module
        .add_function("main", main_type, Some(Linkage::External));

    let basic_block = codegen.context.append_basic_block(function, "entry");
    codegen.builder.position_at_end(basic_block);

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
                functions.push((
                    definition.0.lhs.0.clone(),
                    arguments,
                    body,
                    expr.evaluates_to.clone(),
                ));
            }
            _ => {
                let value = codegen.compile_expr(expr);
                codegen.scope.insert(definition.0.lhs.0.clone(), value);
            }
        }
    }

    for (name, arguments, body, return_type) in functions {
        let function =
            codegen.compile_function(name.clone(), arguments, body, return_type);
        codegen.scope.insert(name, RuntimeValue::Function(function));
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

    // codegen.module.write("./out.o");
}

impl CodeGen<'_> {
    fn compile_expr(
        &mut self,
        expr: &core::ast::typed::TypedExpression,
    ) -> RuntimeValue {
        todo!()
    }

    fn compile_function(
        &mut self,
        name: String,
        arguments: &[AnnotatedIdent],
        body: &[Spanned<TypedExpression>],
        return_type: Type,
    ) -> FunctionValue {
        let int = self.context.i64_type();
        let float = self.context.f64_type();

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

        let function = self
            .module
            .add_function(&name, fn_type, Some(Linkage::External)).;

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        for (expression, _) in body {
            self.compile_expr(expression, scope)
        }

        function
    }
}
