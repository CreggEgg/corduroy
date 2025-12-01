use std::collections::HashMap;

use crate::{
    ast::typed::{Definition, Expression, Type, TypeVariableId, TypedExpression},
    inference::TypeError,
};

pub fn finalize_definition(
    mut inferred_definition: Definition,
    resolved_type_variables: &mut HashMap<TypeVariableId, Type>,
) -> Result<Definition, Box<TypeError>> {
    inferred_definition.rhs.0.finalize();
    Ok(inferred_definition)
}

impl TypedExpression {
    fn finalize(&mut self) {
        self.evaluates_to.finalize();
        match self.expression {
            Expression::FunctionCall {
                function,
                arguments,
            } => todo!(),
            Expression::Literal(literal) => todo!(),
            Expression::Ident(_) => todo!(),
            Expression::Definition { lhs, rhs, mutable } => todo!(),
            Expression::Assignment { lhs, rhs } => todo!(),
            Expression::Block(block) => todo!(),
            Expression::Match { target, arms } => todo!(),
            Expression::BinaryExpression { operator, lhs, rhs } => todo!(),
        }
    }
}

impl Type {
    fn finalize(&mut self) {
        match self {
            Type::Function { args, return_type } => for arg in &mut args {},
            Type::Int => todo!(),
            Type::Float => todo!(),
            Type::String => todo!(),
            Type::Unit => todo!(),
            Type::Boolean => todo!(),
            Type::TypeVariable(type_variable_id) => todo!(),
            Type::Generic(generic_id) => todo!(),
            Type::Array(_, _) => todo!(),
        }
    }
}
