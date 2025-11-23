pub enum MIRType {
    Int,
}

pub struct MIRFile {
    pub definitions: Vec<MIRDefinition>,
}

pub enum MIRDefinition {
    Value {
        ident: String,
        value: MIRExpression,
    },
    Function {
        ident: String,
        function: MIRFunction,
    },
}

pub enum MIRExpression {
    FunctionCall {
        function: String,
        arguments: Vec<MIRExpression>,
    },
    Literal(MIRLiteral),
}

pub enum MIRLiteral {
    Int(i64),
    Float(f64),
    Boolean(bool),
}

pub struct MIRFunction {
    arguments: Vec<MIRAnnotatedIdent>,
    body: Vec<MIRExpression>,
}

pub struct MIRAnnotatedIdent {
    ident: String,
    annotation: MIRType,
}
