pub struct MIRFile {
    pub definitions: Vec<MIRDefinition>,
}

pub struct MIRDefinition {
    pub ident: String,
    pub value: MIRExpression,
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
