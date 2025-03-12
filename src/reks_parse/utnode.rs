use super::operators::{InfixOpKind, PrefixOpKind};

#[derive(Debug, Clone)]
pub enum RetTy<'src> {
    None,
    Ty(Value<'src>),
}

// Values

// So yes they do go from untyped to typed
#[derive(Debug, Clone)]
pub enum Value<'src> {
    Identifier(&'src str),
    String(&'src str),
    Num(i64),
    Float(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Const {
    Yes,
    No,
}

#[derive(Debug, Clone)]
pub struct Param<'src> {
    pub name: Value<'src>,
    pub ty: Value<'src>,
}

#[derive(Debug, Clone)]
pub enum TypePath<'src> {
    Empty,
    Typed { ident: Value<'src> },
}

#[derive(Debug, Clone)]
pub enum UntypedExpr<'src> {
    // Values are also expressions
    Value(Value<'src>),

    // A block of expressions
    Block {
        statements: Vec<UntypedExpr<'src>>,
    },

    Fn {
        name: Value<'src>,
        params: Vec<Param<'src>>,
        retty: Box<Self>,
        body: Box<Self>,
    },

    Call {
        name: Box<Self>,
        args: Vec<Self>,
    },

    BinOp {
        left: Box<Self>,
        op: InfixOpKind,
        right: Box<Self>,
    },

    UnaryOp {
        op: PrefixOpKind,
        operand: Box<Self>,
    },

    Let {
        id: Value<'src>,
        pat: TypePath<'src>,
        expr: Box<Self>,
        constness: Const,
    },

    List {
        items: Vec<Self>,
    },

    Struct {
        id: Value<'src>,
        fields: Vec<Param<'src>>,
    },

    If {
        condition: Box<Self>,
        then_branch: Box<Self>,
        else_branch: Box<Self>,
    },

    FieldAccess {
        id: Box<Self>,
        field: Value<'src>,
    },

    Assign {
        left: Box<Self>,
        right: Box<Self>,
    },
}
