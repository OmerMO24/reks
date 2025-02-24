use super::operators::{InfixOpKind, PrefixOpKind};
use crate::frontend::{lexer, operators};

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
    Bool(bool),
    List(Vec<Self>),
    Func(&'src str),
}

#[derive(Debug)]
pub enum Const {
    Yes,
    No,
}

#[derive(Debug)]
pub struct Param<'src> {
    name: Value<'src>,
    ty: Value<'src>,
}

#[derive(Debug)]
pub struct FnDecl<'src> {
    constness: Const,
    inputs: Vec<Param<'src>>,
    output: RetTy<'src>,
}

// I like this abstraction
// NVM my previous assumptions were wrong
#[derive(Debug, Clone)]
pub enum ArgNames<'src> {
    Named {
        name: Value<'src>,
    },

    Labelled {
        name: Value<'src>,
        label: Value<'src>,
    },
}

#[derive(Debug, Clone)]
pub struct CallArg<A> {
    pub value: A,
}

#[derive(Debug, Clone)]
pub enum UntypedExpr<'src> {
    // Values are also expressions
    Value(Value<'src>),

    // A block of expressions
    Block {
        statements: Vec<UntypedExpr<'src>>,
    },

    // allocate here but whatever
    Fn {
        name: Value<'src>,
        args: Vec<ArgNames<'src>>,
        body: Vec<UntypedExpr<'src>>,
        return_annotation: Value<'src>,
    },

    Call {
        name: Value<'src>,
        args: Vec<CallArg<Self>>,
    },

    BinOp {
        op: InfixOpKind,
        left: Box<Self>,
        right: Box<Self>,
    },

    UnaryOp {
        op: PrefixOpKind,
        operand: Box<Self>,
    },
}
