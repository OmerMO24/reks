use crate::reks_parse::{operators::*, utnode::*};
use crate::reks_type::{
    infer::*,
    resolve::{NameResolver, NodeId},
};
use std::collections::HashMap;

// In src/reks_type/infer.rs (or a new file like cir.rs)

// CIR Value representation (compile-time values)
#[derive(Debug, Clone)]
pub enum CirValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    Struct(HashMap<String, CirValue>),
    List(Vec<CirValue>),
}

// Identifier for values in CIR (variables, temporaries, constants)
#[derive(Debug, Clone)]
pub enum ValueId {
    Var(NodeId),     // Variable reference by NodeId
    Temp(String),    // Temporary result (e.g., "tmp0")
    Const(CirValue), // Constant value
}

// CIR Operations
#[derive(Debug, Clone)]
pub enum CIROp {
    Const(CirValue),                  // Literal value
    Add(ValueId, ValueId),            // Arithmetic addition
    Sub(ValueId, ValueId),            // Subtraction
    Mul(ValueId, ValueId),            // Multiplication
    Div(ValueId, ValueId),            // Division
    Gt(ValueId, ValueId),             // Greater than
    Lt(ValueId, ValueId),             // Less than
    Eq(ValueId, ValueId),             // Equal
    Neq(ValueId, ValueId),            // Not equal
    Store(NodeId, ValueId),           // Assign to variable
    Load(NodeId),                     // Load variable value
    Struct(HashMap<String, ValueId>), // Create struct
    GetField(ValueId, String),        // Access struct field
    List(Vec<ValueId>),               // Create list
    Call(NodeId, Vec<ValueId>),       // Function call
    Branch(ValueId, String, String),  // Conditional jump (cond, then_label, else_label)
    Jump(String),                     // Unconditional jump
    Label(String),                    // Jump target
    Return(ValueId),                  // Return value
}

// CIR Instruction
#[derive(Debug, Clone)]
pub struct CIRInstruction {
    pub op: CIROp,
    value: Option<CirValue>, // Filled by interpreter later
}

impl CIRInstruction {
    pub fn new(op: CIROp) -> Self {
        CIRInstruction { op, value: None }
    }
}

use crate::reks_type::resolve::NameResolutionMap;

pub struct CIRBuilder {
    instructions: Vec<CIRInstruction>,
    temp_counter: usize,
    label_counter: usize,
    functions: HashMap<NodeId, Vec<CIRInstruction>>,
    resolution_map: NameResolutionMap, // Add resolution map for name lookup
}

impl CIRBuilder {
    pub fn new(resolution_map: NameResolutionMap) -> Self {
        CIRBuilder {
            instructions: Vec::new(),
            temp_counter: 0,
            label_counter: 0,
            functions: HashMap::new(),
            resolution_map,
        }
    }

    pub fn emit(&mut self, op: CIROp) -> ValueId {
        let result_id = match op {
            CIROp::Store(_, _)
            | CIROp::Label(_)
            | CIROp::Jump(_)
            | CIROp::Branch(_, _, _)
            | CIROp::Return(_) => None,
            _ => Some(self.new_temp()),
        };
        self.instructions.push(CIRInstruction::new(op));
        result_id.unwrap_or_else(|| ValueId::Temp("unit".to_string()))
    }

    pub fn new_temp(&mut self) -> ValueId {
        let temp = format!("tmp{}", self.temp_counter);
        self.temp_counter += 1;
        ValueId::Temp(temp)
    }

    pub fn new_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    pub fn lower_expr(&mut self, expr: &TypedExpr) -> ValueId {
        match &expr.kind {
            TypedExprKind::Value(val) => match val {
                Value::Num(n) => self.emit(CIROp::Const(CirValue::Int(*n))),
                Value::Identifier(name) => {
                    if let Some(node_id) = self.resolution_map.resolve_name(name) {
                        ValueId::Var(node_id)
                    } else {
                        panic!("Unresolved identifier: {}", name); // For debugging
                    }
                }
                _ => unimplemented!("Other Value types not yet supported"),
            },
            TypedExprKind::Let {
                id,
                expr: init_expr,
                constness: _,
                ..
            } => {
                let init_id = self.lower_expr(init_expr);
                if let Value::Identifier(_) = id {
                    self.emit(CIROp::Store(expr.node_id, init_id));
                }
                ValueId::Var(expr.node_id)
            }
            TypedExprKind::Fn {
                name,
                params,
                retty,
                body,
            } => {
                let fn_node_id = if let Value::Identifier(fn_name) = name {
                    self.resolution_map
                        .resolve_name(fn_name)
                        .expect("Function not found")
                } else {
                    panic!("Function name must be an identifier");
                };
                if let Value::Identifier("main") = name {
                    let body_id = self.lower_expr(body);
                    self.emit(CIROp::Return(body_id));
                } else {
                    let mut fn_builder = CIRBuilder::new(self.resolution_map.clone());
                    let body_id = fn_builder.lower_expr(body);
                    fn_builder.emit(CIROp::Return(body_id));
                    self.functions
                        .insert(fn_node_id, fn_builder.instructions.clone());
                }
                ValueId::Var(fn_node_id)
            }
            TypedExprKind::Call { name, args } => {
                let fn_id = self.lower_expr(name);
                let mut arg_ids = Vec::new();
                for arg in args {
                    arg_ids.push(self.lower_expr(arg));
                }
                let fn_node_id =
                    if let TypedExprKind::Value(Value::Identifier(fn_name)) = &name.kind {
                        let resolved_id = self
                            .resolution_map
                            .resolve_name(fn_name)
                            .expect("Function not found");
                        println!("Resolving {} to NodeId: {:?}", fn_name, resolved_id);
                        resolved_id
                    } else {
                        panic!("Call target must be an identifier");
                    };
                self.emit(CIROp::Call(fn_node_id, arg_ids))
            }
            TypedExprKind::BinOp { left, op, right } => {
                let left_id = self.lower_expr(left);
                let right_id = self.lower_expr(right);
                match op {
                    InfixOpKind::Add => self.emit(CIROp::Add(left_id, right_id)),
                    InfixOpKind::Sub => self.emit(CIROp::Sub(left_id, right_id)),
                    InfixOpKind::Mul => self.emit(CIROp::Mul(left_id, right_id)),
                    InfixOpKind::Div => self.emit(CIROp::Div(left_id, right_id)),
                    InfixOpKind::Greater => self.emit(CIROp::Gt(left_id, right_id)),
                    InfixOpKind::Less => self.emit(CIROp::Lt(left_id, right_id)),
                    InfixOpKind::Equals => self.emit(CIROp::Eq(left_id, right_id)),
                    InfixOpKind::NotEq => self.emit(CIROp::Neq(left_id, right_id)),
                    _ => todo!(),
                }
            }
            TypedExprKind::Block { statements } => {
                let mut last_id = None;
                for stmt in statements {
                    last_id = Some(self.lower_expr(stmt));
                }
                if let Some(TypedExprKind::Let {
                    id: Value::Identifier(_),
                    ..
                }) = statements.last().map(|e| &e.kind)
                {
                    last_id.unwrap_or_else(|| {
                        statements
                            .last()
                            .map(|e| ValueId::Var(e.node_id))
                            .unwrap_or_else(|| {
                                self.emit(CIROp::Const(CirValue::Struct(HashMap::new())))
                            })
                    })
                } else if let Some(TypedExprKind::Value(Value::Identifier(name))) =
                    statements.last().map(|e| &e.kind)
                {
                    if let Some(node_id) = self.resolution_map.resolve_name(name) {
                        ValueId::Var(node_id)
                    } else {
                        last_id.unwrap_or_else(|| {
                            self.emit(CIROp::Const(CirValue::Struct(HashMap::new())))
                        })
                    }
                } else {
                    last_id.unwrap_or_else(|| {
                        self.emit(CIROp::Const(CirValue::Struct(HashMap::new())))
                    })
                }
            }
            TypedExprKind::Struct { id: _, fields: _ } => ValueId::Var(expr.node_id),
            TypedExprKind::List { elements } => {
                let mut elem_ids = Vec::new();
                for elem in elements {
                    elem_ids.push(self.lower_expr(elem));
                }
                self.emit(CIROp::List(elem_ids))
            }
            TypedExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_id = self.lower_expr(condition);
                let then_label = self.new_label();
                let else_label = self.new_label();
                let end_label = self.new_label();

                self.emit(CIROp::Branch(
                    cond_id,
                    then_label.clone(),
                    else_label.clone(),
                ));
                self.emit(CIROp::Label(then_label));
                let then_id = self.lower_expr(then_branch);
                self.emit(CIROp::Jump(end_label.clone()));
                self.emit(CIROp::Label(else_label));
                let else_id = self.lower_expr(else_branch);
                self.emit(CIROp::Label(end_label));
                else_id
            }
            TypedExprKind::FieldAccess { expr, field } => {
                let base_id = self.lower_expr(expr);
                if let Value::Identifier(field_name) = field {
                    self.emit(CIROp::GetField(base_id, field_name.to_string()))
                } else {
                    unimplemented!("Non-identifier field access")
                }
            }
            TypedExprKind::Assign { target, expr } => {
                let value_id = self.lower_expr(expr);
                if let TypedExprKind::Value(Value::Identifier(_)) = &target.kind {
                    self.emit(CIROp::Store(target.node_id, value_id.clone()));
                }
                value_id
            }
        }
    }

    pub fn lower_program(
        &mut self,
        program: &[TypedExpr],
    ) -> (Vec<CIRInstruction>, HashMap<NodeId, Vec<CIRInstruction>>) {
        for expr in program {
            let _ = self.lower_expr(expr);
        }
        (self.instructions.clone(), self.functions.clone())
    }
}
