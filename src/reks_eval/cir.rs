use crate::reks_parse::{operators::*, utnode::*};
use crate::reks_type::{
    infer::*,
    resolve::{NameResolver, NodeId},
};
use std::collections::HashMap;

// CIR Value for compile-time results
#[derive(Debug, Clone)]
pub enum CirValue {
    Int(i32),
    // Add more as we expand (Bool, Struct, etc.)
}

// CIR Type (mirrors Type from infer.rs)
#[derive(Debug, Clone)]
pub enum CIRType {
    Int,
    Function(Vec<CIRType>, Box<CIRType>),
    // Add more as needed
}

impl From<&Type> for CIRType {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Int => CIRType::Int,
            Type::Function(params, ret) => CIRType::Function(
                params.iter().map(|t| t.into()).collect(),
                Box::new(ret.as_ref().into()),
            ),
            _ => unimplemented!("Other types not yet supported"),
        }
    }
}

// CIR Operations
#[derive(Debug, Clone)]
pub enum CIROp {
    Const(CIRType, CirValue),       // Literal value
    Add(CIRType, ValueId, ValueId), // Addition
    Call(usize, Vec<ValueId>),      // Call by block index
    Return(ValueId),                // Return value
}

// SSA ValueId (temporary or parameter slot)
#[derive(Debug, Clone)]
pub struct ValueId(pub String);

// CIR Instruction
#[derive(Debug, Clone)]
pub struct CIRInstruction {
    pub op: CIROp,
    pub result: ValueId, // SSA temporary assigned to this instruction
}

impl CIRInstruction {
    pub fn new(op: CIROp, result: ValueId) -> Self {
        CIRInstruction { op, result }
    }
}

// CIR Block (one per function)
#[derive(Debug, Clone)]
pub struct CIRBlock {
    pub id: usize, // Block index (e.g., 0 for add, 1 for main)
    pub instructions: Vec<CIRInstruction>,
}

// Full CIR program
#[derive(Debug, Clone)]
pub struct CIR {
    pub blocks: Vec<CIRBlock>,
}

pub struct SSACIRBuilder {
    blocks: Vec<CIRBlock>,
    function_map: HashMap<String, usize>,
    block_temp_counters: HashMap<usize, usize>, // Per-block temp counter
    param_map: HashMap<String, ValueId>,
}

impl SSACIRBuilder {
    pub fn new() -> Self {
        SSACIRBuilder {
            blocks: Vec::new(),
            function_map: HashMap::new(),
            block_temp_counters: HashMap::new(),
            param_map: HashMap::new(),
        }
    }

    fn new_temp(&mut self, block_id: usize) -> ValueId {
        let counter = self.block_temp_counters.entry(block_id).or_insert(0);
        let temp = format!("%{}", *counter);
        *counter += 1;
        ValueId(temp)
    }

    fn emit(&mut self, block_id: usize, op: CIROp) -> ValueId {
        let result = self.new_temp(block_id);
        if let Some(block) = self.blocks.iter_mut().find(|b| b.id == block_id) {
            block
                .instructions
                .push(CIRInstruction::new(op, result.clone()));
        }
        result
    }

    fn lower_expr(&mut self, expr: &TypedExpr, block_id: usize) -> ValueId {
        match &expr.kind {
            TypedExprKind::Value(val) => match val {
                Value::Num(n) => {
                    let ty = CIRType::from(expr.type_info.as_type().unwrap());
                    self.emit(
                        block_id,
                        CIROp::Const(ty, CirValue::Int((*n).try_into().unwrap())),
                    )
                }
                Value::Identifier(name) => {
                    println!("Looking up '{}' in param_map: {:?}", name, self.param_map);
                    if let Some(param_id) = self.param_map.get(*name) {
                        param_id.clone()
                    } else {
                        panic!("Unresolved identifier: {}", name);
                    }
                }
                _ => unimplemented!("Other Value types not yet supported"),
            },
            TypedExprKind::Fn {
                name,
                params,
                retty,
                body,
            } => {
                if let Value::Identifier(fn_name) = name {
                    let block_id = self.blocks.len();
                    self.function_map.insert(fn_name.to_string(), block_id);
                    self.blocks.push(CIRBlock {
                        id: block_id,
                        instructions: Vec::new(),
                    });
                    // Reset temp counter for this block
                    self.block_temp_counters.insert(block_id, 0);
                    // Map parameters to ValueIds
                    self.param_map.clear();
                    println!("Params for {}: {:?}", fn_name, params);
                    for (i, param) in params.iter().enumerate() {
                        if let Value::Identifier(param_name) = param.name {
                            let param_id = ValueId(format!("param{}", i));
                            self.param_map
                                .insert(param_name.to_string(), param_id.clone());
                            println!("Inserted '{}': {:?}", param_name, param_id);
                        }
                    }
                    println!("param_map after insertion: {:?}", self.param_map);
                    let body_id = self.lower_expr(body, block_id);
                    self.emit(block_id, CIROp::Return(body_id));
                    ValueId(format!("block{}", block_id))
                } else {
                    panic!("Function name must be an identifier");
                }
            }
            TypedExprKind::Call { name, args } => {
                let mut arg_ids = Vec::new();
                for arg in args {
                    arg_ids.push(self.lower_expr(arg, block_id));
                }
                if let TypedExprKind::Value(Value::Identifier(fn_name)) = &name.kind {
                    let fn_block_id = *self
                        .function_map
                        .get(fn_name.clone())
                        .expect("Function not found");
                    let ty = CIRType::from(expr.type_info.as_type().unwrap());
                    self.emit(block_id, CIROp::Call(fn_block_id, arg_ids))
                } else {
                    panic!("Call target must be an identifier");
                }
            }
            TypedExprKind::BinOp { left, op, right } => {
                let left_id = self.lower_expr(left, block_id);
                let right_id = self.lower_expr(right, block_id);
                let ty = CIRType::from(expr.type_info.as_type().unwrap());
                match op {
                    InfixOpKind::Add => self.emit(block_id, CIROp::Add(ty, left_id, right_id)),
                    _ => unimplemented!("Only addition supported in this subset"),
                }
            }
            TypedExprKind::Block { statements } => {
                let mut last_id = None;
                for stmt in statements {
                    last_id = Some(self.lower_expr(stmt, block_id));
                }
                last_id.unwrap_or_else(|| {
                    panic!("Block must have at least one statement in this subset");
                })
            }
            _ => unimplemented!("Other expressions not yet supported in SSA subset"),
        }
    }

    pub fn lower_program(&mut self, program: &[TypedExpr]) -> CIR {
        for expr in program {
            let _ = self.lower_expr(expr, self.blocks.len());
        }
        CIR {
            blocks: self.blocks.clone(),
        }
    }
}
