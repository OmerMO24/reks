use crate::reks_eval::ripcore::Interpreter;
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
    Bool(bool),
    Struct(HashMap<String, CirValue>),
    List(Vec<CirValue>),
    Unit,
}

#[derive(Debug, Clone)]
pub enum CIRType {
    Int,
    Unit,
    Bool, // For conditionals
    Function(Vec<CIRType>, Box<CIRType>),
    Struct(HashMap<String, CIRType>), // Field name -> type
    List(Box<CIRType>),
}

impl From<&Type> for CIRType {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Int => CIRType::Int,
            Type::Bool => CIRType::Bool,
            Type::Struct(_, fields) => CIRType::Struct(
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), CIRType::from(ty)))
                    .collect(),
            ),
            Type::Function(params, ret) => CIRType::Function(
                params.iter().map(|t| t.into()).collect(),
                Box::new(ret.as_ref().into()),
            ),
            Type::Unit => CIRType::Unit, // Add this
            Type::List(elem_ty) => CIRType::List(Box::new(CIRType::from(elem_ty.as_ref()))),
            _ => unimplemented!("Other types not yet supported"),
        }
    }
}

// CIR Operations
#[derive(Debug, Clone)]
pub enum CIROp {
    Const(CIRType, CirValue),        // Literal value
    Add(CIRType, ValueId, ValueId),  // Addition
    Call(usize, Vec<ValueId>, bool), // Call by block index
    Return(ValueId),
    Branch(ValueId, String, String), // condition, then_label, else_label
    Label(String),
    Jump(String), // Basic block label
    Select(ValueId, ValueId, ValueId),
    Gt(CIRType, ValueId, ValueId), // Greater than
    GtOrEq(CIRType, ValueId, ValueId),
    Lt(CIRType, ValueId, ValueId), // Less than
    LtOrEq(CIRType, ValueId, ValueId),
    Eq(CIRType, ValueId, ValueId),  // Equal
    Neq(CIRType, ValueId, ValueId), // Not equal
    Store(ValueId, ValueId),
    List(Vec<ValueId>),
    Index(ValueId, ValueId),
    Sub(ValueId, ValueId),
    Mul(ValueId, ValueId),
    Div(ValueId, ValueId),
    Exponent(ValueId, ValueId), // base ^ exp
    Modulo(ValueId, ValueId),
    StoreAt(ValueId, ValueId, ValueId),
    // New ops for loops
    While {
        guard: ValueId,     // Condition to check
        body_start: String, // Label for loop body
        exit_label: String, // Label for loop exit
    },
}

// SSA ValueId (temporary or parameter slot)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    pub function_map: HashMap<String, usize>,
}

impl CIR {
    pub fn patch_cir(&mut self) {
        let mut interpreter = Interpreter::new(self.clone());
        for block in &mut self.blocks {
            println!("The block: {:?}", block);
            let mut new_instructions = Vec::new();
            for instr in &block.instructions {
                match &instr.op {
                    CIROp::Call(block_id, arg_ids, true) => {
                        let fn_name = self
                            .function_map
                            .iter()
                            .find(|(_, &id)| id == *block_id)
                            .map(|(name, _)| name.clone())
                            .expect("Function not found");
                        let args: Vec<CirValue> = arg_ids
                            .iter()
                            .map(|id| {
                                block
                                    .instructions
                                    .iter()
                                    .find_map(|prev| match &prev.op {
                                        CIROp::Const(_, val) if prev.result == *id => {
                                            Some(val.clone())
                                        }
                                        _ => None,
                                    })
                                    .unwrap_or_else(|| {
                                        panic!(
                                            "Compeval arg {} not found in block {}",
                                            id.0, block.id
                                        )
                                    })
                            })
                            .collect();
                        println!(
                            "Patching Call in block {}: fn={}, orig_result={:?}",
                            block.id, fn_name, instr.result
                        );
                        if let Some(result) = interpreter.eval_function(&fn_name, args) {
                            let new_instr = CIRInstruction::new(
                                CIROp::Const(CIRType::Int, result),
                                instr.result.clone(), // Should be %2
                            );
                            println!("Patched to: {:?}", new_instr);
                            new_instructions.push(new_instr);
                        } else {
                            panic!("Failed to evaluate compeval call to {}", fn_name);
                        }
                    }
                    _ => new_instructions.push(instr.clone()),
                }
            }
            block.instructions = new_instructions;
        }
    }
}

pub struct SSACIRBuilder {
    blocks: Vec<CIRBlock>,
    function_map: HashMap<String, usize>,
    block_temp_counters: HashMap<usize, usize>,
    param_map: HashMap<String, ValueId>,
    struct_defs: HashMap<String, CIRType>,
    label_counter: usize, // For unique labels
}

impl SSACIRBuilder {
    pub fn new() -> Self {
        SSACIRBuilder {
            blocks: Vec::new(),
            function_map: HashMap::new(),
            block_temp_counters: HashMap::new(),
            param_map: HashMap::new(),
            struct_defs: HashMap::new(),
            label_counter: 0,
        }
    }

    fn new_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    fn new_temp(&mut self, block_id: usize) -> ValueId {
        let counter = self.block_temp_counters.entry(block_id).or_insert(0);
        let temp = format!("%{}", *counter);
        *counter += 1;
        println!(
            "new_temp for block {}: {} (counter now {})",
            block_id, temp, *counter
        );
        ValueId(temp)
    }

    fn emit(&mut self, block_id: usize, op: CIROp) -> ValueId {
        let result = match &op {
            CIROp::Label(_)
            | CIROp::Branch(_, _, _)
            | CIROp::Jump(_)
            | CIROp::Return(_)
            | CIROp::Store(_, _)
            | CIROp::StoreAt(_, _, _) => ValueId("".to_string()),
            CIROp::Call(_, _, _) => panic!("Call should provide its own result ID via lower_expr"),
            _ => self.new_temp(block_id),
        };
        if let Some(block) = self.blocks.iter_mut().find(|b| b.id == block_id) {
            println!(
                "Emitting in block {}: {:?} with result {:?}",
                block_id, op, result
            );
            block
                .instructions
                .push(CIRInstruction::new(op, result.clone()));
        }
        result
    }

    fn lower_expr(&mut self, expr: &TypedExpr, block_id: usize) -> ValueId {
        println!("Lowering expr in block {}: {:?}", block_id, expr.kind);
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
            TypedExprKind::Let {
                id,
                pat,
                expr,
                constness,
            } => {
                let value_id = self.lower_expr(expr, block_id);
                println!("Let binding: value_id = {:?}", value_id);
                if let Value::Identifier(name) = id {
                    let target_id = ValueId(name.to_string());
                    self.emit(block_id, CIROp::Store(target_id.clone(), value_id.clone()));
                    self.param_map.insert(name.to_string(), target_id.clone());
                    target_id
                } else {
                    value_id
                }
            }
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
                    self.block_temp_counters.insert(block_id, 0); // Reset counter
                    self.param_map.clear();
                    println!(
                        "Fn {}: block_id = {}, params = {:?}",
                        fn_name, block_id, params
                    );
                    for (i, param) in params.iter().enumerate() {
                        if let Value::Identifier(param_name) = param.name {
                            let param_id = ValueId(format!("param{}", i + block_id * 10));
                            self.param_map
                                .insert(param_name.to_string(), param_id.clone());
                            println!("Inserted param '{}': {:?}", param_name, param_id);
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
            TypedExprKind::Call {
                name,
                args,
                compeval,
                ..
            } => {
                let mut arg_ids = Vec::new();
                for arg in args {
                    let arg_id = self.lower_expr(arg, block_id);
                    arg_ids.push(arg_id.clone());
                    println!("Call arg: {:?}", arg_id);
                }
                if let TypedExprKind::Value(Value::Identifier(fn_name)) = &name.kind {
                    let fn_block_id = *self.function_map.get(*fn_name).expect("Function not found");
                    let call_result_id = self.new_temp(block_id);
                    println!(
                        "Emitting Call in block {}: fn={}, args={:?}, compeval={}, result={:?}",
                        block_id, fn_name, arg_ids, compeval, call_result_id
                    );
                    // Emit Call directly with call_result_id
                    if let Some(block) = self.blocks.iter_mut().find(|b| b.id == block_id) {
                        block.instructions.push(CIRInstruction::new(
                            CIROp::Call(fn_block_id, arg_ids, *compeval),
                            call_result_id.clone(),
                        ));
                    }
                    println!("Call result ID after emit: {:?}", call_result_id);
                    call_result_id
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
                    InfixOpKind::Greater => self.emit(block_id, CIROp::Gt(ty, left_id, right_id)),
                    InfixOpKind::GreaterOrEq => {
                        self.emit(block_id, CIROp::GtOrEq(ty, left_id, right_id))
                    }
                    InfixOpKind::Less => self.emit(block_id, CIROp::Lt(ty, left_id, right_id)),
                    InfixOpKind::LessOrEq => {
                        self.emit(block_id, CIROp::LtOrEq(ty, left_id, right_id))
                    }
                    InfixOpKind::Equals => self.emit(block_id, CIROp::Eq(ty, left_id, right_id)),
                    InfixOpKind::NotEq => self.emit(block_id, CIROp::Neq(ty, left_id, right_id)),
                    InfixOpKind::Sub => self.emit(block_id, CIROp::Sub(left_id, right_id)),
                    InfixOpKind::Mul => self.emit(block_id, CIROp::Mul(left_id, right_id)),
                    InfixOpKind::Div => self.emit(block_id, CIROp::Div(left_id, right_id)),
                    InfixOpKind::Exp => self.emit(block_id, CIROp::Exponent(left_id, right_id)),
                    InfixOpKind::Mod => self.emit(block_id, CIROp::Modulo(left_id, right_id)),
                    _ => unimplemented!("Only basic comparisons supported in this subset"),
                }
            }
            TypedExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_id = self.lower_expr(condition, block_id);
                let then_label = self.new_label();
                let else_label = self.new_label();
                let merge_label = self.new_label();

                self.emit(
                    block_id,
                    CIROp::Branch(cond_id.clone(), then_label.clone(), else_label.clone()),
                );

                // Then branch
                self.emit(block_id, CIROp::Label(then_label));
                let then_id = self.lower_expr(then_branch, block_id);
                self.emit(block_id, CIROp::Jump(merge_label.clone()));

                // Else branch
                self.emit(block_id, CIROp::Label(else_label));
                let else_id = if let TypedExprKind::Block { statements } = &else_branch.kind {
                    if statements.is_empty() {
                        // Emit a dummy Unit value for empty else block
                        self.emit(block_id, CIROp::Const(CIRType::Unit, CirValue::Unit))
                    } else {
                        self.lower_expr(else_branch, block_id)
                    }
                } else {
                    self.lower_expr(else_branch, block_id)
                };
                self.emit(block_id, CIROp::Jump(merge_label.clone()));

                // Merge point
                self.emit(block_id, CIROp::Label(merge_label));

                // Only use Select if both branches return non-Unit values
                if then_branch.type_info.clone().into_type() != Some(Type::Unit)
                    && else_branch.type_info.clone().into_type() != Some(Type::Unit)
                {
                    let result = self.emit(block_id, CIROp::Select(cond_id, then_id, else_id));
                    result
                } else {
                    ValueId("unit".to_string()) // No value to return, just control flow
                }
            }

            TypedExprKind::Block { statements } => {
                let mut last_id = None;
                for stmt in statements {
                    last_id = Some(self.lower_expr(stmt, block_id));
                }
                println!("Block last_id: {:?}", last_id);
                last_id.unwrap_or_else(|| panic!("Block must have at least one statement"))
            }
            TypedExprKind::List { elements } => {
                let element_ids: Vec<ValueId> = elements
                    .iter()
                    .map(|elem| self.lower_expr(elem, block_id))
                    .collect();
                let list_type = match &expr.type_info {
                    TypeInfo::Known(ty) | TypeInfo::Inferred(ty) => CIRType::from(ty),
                    TypeInfo::Unknown => panic!("List type not inferred"), // Temporary panic
                };
                self.emit(block_id, CIROp::List(element_ids))
            }
            TypedExprKind::Index { expr, index } => {
                let list_id = self.lower_expr(expr, block_id);
                let index_id = self.lower_expr(index, block_id);
                self.emit(block_id, CIROp::Index(list_id, index_id))
            }
            TypedExprKind::Assign { target, expr } => {
                let value_id = self.lower_expr(expr, block_id);
                match &target.kind {
                    TypedExprKind::Index {
                        expr: list_expr,
                        index,
                    } => {
                        let list_id = self.lower_expr(list_expr, block_id);
                        let index_id = self.lower_expr(index, block_id);
                        self.emit(block_id, CIROp::StoreAt(list_id, index_id, value_id));
                        ValueId("unit".to_string()) // No value returned from assignment
                    }
                    TypedExprKind::Value(Value::Identifier(name)) => {
                        let target_id = ValueId(name.to_string());
                        self.emit(block_id, CIROp::Store(target_id.clone(), value_id));
                        self.param_map.insert(name.to_string(), target_id.clone());
                        target_id
                    }

                    _ => unimplemented!("Only array index assignment supported"),
                }
            }
            TypedExprKind::While { guard, body } => {
                let loop_start = self.new_label();
                let body_start = self.new_label();
                let exit_label = self.new_label();

                self.emit(block_id, CIROp::Label(loop_start.clone()));
                let guard_id = self.lower_expr(guard, block_id);
                self.emit(
                    block_id,
                    CIROp::Branch(guard_id, body_start.clone(), exit_label.clone()),
                );

                self.emit(block_id, CIROp::Label(body_start));
                let _ = self.lower_expr(body, block_id);
                self.emit(block_id, CIROp::Jump(loop_start));

                self.emit(block_id, CIROp::Label(exit_label));
                ValueId("unit".to_string())
            }
            _ => unimplemented!("Unsupported expression: {:?}", expr.kind),
        }
    }

    pub fn lower_program(&mut self, program: &[TypedExpr]) -> CIR {
        let mut fn_index = 0;
        for expr in program {
            match &expr.kind {
                TypedExprKind::Fn {
                    name, params, body, ..
                } => {
                    let block_id = fn_index;
                    fn_index += 1;
                    self.blocks.push(CIRBlock {
                        id: block_id,
                        instructions: vec![],
                    });
                    if let Value::Identifier(fn_name) = name {
                        self.function_map.insert(fn_name.to_string(), block_id);
                    }
                    self.param_map.clear();
                    for (i, param) in params.iter().enumerate() {
                        if let Value::Identifier(p_name) = param.name {
                            let param_id = ValueId(format!("param{}", i + block_id * 10));
                            self.param_map.insert(p_name.to_string(), param_id.clone());
                        }
                    }
                    let body_id = self.lower_expr(body, block_id);
                    self.emit(block_id, CIROp::Return(body_id));
                }
                _ => {
                    let block_id = self.blocks.len();
                    self.blocks.push(CIRBlock {
                        id: block_id,
                        instructions: vec![],
                    });
                    let expr_id = self.lower_expr(expr, block_id);
                    self.emit(block_id, CIROp::Return(expr_id));
                }
            }
        }
        let mut cir = CIR {
            blocks: self.blocks.clone(),
            function_map: self.function_map.clone(),
        };
        cir.patch_cir();
        cir
    }
}
