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
    // Add more as we expand (Bool, Struct, etc.)
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
    Const(CIRType, CirValue),       // Literal value
    Add(CIRType, ValueId, ValueId), // Addition
    Call(usize, Vec<ValueId>),      // Call by block index
    Return(ValueId),
    Branch(ValueId, String, String), // condition, then_label, else_label
    Label(String),
    Jump(String), // Basic block label
    Select(ValueId, ValueId, ValueId),
    Gt(CIRType, ValueId, ValueId), // Greater than
    GtOrEq(CIRType, ValueId, ValueId),
    Lt(CIRType, ValueId, ValueId), // Less than
    LtOrEq(CIRType, ValueId, ValueId),
    Eq(CIRType, ValueId, ValueId),             // Equal
    Neq(CIRType, ValueId, ValueId),            // Not equal
    Struct(CIRType, HashMap<String, ValueId>), // Type and field values
    GetField(ValueId, String),
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
    For {
        var: ValueId,       // Variable to bind each element
        list: ValueId,      // List to iterate over
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

pub struct SSACIRBuilder {
    blocks: Vec<CIRBlock>,
    function_map: HashMap<String, usize>,
    block_temp_counters: HashMap<usize, usize>,
    param_map: HashMap<String, ValueId>,
    label_counter: usize, // For unique labels
}

impl SSACIRBuilder {
    pub fn new() -> Self {
        SSACIRBuilder {
            blocks: Vec::new(),
            function_map: HashMap::new(),
            block_temp_counters: HashMap::new(),
            param_map: HashMap::new(),
            label_counter: 0,
        }
    }

    fn new_temp(&mut self, block_id: usize) -> ValueId {
        let counter = self.block_temp_counters.entry(block_id).or_insert(0);
        let temp = format!("%{}", *counter);
        *counter += 1;
        ValueId(temp)
    }

    fn new_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    // fn emit(&mut self, block_id: usize, op: CIROp) -> ValueId {
    //     let result = match op {
    //         CIROp::Label(_) | CIROp::Branch(_, _, _) | CIROp::Return(_) => ValueId("".to_string()), // No result
    //         _ => self.new_temp(block_id),
    //     };
    //     if let Some(block) = self.blocks.iter_mut().find(|b| b.id == block_id) {
    //         block
    //             .instructions
    //             .push(CIRInstruction::new(op, result.clone()));
    //     }
    //     result
    // }

    fn emit(&mut self, block_id: usize, op: CIROp) -> ValueId {
        let result = match op {
            CIROp::Label(_)
            | CIROp::Branch(_, _, _)
            | CIROp::Jump(_)
            | CIROp::Return(_)
            | CIROp::Store(_, _)
            | CIROp::StoreAt(_, _, _) => ValueId("".to_string()),
            _ => self.new_temp(block_id),
        };
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
            TypedExprKind::Let {
                id,
                pat,
                expr,
                constness,
            } => {
                let value_id = self.lower_expr(expr, block_id);
                if let Value::Identifier(name) = id {
                    self.param_map.insert(name.to_string(), value_id.clone());
                }
                value_id // Return the value_id of the expression (e.g., for let p = ...)
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
                    self.block_temp_counters.insert(block_id, 0);
                    self.param_map.clear();
                    println!("Params for {}: {:?}", fn_name, params);
                    for (i, param) in params.iter().enumerate() {
                        if let Value::Identifier(param_name) = &param.name {
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
                    println!("the function map: {:?}", self.function_map);
                    println!("The function's name is: {:?}", fn_name);
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
            // TypedExprKind::If {
            //     condition,
            //     then_branch,
            //     else_branch,
            // } => {
            //     let cond_id = self.lower_expr(condition, block_id);
            //     let then_label = self.new_label();
            //     let else_label = self.new_label();
            //     let merge_label = self.new_label();
            //     self.emit(
            //         block_id,
            //         CIROp::Branch(cond_id.clone(), then_label.clone(), else_label.clone()),
            //     );
            //     self.emit(block_id, CIROp::Label(then_label));
            //     let then_id = self.lower_expr(then_branch, block_id);
            //     self.emit(block_id, CIROp::Jump(merge_label.clone()));
            //     self.emit(block_id, CIROp::Label(else_label));
            //     let else_id = self.lower_expr(else_branch, block_id);
            //     self.emit(block_id, CIROp::Label(merge_label));
            //     // Only use Select if both branches return non-Unit values
            //     if then_branch.type_info.clone().into_type() != Some(Type::Unit)
            //         && else_branch.type_info.clone().into_type() != Some(Type::Unit)
            //     {
            //         let result = self.emit(block_id, CIROp::Select(cond_id, then_id, else_id));
            //         result
            //     } else {
            //         ValueId("unit".to_string()) // No value to return, just control flow
            //     }
            // }
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
            TypedExprKind::Block { statements } => {
                let mut last_id = None;
                for stmt in statements {
                    last_id = Some(self.lower_expr(stmt, block_id));
                }
                println!("The last id: {:?}", last_id);
                last_id.unwrap_or_else(|| {
                    panic!("Block must have at least one statement in this subset");
                })
            }
            TypedExprKind::Struct { id, fields } => {
                // Define struct type (stored in CIRType, no runtime effect yet)
                let struct_type = CIRType::from(expr.type_info.as_type().unwrap());
                // For now, we’ll assume this is an instance creation if fields have values
                // If it’s a pure definition, it’s handled by type inference, not CIR
                ValueId(format!("struct_def_{}", self.blocks.len())) // Placeholder, typically no CIR for pure defs
            }
            TypedExprKind::FieldAccess { expr, field } => {
                let base_id = self.lower_expr(expr, block_id);
                if let Value::Identifier(field_name) = field {
                    let ty = CIRType::from(expr.type_info.as_type().unwrap());
                    self.emit(block_id, CIROp::GetField(base_id, field_name.to_string()))
                } else {
                    panic!("Field access must use an identifier");
                }
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
            // TypedExprKind::Assign { target, expr } => {
            //     let target_id = self.lower_expr(target, block_id);
            //     let value_id = self.lower_expr(expr, block_id);
            //     self.emit(block_id, CIROp::Store(target_id, value_id));
            //     ValueId("unit".to_string()) // Return a dummy ID for Unit
            // }
            // TypedExprKind::Assign { target, expr } => {
            //     let target_id = self.lower_expr(target, block_id);
            //     let expr_id = self.lower_expr(expr, block_id);
            //     match &target.kind {
            //         TypedExprKind::Index { expr: base, .. } => {
            //             if let TypedExprKind::Value(Value::Identifier(name)) = &base.kind {
            //                 let list_id = self.param_map[*name].clone();
            //                 self.emit(block_id, CIROp::Store(list_id, expr_id))
            //             } else {
            //                 panic!("Index target must be an identifier-based list");
            //             }
            //         }
            //         _ => self.emit(block_id, CIROp::Store(target_id.clone(), expr_id)),
            //     }
            // }
            TypedExprKind::Assign { target, expr } => {
                let expr_id = self.lower_expr(expr, block_id);
                match &target.kind {
                    TypedExprKind::Index { expr: base, index } => {
                        let base_id = self.lower_expr(base, block_id);
                        let index_id = self.lower_expr(index, block_id);
                        self.emit(block_id, CIROp::StoreAt(base_id, index_id, expr_id));
                        ValueId("unit".to_string())
                    }
                    _ => {
                        let target_id = self.lower_expr(target, block_id);
                        self.emit(block_id, CIROp::Store(target_id, expr_id));
                        ValueId("unit".to_string())
                    }
                }
            }
            TypedExprKind::StructInit { id, fields } => {
                let struct_name = if let Value::Identifier(name) = id {
                    name.to_string()
                } else {
                    panic!("StructInit id must be an identifier");
                };
                let field_values: HashMap<String, ValueId> = fields
                    .iter()
                    .map(|(field_name, value_expr)| {
                        let field_name_str: String = if let Value::Identifier(name) = field_name {
                            name.to_string()
                        } else {
                            panic!("Field name must be an identifier");
                        };
                        let value_id = self.lower_expr(value_expr, block_id);
                        (field_name_str, value_id)
                    })
                    .collect();
                let struct_type = match &expr.type_info {
                    TypeInfo::Known(ty) | TypeInfo::Inferred(ty) => CIRType::from(ty),
                    TypeInfo::Unknown => panic!("Struct type not inferred for {}", struct_name),
                };
                self.emit(block_id, CIROp::Struct(struct_type, field_values))
            }
            TypedExprKind::Index { expr, index } => {
                let list_id = self.lower_expr(expr, block_id);
                let index_id = self.lower_expr(index, block_id);
                self.emit(block_id, CIROp::Index(list_id, index_id))
            }
            TypedExprKind::While { guard, body } => {
                let body_start = self.new_label();
                let exit_label = self.new_label();
                let loop_start = self.new_label();

                // Loop header: compute guard each iteration
                self.emit(block_id, CIROp::Label(loop_start.clone()));
                let guard_id = self.lower_expr(guard, block_id); // Recompute guard
                self.emit(
                    block_id,
                    CIROp::Branch(guard_id.clone(), body_start.clone(), exit_label.clone()),
                );

                // Body
                self.emit(block_id, CIROp::Label(body_start));
                let _ = self.lower_expr(body, block_id);
                self.emit(block_id, CIROp::Jump(loop_start));

                // Exit
                self.emit(block_id, CIROp::Label(exit_label));

                ValueId("unit".to_string())
            }
            TypedExprKind::For {
                var,
                iterable,
                body,
            } => {
                let list_id = self.lower_expr(iterable, block_id);
                let var_id = if let Value::Identifier(name) = var {
                    let new_var_id = self.new_temp(block_id);
                    self.param_map.insert(name.to_string(), new_var_id.clone());
                    new_var_id
                } else {
                    self.new_temp(block_id)
                };
                let body_start = self.new_label();
                let exit_label = self.new_label();

                // Emit the For op to start iteration
                self.emit(
                    block_id,
                    CIROp::For {
                        var: var_id.clone(),
                        list: list_id,
                        body_start: body_start.clone(),
                        exit_label: exit_label.clone(),
                    },
                );

                // Body block
                self.emit(block_id, CIROp::Label(body_start.clone()));
                let _ = self.lower_expr(body, block_id); // Execute body
                self.emit(block_id, CIROp::Jump(body_start)); // Loop back

                // Exit block
                self.emit(block_id, CIROp::Label(exit_label));

                // Clean up param_map (optional, depending on scope handling)
                if let Value::Identifier(name) = var {
                    self.param_map.remove(*name);
                }

                ValueId("unit".to_string())
            }
            _ => unimplemented!("Other expressions not yet supported in SSA subset"),
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
                TypedExprKind::Struct { .. } => {}
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
        CIR {
            blocks: self.blocks.clone(),
            function_map: self.function_map.clone(),
        }
    }
}
