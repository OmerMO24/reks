use crate::reks_eval::cir::{CIRBlock, CIRInstruction, CIROp, CIRType, CirValue, ValueId, CIR};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};
use std::collections::HashMap;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    values: HashMap<ValueId, BasicValueEnum<'ctx>>,
    variables: HashMap<ValueId, PointerValue<'ctx>>,
    current_function: Option<inkwell::values::FunctionValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("program");
        let builder = context.create_builder();
        LLVMCodegen {
            context,
            module,
            builder,
            values: HashMap::new(),
            variables: HashMap::new(),
            current_function: None,
        }
    }

    // pub fn codegen_cir(&mut self, cir: &CIR) {
    //     for block in &cir.blocks {
    //         let fn_name = cir
    //             .function_map
    //             .iter()
    //             .find(|(_, &id)| id == block.id)
    //             .map(|(name, _)| name.clone())
    //             .unwrap_or_else(|| format!("block{}", block.id));
    //         let i32_type = self.context.i32_type();
    //         let param_types: Vec<BasicMetadataTypeEnum> = vec![i32_type.into()];
    //         let fn_type = i32_type.fn_type(&param_types, false);
    //         let function = self.module.add_function(&fn_name, fn_type, None);
    //         self.current_function = Some(function);
    //         let entry_block = self.context.append_basic_block(function, "entry");
    //         self.builder.position_at_end(entry_block);

    //         // Store parameters
    //         let params = function.get_params();
    //         if !params.is_empty() {
    //             self.values
    //                 .insert(ValueId("param0".to_string()), params[0].into());
    //         }

    //         self.variables.clear();
    //         for instr in &block.instructions {
    //             match &instr.op {
    //                 CIROp::Const(ty, value) => match (ty, value) {
    //                     (CIRType::Int, CirValue::Int(n)) => {
    //                         let int_val = i32_type.const_int(*n as u64, false);
    //                         self.values.insert(instr.result.clone(), int_val.into());
    //                     }
    //                     _ => unimplemented!("Only Int constants supported"),
    //                 },
    //                 CIROp::Store(target_id, value_id) => {
    //                     let value = self.values.get(value_id).expect("Value not found");
    //                     if let Some(&ptr) = self.variables.get(target_id) {
    //                         self.builder.build_store(ptr, *value);
    //                     } else {
    //                         let ptr = self
    //                             .builder
    //                             .build_alloca(i32_type, &target_id.0)
    //                             .expect("Failed to allocate variable");
    //                         self.builder.build_store(ptr, *value);
    //                         self.variables.insert(target_id.clone(), ptr);
    //                     }
    //                 }
    //                 CIROp::Add(_, left_id, right_id) => {
    //                     let left = if let Some(&ptr) = self.variables.get(left_id) {
    //                         self.builder
    //                             .build_load(i32_type, ptr, "load_left")
    //                             .expect("Failed to load left operand")
    //                             .into_int_value()
    //                     } else {
    //                         self.values
    //                             .get(left_id)
    //                             .expect("Left operand not found")
    //                             .into_int_value()
    //                     };
    //                     let right = if let Some(&ptr) = self.variables.get(right_id) {
    //                         self.builder
    //                             .build_load(i32_type, ptr, "load_right")
    //                             .expect("Failed to load right operand")
    //                             .into_int_value()
    //                     } else {
    //                         self.values
    //                             .get(right_id)
    //                             .expect("Right operand not found")
    //                             .into_int_value()
    //                     };
    //                     let result = self
    //                         .builder
    //                         .build_int_add(left, right, &instr.result.0)
    //                         .expect("Failed to build add instruction");
    //                     self.values.insert(instr.result.clone(), result.into());
    //                 }
    //                 CIROp::Mul(left_id, right_id) => {
    //                     let left = if let Some(&ptr) = self.variables.get(left_id) {
    //                         self.builder
    //                             .build_load(i32_type, ptr, "load_left")
    //                             .expect("Failed to load left operand")
    //                             .into_int_value()
    //                     } else {
    //                         self.values
    //                             .get(left_id)
    //                             .expect("Left operand not found")
    //                             .into_int_value()
    //                     };
    //                     let right = if let Some(&ptr) = self.variables.get(right_id) {
    //                         self.builder
    //                             .build_load(i32_type, ptr, "load_right")
    //                             .expect("Failed to load right operand")
    //                             .into_int_value()
    //                     } else {
    //                         self.values
    //                             .get(right_id)
    //                             .expect("Right operand not found")
    //                             .into_int_value()
    //                     };
    //                     let result = self
    //                         .builder
    //                         .build_int_mul(left, right, &instr.result.0)
    //                         .expect("Failed to build mul instruction");
    //                     self.values.insert(instr.result.clone(), result.into());
    //                 }
    //                 CIROp::Return(value_id) => {
    //                     let value = if let Some(&ptr) = self.variables.get(value_id) {
    //                         self.builder
    //                             .build_load(i32_type, ptr, "load_ret")
    //                             .expect("Failed to load return value")
    //                     } else {
    //                         *self.values.get(value_id).expect("Return value not found")
    //                     };
    //                     self.builder.build_return(Some(&value));
    //                 }
    //                 _ => unimplemented!("Op not yet supported: {:?}", instr.op),
    //             }
    //         }
    //     }
    // }

    pub fn codegen_cir(&mut self, cir: &CIR) {
        for block in &cir.blocks {
            let fn_name = cir
                .function_map
                .iter()
                .find(|(_, &id)| id == block.id)
                .map(|(name, _)| name.clone())
                .unwrap_or_else(|| format!("block{}", block.id));
            let i32_type = self.context.i32_type();
            let param_types: Vec<BasicMetadataTypeEnum> = vec![i32_type.into()];
            let fn_type = i32_type.fn_type(&param_types, false);
            let function = self.module.add_function(&fn_name, fn_type, None);
            self.current_function = Some(function);
            let entry_block = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry_block);

            // Store parameters before clearing
            let params = function.get_params();
            if !params.is_empty() {
                self.values
                    .insert(ValueId("param0".to_string()), params[0].into());
            }

            let mut label_blocks: HashMap<String, BasicBlock> = HashMap::new();
            for instr in &block.instructions {
                if let CIROp::Label(label) = &instr.op {
                    let bb = self.context.append_basic_block(function, label);
                    label_blocks.insert(label.clone(), bb);
                }
            }

            let mut then_block: Option<BasicBlock> = None;
            let mut else_block: Option<BasicBlock> = None;

            self.variables.clear();
            self.builder.position_at_end(entry_block);

            for instr in &block.instructions {
                match &instr.op {
                    CIROp::Const(ty, value) => match (ty, value) {
                        (CIRType::Int, CirValue::Int(n)) => {
                            let int_val = i32_type.const_int(*n as u64, false);
                            self.values.insert(instr.result.clone(), int_val.into());
                        }
                        _ => unimplemented!("Only Int constants supported"),
                    },
                    CIROp::Store(target_id, value_id) => {
                        let value = self.values.get(value_id).expect("Value not found");
                        if let Some(&ptr) = self.variables.get(target_id) {
                            self.builder.build_store(ptr, *value);
                        } else {
                            let ptr = self
                                .builder
                                .build_alloca(i32_type, &target_id.0)
                                .expect("Failed to allocate variable");
                            self.builder.build_store(ptr, *value);
                            self.variables.insert(target_id.clone(), ptr);
                        }
                    }
                    CIROp::Add(_, left_id, right_id) => {
                        let left = if let Some(&ptr) = self.variables.get(left_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_left")
                                .expect("Failed to load left operand")
                                .into_int_value()
                        } else {
                            self.values
                                .get(left_id)
                                .expect("Left operand not found")
                                .into_int_value()
                        };
                        let right = if let Some(&ptr) = self.variables.get(right_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_right")
                                .expect("Failed to load right operand")
                                .into_int_value()
                        } else {
                            self.values
                                .get(right_id)
                                .expect("Right operand not found")
                                .into_int_value()
                        };
                        let result = self
                            .builder
                            .build_int_add(left, right, &instr.result.0)
                            .expect("Failed to build add instruction");
                        self.values.insert(instr.result.clone(), result.into());
                    }
                    CIROp::Mul(left_id, right_id) => {
                        let left = if let Some(&ptr) = self.variables.get(left_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_left")
                                .expect("Failed to load left operand")
                                .into_int_value()
                        } else {
                            self.values
                                .get(left_id)
                                .expect("Left operand not found")
                                .into_int_value()
                        };
                        let right = if let Some(&ptr) = self.variables.get(right_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_right")
                                .expect("Failed to load right operand")
                                .into_int_value()
                        } else {
                            self.values
                                .get(right_id)
                                .expect("Right operand not found")
                                .into_int_value()
                        };
                        let result = self
                            .builder
                            .build_int_mul(left, right, &instr.result.0)
                            .expect("Failed to build mul instruction");
                        self.values.insert(instr.result.clone(), result.into());
                    }
                    CIROp::Gt(ty, left_id, right_id) => {
                        let left = if let Some(&ptr) = self.variables.get(left_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_left")
                                .expect("Failed to load left operand")
                                .into_int_value()
                        } else {
                            self.values
                                .get(left_id)
                                .expect("Left operand not found")
                                .into_int_value()
                        };
                        let right = if let Some(&ptr) = self.variables.get(right_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_right")
                                .expect("Failed to load right operand")
                                .into_int_value()
                        } else {
                            self.values
                                .get(right_id)
                                .expect("Right operand not found")
                                .into_int_value()
                        };
                        let result = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::SGT,
                                left,
                                right,
                                &instr.result.0,
                            )
                            .expect("Failed to build gt instruction");
                        self.values.insert(instr.result.clone(), result.into());
                    }
                    CIROp::Lt(ty, left_id, right_id) => {
                        let left = if let Some(&ptr) = self.variables.get(left_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_left")
                                .expect("Failed to load left operand")
                                .into_int_value()
                        } else {
                            self.values
                                .get(left_id)
                                .expect("Left operand not found")
                                .into_int_value()
                        };
                        let right = if let Some(&ptr) = self.variables.get(right_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_right")
                                .expect("Failed to load right operand")
                                .into_int_value()
                        } else {
                            self.values
                                .get(right_id)
                                .expect("Right operand not found")
                                .into_int_value()
                        };
                        let result = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::SLT,
                                left,
                                right,
                                &instr.result.0,
                            )
                            .expect("Failed to build lt instruction");
                        self.values.insert(instr.result.clone(), result.into());
                    }
                    CIROp::Branch(cond_id, then_label, else_label) => {
                        let cond = self
                            .values
                            .get(cond_id)
                            .expect("Condition not found")
                            .into_int_value();
                        let then_bb = *label_blocks.get(then_label).expect("Then label not found");
                        let else_bb = *label_blocks.get(else_label).expect("Else label not found");
                        self.builder
                            .build_conditional_branch(cond, then_bb, else_bb)
                            .expect("Failed to build branch");
                        then_block = Some(then_bb);
                        else_block = Some(else_bb);
                    }
                    CIROp::Label(label) => {
                        let bb = *label_blocks.get(label).expect("Label not found");
                        self.builder.position_at_end(bb);
                    }
                    CIROp::Jump(target_label) => {
                        let target_bb = *label_blocks
                            .get(target_label)
                            .expect("Target label not found");
                        self.builder
                            .build_unconditional_branch(target_bb)
                            .expect("Failed to build jump");
                    }
                    CIROp::Select(cond_id, then_id, else_id) => {
                        let then_val = self
                            .values
                            .get(then_id)
                            .expect("Then value not found")
                            .into_int_value();
                        let else_val = self
                            .values
                            .get(else_id)
                            .expect("Else value not found")
                            .into_int_value();
                        let result = self
                            .builder
                            .build_phi(i32_type, &instr.result.0)
                            .expect("Failed to build phi node");
                        let then_bb = then_block.expect("Then block not set");
                        let else_bb = else_block.expect("Else block not set");
                        result.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                        self.values
                            .insert(instr.result.clone(), result.as_basic_value());
                    }
                    CIROp::Return(value_id) => {
                        let value = if let Some(&ptr) = self.variables.get(value_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_ret")
                                .expect("Failed to load return value")
                        } else {
                            *self.values.get(value_id).expect("Return value not found")
                        };
                        self.builder.build_return(Some(&value));
                    }
                    _ => unimplemented!("Op not yet supported: {:?}", instr.op),
                }
            }
        }
    }

    pub fn print_to_file(&self, path: &str) -> Result<(), String> {
        self.module.verify().map_err(|e| e.to_string())?;
        self.module.print_to_file(path).map_err(|e| e.to_string())
    }

    pub fn print_to_string(&self) -> String {
        self.module.print_to_string().to_string()
    }
}
