use crate::reks_eval::cir::{CIRBlock, CIRInstruction, CIROp, CIRType, CirValue, ValueId, CIR};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, IntValue, PointerValue};
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

    //         // Store parameters in both variables and values
    //         let params = function.get_params();
    //         if !params.is_empty() {
    //             let param_id = ValueId(format!("param{}", block.id * 10)); // e.g., param0, param10
    //             let ptr = self
    //                 .builder
    //                 .build_alloca(i32_type, &param_id.0)
    //                 .expect("Failed to allocate param");
    //             self.builder.build_store(ptr, params[0]);
    //             self.variables.insert(param_id.clone(), ptr);
    //             let loaded_param = self
    //                 .builder
    //                 .build_load(i32_type, ptr, "load_param")
    //                 .expect("Failed to load param");
    //             self.values.insert(param_id.clone(), loaded_param.into());
    //             println!("Param setup: {:?} = {:?}", param_id, ptr);
    //         }

    //         let mut label_blocks: HashMap<String, BasicBlock> = HashMap::new();
    //         for instr in &block.instructions {
    //             if let CIROp::Label(label) = &instr.op {
    //                 let bb = self.context.append_basic_block(function, label);
    //                 label_blocks.insert(label.clone(), bb);
    //             }
    //         }

    //         let mut then_block: Option<BasicBlock> = None;
    //         let mut else_block: Option<BasicBlock> = None;

    //         // Only clear variables, keep values for call results
    //         self.variables.clear();
    //         self.builder.position_at_end(entry_block);

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
    //                     println!("Store: target_id={:?}, value_id={:?}", target_id, value_id);
    //                     println!("variables={:?}", self.variables);
    //                     println!("values={:?}", self.values);
    //                     let value = if let Some(&ptr) = self.variables.get(value_id) {
    //                         self.builder
    //                             .build_load(i32_type, ptr, "load_store_value")
    //                             .expect("Failed to load store value")
    //                     } else {
    //                         *self.values.get(value_id).expect("Value not found")
    //                     };
    //                     if let Some(&ptr) = self.variables.get(target_id) {
    //                         self.builder.build_store(ptr, value);
    //                     } else {
    //                         let ptr = self
    //                             .builder
    //                             .build_alloca(i32_type, &target_id.0)
    //                             .expect("Failed to allocate variable");
    //                         self.builder.build_store(ptr, value);
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
    //                 CIROp::Gt(_, left_id, right_id) => {
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
    //                         .build_int_compare(
    //                             inkwell::IntPredicate::SGT, // Signed greater than
    //                             left,
    //                             right,
    //                             &instr.result.0,
    //                         )
    //                         .expect("Failed to build gt instruction");
    //                     self.values.insert(instr.result.clone(), result.into());
    //                 }
    //                 CIROp::Call(block_id, arg_ids) => {
    //                     let func_name = cir
    //                         .function_map
    //                         .iter()
    //                         .find(|(_, &id)| id == *block_id)
    //                         .map(|(name, _)| name.clone())
    //                         .expect("Function block not found");
    //                     let func = self
    //                         .module
    //                         .get_function(&func_name)
    //                         .expect("Function not defined");
    //                     let args: Vec<BasicMetadataValueEnum> = arg_ids
    //                         .iter()
    //                         .map(|id| {
    //                             if let Some(&ptr) = self.variables.get(id) {
    //                                 let value = self
    //                                     .builder
    //                                     .build_load(i32_type, ptr, "load_arg")
    //                                     .expect("Failed to load arg");
    //                                 value.into()
    //                             } else {
    //                                 let value = self.values.get(id).expect("Arg not found");
    //                                 value.into_int_value().into()
    //                             }
    //                         })
    //                         .collect();
    //                     let result = self
    //                         .builder
    //                         .build_call(func, &args, &instr.result.0)
    //                         .expect("Failed to build call")
    //                         .try_as_basic_value()
    //                         .left()
    //                         .expect("Call should return a value");
    //                     self.values.insert(instr.result.clone(), result);
    //                     println!("Call result: {:?} = {:?}", instr.result, result);
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
    //                 _ => unimplemented!("Unsupported op: {:?}", instr.op),
    //             }
    //         }
    //     }
    // }

    // pub fn codegen_cir(&mut self, cir: &CIR) {
    //     for block in &cir.blocks {
    //         let fn_name = cir
    //             .function_map
    //             .iter()
    //             .find(|(_, &id)| id == block.id)
    //             .map(|(name, _)| name.clone())
    //             .unwrap_or_else(|| format!("block{}", block.id));
    //         let i32_type = self.context.i32_type();
    //         let param_count = 1; //if block.id == 0 { 2 } else { 1 }; // max: 2 params, main: 1 param
    //         let param_types: Vec<BasicMetadataTypeEnum> = vec![i32_type.into(); param_count];
    //         let fn_type = i32_type.fn_type(&param_types, false);
    //         let function = self.module.add_function(&fn_name, fn_type, None);
    //         self.current_function = Some(function);
    //         let entry_block = self.context.append_basic_block(function, "entry");
    //         self.builder.position_at_end(entry_block);

    //         // Store all parameters
    //         let params = function.get_params();
    //         for (i, param) in params.iter().enumerate() {
    //             let param_id = ValueId(format!("param{}", i + block.id * 10));
    //             let ptr = self
    //                 .builder
    //                 .build_alloca(i32_type, &param_id.0)
    //                 .expect("Failed to allocate param");
    //             self.builder.build_store(ptr, *param);
    //             self.variables.insert(param_id.clone(), ptr);
    //             let loaded_param = self
    //                 .builder
    //                 .build_load(i32_type, ptr, "load_param")
    //                 .expect("Failed to load param");
    //             self.values.insert(param_id.clone(), loaded_param.into());
    //             println!("Param setup: {:?} = {:?}", param_id, ptr);
    //         }

    //         // Rest of your existing codegen_cir remains unchanged...
    //         let mut label_blocks: HashMap<String, BasicBlock> = HashMap::new();
    //         for instr in &block.instructions {
    //             if let CIROp::Label(label) = &instr.op {
    //                 let bb = self.context.append_basic_block(function, label);
    //                 label_blocks.insert(label.clone(), bb);
    //             }
    //         }

    //         let mut then_block: Option<BasicBlock> = None;
    //         let mut else_block: Option<BasicBlock> = None;

    //         self.variables.clear();
    //         self.builder.position_at_end(entry_block);

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
    //                     println!("Store: target_id={:?}, value_id={:?}", target_id, value_id);
    //                     println!("variables={:?}", self.variables);
    //                     println!("values={:?}", self.values);
    //                     let value = if let Some(&ptr) = self.variables.get(value_id) {
    //                         self.builder
    //                             .build_load(i32_type, ptr, "load_store_value")
    //                             .expect("Failed to load store value")
    //                     } else {
    //                         *self.values.get(value_id).expect("Value not found")
    //                     };
    //                     if let Some(&ptr) = self.variables.get(target_id) {
    //                         self.builder.build_store(ptr, value);
    //                     } else {
    //                         let ptr = self
    //                             .builder
    //                             .build_alloca(i32_type, &target_id.0)
    //                             .expect("Failed to allocate variable");
    //                         self.builder.build_store(ptr, value);
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
    //                 CIROp::Gt(_, left_id, right_id) => {
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
    //                         .build_int_compare(
    //                             inkwell::IntPredicate::SGT,
    //                             left,
    //                             right,
    //                             &instr.result.0,
    //                         )
    //                         .expect("Failed to build gt instruction");
    //                     self.values.insert(instr.result.clone(), result.into());
    //                 }
    //                 CIROp::Call(block_id, arg_ids) => {
    //                     let func_name = cir
    //                         .function_map
    //                         .iter()
    //                         .find(|(_, &id)| id == *block_id)
    //                         .map(|(name, _)| name.clone())
    //                         .expect("Function block not found");
    //                     let func = self
    //                         .module
    //                         .get_function(&func_name)
    //                         .expect("Function not defined");
    //                     let args: Vec<BasicMetadataValueEnum> = arg_ids
    //                         .iter()
    //                         .map(|id| {
    //                             if let Some(&ptr) = self.variables.get(id) {
    //                                 let value = self
    //                                     .builder
    //                                     .build_load(i32_type, ptr, "load_arg")
    //                                     .expect("Failed to load arg");
    //                                 value.into()
    //                             } else {
    //                                 let value = self.values.get(id).expect("Arg not found");
    //                                 value.into_int_value().into()
    //                             }
    //                         })
    //                         .collect();
    //                     let result = self
    //                         .builder
    //                         .build_call(func, &args, &instr.result.0)
    //                         .expect("Failed to build call")
    //                         .try_as_basic_value()
    //                         .left()
    //                         .expect("Call should return a value");
    //                     self.values.insert(instr.result.clone(), result);
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
    //                 CIROp::Branch(cond_id, then_label, else_label) => {
    //                     let cond = self
    //                         .values
    //                         .get(cond_id)
    //                         .expect("Condition not found")
    //                         .into_int_value();
    //                     let then_bb = *label_blocks.get(then_label).expect("Then label not found");
    //                     let else_bb = *label_blocks.get(else_label).expect("Else label not found");
    //                     self.builder
    //                         .build_conditional_branch(cond, then_bb, else_bb)
    //                         .expect("Failed to build branch");
    //                     then_block = Some(then_bb);
    //                     else_block = Some(else_bb);
    //                 }
    //                 CIROp::Select(cond_id, then_id, else_id) => {
    //                     let then_val = self
    //                         .values
    //                         .get(then_id)
    //                         .expect("Then value not found")
    //                         .into_int_value();
    //                     let else_val = self
    //                         .values
    //                         .get(else_id)
    //                         .expect("Else value not found")
    //                         .into_int_value();
    //                     let result = self
    //                         .builder
    //                         .build_phi(i32_type, &instr.result.0)
    //                         .expect("Failed to build phi node");
    //                     let then_bb = then_block.expect("Then block not set");
    //                     let else_bb = else_block.expect("Else block not set");
    //                     result.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
    //                     self.values
    //                         .insert(instr.result.clone(), result.as_basic_value());
    //                 }
    //                 CIROp::Label(label) => {
    //                     let bb = *label_blocks.get(label).expect("Label not found");
    //                     self.builder.position_at_end(bb);
    //                 }
    //                 CIROp::Jump(target_label) => {
    //                     let target_bb = *label_blocks
    //                         .get(target_label)
    //                         .expect("Target label not found");
    //                     self.builder
    //                         .build_unconditional_branch(target_bb)
    //                         .expect("Failed to build jump");
    //                 }
    //                 CIROp::List(element_ids) => {
    //                     // Allocate array on stack (fixed size for simplicity)
    //                     let array_type = i32_type.array_type(element_ids.len() as u32);
    //                     let array_ptr = self
    //                         .builder
    //                         .build_alloca(array_type, &instr.result.0)
    //                         .expect("Failed to allocate array");
    //                     for (i, elem_id) in element_ids.iter().enumerate() {
    //                         let elem_value = if let Some(&ptr) = self.variables.get(elem_id) {
    //                             self.builder
    //                                 .build_load(i32_type, ptr, "load_elem")
    //                                 .expect("Failed to load element")
    //                         } else {
    //                             *self.values.get(elem_id).expect("Element not found")
    //                         };
    //                         let index = i32_type.const_int(i as u64, false);
    //                         let gep = unsafe {
    //                             self.builder
    //                                 .build_gep(
    //                                     array_type,
    //                                     array_ptr,
    //                                     &[i32_type.const_int(0, false), index],
    //                                     "gep",
    //                                 )
    //                                 .expect("Failed to build GEP")
    //                         };
    //                         self.builder.build_store(gep, elem_value);
    //                     }
    //                     self.variables.insert(instr.result.clone(), array_ptr);
    //                 }
    //                 CIROp::Index(list_id, index_id) => {
    //                     let list_ptr = self.variables.get(list_id).expect("List pointer not found");
    //                     let index = if let Some(&ptr) = self.variables.get(index_id) {
    //                         self.builder
    //                             .build_load(i32_type, ptr, "load_index")
    //                             .expect("Failed to load index")
    //                             .into_int_value()
    //                     } else {
    //                         self.values
    //                             .get(index_id)
    //                             .expect("Index not found")
    //                             .into_int_value()
    //                     };
    //                     let array_type = i32_type.array_type(2); // Fixed size 2 for this test
    //                     let gep = unsafe {
    //                         self.builder
    //                             .build_gep(
    //                                 array_type,
    //                                 *list_ptr,
    //                                 &[i32_type.const_int(0, false), index],
    //                                 "gep",
    //                             )
    //                             .expect("Failed to build GEP")
    //                     };
    //                     let result = self
    //                         .builder
    //                         .build_load(i32_type, gep, &instr.result.0)
    //                         .expect("Failed to load array element");
    //                     self.values.insert(instr.result.clone(), result.into());
    //                 }
    //                 // Add other cases as needed (Branch, Label, Jump, Select) if not already present
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
            let param_count = 1; // Only one param for main
            let param_types: Vec<BasicMetadataTypeEnum> = vec![i32_type.into(); param_count];
            let fn_type = i32_type.fn_type(&param_types, false);
            let function = self.module.add_function(&fn_name, fn_type, None);
            self.current_function = Some(function);
            let entry_block = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry_block);

            // Store parameters
            let params = function.get_params();
            for (i, param) in params.iter().enumerate() {
                let param_id = ValueId(format!("param{}", i + block.id * 10));
                let ptr = self
                    .builder
                    .build_alloca(i32_type, &param_id.0)
                    .expect("Failed to allocate param");
                self.builder.build_store(ptr, *param);
                self.variables.insert(param_id.clone(), ptr);
                let loaded_param = self
                    .builder
                    .build_load(i32_type, ptr, "load_param")
                    .expect("Failed to load param");
                self.values.insert(param_id.clone(), loaded_param.into());
                println!("Param setup: {:?} = {:?}", param_id, ptr);
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
                        println!("Store: target_id={:?}, value_id={:?}", target_id, value_id);
                        println!("variables={:?}", self.variables);
                        println!("values={:?}", self.values);
                        let value = if let Some(&ptr) = self.variables.get(value_id) {
                            ptr // Use pointer directly for arrays
                        } else {
                            let val = self.values.get(value_id).expect("Value not found");
                            if val.is_pointer_value() {
                                val.into_pointer_value()
                            } else {
                                let ptr = self
                                    .builder
                                    .build_alloca(i32_type, "temp_store")
                                    .expect("Failed to allocate temp");
                                self.builder.build_store(ptr, *val);
                                ptr
                            }
                        };
                        let target_ptr = if let Some(&ptr) = self.variables.get(target_id) {
                            ptr
                        } else {
                            let ptr_type = i32_type.ptr_type(inkwell::AddressSpace::default());
                            let ptr = self
                                .builder
                                .build_alloca(ptr_type, &target_id.0)
                                .expect("Failed to allocate variable pointer");
                            self.variables.insert(target_id.clone(), ptr);
                            ptr
                        };
                        self.builder.build_store(target_ptr, value);
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
                    CIROp::List(element_ids) => {
                        let array_type = i32_type.array_type(element_ids.len() as u32);
                        let array_ptr = self
                            .builder
                            .build_alloca(array_type, &instr.result.0)
                            .expect("Failed to allocate array");
                        for (i, elem_id) in element_ids.iter().enumerate() {
                            let elem_value = if let Some(&ptr) = self.variables.get(elem_id) {
                                self.builder
                                    .build_load(i32_type, ptr, "load_elem")
                                    .expect("Failed to load element")
                            } else {
                                *self.values.get(elem_id).expect("Element not found")
                            };
                            let index = i32_type.const_int(i as u64, false);
                            let gep = unsafe {
                                self.builder
                                    .build_gep(
                                        array_type,
                                        array_ptr,
                                        &[i32_type.const_int(0, false), index],
                                        "gep",
                                    )
                                    .expect("Failed to build GEP")
                            };
                            self.builder.build_store(gep, elem_value);
                        }
                        self.variables.insert(instr.result.clone(), array_ptr);
                    }
                    CIROp::Index(list_id, index_id) => {
                        let list_ptr_ptr =
                            self.variables.get(list_id).expect("List pointer not found");
                        let list_ptr = self
                            .builder
                            .build_load(
                                i32_type.ptr_type(inkwell::AddressSpace::default()),
                                *list_ptr_ptr,
                                "load_array_ptr",
                            )
                            .expect("Failed to load array pointer")
                            .into_pointer_value();
                        let index = if let Some(&ptr) = self.variables.get(index_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_index")
                                .expect("Failed to load index")
                                .into_int_value()
                        } else {
                            self.values
                                .get(index_id)
                                .expect("Index not found")
                                .into_int_value()
                        };
                        let array_type = i32_type.array_type(2); // Fixed size 2
                        let gep = unsafe {
                            self.builder
                                .build_gep(
                                    array_type,
                                    list_ptr,
                                    &[i32_type.const_int(0, false), index],
                                    "gep",
                                )
                                .expect("Failed to build GEP")
                        };
                        let result = self
                            .builder
                            .build_load(i32_type, gep, &instr.result.0)
                            .expect("Failed to load array element");
                        self.values.insert(instr.result.clone(), result.into());
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
