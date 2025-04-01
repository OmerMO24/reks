use crate::reks_eval::cir::{CIRBlock, CIRInstruction, CIROp, CIRType, CirValue, ValueId, CIR};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, IntValue, PointerValue};
use std::collections::HashMap;

// pub struct LLVMCodegen<'ctx> {
//     context: &'ctx Context,
//     module: Module<'ctx>,
//     builder: Builder<'ctx>,
//     values: HashMap<ValueId, BasicValueEnum<'ctx>>,
//     variables: HashMap<ValueId, PointerValue<'ctx>>,
//     current_function: Option<inkwell::values::FunctionValue<'ctx>>,
// }

// impl<'ctx> LLVMCodegen<'ctx> {
//     pub fn new(context: &'ctx Context) -> Self {
//         let module = context.create_module("program");
//         let builder = context.create_builder();
//         LLVMCodegen {
//             context,
//             module,
//             builder,
//             values: HashMap::new(),
//             variables: HashMap::new(),
//             current_function: None,
//         }
//     }

//     pub fn codegen_cir(&mut self, cir: &CIR) {
//         for block in &cir.blocks {
//             let fn_name = cir
//                 .function_map
//                 .iter()
//                 .find(|(_, &id)| id == block.id)
//                 .map(|(name, _)| name.clone())
//                 .unwrap_or_else(|| format!("block{}", block.id));
//             let i32_type = self.context.i32_type();
//             let param_types: Vec<BasicMetadataTypeEnum> = vec![i32_type.into()];
//             let fn_type = i32_type.fn_type(&param_types, false);
//             let function = self.module.add_function(&fn_name, fn_type, None);
//             self.current_function = Some(function);
//             let entry_block = self.context.append_basic_block(function, "entry");
//             self.builder.position_at_end(entry_block);

//             // Store parameters
//             let params = function.get_params();
//             if !params.is_empty() {
//                 self.values
//                     .insert(ValueId("param0".to_string()), params[0].into());
//             }

//             self.variables.clear();
//             for instr in &block.instructions {
//                 match &instr.op {
//                     CIROp::Const(ty, value) => match (ty, value) {
//                         (CIRType::Int, CirValue::Int(n)) => {
//                             let int_val = i32_type.const_int(*n as u64, false);
//                             self.values.insert(instr.result.clone(), int_val.into());
//                         }
//                         _ => unimplemented!("Only Int constants supported"),
//                     },
//                     CIROp::Store(target_id, value_id) => {
//                         let value = self.values.get(value_id).expect("Value not found");
//                         if let Some(&ptr) = self.variables.get(target_id) {
//                             self.builder.build_store(ptr, *value);
//                         } else {
//                             let ptr = self
//                                 .builder
//                                 .build_alloca(i32_type, &target_id.0)
//                                 .expect("Failed to allocate variable");
//                             self.builder.build_store(ptr, *value);
//                             self.variables.insert(target_id.clone(), ptr);
//                         }
//                     }
//                     CIROp::Add(_, left_id, right_id) => {
//                         let left = if let Some(&ptr) = self.variables.get(left_id) {
//                             self.builder
//                                 .build_load(i32_type, ptr, "load_left")
//                                 .expect("Failed to load left operand")
//                                 .into_int_value()
//                         } else {
//                             self.values
//                                 .get(left_id)
//                                 .expect("Left operand not found")
//                                 .into_int_value()
//                         };
//                         let right = if let Some(&ptr) = self.variables.get(right_id) {
//                             self.builder
//                                 .build_load(i32_type, ptr, "load_right")
//                                 .expect("Failed to load right operand")
//                                 .into_int_value()
//                         } else {
//                             self.values
//                                 .get(right_id)
//                                 .expect("Right operand not found")
//                                 .into_int_value()
//                         };
//                         let result = self
//                             .builder
//                             .build_int_add(left, right, &instr.result.0)
//                             .expect("Failed to build add instruction");
//                         self.values.insert(instr.result.clone(), result.into());
//                     }
//                     CIROp::Mul(left_id, right_id) => {
//                         let left = if let Some(&ptr) = self.variables.get(left_id) {
//                             self.builder
//                                 .build_load(i32_type, ptr, "load_left")
//                                 .expect("Failed to load left operand")
//                                 .into_int_value()
//                         } else {
//                             self.values
//                                 .get(left_id)
//                                 .expect("Left operand not found")
//                                 .into_int_value()
//                         };
//                         let right = if let Some(&ptr) = self.variables.get(right_id) {
//                             self.builder
//                                 .build_load(i32_type, ptr, "load_right")
//                                 .expect("Failed to load right operand")
//                                 .into_int_value()
//                         } else {
//                             self.values
//                                 .get(right_id)
//                                 .expect("Right operand not found")
//                                 .into_int_value()
//                         };
//                         let result = self
//                             .builder
//                             .build_int_mul(left, right, &instr.result.0)
//                             .expect("Failed to build mul instruction");
//                         self.values.insert(instr.result.clone(), result.into());
//                     }
//                     CIROp::Return(value_id) => {
//                         let value = if let Some(&ptr) = self.variables.get(value_id) {
//                             self.builder
//                                 .build_load(i32_type, ptr, "load_ret")
//                                 .expect("Failed to load return value")
//                         } else {
//                             *self.values.get(value_id).expect("Return value not found")
//                         };
//                         self.builder.build_return(Some(&value));
//                     }
//                     _ => unimplemented!("Op not yet supported: {:?}", instr.op),
//                 }
//             }
//         }
//     }

//     pub fn print_to_file(&self, path: &str) -> Result<(), String> {
//         self.module.verify().map_err(|e| e.to_string())?;
//         self.module.print_to_file(path).map_err(|e| e.to_string())
//     }

//     pub fn print_to_string(&self) -> String {
//         self.module.print_to_string().to_string()
//     }
// }

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<ValueId, inkwell::values::PointerValue<'ctx>>,
    values: HashMap<ValueId, inkwell::values::BasicValueEnum<'ctx>>,
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
            variables: HashMap::new(),
            values: HashMap::new(),
            current_function: None,
        }
    }

    pub fn codegen_cir(&mut self, cir: &CIR) {
        let i32_type = self.context.i32_type();

        for block in &cir.blocks {
            let fn_name = cir
                .function_map
                .iter()
                .find(|(_, &id)| id == block.id)
                .map(|(name, _)| name.clone())
                .expect("Function block not found");
            let param_types = vec![i32_type.into()];
            let fn_type = i32_type.fn_type(&param_types, false);
            let function = self.module.add_function(&fn_name, fn_type, None);
            let entry_block = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry_block);

            self.variables.clear();
            self.values.clear();
            self.current_function = Some(function);

            let params = function.get_params();
            for (i, param) in params.iter().enumerate() {
                let param_id = ValueId(format!("param{}", i + block.id * 10));
                let ptr = self
                    .builder
                    .build_alloca(i32_type, param_id.0.as_ref())
                    .expect("Failed to allocate param");
                self.builder.build_store(ptr, *param);
                self.variables.insert(param_id.clone(), ptr);
                println!("Param setup: {} = {:?}", param_id, ptr);
            }

            for instr in &block.instructions {
                match &instr.op {
                    CIROp::Const(ty, value) => {
                        if let (CIRType::Int, CirValue::Int(n)) = (ty, value) {
                            let int_val = i32_type.const_int(*n as u64, false);
                            self.values.insert(instr.result.clone(), int_val.into());
                        }
                    }
                    CIROp::Store(target_id, value_id) => {
                        println!("Store: target_id={:?}, value_id={:?}", target_id, value_id);
                        println!("variables={:?}", self.variables);
                        println!("values={:?}", self.values);
                        let value = if let Some(&ptr) = self.variables.get(value_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_store_value")
                                .expect("Failed to load store value")
                        } else {
                            *self.values.get(value_id).expect("Value not found")
                        };
                        if let Some(&ptr) = self.variables.get(target_id) {
                            self.builder.build_store(ptr, value);
                        } else {
                            let ptr = self
                                .builder
                                .build_alloca(i32_type, &target_id.0)
                                .expect("Failed to allocate variable");
                            self.builder.build_store(ptr, value);
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
                    CIROp::Call(block_id, arg_ids) => {
                        let func_name = cir
                            .function_map
                            .iter()
                            .find(|(_, &id)| id == *block_id)
                            .map(|(name, _)| name.clone())
                            .expect("Function block not found");
                        let func = self
                            .module
                            .get_function(&func_name)
                            .expect("Function not defined");
                        let args: Vec<BasicMetadataValueEnum> = arg_ids
                            .iter()
                            .map(|id| {
                                if let Some(&ptr) = self.variables.get(id) {
                                    let value = self
                                        .builder
                                        .build_load(i32_type, ptr, "load_arg")
                                        .expect("Failed to load arg");
                                    value.into()
                                } else {
                                    let value = self.values.get(id).expect("Arg not found");
                                    value.into_int_value().into()
                                }
                            })
                            .collect();
                        let result = self
                            .builder
                            .build_call(func, &args, &instr.result.0)
                            .expect("Failed to build call")
                            .try_as_basic_value()
                            .left()
                            .expect("Call should return a value");
                        self.values.insert(instr.result.clone(), result);
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
                    _ => unimplemented!("Unsupported op: {:?}", instr.op),
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
