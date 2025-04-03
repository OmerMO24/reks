use crate::reks_eval::cir::{CIROp, CirValue, ValueId, CIR};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, PointerValue};
use std::collections::HashMap;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    current_function: Option<inkwell::values::FunctionValue<'ctx>>,
    variables: HashMap<ValueId, PointerValue<'ctx>>,
    values: HashMap<ValueId, inkwell::values::BasicValueEnum<'ctx>>,
    array_sizes: HashMap<ValueId, u32>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("program");
        let builder = context.create_builder();
        LLVMCodegen {
            context,
            module,
            builder,
            current_function: None,
            variables: HashMap::new(),
            values: HashMap::new(),
            array_sizes: HashMap::new(),
        }
    }

    pub fn print_to_file(&self, filename: &str) -> Result<(), String> {
        self.module
            .print_to_file(filename)
            .map_err(|e| e.to_string())
    }

    pub fn codegen_cir(&mut self, cir: &CIR) {
        let mut reachable_blocks = HashMap::new();
        let main_id = *cir
            .function_map
            .get("main")
            .expect("No main function found");
        reachable_blocks.insert(main_id, true);

        for block in &cir.blocks {
            for instr in &block.instructions {
                if let CIROp::Call(block_id, _, false) = &instr.op {
                    reachable_blocks.insert(*block_id, true);
                }
            }
        }

        for block in &cir.blocks {
            if !reachable_blocks.contains_key(&block.id) {
                println!("Skipping block {} - not reachable", block.id);
                continue;
            }

            let fn_name = cir
                .function_map
                .iter()
                .find(|(_, &id)| id == block.id)
                .map(|(name, _)| name.clone())
                .unwrap_or_else(|| format!("block{}", block.id));
            println!("Generating IR for block {}: {}", block.id, fn_name);

            let i32_type = self.context.i32_type();
            let param_types: Vec<BasicMetadataTypeEnum> = cir
                .blocks
                .iter()
                .flat_map(|b| b.instructions.iter())
                .filter_map(|instr| match &instr.op {
                    CIROp::Call(target_id, arg_ids, false) if *target_id == block.id => {
                        Some(arg_ids.len())
                    }
                    _ => None,
                })
                .max()
                .map(|num_args| vec![i32_type.into(); num_args])
                .unwrap_or_else(|| vec![]);
            let fn_type = i32_type.fn_type(&param_types, false);
            let function = self.module.add_function(&fn_name, fn_type, None);
            self.current_function = Some(function);
            let entry_block = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry_block);

            self.variables.clear();
            self.values.clear();
            self.array_sizes.clear();
            let params = function.get_params();
            for (i, param) in params.iter().enumerate() {
                let param_id = ValueId(format!("param{}", i + block.id * 10));
                let ptr = self
                    .builder
                    .build_alloca(i32_type, param_id.0.as_ref())
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
            println!(
                "After param setup: variables={:?}, values={:?}",
                self.variables, self.values
            );

            let mut label_blocks: HashMap<String, BasicBlock> = HashMap::new();
            for instr in &block.instructions {
                if let CIROp::Label(label) = &instr.op {
                    let bb = self.context.append_basic_block(function, label);
                    label_blocks.insert(label.clone(), bb);
                }
            }

            let mut seen_first_label = false;
            for (idx, instr) in block.instructions.iter().enumerate() {
                println!("Processing instr: {} = {:?}", instr.result.0, instr.op);
                println!("Values before: {:?}", self.values);
                match &instr.op {
                    CIROp::Const(_, value) => match value {
                        CirValue::Int(n) => {
                            let int_val = i32_type.const_int(*n as u64, true);
                            self.values.insert(instr.result.clone(), int_val.into());
                        }
                        _ => unimplemented!("Only i32 constants supported"),
                    },

                    CIROp::Store(target_id, value_id) => {
                        if target_id.0.starts_with("param")
                            && block.instructions[idx + 1..]
                                .iter()
                                .any(|i| matches!(i.op, CIROp::Const(_, _)))
                        {
                            println!("Skipping dead Store to {:?}", target_id);
                            continue;
                        }
                        if let Some(&ptr) = self.variables.get(value_id) {
                            // Array pointer—store it directly
                            self.variables.insert(target_id.clone(), ptr);
                            // Copy array size if it exists
                            if let Some(&size) = self.array_sizes.get(value_id) {
                                self.array_sizes.insert(target_id.clone(), size);
                            }
                        } else {
                            let value = *self.values.get(value_id).expect("Value not found");
                            let target_ptr =
                                self.variables.entry(target_id.clone()).or_insert_with(|| {
                                    self.builder.build_alloca(i32_type, &target_id.0).unwrap()
                                });
                            self.builder.build_store(*target_ptr, value);
                        }
                    }

                    CIROp::Add(_, left_id, right_id) => {
                        let left_val = if let Some(&ptr) = self.variables.get(left_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_left")
                                .expect("Failed to load left")
                                .into_int_value()
                        } else {
                            self.values
                                .get(left_id)
                                .expect(&format!("Left operand {} not found", left_id.0))
                                .into_int_value()
                        };
                        let right_val = if let Some(&ptr) = self.variables.get(right_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_right")
                                .expect("Failed to load right")
                                .into_int_value()
                        } else {
                            self.values
                                .get(right_id)
                                .expect(&format!("Right operand {} not found", right_id.0))
                                .into_int_value()
                        };
                        let result = self
                            .builder
                            .build_int_add(left_val, right_val, &instr.result.0)
                            .expect("Failed to build add");
                        self.values.insert(instr.result.clone(), result.into());
                    }
                    CIROp::Sub(left_id, right_id) => {
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
                            .build_int_sub(left, right, &instr.result.0)
                            .expect("Failed to build sub instruction");
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
                    CIROp::Gt(_, left_id, right_id) => {
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
                        // Don’t reposition—let next Label handle it
                    }
                    CIROp::Label(label) => {
                        let bb = *label_blocks.get(label).expect("Label not found");
                        // Ensure prior block is terminated before moving
                        if !self
                            .builder
                            .get_insert_block()
                            .map_or(true, |b| b.get_terminator().is_some())
                        {
                            self.builder
                                .build_unconditional_branch(bb)
                                .expect("Failed to branch to label");
                        }
                        self.builder.position_at_end(bb);
                        seen_first_label = true;
                    }

                    CIROp::Jump(target_label) => {
                        let target_bb = *label_blocks
                            .get(target_label)
                            .expect("Target label not found");
                        self.builder
                            .build_unconditional_branch(target_bb)
                            .expect("Failed to build jump");
                        self.builder.position_at_end(target_bb);
                    }
                    CIROp::Select(cond_id, then_id, else_id) => {
                        let cond = self
                            .values
                            .get(cond_id)
                            .expect("Condition not found")
                            .into_int_value();
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
                            .build_select(cond, then_val, else_val, &instr.result.0)
                            .expect("Failed to build select")
                            .into_int_value();
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

                    CIROp::List(element_ids) => {
                        let array_type = i32_type.array_type(element_ids.len() as u32);
                        let array_ptr = self
                            .builder
                            .build_alloca(array_type, &instr.result.0)
                            .expect("Failed to allocate array");
                        for (i, elem_id) in element_ids.iter().enumerate() {
                            let elem_value = if let Some(&ptr) = self.variables.get(elem_id) {
                                self.builder
                                    .build_load(i32_type, ptr, &format!("load_elem_{}", elem_id.0))
                                    .expect("Failed to load element")
                            } else {
                                *self
                                    .values
                                    .get(elem_id)
                                    .expect(&format!("List element {} not found", elem_id.0))
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
                            self.builder
                                .build_store(gep, elem_value)
                                .expect("Failed to store array element");
                        }
                        self.variables.insert(instr.result.clone(), array_ptr);
                        self.array_sizes
                            .insert(instr.result.clone(), element_ids.len() as u32);
                        // Store size
                    }
                    CIROp::Index(list_id, index_id) => {
                        let list_ptr = self.variables.get(list_id).expect("List pointer not found");
                        let array_size = *self
                            .array_sizes
                            .get(list_id)
                            .expect("Array size not found for list");
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
                        let array_type = i32_type.array_type(array_size);
                        let gep = unsafe {
                            self.builder
                                .build_gep(
                                    array_type,
                                    *list_ptr,
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

                    CIROp::StoreAt(list_id, index_id, value_id) => {
                        let list_ptr = self.variables.get(list_id).expect("List pointer not found");
                        let array_size = *self
                            .array_sizes
                            .get(list_id)
                            .expect("Array size not found for list");
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
                        let value = if let Some(&ptr) = self.variables.get(value_id) {
                            self.builder
                                .build_load(i32_type, ptr, "load_value")
                                .expect("Failed to load value")
                        } else {
                            *self.values.get(value_id).expect("Value not found")
                        };
                        let array_type = i32_type.array_type(array_size);
                        let gep = unsafe {
                            self.builder
                                .build_gep(
                                    array_type,
                                    *list_ptr,
                                    &[i32_type.const_int(0, false), index],
                                    "gep",
                                )
                                .expect("Failed to build GEP")
                        };
                        self.builder
                            .build_store(gep, value)
                            .expect("Failed to store array element");
                    }
                    CIROp::Call(block_id, arg_ids, _) => {
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
                                println!(
                                    "Call arg {}: values={:?}, variables={:?}",
                                    id.0, self.values, self.variables
                                );
                                if let Some(val) = self.values.get(id) {
                                    // Use value directly if in values (e.g., %3)
                                    println!("Using value for {}: {:?}", id.0, val);
                                    *val
                                } else if let Some(&ptr) = self.variables.get(id) {
                                    // Fallback to variables only for params
                                    println!("Loading from variables for {}: {:?}", id.0, ptr);
                                    self.builder
                                        .build_load(i32_type, ptr, "load_arg")
                                        .expect("Failed to load arg")
                                } else {
                                    panic!("Call arg {} not found in values or variables", id.0)
                                }
                                .into()
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
                    CIROp::Lt(_, left_id, right_id) => {
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
                                inkwell::IntPredicate::SLT, // Signed less than
                                left,
                                right,
                                &instr.result.0,
                            )
                            .expect("Failed to build lt instruction");
                        self.values.insert(instr.result.clone(), result.into());
                    }
                    _ => unimplemented!("Op not yet supported: {:?}", instr.op),
                }
            }

            if !seen_first_label && !label_blocks.is_empty() {
                let first_bb = *label_blocks.values().next().unwrap();
                self.builder
                    .build_unconditional_branch(first_bb)
                    .expect("Failed to branch to first label");
                self.builder.position_at_end(first_bb);
            }
        }
    }

    pub fn print_to_string(&self) -> String {
        self.module.print_to_string().to_string()
    }
}
