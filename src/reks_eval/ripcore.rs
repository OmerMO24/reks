use crate::reks_eval::cir::*;
use crate::reks_type::{infer::*, resolve::*};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct CallFrame {
    block_id: usize,
    return_pc: usize,
    return_slot: Option<ValueId>,
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    cir: CIR,
    stack: Vec<CirValue>,
    temps: HashMap<usize, HashMap<ValueId, CirValue>>,
    call_stack: Vec<CallFrame>,
    current_block: usize,
    pc: usize,
    label_map: HashMap<usize, HashMap<String, usize>>, // Block ID -> Label -> Instruction Index
}

impl Interpreter {
    pub fn new(cir: CIR) -> Self {
        // Precompute label map
        let mut label_map = HashMap::new();
        for block in &cir.blocks {
            let mut block_labels = HashMap::new();
            for (i, instr) in block.instructions.iter().enumerate() {
                if let CIROp::Label(label) = &instr.op {
                    block_labels.insert(label.clone(), i);
                }
            }
            label_map.insert(block.id, block_labels);
        }

        Interpreter {
            cir,
            stack: Vec::new(),
            temps: HashMap::new(),
            call_stack: Vec::new(),
            current_block: 0, // Start at main (assumed block 1, we’ll set it)
            pc: 0,
            label_map,
        }
    }

    pub fn run(&mut self, entry_block: usize) -> Option<CirValue> {
        self.current_block = entry_block;
        self.pc = 0;
        self.temps.insert(self.current_block, HashMap::new());
        println!("Starting at block {}:", self.current_block);

        loop {
            let block = match self.cir.blocks.get(self.current_block) {
                Some(b) => b,
                None => {
                    println!("Invalid block {} - exiting", self.current_block);
                    break;
                }
            };
            println!(
                "Processing block {}: {} instructions",
                self.current_block,
                block.instructions.len()
            );
            if self.pc >= block.instructions.len() {
                println!(
                    "PC {} exceeds block {} length - handling exhaustion",
                    self.pc, self.current_block
                );
                if self.call_stack.is_empty() {
                    println!("No more blocks to process - exiting");
                    break;
                }
                let frame = self.call_stack.pop().unwrap();
                self.current_block = frame.block_id;
                self.pc = frame.return_pc;
                println!(
                    "Popped frame, now at block {} PC {}",
                    self.current_block, self.pc
                );
                continue;
            }

            let instr = &block.instructions[self.pc];
            println!(
                "Block {}: PC {}: {} = {:?}, Stack: {:?}, Call Stack: {:?}, Temps: {:?}",
                self.current_block,
                self.pc,
                instr.result.0,
                instr.op,
                self.stack,
                self.call_stack,
                self.temps
            );
            match &instr.op {
                CIROp::Const(ty, val) => {
                    self.stack.push(val.clone());
                    self.temps
                        .get_mut(&self.current_block)
                        .unwrap()
                        .insert(instr.result.clone(), val.clone());
                    self.pc += 1;
                }
                CIROp::Add(ty, left_id, right_id) => {
                    let right = self.temps[&self.current_block][right_id].clone();
                    let left = self.temps[&self.current_block][left_id].clone();
                    let result = match (left, right) {
                        (CirValue::Int(l), CirValue::Int(r)) => CirValue::Int(l + r),
                        _ => panic!("Invalid types for Add"),
                    };
                    self.stack.push(result.clone());
                    self.temps
                        .get_mut(&self.current_block)
                        .unwrap()
                        .insert(instr.result.clone(), result);
                    self.pc += 1;
                }
                CIROp::Call(block_id, arg_ids) => {
                    let args: Vec<CirValue> = arg_ids
                        .iter()
                        .map(|id| self.temps[&self.current_block][id].clone())
                        .collect();
                    self.call_stack.push(CallFrame {
                        block_id: self.current_block,
                        return_pc: self.pc + 1,
                        return_slot: Some(instr.result.clone()),
                    });
                    self.current_block = *block_id;
                    self.pc = 0;
                    self.temps
                        .entry(self.current_block)
                        .or_insert_with(HashMap::new);
                    let block = &self.cir.blocks[self.current_block];
                    for (i, arg) in args.into_iter().enumerate() {
                        let param_id = ValueId(format!("param{}", i));
                        self.temps
                            .get_mut(&self.current_block)
                            .unwrap()
                            .insert(param_id, arg);
                    }
                    println!(
                        "Called block {}: Stack: {:?}",
                        self.current_block, self.call_stack
                    );
                }
                CIROp::Return(val_id) => {
                    let result = self.temps[&self.current_block][val_id].clone();
                    println!("Returning from block {}: {:?}", self.current_block, result);
                    if let Some(frame) = self.call_stack.pop() {
                        self.current_block = frame.block_id;
                        self.pc = frame.return_pc;
                        if let Some(slot) = frame.return_slot {
                            self.temps
                                .get_mut(&self.current_block)
                                .unwrap()
                                .insert(slot, result.clone());
                            self.stack.push(result.clone());
                        }
                        println!(
                            "Returned to block {} at PC {}: Call Stack: {:?}",
                            self.current_block, self.pc, self.call_stack
                        );
                    } else {
                        println!("Top-level return: {:?}", result);
                        return Some(result);
                    }
                }
                CIROp::Gt(ty, left_id, right_id) => {
                    let right = self.temps[&self.current_block][right_id].clone();
                    let left = self.temps[&self.current_block][left_id].clone();
                    let result = match (left, right) {
                        (CirValue::Int(l), CirValue::Int(r)) => CirValue::Bool(l > r),
                        _ => panic!("Invalid types for Gt"),
                    };
                    self.stack.push(result.clone());
                    self.temps
                        .get_mut(&self.current_block)
                        .unwrap()
                        .insert(instr.result.clone(), result);
                    self.pc += 1;
                }
                CIROp::Lt(ty, left_id, right_id) => {
                    let right = self.temps[&self.current_block][right_id].clone();
                    let left = self.temps[&self.current_block][left_id].clone();
                    let result = match (left, right) {
                        (CirValue::Int(l), CirValue::Int(r)) => CirValue::Bool(l < r),
                        _ => panic!("Invalid types for Lt"),
                    };
                    self.stack.push(result.clone());
                    self.temps
                        .get_mut(&self.current_block)
                        .unwrap()
                        .insert(instr.result.clone(), result);
                    self.pc += 1;
                }
                CIROp::Eq(ty, left_id, right_id) => {
                    let right = self.temps[&self.current_block][right_id].clone();
                    let left = self.temps[&self.current_block][left_id].clone();
                    let result = match (left, right) {
                        (CirValue::Int(l), CirValue::Int(r)) => CirValue::Bool(l == r),
                        _ => panic!("Invalid types for Eq"),
                    };
                    self.stack.push(result.clone());
                    self.temps
                        .get_mut(&self.current_block)
                        .unwrap()
                        .insert(instr.result.clone(), result);
                    self.pc += 1;
                }
                CIROp::Neq(ty, left_id, right_id) => {
                    let right = self.temps[&self.current_block][right_id].clone();
                    let left = self.temps[&self.current_block][left_id].clone();
                    let result = match (left, right) {
                        (CirValue::Int(l), CirValue::Int(r)) => CirValue::Bool(l != r),
                        _ => panic!("Invalid types for Neq"),
                    };
                    self.stack.push(result.clone());
                    self.temps
                        .get_mut(&self.current_block)
                        .unwrap()
                        .insert(instr.result.clone(), result);
                    self.pc += 1;
                }
                CIROp::Branch(cond_id, then_label, else_label) => {
                    let cond = self.temps[&self.current_block][cond_id].clone();
                    let label = match cond {
                        CirValue::Bool(true) => then_label,
                        CirValue::Bool(false) => else_label,
                        _ => panic!("Invalid condition type for Branch"),
                    };
                    self.pc = self.label_map[&self.current_block][label];
                    println!("Branching to {} at PC {}", label, self.pc);
                }
                CIROp::Label(label) => {
                    println!("Label reached: {}", label);
                    self.pc += 1;
                }
                CIROp::Jump(label) => {
                    self.pc = self.label_map[&self.current_block][label];
                    println!("Jumping to {} at PC {}", label, self.pc);
                }
                CIROp::Select(cond_id, then_id, else_id) => {
                    let cond = self.temps[&self.current_block][cond_id].clone();
                    let then_val = self.temps[&self.current_block].get(then_id).cloned();
                    let else_val = self.temps[&self.current_block].get(else_id).cloned();
                    let result = match cond {
                        CirValue::Bool(true) => then_val.expect("Then branch value missing"),
                        CirValue::Bool(false) => else_val.expect("Else branch value missing"),
                        _ => panic!("Invalid condition type for Select"),
                    };
                    self.stack.push(result.clone());
                    self.temps
                        .get_mut(&self.current_block)
                        .unwrap()
                        .insert(instr.result.clone(), result);
                    self.pc += 1;
                }
                CIROp::Struct(ty, field_values) => {
                    let fields: HashMap<String, CirValue> = field_values
                        .iter()
                        .map(|(name, value_id)| {
                            let value = self.temps[&self.current_block][value_id].clone();
                            (name.clone(), value)
                        })
                        .collect();
                    let result = CirValue::Struct(fields);
                    self.stack.push(result.clone());
                    self.temps
                        .get_mut(&self.current_block)
                        .unwrap()
                        .insert(instr.result.clone(), result);
                    self.pc += 1;
                }
                CIROp::GetField(base_id, field_name) => {
                    let base = self.temps[&self.current_block][base_id].clone();
                    let result = match base {
                        CirValue::Struct(fields) => fields[field_name].clone(),
                        _ => panic!("GetField on non-struct"),
                    };
                    self.stack.push(result.clone());
                    self.temps
                        .get_mut(&self.current_block)
                        .unwrap()
                        .insert(instr.result.clone(), result);
                    self.pc += 1;
                }
                _ => unimplemented!("Instruction not yet supported"),
            }
        }
        println!("Interpreter finished with no result");
        None
    }
}
