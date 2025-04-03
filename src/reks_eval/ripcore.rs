use crate::reks_eval::cir::*;
use crate::reks_type::{infer::*, resolve::*};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct CallFrame {
    block_id: usize,
    return_pc: usize,
    return_slot: Option<ValueId>,
    temps: Option<HashMap<ValueId, CirValue>>,
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub(crate) cir: CIR,
    pub(crate) stack: Vec<CirValue>,
    pub(crate) temps: HashMap<usize, HashMap<ValueId, CirValue>>,
    pub(crate) call_stack: Vec<CallFrame>,
    pub(crate) current_block: usize,
    pub(crate) pc: usize,
    pub(crate) label_map: HashMap<usize, HashMap<String, usize>>, // Block ID -> Label -> Instruction Index
}

impl Interpreter {
    pub fn new(cir: CIR) -> Self {
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
        let main_block = *cir
            .function_map
            .get("main")
            .expect("No 'main' function found");
        Interpreter {
            cir,
            stack: Vec::new(),
            temps: HashMap::new(),
            call_stack: Vec::new(),
            current_block: main_block,
            pc: 0,
            label_map,
        }
    }

    // Query a function with args, return result
    pub fn eval_function(&mut self, fn_name: &str, args: Vec<CirValue>) -> Option<CirValue> {
        let block_id = *self.cir.function_map.get(fn_name)?;
        let mut block_temps = HashMap::new();
        for (i, arg) in args.into_iter().enumerate() {
            let param_id = ValueId(format!("param{}", i + block_id * 10));
            block_temps.insert(param_id.clone(), arg);
            println!(
                "eval_function: Set param {} = {:?} for block {}",
                param_id.0,
                block_temps.get(&param_id),
                block_id
            );
        }
        self.temps.insert(block_id, block_temps);
        println!(
            "eval_function: Temps after setup for block {}: {:?}",
            block_id,
            self.temps.get(&block_id)
        );
        self.current_block = block_id;
        self.run()
    }

    pub fn run(&mut self) -> Option<CirValue> {
        self.pc = 0;
        println!(
            "Starting at block {} with temps: {:?}",
            self.current_block,
            self.temps.get(&self.current_block)
        );

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
                self.temps.get(&self.current_block)
            );
            let mut block_temps = self
                .temps
                .get(&self.current_block)
                .cloned()
                .unwrap_or_else(HashMap::new);
            match &instr.op {
                CIROp::Const(ty, val) => {
                    self.stack.push(val.clone());
                    block_temps.insert(instr.result.clone(), val.clone());
                    self.temps.insert(self.current_block, block_temps.clone());
                    self.pc += 1;
                }
                CIROp::Add(_, left_id, right_id) => {
                    println!(
                        "run: Fetching left_id {}: {:?}",
                        left_id.0,
                        block_temps.get(left_id)
                    );
                    println!(
                        "run: Fetching right_id {}: {:?}",
                        right_id.0,
                        block_temps.get(right_id)
                    );
                    let left = block_temps.get(left_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            left_id.0, self.current_block
                        )
                    });
                    let right = block_temps.get(right_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            right_id.0, self.current_block
                        )
                    });
                    if let (CirValue::Int(l), CirValue::Int(r)) = (left, right) {
                        block_temps.insert(instr.result.clone(), CirValue::Int(l + r));
                        self.temps.insert(self.current_block, block_temps.clone());
                    }
                    self.pc += 1;
                }
                CIROp::Sub(left_id, right_id) => {
                    let left = block_temps.get(left_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            left_id.0, self.current_block
                        )
                    });
                    let right = block_temps.get(right_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            right_id.0, self.current_block
                        )
                    });
                    if let (CirValue::Int(l), CirValue::Int(r)) = (left, right) {
                        block_temps.insert(instr.result.clone(), CirValue::Int(l - r));
                        self.temps.insert(self.current_block, block_temps.clone());
                    }
                    self.pc += 1;
                }
                CIROp::Mul(left_id, right_id) => {
                    let left = block_temps.get(left_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            left_id.0, self.current_block
                        )
                    });
                    let right = block_temps.get(right_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            right_id.0, self.current_block
                        )
                    });
                    if let (CirValue::Int(l), CirValue::Int(r)) = (left, right) {
                        block_temps.insert(instr.result.clone(), CirValue::Int(l * r));
                        self.temps.insert(self.current_block, block_temps.clone());
                    }
                    self.pc += 1;
                }
                CIROp::Call(block_id, arg_ids, is_compeval) => {
                    let args: Vec<CirValue> = arg_ids
                        .iter()
                        .map(|id| {
                            block_temps.get(id).cloned().unwrap_or_else(|| {
                                panic!("Arg {} not found in block {}", id.0, self.current_block)
                            })
                        })
                        .collect();
                    println!("Calling block {} with args: {:?}", block_id, args);
                    self.call_stack.push(CallFrame {
                        block_id: self.current_block,
                        return_pc: self.pc + 1,
                        return_slot: if instr.result.0.is_empty() {
                            None
                        } else {
                            Some(instr.result.clone())
                        },
                        temps: Some(block_temps.clone()),
                    });
                    self.current_block = *block_id;
                    self.pc = 0;
                    self.temps
                        .entry(self.current_block)
                        .or_insert_with(HashMap::new)
                        .clear();
                    for (i, arg) in args.into_iter().enumerate() {
                        let param_id = ValueId(format!("param{}", i + self.current_block * 10));
                        self.temps
                            .get_mut(&self.current_block)
                            .unwrap()
                            .insert(param_id, arg);
                    }
                }
                CIROp::Return(val_id) => {
                    let result = block_temps.get(val_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            val_id.0, self.current_block
                        )
                    });
                    println!("Returning from block {}: {:?}", self.current_block, result);
                    self.temps.insert(self.current_block, block_temps);
                    if let Some(frame) = self.call_stack.pop() {
                        self.current_block = frame.block_id;
                        self.pc = frame.return_pc;
                        if let Some(saved_temps) = frame.temps {
                            self.temps.insert(self.current_block, saved_temps);
                        }
                        if let Some(slot) = frame.return_slot {
                            self.temps
                                .entry(self.current_block)
                                .or_insert_with(HashMap::new)
                                .insert(slot.clone(), result.clone());
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
                CIROp::Lt(_, left_id, right_id) => {
                    let left = block_temps.get(left_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            left_id.0, self.current_block
                        )
                    });
                    let right = block_temps.get(right_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            right_id.0, self.current_block
                        )
                    });
                    if let (CirValue::Int(l), CirValue::Int(r)) = (left, right) {
                        block_temps.insert(instr.result.clone(), CirValue::Bool(l < r));
                        self.temps.insert(self.current_block, block_temps.clone());
                    }
                    self.pc += 1;
                }
                CIROp::Gt(_, left_id, right_id) => {
                    let left = block_temps.get(left_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            left_id.0, self.current_block
                        )
                    });
                    let right = block_temps.get(right_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            right_id.0, self.current_block
                        )
                    });
                    if let (CirValue::Int(l), CirValue::Int(r)) = (left, right) {
                        block_temps.insert(instr.result.clone(), CirValue::Bool(l > r));
                        self.temps.insert(self.current_block, block_temps.clone());
                    }
                    self.pc += 1;
                }
                CIROp::Select(cond_id, then_id, else_id) => {
                    println!(
                        "run: Selecting with cond {}: {:?}, then {}: {:?}, else {}: {:?}",
                        cond_id.0,
                        block_temps.get(cond_id),
                        then_id.0,
                        block_temps.get(then_id),
                        else_id.0,
                        block_temps.get(else_id)
                    );
                    let cond = block_temps.get(cond_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            cond_id.0, self.current_block
                        )
                    });
                    let then_val = block_temps.get(then_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            then_id.0, self.current_block
                        )
                    });
                    let else_val = block_temps.get(else_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            else_id.0, self.current_block
                        )
                    });
                    let result = match cond {
                        CirValue::Bool(true) => then_val,
                        CirValue::Bool(false) => else_val,
                        _ => panic!("Invalid condition type for Select: {:?}", cond),
                    };
                    block_temps.insert(instr.result.clone(), result.clone());
                    self.temps.insert(self.current_block, block_temps.clone());
                    self.pc += 1;
                }
                CIROp::LtOrEq(_, left_id, right_id) => {
                    let left = block_temps.get(left_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            left_id.0, self.current_block
                        )
                    });
                    let right = block_temps.get(right_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            right_id.0, self.current_block
                        )
                    });
                    if let (CirValue::Int(l), CirValue::Int(r)) = (left, right) {
                        block_temps.insert(instr.result.clone(), CirValue::Bool(l <= r));
                        self.temps.insert(self.current_block, block_temps.clone());
                    }
                    self.pc += 1;
                }
                CIROp::Branch(cond_id, then_label, else_label) => {
                    let cond = block_temps.get(cond_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Condition {} not found in block {}",
                            cond_id.0, self.current_block
                        )
                    });
                    println!(
                        "run: Branching on cond {}: {:?}, then={}, else={}",
                        cond_id.0, cond, then_label, else_label
                    );
                    let next_pc = match cond {
                        CirValue::Bool(true) => block
                            .instructions
                            .iter()
                            .position(|i| matches!(&i.op, CIROp::Label(l) if l == then_label)),
                        CirValue::Bool(false) => block
                            .instructions
                            .iter()
                            .position(|i| matches!(&i.op, CIROp::Label(l) if l == else_label)),
                        _ => panic!("Invalid condition type for Branch: {:?}", cond),
                    }
                    .unwrap_or_else(|| {
                        panic!(
                            "Label {} or {} not found in block {}",
                            then_label, else_label, self.current_block
                        )
                    });
                    self.pc = next_pc;
                }
                CIROp::Label(label) => {
                    println!(
                        "run: At label {} in block {} at PC {}",
                        label, self.current_block, self.pc
                    );
                    self.pc += 1; // Move past the label
                }
                CIROp::Jump(label) => {
                    println!(
                        "run: Jumping to label {} in block {} from PC {}",
                        label, self.current_block, self.pc
                    );
                    let next_pc = block
                        .instructions
                        .iter()
                        .position(|i| matches!(&i.op, CIROp::Label(l) if l == label))
                        .unwrap_or_else(|| {
                            panic!("Label {} not found in block {}", label, self.current_block)
                        });
                    self.pc = next_pc;
                }
                CIROp::While {
                    guard,
                    body_start,
                    exit_label,
                } => {
                    println!(
                        "run: While with guard {}: {:?}, body_start={}, exit_label={}",
                        guard.0,
                        block_temps.get(guard),
                        body_start,
                        exit_label
                    );
                    let guard_val = block_temps.get(guard).cloned().unwrap_or_else(|| {
                        panic!(
                            "Guard {} not found in block {}",
                            guard.0, self.current_block
                        )
                    });
                    let next_pc = match guard_val {
                        CirValue::Bool(true) => block
                            .instructions
                            .iter()
                            .position(|i| matches!(&i.op, CIROp::Label(l) if l == body_start))
                            .unwrap_or_else(|| {
                                panic!(
                                    "Body label {} not found in block {}",
                                    body_start, self.current_block
                                )
                            }),
                        CirValue::Bool(false) => block
                            .instructions
                            .iter()
                            .position(|i| matches!(&i.op, CIROp::Label(l) if l == exit_label))
                            .unwrap_or_else(|| {
                                panic!(
                                    "Exit label {} not found in block {}",
                                    exit_label, self.current_block
                                )
                            }),
                        _ => panic!("Invalid guard type for While: {:?}", guard_val),
                    };
                    self.pc = next_pc;
                }
                CIROp::List(elements) => {
                    let list_vals: Vec<CirValue> = elements
                        .iter()
                        .map(|id| {
                            block_temps.get(id).cloned().unwrap_or_else(|| {
                                panic!("Element {} not found in block {}", id.0, self.current_block)
                            })
                        })
                        .collect();
                    block_temps.insert(instr.result.clone(), CirValue::List(list_vals));
                    self.temps.insert(self.current_block, block_temps.clone());
                    self.pc += 1;
                }
                CIROp::Index(list_id, index_id) => {
                    let list = block_temps.get(list_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "List {} not found in block {}",
                            list_id.0, self.current_block
                        )
                    });
                    let index = block_temps.get(index_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Index {} not found in block {}",
                            index_id.0, self.current_block
                        )
                    });
                    if let (CirValue::List(vals), CirValue::Int(idx)) = (list, index) {
                        let val = vals.get(idx as usize).cloned().unwrap_or_else(|| {
                            panic!(
                                "Index {} out of bounds in block {}",
                                idx, self.current_block
                            )
                        });
                        block_temps.insert(instr.result.clone(), val);
                        self.temps.insert(self.current_block, block_temps.clone());
                    }
                    self.pc += 1;
                }
                CIROp::Store(target_id, value_id) => {
                    println!(
                        "run: Storing {} into {} in block {}",
                        value_id.0, target_id.0, self.current_block
                    );
                    let value = block_temps.get(value_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            value_id.0, self.current_block
                        )
                    });
                    block_temps.insert(target_id.clone(), value);
                    self.temps.insert(self.current_block, block_temps.clone());
                    self.pc += 1;
                }
                CIROp::StoreAt(target_id, index_id, value_id) => {
                    println!(
                        "run: Storing {} at {}[{}] in block {}",
                        value_id.0, target_id.0, index_id.0, self.current_block
                    );
                    let target = block_temps.get(target_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Target {} not found in block {}",
                            target_id.0, self.current_block
                        )
                    });
                    let index = block_temps.get(index_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Index {} not found in block {}",
                            index_id.0, self.current_block
                        )
                    });
                    let value = block_temps.get(value_id).cloned().unwrap_or_else(|| {
                        panic!(
                            "Value {} not found in block {}",
                            value_id.0, self.current_block
                        )
                    });
                    if let (CirValue::List(mut vals), CirValue::Int(idx)) = (target, index) {
                        if idx as usize >= vals.len() {
                            panic!(
                                "Index {} out of bounds in block {}",
                                idx, self.current_block
                            );
                        }
                        vals[idx as usize] = value;
                        block_temps.insert(target_id.clone(), CirValue::List(vals));
                        self.temps.insert(self.current_block, block_temps.clone());
                    } else {
                        panic!(
                            "StoreAt requires a list target and int index in block {}",
                            self.current_block
                        );
                    }
                    self.pc += 1;
                }
                _ => unimplemented!("Instruction not yet supported: {:?}", instr.op),
            }
        }
        println!("Interpreter finished with no result");
        None
    }

    fn get_value(&self, id: &ValueId) -> CirValue {
        self.temps
            .get(&self.current_block)
            .and_then(|block_temps| block_temps.get(id))
            .cloned()
            .unwrap_or_else(|| panic!("Value {} not found in block {}", id.0, self.current_block))
    }
}
