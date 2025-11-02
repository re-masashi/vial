use std::collections::HashMap;

use super::builtins::execute_builtin;
use super::error::InterpreterError;
use super::heap::Heap;
use super::stack::CallStack;
use super::value::{HeapPtr, Value};
use crate::ast::{BinOp, UnOp};
use crate::ir::{
    AddressKind, BasicBlockId, FunctionId, IRFunction, IRInstruction, IRModule, IRTerminator,
    IRValue, ValueId,
};

#[derive(Debug)]
pub enum TerminatorResult {
    Return(Value),
    Jump(BasicBlockId),
    Branch {
        then_block: BasicBlockId,
        else_block: BasicBlockId,
        condition: bool,
    },
    Switch {
        target: BasicBlockId,
    },
}

pub struct Interpreter {
    module: IRModule,
    call_stack: CallStack,
    heap: Heap,
    pub globals: HashMap<ValueId, Value>,
}

impl Interpreter {
    pub fn new(module: IRModule) -> Self {
        Self {
            module,
            call_stack: CallStack::new(),
            heap: Heap::new(),
            globals: HashMap::new(),
        }
    }

    // Main entry point
    pub fn run(&mut self, entry_function: FunctionId) -> Result<Value, InterpreterError> {
        // Find the entry function in the module
        let func_idx = self
            .module
            .function_map
            .get(&entry_function)
            .ok_or(InterpreterError::UndefinedFunction(entry_function))?;
        let func = self.module.functions[*func_idx].clone(); // Clone to avoid borrow issues

        // Execute the function with no arguments initially
        self.execute_function_with_args(&func, vec![])
    }

    // Execute a single function
    fn _execute_function(&mut self, function: &IRFunction) -> Result<Value, InterpreterError> {
        self.execute_function_with_args(function, vec![])
    }

    // Execute a single function with arguments
    fn execute_function_with_args(
        &mut self,
        function: &IRFunction,
        args: Vec<Value>,
    ) -> Result<Value, InterpreterError> {
        // Create a new call frame
        let entry_block = function.body.ok_or(InterpreterError::TypeError {
            expected: "function with entry block",
            got: "function without body".to_string(),
        })?;

        // First, collect all the argument mappings without borrowing self mutably
        let arg_mappings: Vec<_> = args
            .into_iter()
            .enumerate()
            .filter(|(i, _)| *i < function.args.len())
            .filter_map(|(i, arg_value)| {
                self.map_arg_to_value_id(&function.args[i])
                    .map(|value_id| (value_id, arg_value))
            })
            .collect();

        let frame = self.call_stack.push(function.id, entry_block);

        // Bind arguments to the function parameters
        for (value_id, arg_value) in arg_mappings {
            frame.locals.insert(value_id, arg_value);
        }

        // Process each basic block until we return
        loop {
            // Check for stack overflow
            if self.call_stack.len() > 1000 {
                return Err(InterpreterError::StackOverflow);
            }

            let current_frame = self.call_stack.current().unwrap();
            let block_id = current_frame.current_block;

            // Find the basic block in the function
            let block = function
                .basic_blocks
                .iter()
                .find(|b| b.id == block_id)
                .ok_or_else(|| InterpreterError::TypeError {
                    expected: "valid basic block",
                    got: format!("block id: {:?}", block_id),
                })?;

            // Execute the block
            let result = self.execute_block(block, function)?;

            match result {
                Some(terminator_result) => match terminator_result {
                    TerminatorResult::Return(value) => {
                        self.call_stack.pop();
                        return Ok(value);
                    }
                    TerminatorResult::Jump(target) => {
                        let current_frame = self.call_stack.current().unwrap();
                        current_frame.last_block = Some(current_frame.current_block);
                        current_frame.current_block = target;
                    }
                    TerminatorResult::Branch {
                        then_block,
                        else_block,
                        condition,
                    } => {
                        let current_frame = self.call_stack.current().unwrap();
                        current_frame.last_block = Some(current_frame.current_block);
                        let target = if condition { then_block } else { else_block };
                        current_frame.current_block = target;
                    }
                    TerminatorResult::Switch { target } => {
                        let current_frame = self.call_stack.current().unwrap();
                        current_frame.last_block = Some(current_frame.current_block);
                        current_frame.current_block = target;
                    }
                },
                None => {
                    // No terminator result means continue to next block (though this shouldn't happen in proper SSA)
                    break;
                }
            }
        }

        Ok(Value::Null)
    }

    // Helper to get the ValueId that corresponds to a function argument
    fn map_arg_to_value_id(&self, arg: &crate::ir::IRFunctionArg) -> Option<ValueId> {
        // In SSA form, function arguments should have corresponding ValueIds
        // For now, we'll use the memory slot ID as a proxy for the argument's ValueId
        // In a real system, the argument's binding_id or other metadata would provide the ValueId
        // This is a simplification for the interpreter
        Some(ValueId(arg.memory_slot.0)) // Using the memory slot ID as value ID
    }

    // Execute a basic block
    fn execute_block(
        &mut self,
        block: &crate::ir::BasicBlock,
        _function: &IRFunction,
    ) -> Result<Option<TerminatorResult>, InterpreterError> {
        // Execute all PHI nodes first
        for phi in &block.phi_nodes {
            self.execute_instruction(phi)?;
        }

        // Execute all regular instructions
        for instruction in &block.instructions {
            self.execute_instruction(instruction)?;
        }

        // Execute terminator if it exists
        if let Some(terminator) = &block.terminator {
            let result = self.execute_terminator(terminator)?;
            Ok(Some(result))
        } else {
            // Basic block without terminator - this should not happen in proper SSA
            Ok(None)
        }
    }

    // Execute a single instruction
    fn execute_instruction(&mut self, inst: &IRInstruction) -> Result<(), InterpreterError> {
        match inst {
            IRInstruction::BinOp {
                result,
                left,
                op,
                right,
                ..
            } => {
                let l = self.eval_value(left)?;
                let r = self.eval_value(right)?;
                let res = match op {
                    BinOp::Add => l.add(&r)?,
                    BinOp::Sub => l.sub(&r)?,
                    BinOp::Mul => l.mul(&r)?,
                    BinOp::Div => l.div(&r)?,
                    BinOp::Mod => {
                        // Handle modulo specially
                        match (&l, &r) {
                            (Value::Int(a), Value::Int(b)) => {
                                if *b == 0 {
                                    return Err(InterpreterError::DivisionByZero);
                                }
                                Value::Int(a % b)
                            }
                            _ => {
                                return Err(InterpreterError::TypeError {
                                    expected: "integers for modulo",
                                    got: format!("{} and {}", l.type_name(), r.type_name()),
                                });
                            }
                        }
                    }
                    BinOp::Eq => l.eq(&r)?,
                    BinOp::NotEq => l.ne(&r)?,
                    BinOp::Less => l.lt(&r)?,
                    BinOp::Greater => l.gt(&r)?,
                    BinOp::LessEq => l.le(&r)?,
                    BinOp::GreaterEq => l.ge(&r)?,
                    BinOp::And => {
                        let l_bool = l.as_bool()?;
                        if !l_bool {
                            Value::Bool(false) // Short circuit
                        } else {
                            r.as_bool().map(Value::Bool)?
                        }
                    }
                    BinOp::Or => {
                        let l_bool = l.as_bool()?;
                        if l_bool {
                            Value::Bool(true) // Short circuit
                        } else {
                            r.as_bool().map(Value::Bool)?
                        }
                    }
                    _ => {
                        return Err(InterpreterError::TypeError {
                            expected: "supported binary operation",
                            got: format!("{:?}", op),
                        });
                    }
                };
                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, res);
            }

            IRInstruction::UnOp {
                result,
                op,
                operand,
                ..
            } => {
                let val = self.eval_value(operand)?;
                let res = match op {
                    UnOp::Minus => match val {
                        Value::Int(i) => Value::Int(-i),
                        Value::Float(f) => Value::Float(-f),
                        _ => {
                            return Err(InterpreterError::TypeError {
                                expected: "numeric value for negation",
                                got: val.type_name().to_string(),
                            });
                        }
                    },
                    UnOp::Not => Value::Bool(!val.as_bool()?),
                    _ => {
                        return Err(InterpreterError::TypeError {
                            expected: "supported unary operation",
                            got: format!("{:?}", op),
                        });
                    }
                };
                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, res);
            }

            IRInstruction::Let { result, value, .. } => {
                let val = self.eval_value(value)?;
                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, val);
            }

            IRInstruction::Array {
                result, elements, ..
            } => {
                let elements: Result<Vec<_>, _> =
                    elements.iter().map(|e| self.eval_value(e)).collect();
                let elements = elements?;

                let ptr = self
                    .heap
                    .allocate(super::heap::HeapObject::Array { elements });
                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, Value::Ptr(ptr));
            }

            IRInstruction::Tuple {
                result, elements, ..
            } => {
                let elements: Result<Vec<_>, _> =
                    elements.iter().map(|e| self.eval_value(e)).collect();
                let elements = elements?;

                let ptr = self
                    .heap
                    .allocate(super::heap::HeapObject::Tuple { elements });
                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, Value::Ptr(ptr));
            }

            IRInstruction::StructConstruct {
                result,
                struct_id,
                fields,
                ..
            } => {
                let mut field_values = std::collections::HashMap::new();
                for (field_id, val) in fields {
                    let value = self.eval_value(val)?;
                    field_values.insert(*field_id, value);
                }

                let ptr = self.heap.allocate(super::heap::HeapObject::Struct {
                    struct_id: *struct_id,
                    fields: field_values,
                });
                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, Value::Ptr(ptr));
            }

            IRInstruction::EnumConstruct {
                result,
                enum_id,
                variant_id,
                args,
                ..
            } => {
                let args: Result<Vec<_>, _> = args.iter().map(|a| self.eval_value(a)).collect();
                let args = args?;

                let ptr = self.heap.allocate(super::heap::HeapObject::Enum {
                    enum_id: *enum_id,
                    variant_id: *variant_id,
                    data: args,
                });
                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, Value::Ptr(ptr));
            }

            IRInstruction::Call {
                result,
                function,
                args,
                ..
            } => {
                let func_id = match function {
                    IRValue::FunctionRef(id) => *id,
                    _ => {
                        return Err(InterpreterError::TypeError {
                            expected: "function reference",
                            got: "non-function value".to_string(),
                        });
                    }
                };

                let arg_values: Result<Vec<_>, _> =
                    args.iter().map(|a| self.eval_value(a)).collect();
                let arg_values = arg_values?;

                // Find the function in the module
                let func_idx = self
                    .module
                    .function_map
                    .get(&func_id)
                    .ok_or(InterpreterError::UndefinedFunction(func_id))?;
                let func_to_call = self.module.functions[*func_idx].clone();

                // Execute the function and get result
                // We need to properly set up the call frame with arguments
                let ret = self.execute_function_with_args(&func_to_call, arg_values)?;

                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, ret);
            }

            IRInstruction::CallBuiltin {
                result,
                builtin_name,
                args,
                ..
            } => {
                let arg_values: Result<Vec<_>, _> =
                    args.iter().map(|a| self.eval_value(a)).collect();
                let arg_values = arg_values?;

                let ret = execute_builtin(builtin_name, arg_values)?;

                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, ret);
            }

            IRInstruction::Load {
                result,
                address,
                address_kind,
                ..
            } => {
                match address_kind {
                    AddressKind::StackSlot(_) => {
                        // For now, we treat all loads as loading from locals using address as ValueId
                        let addr_val = self.eval_value(address)?;
                        // If it's an SSA value, look it up in the current frame
                        if let Value::Ptr(ptr) = addr_val {
                            // This is a heap object - we need to dereference it
                            let _obj = self.heap.get(ptr)?;
                            // For now, store the object itself (this needs more specific handling)
                            self.call_stack
                                .current()
                                .unwrap()
                                .locals
                                .insert(*result, addr_val);
                        } else {
                            // Store the value directly
                            self.call_stack
                                .current()
                                .unwrap()
                                .locals
                                .insert(*result, addr_val);
                        }
                    }
                    AddressKind::StructField { base: _, field } => {
                        // The 'base' field ID here refers to the ValueId of the struct in locals
                        // The actual base address should be evaluated from the 'address' field in the IR instruction
                        let addr_val = self.eval_value(address)?;
                        let base_ptr = addr_val.as_ptr()?;
                        let obj = self.heap.get(base_ptr)?;

                        if let super::heap::HeapObject::Struct { fields, .. } = obj {
                            let field_val =
                                fields
                                    .get(field)
                                    .cloned()
                                    .ok_or(InterpreterError::TypeError {
                                        expected: "valid struct field",
                                        got: format!("field id: {:?}", field),
                                    })?;
                            self.call_stack
                                .current()
                                .unwrap()
                                .locals
                                .insert(*result, field_val);
                        } else {
                            return Err(InterpreterError::TypeError {
                                expected: "struct object",
                                got: "non-struct object".to_string(),
                            });
                        }
                    }
                    _ => {
                        // Other address kinds not fully implemented yet
                        let addr_val = self.eval_value(address)?;
                        self.call_stack
                            .current()
                            .unwrap()
                            .locals
                            .insert(*result, addr_val);
                    }
                }
            }

            IRInstruction::Store {
                address,
                value,
                address_kind,
                ..
            } => {
                let value_to_store = self.eval_value(value)?;

                match address_kind {
                    AddressKind::StackSlot(_) => {
                        // For now, treat as if storing to a local
                        let _addr_val = self.eval_value(address)?;
                        if let IRValue::SSA(target_id) = address {
                            self.call_stack
                                .current()
                                .unwrap()
                                .locals
                                .insert(*target_id, value_to_store);
                        }
                    }
                    AddressKind::StructField { base: _, field } => {
                        // The 'base' field ID here refers to the ValueId of the struct in locals
                        // The actual base address should be evaluated from the 'address' field in the IR instruction
                        let addr_val = self.eval_value(address)?;
                        let base_ptr = addr_val.as_ptr()?;
                        let obj = self.heap.get_mut(base_ptr)?;

                        if let super::heap::HeapObject::Struct { fields, .. } = obj {
                            fields.insert(*field, value_to_store);
                        } else {
                            return Err(InterpreterError::TypeError {
                                expected: "struct object",
                                got: "non-struct object".to_string(),
                            });
                        }
                    }
                    _ => {
                        // Other address kinds not fully implemented yet
                        // For now, just evaluate the address and value
                        let _ = self.eval_value(address)?;
                    }
                }
            }

            IRInstruction::Allocate { result, .. } => {
                // For now, just create a null pointer
                // This should be more sophisticated based on the allocation size/type
                let ptr = self.heap.allocate(super::heap::HeapObject::Struct {
                    struct_id: crate::ir::StructId(0), // placeholder
                    fields: std::collections::HashMap::new(),
                });
                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, Value::Ptr(ptr));
            }

            IRInstruction::FieldAccess {
                result,
                target,
                field_id,
                ..
            } => {
                let target_val = self.eval_value(target)?;
                let target_ptr = target_val.as_ptr()?;
                let obj = self.heap.get(target_ptr)?;

                if let super::heap::HeapObject::Struct { fields, .. } = obj {
                    let field_val =
                        fields
                            .get(field_id)
                            .cloned()
                            .ok_or(InterpreterError::TypeError {
                                expected: "valid struct field",
                                got: format!("field id: {:?}", field_id),
                            })?;
                    self.call_stack
                        .current()
                        .unwrap()
                        .locals
                        .insert(*result, field_val);
                } else {
                    return Err(InterpreterError::TypeError {
                        expected: "struct object",
                        got: "non-struct object".to_string(),
                    });
                }
            }

            IRInstruction::Index {
                result,
                target,
                index,
                ..
            } => {
                let target_val = self.eval_value(target)?;
                let index_val = self.eval_value(index)?.as_int()?;

                let target_ptr = target_val.as_ptr()?;
                let obj = self.heap.get(target_ptr)?;

                match obj {
                    super::heap::HeapObject::Array { elements } => {
                        let element = elements.get(index_val as usize).cloned().ok_or(
                            InterpreterError::TypeError {
                                expected: "valid array index",
                                got: format!(
                                    "index {} in array of length {}",
                                    index_val,
                                    elements.len()
                                ),
                            },
                        )?;
                        self.call_stack
                            .current()
                            .unwrap()
                            .locals
                            .insert(*result, element);
                    }
                    super::heap::HeapObject::Tuple { elements } => {
                        let element = elements.get(index_val as usize).cloned().ok_or(
                            InterpreterError::TypeError {
                                expected: "valid tuple index",
                                got: format!(
                                    "index {} in tuple of length {}",
                                    index_val,
                                    elements.len()
                                ),
                            },
                        )?;
                        self.call_stack
                            .current()
                            .unwrap()
                            .locals
                            .insert(*result, element);
                    }
                    _ => {
                        return Err(InterpreterError::TypeError {
                            expected: "array or tuple",
                            got: "non-indexable object".to_string(),
                        });
                    }
                }
            }

            IRInstruction::Copy { result, source, .. } => {
                let val = self.eval_value(source)?;
                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, val);
            }

            IRInstruction::Select {
                result,
                condition,
                if_true,
                if_false,
                ..
            } => {
                let cond = self.eval_value(condition)?.as_bool()?;
                let val = if cond {
                    self.eval_value(if_true)?
                } else {
                    self.eval_value(if_false)?
                };
                self.call_stack
                    .current()
                    .unwrap()
                    .locals
                    .insert(*result, val);
            }

            IRInstruction::Phi {
                result, incoming, ..
            } => {
                // Handle PHI nodes: in an interpreter with concrete execution, we use the value
                // that comes from the block that led to this one
                let last_block = self.call_stack.current().unwrap().last_block;

                let value_to_use = if let Some(last_block_id) = last_block {
                    // Find the incoming value that matches the last block
                    if let Some((value, _)) = incoming
                        .iter()
                        .find(|(_, source_block)| source_block == &last_block_id)
                    {
                        Some(value.clone())
                    } else {
                        // Fallback: use the first available value if no match is found
                        incoming.first().map(|(v, _)| v.clone())
                    }
                } else {
                    // If there's no last block info, use the first available value
                    incoming.first().map(|(v, _)| v.clone())
                };

                if let Some(val_to_insert) = value_to_use {
                    let val = self.eval_value(&val_to_insert)?;
                    self.call_stack
                        .current()
                        .unwrap()
                        .locals
                        .insert(*result, val);
                } else {
                    self.call_stack
                        .current()
                        .unwrap()
                        .locals
                        .insert(*result, Value::Null);
                }
            }

            // Handle other instructions as needed
            _ => {
                // For now, skip unsupported instructions but log a warning
                eprintln!("Unsupported instruction: {:?}", inst);
            }
        }

        Ok(())
    }

    // Handle terminators
    fn execute_terminator(
        &mut self,
        term: &IRTerminator,
    ) -> Result<TerminatorResult, InterpreterError> {
        match term {
            IRTerminator::Return { value, .. } => {
                let ret_val = value
                    .as_ref()
                    .map(|v| self.eval_value(v))
                    .transpose()?
                    .unwrap_or(Value::Null);
                Ok(TerminatorResult::Return(ret_val))
            }

            IRTerminator::Jump { target, .. } => Ok(TerminatorResult::Jump(*target)),

            IRTerminator::Branch {
                condition,
                then_block,
                else_block,
                ..
            } => {
                let cond = self.eval_value(condition)?.as_bool()?;
                Ok(TerminatorResult::Branch {
                    then_block: *then_block,
                    else_block: *else_block,
                    condition: cond,
                })
            }

            IRTerminator::Switch {
                value,
                cases,
                default,
                ..
            } => {
                let val = self.eval_value(value)?;
                for (case_val, target) in cases {
                    if self.eval_value(case_val)?.eq(&val)?.as_bool()? {
                        return Ok(TerminatorResult::Switch { target: *target });
                    }
                }
                if let Some(def) = default {
                    Ok(TerminatorResult::Switch { target: *def })
                } else {
                    Err(InterpreterError::NoMatchingCase)
                }
            }

            IRTerminator::Unreachable { .. } => Err(InterpreterError::TypeError {
                expected: "reachable code",
                got: "unreachable terminator".to_string(),
            }),

            // Handle other terminators as needed
            _ => Err(InterpreterError::TypeError {
                expected: "supported terminator",
                got: format!("{:?}", term),
            }),
        }
    }

    // Evaluate an IRValue to a Value
    fn eval_value(&mut self, ir_value: &IRValue) -> Result<Value, InterpreterError> {
        match ir_value {
            IRValue::Int(i) => Ok(Value::Int(*i)),
            IRValue::Float(f) => Ok(Value::Float(*f)),
            IRValue::Bool(b) => Ok(Value::Bool(*b)),
            IRValue::String(s) => Ok(Value::String(s.clone())),
            IRValue::SSA(id) => {
                // Look up in current frame's locals
                self.call_stack
                    .current()
                    .ok_or(InterpreterError::TypeError {
                        expected: "active call frame",
                        got: "no active call frame".to_string(),
                    })?
                    .locals
                    .get(id)
                    .cloned()
                    .ok_or(InterpreterError::UndefinedValue(*id))
            }
            IRValue::FunctionRef(id) => {
                // For now, function references are handled as null values,
                // but in the future they should be callable
                Ok(Value::Ptr(HeapPtr { id: id.0 })) // Using the FunctionId's inner value
            }
            IRValue::Unit => Ok(Value::Null),
            _ => Err(InterpreterError::TypeError {
                expected: "evaluable value",
                got: format!("{:?}", ir_value),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interpreter_creation() {
        let module = IRModule::new(crate::ir::TargetInfo::vm_target());
        let interpreter = Interpreter::new(module);
        assert_eq!(interpreter.call_stack.len(), 0);
    }

    #[test]
    fn test_interpreter_error_types() {
        let error = InterpreterError::DivisionByZero;
        match error {
            InterpreterError::DivisionByZero => (), // This should match
            _ => panic!("Expected DivisionByZero error"),
        }

        let error = InterpreterError::TypeError {
            expected: "int",
            got: "float".to_string(),
        };
        match error {
            InterpreterError::TypeError { expected, got } => {
                assert_eq!(expected, "int");
                assert_eq!(got, "float");
            }
            _ => panic!("Expected TypeError"),
        }
    }
}
