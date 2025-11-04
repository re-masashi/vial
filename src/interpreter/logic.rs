use std::collections::HashMap;
use std::io;

use super::builtins::execute_builtin;
use super::error::InterpreterError;
use super::heap::{Heap, HeapObject};
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
    /// Creates a new Interpreter for the given IR module.
    ///
    /// The interpreter is initialized with an empty call stack, a fresh heap, and an empty globals map.
    ///
    /// # Parameters
    ///
    /// * `module` - The IRModule to be executed by the interpreter; ownership is moved into the returned interpreter.
    ///
    /// # Returns
    ///
    /// An Interpreter instance ready to run the provided module.
    ///
    pub fn new(module: IRModule) -> Self {
        Self {
            module,
            call_stack: CallStack::new(),
            heap: Heap::new(),
            globals: HashMap::new(),
        }
    }

    /// Print values with heap access for better formatting of complex objects like arrays
    fn print_values(&self, args: &[Value]) -> Result<(), InterpreterError> {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" "); // Add space between arguments like regular print
            }

            match arg {
                Value::Int(i) => print!("{}", i),
                Value::Float(f) => print!("{}", f),
                Value::Bool(b) => print!("{}", b),
                Value::String(s) => print!("{}", s),
                Value::Ptr(ptr) => {
                    // Try to print array contents if it's an array
                    match self.heap.get(*ptr) {
                        Ok(heap_obj) => match heap_obj {
                            HeapObject::Array { elements } => {
                                print!("[");
                                for (j, elem) in elements.iter().enumerate() {
                                    if j > 0 {
                                        print!(", ");
                                    }
                                    self.print_single_value(elem)?;
                                }
                                print!("]");
                            }
                            _ => print!("<ptr>"),
                        },
                        Err(_) => print!("<invalid-ptr>"),
                    }
                }
                Value::Null => print!("null"),
            }
        }
        Ok(())
    }

    /// Helper function to print a single value, handling nested structures recursively
    fn print_single_value(&self, value: &Value) -> Result<(), InterpreterError> {
        match value {
            Value::Int(i) => print!("{}", i),
            Value::Float(f) => print!("{}", f),
            Value::Bool(b) => print!("{}", b),
            Value::String(s) => print!("{}", s),
            Value::Ptr(ptr) => {
                // Handle nested arrays
                match self.heap.get(*ptr) {
                    Ok(heap_obj) => match heap_obj {
                        HeapObject::Array { elements } => {
                            print!("[");
                            for (i, elem) in elements.iter().enumerate() {
                                if i > 0 {
                                    print!(", ");
                                }
                                self.print_single_value(elem)?;
                            }
                            print!("]");
                        }
                        _ => print!("<ptr>"), // For non-array heap objects
                    },
                    Err(_) => print!("<invalid-ptr>"),
                }
            }
            Value::Null => print!("null"),
        }
        Ok(())
    }

    // Main entry point
    /// Execute the module starting at the specified function and return its resulting value.
    ///
    /// # Returns
    ///
    /// `Ok(Value)` with the function's return value, or `Err(InterpreterError)` if execution fails (for example: undefined function, type errors, division by zero, or other runtime/interpreter errors).
    ///
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
    /// Executes the given IR function with no arguments and returns its result.
    ///
    /// # Returns
    ///
    /// `Value` produced by the function, or an `InterpreterError` if execution fails.
    ///
    /// ```
    fn _execute_function(&mut self, function: &IRFunction) -> Result<Value, InterpreterError> {
        self.execute_function_with_args(function, vec![])
    }

    // Execute a single function with arguments
    /// Executes the given IR function with the provided argument values and drives control flow until the function returns.
    ///
    /// Binds passed `args` to the function's parameters, pushes a new call frame, then repeatedly executes basic blocks and their terminators, updating the current frame's block pointer until a `Return` terminator produces a value. If execution completes without an explicit return, `Value::Null` is returned.
    ///
    /// # Returns
    ///
    /// `Ok(Value)` with the function's return value, or `Ok(Value::Null)` if the function finishes without an explicit return. Returns `Err(InterpreterError)` for runtime or validation failures (for example: missing entry block, invalid basic block id, stack overflow, division-by-zero, or type errors).
    ///
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

        // Process each basic block until we return - use a Result to handle errors and cleanup
        let result = (|| -> Result<Value, InterpreterError> {
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
                            return Ok(value); // Will be handled by outer function
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
        })();

        match result {
            Ok(value) => {
                self.call_stack.pop(); // Clean up frame on success
                Ok(value)
            }
            Err(error) => {
                self.call_stack.pop(); // Clean up frame on error
                Err(error)
            }
        }
    }

    // Helper to get the ValueId that corresponds to a function argument
    /// Map an IR function argument to the interpreter's ValueId representation.
    ///
    /// The current interpreter uses the argument's memory slot ID as a proxy for its SSA ValueId.
    /// This returns the mapped `ValueId` when the argument can be represented, or `None` if no mapping
    /// is available.
    ///
    /// # Parameters
    ///
    /// - `arg`: the IR-level function argument to map; its `memory_slot` is used as the source of the ID.
    ///
    /// # Returns
    ///
    /// `Some(ValueId)` containing the mapped value identifier when mapping is possible, `None` otherwise.
    ///
    fn map_arg_to_value_id(&self, arg: &crate::ir::IRFunctionArg) -> Option<ValueId> {
        // In SSA form, function arguments should have corresponding ValueIds
        // For now, we'll use the memory slot ID as a proxy for the argument's ValueId
        // In a real system, the argument's binding_id or other metadata would provide the ValueId
        // This is a simplification for the interpreter
        Some(ValueId(arg.memory_slot.0)) // Using the memory slot ID as value ID
    }

    // Execute a basic block
    /// Executes a basic block's PHI nodes, instructions, and terminator within the current frame.
    ///
    /// The function evaluates all PHI nodes first, then all regular instructions, and finally
    /// executes the block's terminator (if present). It returns the resulting `TerminatorResult`
    /// when a terminator was executed, or `None` when the block has no terminator.
    ///
    /// # Parameters
    ///
    /// - `block`: the basic block to execute.
    /// - `_function`: the parent function containing the block (currently unused but provided for context).
    ///
    /// # Returns
    ///
    /// `Ok(Some(TerminatorResult))` if the block had a terminator and it was executed, `Ok(None)` if the
    /// block contained no terminator, or `Err(InterpreterError)` on execution failure.
    ///
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
    /// Executes a single IR instruction, mutating the interpreter's state (call stack, current frame locals, heap, and globals) as required.
    ///
    /// Supported instruction behaviors include arithmetic and logical operations, unary ops, value binding (`let`/`copy`/`select`/`phi`), compound value construction (arrays, tuples, structs, enums), memory operations (`load`, `store`, `allocate`, `field access`, `index`), function and builtin calls, and other SSA-style instructions. The method updates the current call frame's locals, performs heap allocations, and may push/pop frames indirectly by invoking other functions through `Call`.
    ///
    /// On failure, it returns an `InterpreterError` describing issues such as type mismatches, undefined functions, or division-by-zero.
    ///
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
                    BinOp::Add => {
                        // Check if both operands are arrays, if so concatenate them
                        if let (Value::Ptr(ptr1), Value::Ptr(ptr2)) = (&l, &r) {
                            match (self.heap.get(*ptr1)?, self.heap.get(*ptr2)?) {
                                (
                                    HeapObject::Array { elements: elems1 },
                                    HeapObject::Array { elements: elems2 },
                                ) => {
                                    // Create a new array with concatenated elements
                                    let mut new_elements = elems1.clone();
                                    new_elements.extend(elems2.clone());
                                    Value::Ptr(self.heap.allocate(HeapObject::Array {
                                        elements: new_elements,
                                    }))
                                }
                                _ => l.add(&r)?, // Fall back to regular addition for other types
                            }
                        } else {
                            l.add(&r)? // Regular addition for non-pointer types
                        }
                    }
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

                let ret = match builtin_name.as_str() {
                    "push" => {
                        if arg_values.len() != 2 {
                            return Err(InterpreterError::TypeError {
                                expected: "two arguments (array and value)",
                                got: format!("{} arguments", arg_values.len()),
                            });
                        }

                        // First argument should be a pointer to an array
                        let array_ptr = arg_values[0].as_ptr()?;
                        let value_to_push = arg_values[1].clone();

                        // Get the array from the heap and modify it
                        let heap_obj = self.heap.get_mut(array_ptr).map_err(|_| {
                            InterpreterError::TypeError {
                                expected: "valid array pointer",
                                got: "invalid pointer".to_string(),
                            }
                        })?;

                        match heap_obj {
                            super::heap::HeapObject::Array { elements } => {
                                // Push the value to the array
                                elements.push(value_to_push);
                                Value::Null // push! returns unit
                            }
                            _ => {
                                return Err(InterpreterError::TypeError {
                                    expected: "array object",
                                    got: "non-array object".to_string(),
                                });
                            }
                        }
                    }
                    "print" | "println" => {
                        // Handle print/println with heap access for better array formatting
                        self.print_values(&arg_values)?;
                        if builtin_name == "println" {
                            println!();
                        }
                        Value::Null
                    }
                    "input" => {
                        // Handle input builtin
                        self.print_values(&arg_values)?; // Print prompt if any
                        std::io::Write::flush(&mut std::io::stdout())
                            .map_err(InterpreterError::IOError)?;
                        let mut buffer = String::new();
                        io::stdin()
                            .read_line(&mut buffer)
                            .map_err(InterpreterError::IOError)?;
                        Value::String(buffer.trim().to_string())
                    }
                    _ => execute_builtin(builtin_name, arg_values)?,
                };

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

                        match obj {
                            super::heap::HeapObject::Struct { fields, .. } => {
                                let field_val = fields.get(field).cloned().ok_or(
                                    InterpreterError::TypeError {
                                        expected: "valid struct field",
                                        got: format!("field id: {:?}", field),
                                    },
                                )?;
                                self.call_stack
                                    .current()
                                    .unwrap()
                                    .locals
                                    .insert(*result, field_val);
                            }
                            super::heap::HeapObject::Enum {
                                variant_id, data, ..
                            } => {
                                if field.0 == 0 {
                                    // Field 0 is the discriminant (variant_id)
                                    let discriminant_val = Value::Int(variant_id.0 as i64);
                                    self.call_stack
                                        .current()
                                        .unwrap()
                                        .locals
                                        .insert(*result, discriminant_val);
                                } else {
                                    // Convert field to payload index (subtract 1 to account for discriminant)
                                    let payload_index = field.0 - 1;

                                    if payload_index < data.len() {
                                        let field_val = data[payload_index].clone();
                                        self.call_stack
                                            .current()
                                            .unwrap()
                                            .locals
                                            .insert(*result, field_val);
                                    } else {
                                        return Err(InterpreterError::TypeError {
                                            expected: "valid enum payload field",
                                            got: format!(
                                                "field id: {:?} (index {} >= {})",
                                                field,
                                                payload_index,
                                                data.len()
                                            ),
                                        });
                                    }
                                }
                            }
                            _ => {
                                return Err(InterpreterError::TypeError {
                                    expected: "struct or enum object",
                                    got: "non-struct/enum object".to_string(),
                                });
                            }
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

                match obj {
                    super::heap::HeapObject::Struct { fields, .. } => {
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
                    }
                    super::heap::HeapObject::Enum {
                        variant_id, data, ..
                    } => {
                        if field_id.0 == 0 {
                            // Field 0 is the discriminant (variant_id)
                            let discriminant_val = Value::Int(variant_id.0 as i64);
                            self.call_stack
                                .current()
                                .unwrap()
                                .locals
                                .insert(*result, discriminant_val);
                        } else {
                            // Convert field_id to payload index (subtract 1 because field_id 0 is the discriminant)
                            // This follows the pattern in extract_enum_field where field_id = field_index + 1
                            let payload_index = field_id.0 - 1;

                            if payload_index < data.len() {
                                let field_val = data[payload_index].clone();
                                self.call_stack
                                    .current()
                                    .unwrap()
                                    .locals
                                    .insert(*result, field_val);
                            } else {
                                return Err(InterpreterError::TypeError {
                                    expected: "valid enum payload field",
                                    got: format!(
                                        "field id: {:?} (index {} >= {})",
                                        field_id,
                                        payload_index,
                                        data.len()
                                    ),
                                });
                            }
                        }
                    }
                    _ => {
                        return Err(InterpreterError::TypeError {
                            expected: "struct or enum object",
                            got: "non-struct/enum object".to_string(),
                        });
                    }
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
    /// Evaluate an IR terminator and produce the interpreter's next control-flow action.
    ///
    /// Interprets `IRTerminator` variants and returns a `TerminatorResult` that describes the
    /// next step (function return, block jump, conditional branch, or switch target). On error,
    /// returns an `InterpreterError` describing the failure (e.g., type mismatch, no matching
    /// switch case, unreachable terminator).
    ///
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
    /// Evaluate an IRValue into a runtime Value.
    ///
    /// For SSA values, looks up the corresponding runtime value in the current call frame's locals and
    /// returns an `UndefinedValue` error if not found or a `TypeError` if no active frame exists.
    /// `FunctionRef` values are represented as a pointer using the function's inner id. Literal and
    /// unit IR values map directly to their corresponding runtime Value variants.
    ///
    /// # Returns
    ///
    /// `Ok(Value)` containing the evaluated runtime value, or an `InterpreterError` describing why the
    /// value could not be evaluated (e.g., missing frame, undefined SSA id, or unexpected IRValue kind).
    ///
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
    fn test_terminator_result_debug() {
        // Test that the TerminatorResult enum can be debug formatted
        let return_result = TerminatorResult::Return(Value::Int(42));
        assert!(format!("{:?}", return_result).contains("Return"));

        let jump_result = TerminatorResult::Jump(BasicBlockId(1));
        assert!(format!("{:?}", jump_result).contains("Jump"));

        let branch_result = TerminatorResult::Branch {
            then_block: BasicBlockId(2),
            else_block: BasicBlockId(3),
            condition: true,
        };
        assert!(format!("{:?}", branch_result).contains("Branch"));

        let switch_result = TerminatorResult::Switch {
            target: BasicBlockId(4),
        };
        assert!(format!("{:?}", switch_result).contains("Switch"));
    }
}
