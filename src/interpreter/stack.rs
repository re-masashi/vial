use std::collections::HashMap;

use super::value::Value;
use crate::ir::{BasicBlockId, FunctionId, ValueId};

#[derive(Debug)]
pub struct CallFrame {
    pub function_id: FunctionId,
    pub current_block: BasicBlockId,
    pub last_block: Option<BasicBlockId>, // Track which block led to the current block
    pub locals: HashMap<ValueId, Value>,  // SSA values
}

#[derive(Debug, Default)]
pub struct CallStack {
    frames: Vec<CallFrame>,
}

impl CallStack {
    pub fn new() -> Self {
        Self { frames: Vec::new() }
    }

    pub fn push(&mut self, function_id: FunctionId, current_block: BasicBlockId) -> &mut CallFrame {
        self.frames.push(CallFrame {
            function_id,
            current_block,
            last_block: None,
            locals: HashMap::new(),
        });
        self.frames.last_mut().unwrap()
    }

    pub fn pop(&mut self) -> Option<CallFrame> {
        self.frames.pop()
    }

    pub fn current(&mut self) -> Option<&mut CallFrame> {
        self.frames.last_mut()
    }

    pub fn current_function_id(&self) -> Option<FunctionId> {
        self.frames.last().map(|frame| frame.function_id)
    }

    pub fn len(&self) -> usize {
        self.frames.len()
    }

    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_call_stack_creation() {
        let stack = CallStack::new();
        assert!(stack.is_empty());
        assert_eq!(stack.len(), 0);
    }

    #[test]
    fn test_call_stack_operations() {
        let mut stack = CallStack::new();

        // Push a frame
        let func_id = FunctionId(1);
        let block_id = BasicBlockId(0);
        stack.push(func_id, block_id);

        assert!(!stack.is_empty());
        assert_eq!(stack.len(), 1);
        assert_eq!(stack.current_function_id(), Some(func_id));

        // Add a local to the current frame
        if let Some(frame) = stack.current() {
            frame.locals.insert(ValueId(1), Value::Int(42));
            assert_eq!(frame.function_id, func_id);
            assert_eq!(frame.current_block, block_id);
        }

        // Pop the frame
        let popped_frame = stack.pop().unwrap();
        assert_eq!(popped_frame.function_id, func_id);
        assert_eq!(popped_frame.current_block, block_id);
        assert_eq!(popped_frame.locals.get(&ValueId(1)), Some(&Value::Int(42)));

        assert!(stack.is_empty());
        assert_eq!(stack.len(), 0);
    }

    #[test]
    fn test_multiple_frames() {
        let mut stack = CallStack::new();

        // Push multiple frames
        stack.push(FunctionId(1), BasicBlockId(0));
        stack.push(FunctionId(2), BasicBlockId(1));
        stack.push(FunctionId(3), BasicBlockId(2));

        assert_eq!(stack.len(), 3);
        assert_eq!(stack.current_function_id(), Some(FunctionId(3)));

        // Pop frames and verify
        assert_eq!(stack.pop().unwrap().function_id, FunctionId(3));
        assert_eq!(stack.current_function_id(), Some(FunctionId(2)));

        assert_eq!(stack.pop().unwrap().function_id, FunctionId(2));
        assert_eq!(stack.current_function_id(), Some(FunctionId(1)));

        assert_eq!(stack.pop().unwrap().function_id, FunctionId(1));
        assert!(stack.is_empty());
    }

    #[test]
    fn test_locals_in_frames() {
        let mut stack = CallStack::new();

        // Push a frame and add locals
        let frame = stack.push(FunctionId(1), BasicBlockId(0));
        frame
            .locals
            .insert(ValueId(10), Value::String("hello".to_string()));
        frame.locals.insert(ValueId(20), Value::Bool(true));

        // Access locals through the current frame
        if let Some(frame) = stack.current() {
            assert_eq!(
                frame.locals.get(&ValueId(10)),
                Some(&Value::String("hello".to_string()))
            );
            assert_eq!(frame.locals.get(&ValueId(20)), Some(&Value::Bool(true)));
        }
    }
}
